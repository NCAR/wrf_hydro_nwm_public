##############################################
#
# This script takes a list of image files and uploads them as a comment
# on the selected Github pull request.
#
# Images are added as a commit to an orphaned 'images' branch on the selected
# repo, where they can be linked to using the commit hash. Images are stored
# at the top level of the repo, under q directory 'images_{pull request number}'
#
# The 'images' branch is an orphaned branch - it is invisible to the Github UI
# and isn't included in clone operations (unless the --mirror option is used)
#
#
##############################################

# check python version. 'f' formatting strings aren't available
# until python v3.6
import sys
if sys.version_info[0] < 3 or \
        (sys.version_info[0] == 3 and sys.version_info[1] < 6):
    print("Requires python version 3.6 or later")
    exit(-1)

import os
from github import Github
import argparse
import subprocess

# set up logging for this module
import logging
logging.basicConfig(format="%(levelname)s, %(asctime)s %(message)s")
logger = logging.getLogger("attach_plots_to_pr")
logger.setLevel(logging.DEBUG)

# location for writing temporary local files, e.g. the cloned repository
TEMP_DIR = "/tmp"

# The name of the images repository
IMAGE_REPO = "hydro-automerge/wrf_hydro_nwm_public_images"

# The name of the images branch and branch reference
IMAGEREF = "images/image-ref"


def get_options():
    """
    Get command line options
    Returns: processed options, or None
    """

    parser = argparse.ArgumentParser(description="Upload plot images as a Github PR comment")

    parser.add_argument('images', metavar='image', nargs='+', help='A list of image files to attach')
    parser.add_argument("-t", "--token", dest="token", default=None, help="Github authentication token, or path to a" \
                        + " file containing a token")
    parser.add_argument("-r", "--repo", dest="repo", required=True, help="GHithub repository name")
    parser.add_argument("-p", "--pull", dest="pull", required=True, type=int, help="Pull request number")
    parser.add_argument("--title", dest="title", default=None, help="Title to give to this set of plots")

    parser.add_argument("-d", dest="debug", action="store_true", default=False, help="Print debug messages")

    options = parser.parse_args()

    if len(options.images) < 1:
        print("ERROR: Must supply one or more image files!")
        return None

    if options.token is not None and os.path.isfile(options.token):
        try:
            options.token = open(options.token).read()[:-1]
        except Exception as e:
            print(f"Problem reading token file: {e}")
            return None

    logger.setLevel(logging.DEBUG if options.debug is True else logging.INFO)

    return options


def login(token=None):
    """
    Login to the github API using the supplied token
    @param token: The personal access token to use to read/write from Github, or None
    @return: A logged-in pygithub object, or None
    """
    try:
        gh = Github(token)
        return gh
    except Exception as e:
        logger.error(f"Problem logging into github: {e}")
        return None


def runcmd(cmd, pwd=None, logError=True):
    """
    Run a shell command using the subprocess module
    @param cmd: The command to run
    @param pwd: The working directory to issue the command from
    @param logError: If True, log any exceptions
    @return: The process output, or None on error
    """
    logger.debug(f"Executing command: {cmd}")
    try:
        output = subprocess.check_output(cmd, shell=True, cwd=pwd, stderr=subprocess.STDOUT)
        return output.decode("utf-8")
    except subprocess.CalledProcessError as e:
        if logError:
            logger.error(e)
            logger.error(e.output)
        return None


def clone_repo(repo, outdir, token=None):
    """
    Clone the repository and check out the orphaned images branch
    @param repo: The repository name on Github
    @param outdir: The local path to the cloned repository
    @param token: The personal access token to use to read/write from Github, or None
    @return: True on success, False on error
    """
    try:
        if not os.path.isdir(outdir):
            login = "" if token is None else f"x-access-token:{token}@"
            runcmd(f"git clone https://{login}github.com/{repo}", TEMP_DIR, logError=False)

        logging.debug("Checking out latest images commit")

        runcmd(f"git pull origin {IMAGEREF}", outdir, logError=False)
        runcmd("git checkout FETCH_HEAD", outdir, logError=False)
        logs = runcmd("git log --format=%H", outdir)
        if not logs:
            raise Exception("Unable to get commit history")

        latest = logs.split("\n")[0]
        if runcmd(f"git checkout {latest}", outdir) is None:
            raise Exception("Unable to checkout latest commit")

        return True

    except Exception as e:
        logger.error(f"Unable to clone repository: {e}")
        return False


def add_images(images, outdir, pull):
    """
    Add images to the images branch, commit them and push the
    commit to github
    @param images: A list of images to upload, including the local path to the images
    @param outdir: The path to the locally-cloned repository
    @param pull: The pull request number
    @return: The image commit hash, or False on error
    """
    logger.debug("Uploading images to github images branch")

    imagedir = f"{outdir}/images_{pull}"
    os.makedirs(imagedir, exist_ok=True)

    try:
        # remove duplicate images first, then commit the deletion
        # so they can get reattached to the new commit
        removed = False
        for img in images:
            imgname = os.path.basename(img)
            if os.path.isfile(f"{imagedir}/{imgname}"):
                runcmd(f"git rm {imgname}", imagedir)
                removed = True

        if removed:
            logger.debug("Removing duplicate images from repository")
            if runcmd(f"git commit -m 'removing images'", outdir) is None:
                raise Exception("Problem committing removal of duplicate images")

        logger.debug(f"Copying images to {imagedir}")
        for img in images:
            if runcmd(f"cp {img} {imagedir}/.") is None:
                raise Exception("Not all image files could be copied")

        logger.debug("Adding images to commit")
        if runcmd(f"git add images_{pull}", outdir) is None:
            raise Exception("Problem committing new images")

        logger.debug("Commiting images")
        if runcmd(f"git commit -m 'adding images'", outdir) is None:
            raise Exception("Problem committing new images")

        logger.debug("Pushing commit to github")
        if runcmd(f"git push origin HEAD:refs/{IMAGEREF}", outdir) is None:
            raise Exception("Problem pushing commits")

        logger.debug("Getting commit hash")
        logs = runcmd("git log --format=%H", outdir)
        if logs is None:
            raise Exception("Couldn't get latest commit hash after push")

        sha = logs.split("\n")[0]

        logger.debug("Done uploading images")

        return sha

    except Exception as e:
        logger.error(f"Problem uploading images: {e}")
        return False


def create_pull_comment(gh, repo, sha, images, pull, title=None):
    """
    Add uploaded images to the pull request comments using the commit hash
    @param gh: The logged-in pygithub object
    @param repo: The name of the Github repository
    @param sha: The commit hash from the uploaded image commit
    @param images: A list of images to attach
    @param pull: The pull request number
    @param title: The title to give to this set of plots
    @return: True on success, False on error
    """
    try:
        logger.debug(f"Getting pull request #{pull}")
        pr = gh.get_repo(repo).get_pull(pull)

        logger.debug("Assembling comment images")
        comment = ""

        if title:
            comment += "## " + title + "\n"

        for img in images:
            img = os.path.basename(img)
            path = f"https://github.com/{IMAGE_REPO}/blob/{sha}/images_{pull}/{img}"
            comment += f"![alt text]({path}?raw=true)\n"

        logger.debug("Adding comment to pull request")
        pr.create_issue_comment(comment)

        logger.debug("Comment added.")
        return True

    except Exception as e:
        logger.error(f"Problem opening pull request: {e}")
        return False


def run():
    """
    Get command line options and run the algorithm
    @return: True on success, False on error
    """
    options = get_options()
    if not options: return False

    gh = login(options.token)
    if not gh: return False

    reponame = IMAGE_REPO.split("/")[-1]
    outdir = f"{TEMP_DIR}/{reponame}"

    if not clone_repo(IMAGE_REPO, outdir, token=options.token): return False

    sha = add_images(options.images, outdir, options.pull)
    if not sha: return False

    if not create_pull_comment(gh, options.repo, sha, options.images, options.pull, options.title):
        return False

    logging.debug("Done")

    return True


if __name__ == "__main__":
    if run():
        exit(0)
    exit(-1)
