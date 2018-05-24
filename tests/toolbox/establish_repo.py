import sys
import subprocess
from pathlib import *
from color_logs import log
from multitool import delete_dir_and_contents

def form_authtoken_url(repo_tag, candidate_spec, user_spec):

    # Allow the authtoken to be
    # 1) a file or
    # 2) actually in the dictionary.
    # Handle a blank authoken too.
    
    # Assume it's in the dictionary.
    authtoken = user_spec['github']['authtoken']

    # If authoken is not blank, check if its a file. If so, get the
    # authtoken from the file.
    if authtoken is not None and Path(authtoken).exists():
        authtoken = subprocess.run(["cat", "/Users/james/.github_authtoken"])
        authtoken = authtoken.communicate()[0].decode("utf-8")

    if authtoken is None:
        auth_info = user_spec['github']['username']
    else:
        auth_info = user_spec['github']['username']+':'+authtoken

    url = 'https://'+auth_info+'@github.com/'+candidate_spec[repo_tag]['fork']
    return(url)


def clone_repo(repo_tag, candidate_spec, user_spec, dir_for_clone):

    # If ssh_priv_key is NOT supplied, use https, else ssh.
    if user_spec['github']['ssh_priv_key'] is None:
        protocol = 'https'
        url = form_authtoken_url(repo_tag, candidate_spec, user_spec)
    else:
        protocol = 'ssh'
        url = 'git@github.com:'+candidate_spec[repo_tag]['fork']

    log.debug('Cloning ' + candidate_spec[repo_tag]['fork'] +
              ' into ' + str(dir_for_clone) + ' using ' + protocol + ' ...')

    process = subprocess.run(['git', 'clone', url, dir_for_clone])

    if process.returncode != 0:
        return(False)
    return(True)


def establish_repo(repo_tag, candidate_spec, user_spec):

    repo_tag_base = repo_tag.split('_')[0]
    log.debug('')
    log.info(repo_tag_base.title() + ' repo')

    if candidate_spec[repo_tag]['local_path'] is None:

        # The case when local_path is not set.
        candidate_spec[repo_tag]['local_path_setby'] = 'fork & commitish'
        dir_for_clone = Path(candidate_spec['repos_dir'] + '/' + repo_tag_base)
        #print('dir_for_clone: ',dir_for_clone)

        candidate_spec[repo_tag]['local_path'] = dir_for_clone

        if dir_for_clone.exists():
            delete_dir_and_contents(dir_for_clone)
        Path.mkdir(dir_for_clone, parents=True)

        clone_repo(repo_tag, candidate_spec, user_spec, dir_for_clone)

        # check out the commitish
        commitish = candidate_spec[repo_tag]['commitish']
        if commitish is None:
            commitish = 'master'

        log.debug('Checking out commitish: '+commitish)
        subprocess.run(['git', 'checkout', commitish], cwd=dir_for_clone)
        git_log = subprocess.run(['git', 'log', '-n1'], stdout=subprocess.PIPE, cwd=dir_for_clone)
        log.debug(git_log.stdout.decode('utf-8'))

    else:

        candidate_spec[repo_tag]['local_path_setby'] = 'candidate spec'
