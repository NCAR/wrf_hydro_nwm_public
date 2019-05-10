import json
import urllib.request
from argparse import ArgumentParser


def get_release_asset(download_dir: str,
                      repo_name: str,
                      tag: str,
                      asset_name: str = 'testcase'):
    """Function to download an asset from a specified public github repository
        Args:
            download_dir: The local directory to hold downloaded assets
            repo_name: The repository name, e.g. NCAR/wrf_hydro_nwm_public
            tag: The release tag, e.g. v5.0.1
            asset_name: The name of the asset, can be partial
    """
    url = "https://api.github.com/repos/" + repo_name + "/releases/tags/" + tag

    # Get json data from api as a string
    fp = urllib.request.urlopen(url)
    json_string = fp.read().decode("utf8")
    fp.close()

    # load into list of dicts
    asset_list = json.loads(json_string)['assets']

    # Iterate over assets and find asset url from asset name matching
    for asset in asset_list:
        if asset_name in asset['name']:
            asset_url = asset['browser_download_url']
            full_asset_name = asset['name']

            #download asset
            print('downloading asset ' + full_asset_name + ' to ' + download_dir)
            download_filepath = str(download_dir) + '/' + full_asset_name
            urllib.request.urlretrieve(asset_url, download_filepath)

def main():
    parser = ArgumentParser()
    parser.add_argument("--download_dir",
                        dest="download_dir",
                        help="The local directory to hold downloaded assets")
    parser.add_argument("--repo_name",
                        dest="repo_name",
                        help="The repository name, e.g. NCAR/wrf_hydro_nwm_public")
    parser.add_argument("--tag",
                        dest="tag",
                        help="The release tag, e.g. v5.0.1")
    parser.add_argument("--asset_name",
                        dest="asset_name",
                        default='testcase',
                        help="The name of the asset, can be partial")

    args = parser.parse_args()

    get_release_asset(download_dir = args.download_dir,
                      repo_name = args.repo_name,
                      tag = args.tag,
                      asset_name = args.asset_name)

if __name__ == '__main__':
    main()

