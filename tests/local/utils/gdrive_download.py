from argparse import ArgumentParser

import requests


def download_file_from_google_drive(id, destination):
    print('downloading google drive file id ' + id + ' to ' + destination)
    URL = "https://docs.google.com/uc?export=download"

    session = requests.Session()

    response = session.get(URL, params={'id': id}, stream=True)
    token = get_confirm_token(response)

    if token:
        params = {'id': id, 'confirm': token}
        response = session.get(URL, params=params, stream=True)

    save_response_content(response, destination)



def get_confirm_token(response):
    for key, value in response.cookies.items():
        if key.startswith('download_warning'):
            return value

    return None


def save_response_content(response, destination):
    CHUNK_SIZE = 32768

    with open(destination, "wb") as f:
        for chunk in response.iter_content(CHUNK_SIZE):
            if chunk:  # filter out keep-alive new chunks
                f.write(chunk)


def main():

    parser = ArgumentParser()
    parser.add_argument("--file_id",
                        dest="file_id",
                        help="Google drive file ID. Get from shareable link")
    parser.add_argument("--dest_file",
                        dest="dest_file",
                        help="Full path including filename for downloaded file.")

    args = parser.parse_args()
    file_id = args.file_id
    dest_file = args.dest_file

    download_file_from_google_drive(file_id, dest_file)

if __name__ == "__main__":
    main()