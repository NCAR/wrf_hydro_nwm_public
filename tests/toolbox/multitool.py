from pathlib import *

# A libpath way to 'rm -rf'

def delete_dir_and_contents(pth):
    for sub in pth.iterdir():
        if sub.is_dir():
            delete_dir_and_contents(sub)
        else:
            sub.unlink()
    pth.rmdir()
