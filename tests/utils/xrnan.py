# tool copied from wrfhydropy

from multiprocessing import Pool
import pathlib
import sys
from typing import Union
import xarray as xr


def check_nans(arg_dict):
    var_name = arg_dict['var_name']
    if 'path' in arg_dict.keys():
        path = arg_dict['path']
        ds = xr.open_dataset(path, mask_and_scale=False)
    else:
        ds = arg_dict['ds']
    if ds[var_name].isnull().any().values:
        return var_name
    else:
        return None


def xrnan(
    dataset_or_path: Union[str, pathlib.Path, xr.Dataset],
    log_file: str = None,
    exclude_vars: list = [],
    chunks=None,
    n_cores: int = 1
) -> int:
    # Set filepath to strings
    if not isinstance(dataset_or_path, xr.Dataset):
        ds = xr.open_dataset(str(dataset_or_path), mask_and_scale=False, chunks=chunks)
    else:
        ds = dataset_or_path

    # Looping on variables is much faster for small applications and parallelizable
    # for larger ones.
    if n_cores < 2 or isinstance(dataset_or_path, xr.Dataset):
        nan_vars = []
        for var_name in ds.variables:
            nan_vars.append(check_nans({'var_name': var_name, 'ds': ds}))
        ds.close()
    else:
        # The following ds.close() is CRITICAL to the correct results being returned by
        # multiprocessing
        ds.close()
        the_args = [{'var_name': var_name, 'path': dataset_or_path}
                    for var_name in ds.variables]
        with Pool(n_cores) as pool:
            nan_vars = pool.map(check_nans, the_args)

    nan_vars_2 = [var for var in nan_vars if var is not None]

    if len(nan_vars_2) == 0:
        return None
    else:
        for var in nan_vars_2:
            print(str(dataset_or_path), ': variable "' + var + ''
                  '' + '" contains NaNs')
        return {'vars': nan_vars_2}


def parse_arguments():

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--path", metavar="FILE", type=str, required=True,
        help="File to check for NaNs."
    )
    parser.add_argument(
        "--log_file", metavar="FILE", type=str, required=True,
        help="File to log potential differences to. "
        "Existing file is clobbered."
    )
    args = parser.parse_args()
    path = args.path
    log_file = args.log_file
    return path, log_file


if __name__ == "__main__":

    path, log_file = parse_arguments()
    ret = xrnan(path, log_file=log_file)
    if ret is None:
        exit_code = 0
    else:
        exit_code = 1
    sys.exit(exit_code)
