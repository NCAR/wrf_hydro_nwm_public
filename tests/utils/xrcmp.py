#!/usr/bin/env python3

# tool copied from wrfhydropy

# Example Usage
# ipython --pdb  xrcmp.py -- \
#     --candidate conus_test/201806012300.RTOUT_DOMAIN1 \
#     --reference conus_test/201806020000.RTOUT_DOMAIN1 \
#     --n_cores 8 \
#     --log_file log.txt

import math
from multiprocessing import Pool
import os
import pathlib
import sys
# import time
import xarray as xr


# A dictionary of chunks for various variables for CONUS testing
# These are for the larger fields which need some control
conus_chunks_dict = {
    # RTOUT variables to control
    'SOIL_M': {},  # with {} maxes out at < 18% memory when files do NOT match
    # HYDRO_RST variables: None currently
}


# # A decorator/closure to check timings.
# def stopwatch(the_func):
#     def the_closure(*args, **kw):
#         ts = time.time()
#         result = the_func(*args, **kw)
#         te = time.time()
#         print('Timing: ' + the_func.__name__ + ' took ', round(te - ts, 2),' seconds.')
#         return result
#     return the_closure


def calc_stats(arg_tuple):
    key = arg_tuple[0]
    can_file = arg_tuple[1]
    ref_file = arg_tuple[2]
    chunks = arg_tuple[3]
    exclude_vars = arg_tuple[4]

    # ignore excluded vars
    if key in exclude_vars:
        return None

    if chunks is None:
        chunks = {}  # default is no chunks
        if key in conus_chunks_dict:
            chunks = conus_chunks_dict[key]

    can_ds = xr.open_dataset(can_file, chunks=chunks, mask_and_scale=False)
    ref_ds = xr.open_dataset(ref_file, chunks=chunks, mask_and_scale=False)

    # Check for variables in reference and not in candidate?
    # Check for variables in candidate and not in reference?

    if can_ds[key].equals(ref_ds[key]):
        return None

    else:
        cc = can_ds[key]
        rr = ref_ds[key]

        if '|S' in str(cc.dtype):

            # Deal with strings
            # Use boolean indexer with `where` function
            indexer = (cc != rr).compute()
            nz_xr = cc.where(indexer, drop=True)
            if len(nz_xr) == 0:
                return None
            else:
                the_count = nz_xr.count().load().item(0)
                inf = float('inf')
                result = {
                    'Variable': key,
                    'Count': the_count,
                    'Sum': inf,
                    'Min': inf,
                    'Max': inf,
                    'Range': inf,
                    'Mean':  inf,
                    'StdDev': inf
                }
                return result

        else:
            # All non-string types
            cc = cc.astype(float)
            rr = rr.astype(float)

            # THIS NEEDS REMOVED AFTER TESTING IS COMPLETE
            # FOR convenience of comparing two files at different times.
            # if 'time' in rr.coords:
            #    rr['time'] = cc.time
            # if key == 'time':
            #    rr.values = cc.values

            diff_da = cc - rr
            diff_xr = xr.DataArray(diff_da.compute())
            # TODO: This threshold should be type dependent
            nz_xr = diff_xr.where(abs(diff_xr) > 0.000000, drop=True)
            if len(nz_xr) == 0:
                return None

            the_count = nz_xr.count().load().item(0)
            the_sum = nz_xr.sum().load().item(0)
            the_min = nz_xr.min().load().item(0)
            the_max = nz_xr.max().load().item(0)
            the_range = the_max - the_min
            the_mean = the_sum / the_count
            the_z = (nz_xr - the_mean)
            the_std = math.sqrt((the_z * the_z).sum() / the_count)
            del the_z

            result = {
                'Variable': key,
                'Count': the_count,
                'Sum': the_sum,
                'Min': the_min,
                'Max': the_max,
                'Range': the_range,
                'Mean':  the_mean,
                'StdDev': the_std
            }
            return result


# @stopwatch
def xrcmp(
    can_file: str,
    ref_file: str,
    log_file: str,
    n_cores: int = 1,
    chunks={},
    exclude_vars: list = [],
    interactive: bool = False
) -> int:

    if exclude_vars is None:
        exclude_vars = []

    # Delete log file first
    # Should write a log file that says nothing yet determined?
    log_file = pathlib.Path(log_file)
    if log_file.exists() and not interactive:
        log_file.unlink()

    # Dont chunk, this is just a meta-data read.
    can_ds = xr.open_dataset(can_file)
    ref_ds = xr.open_dataset(ref_file)

    # May need to check that they have the same vars.
    can_vars = set([kk for kk in can_ds.variables.keys()])
    ref_vars = set([kk for kk in ref_ds.variables.keys()])
    have_same_variables = can_vars.difference(ref_vars) == set([])
    can_ds.close()  # These are likely critical to the success
    ref_ds.close()  # of multiprocessing

    # TODO: Check that the meta data matches

    # This is quick if not true
    # ds_equal = can_ds.equals(re_ds)
    # if not ds_equal:

    if n_cores == 1:
        all_stats_list = []
        for key, val in can_ds.items():
            result = calc_stats(
                (key, can_file, ref_file, chunks, exclude_vars))
            all_stats_list.append(result)
    else:
        the_args = [
            (key, can_file, ref_file, chunks, exclude_vars) for key in can_ds.keys()]
        with Pool(n_cores) as pool:
            all_stats_list = pool.map(calc_stats, the_args)

    all_stats = {item['Variable']: item for item in all_stats_list if item is not None}

    diff_var_names = sorted(all_stats.keys())
    if not diff_var_names:
        with open(log_file, 'a') as opened_file:
            opened_file.write(f"Files are identical: {os.path.basename(ref_file)}\n")
        return 0

    # Formatting:

    # The goal is to print something like this which is what nccmp outputs.
    # channel_rt
    #      Variable Group  Count       Sum  ...       Max     Range      Mean    StdDev
    # 0  streamflow     /    162  0.003022  ...  0.003832  0.004315  0.000019  0.000361
    # 1       nudge     /      4 -0.001094  ...  0.000093  0.001272 -0.000274  0.000605
    # 2   q_lateral     /    170  0.000345  ...  0.000700  0.001145  0.000002  0.000086
    # 3    velocity     /    165  0.010788  ...  0.005488  0.006231  0.000065  0.000503
    # 4        Head     /    177  0.002717  ...  0.002662  0.003292  0.000015  0.000258

    stat_names = sorted(all_stats[diff_var_names[0]].keys())
    stat_lens = {}  # the length/width of each column/stat
    n_dec = 3  # number of decimals for floats
    n_dec_p = n_dec + 1  # plus the decimal point

    # The format for each type, where full_len sepcifices the width of the field.
    type_fmt = {
        str: '{{:{full_len}}}',
        int: '{{:{full_len}}}',
        float: '{{:{full_len}.' + str(n_dec) + 'f}}'
    }

    # Now solve the full_len field widths for all stats. Do this by
    # just formatting each as it's type and finding the max (best way
    # to handle negatives). For floats, take the integer part to find
    # its length to the left of the decimal.
    for stat_name in stat_names:
        all_lens = []
        for key, val in all_stats.items():
            the_val = val[stat_name]
            the_type = type(the_val)
            the_fmt0 = type_fmt[the_type]
            if the_type is str:
                full_len = len(the_val)
            elif not math.isfinite(the_val):
                full_len = len(str(the_val))
            else:
                full_len = len(str(int(the_val)))
                if the_type is float:
                    full_len = full_len + n_dec_p
            the_fmt = the_fmt0.format(**{'full_len': full_len})
            the_string = the_fmt.format(*[the_val])
            all_lens.append(len(the_string))

        stat_lens[stat_name] = max(all_lens)

    header_string = (
        '{Variable:>' + str(stat_lens['Variable']) + '}  '
        '{Count:>' + str(stat_lens['Count']) + '}  '
        '{Sum:>' + str(stat_lens['Sum']) + '}  '
        '{Min:>' + str(stat_lens['Min']) + '}  '
        '{Max:>' + str(stat_lens['Max']) + '}  '
        '{Range:>' + str(stat_lens['Range']) + '}  '
        '{Mean:>' + str(stat_lens['Mean']) + '}  '
        '{StdDev:>' + str(stat_lens['StdDev']) + '}  \n'
    )

    var_string = (
        '{Variable:>' + str(stat_lens['Variable']) + '}  '
        '{Count:>' + str(stat_lens['Count']) + '}  '
        '{Sum:>' + str(stat_lens['Sum']) + '.' + str(n_dec) + 'f}  '
        '{Min:>' + str(stat_lens['Min']) + '.' + str(n_dec) + 'f}  '
        '{Max:>' + str(stat_lens['Max']) + '.' + str(n_dec) + 'f}  '
        '{Range:>' + str(stat_lens['Range']) + '.' + str(n_dec) + 'f}  '
        '{Mean:>' + str(stat_lens['Mean']) + '.' + str(n_dec) + 'f}  '
        '{StdDev:>' + str(stat_lens['StdDev']) + '.' + str(n_dec) + 'f}  \n'
    )

    header_dict = {name: name for name in stat_names}
    the_header = header_string.format(**header_dict)

    if len(all_stats) > 0:
        print(the_header, end='')
    for key in all_stats.keys():
        print(var_string.format(**all_stats[key]), end='')

    with open(log_file, 'w') as opened_file:
        opened_file.write(the_header)
        for key in all_stats.keys():
            opened_file.write(var_string.format(**all_stats[key]))

    return 1


def parse_arguments():

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--candidate", metavar="FILE", type=str, required=True,
        help="Candidate file to compare."
    )
    parser.add_argument(
        "--reference", metavar="FILE", type=str, required=True,
        help="Reference file to compare."
    )
    parser.add_argument(
        "--log_file", metavar="FILE", type=str, required=True,
        help="File to log potential differences to. "
        "Existing file is clobbered."
    )
    parser.add_argument(
        "--n_cores", metavar="n_cores", type=int, required=False,
        default=1,
        help="The number of processors to use."
    )
    parser.add_argument(
        "--chunks", metavar="chunks", type=int, required=False,
        default=1,
        help="Chunks as integer."
    )
    parser.add_argument(
        "-i", "--interactive", action='store_true', default=False,
        help="Run in interactive mode for local analysis."
    )

    args = parser.parse_args()
    can_file = args.candidate
    ref_file = args.reference
    log_file = args.log_file
    chunks = args.chunks
    n_cores = args.n_cores
    interactive = args.interactive

    if chunks == 1:
        chunks = {}    # No chunking
    elif chunks == 0:
        chunks = None  # This will use the conus_chunks_dict

    return can_file, ref_file, log_file, chunks, n_cores, interactive

if __name__ == "__main__":

    can_file, ref_file, log_file, chunks, n_cores, inter = parse_arguments()
    ret = xrcmp(
        can_file=can_file,
        ref_file=ref_file,
        log_file=log_file,
        n_cores=n_cores,
        chunks=chunks,
        interactive=inter
    )
    sys.exit(ret)
