##########################################################
#
# This script concatenates all model output files for the given
# file type(s), across the time dimension, and creates difference
# statistics plots for each gridded point (heatmap) or feature
# point (boxplot).
#
# Note this script assumes that gridded files have (at least)
# x, y, and time dimensions (named as such), and point files
# have a feature_id dimension
#
###########################################################

# check python version. 'f' formatting strings aren't available
# until python v3.6
import sys
if sys.version_info[0] < 3 or \
        (sys.version_info[0] == 3 and sys.version_info[1] < 6):
    print("Requires python version 3.6 or later")
    exit(-1)


import xarray as xr
import os
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm
import argparse


# set up logging for this module
import logging
logging.basicConfig(format="%(levelname)s, %(asctime)s %(message)s")
logger = logging.getLogger("generate_diff_plots")
logger.setLevel(logging.DEBUG)

import warnings
warnings.filterwarnings("ignore", message="All-NaN slice encountered")
warnings.filterwarnings("ignore", message="invalid value encountered in true_divide")
warnings.filterwarnings("ignore", message="invalid value encountered in divide")


GRIDDED = "gridded"
FEATURE = "feature"

FEATURE_ID = "feature_id"
TIME = "time"
XCOORD = "x"
YCOORD= "y"

# stats to calculate. Keys must be handled in get_stat() function below
STATS = {
    'sum': "Sum of model differences",
    'abssum': "Sum of absolute model differences",
    'mean': "Mean of model differences",
    'min': "Minimum of model differences",
    'max': "Maximum of model differences",
    'std': "Standard deviation of model differences"
}

# variables to diff from each model output file
FILE_TYPES = {
    'ldas': {
        'datatype': GRIDDED,
        'pattern': ".LDASOUT_"
    },
    'chanobs': {
        'datatype': FEATURE,
        'pattern': ".CHANOBS_"
    },
    'gwout': {
        'datatype': FEATURE,
        'pattern': ".GWOUT_"
    },
    'chrtout': {
        'datatype': FEATURE,
        'pattern': ".CHRTOUT_"
    },
    'lakeout': {
        'datatype': FEATURE,
        'pattern': ".LAKEOUT_"
    },
    'rtout': {
        'datatype': GRIDDED,
        'pattern': ".RTOUT_"
    }
}


def get_options():
    """
    Get command line options
    Returns: Filtered options, or None

    """
    global FILE_TYPES

    available_types = FILE_TYPES.keys()

    parser = argparse.ArgumentParser(description="Create difference statistics plots for wrfhydro model output")
    parser.add_argument("-f", "--filetype", dest="filetype", required=True, action="append", help="File type. One of "
                    + ",".join(available_types) + ". Can be specified more than once. To select only specific variables "
                    + "for a filetype, follow filetype with a colon and separate variable names with a comma, e.g. "
                      "-f ldas:ACCET,SNOWH,FIRA")
    parser.add_argument("-o", "--output", dest="outdir", required=True, help="Write plot images to this directory.")
    parser.add_argument("-b", "--basepath", dest="base_path", required=True, help="Path to base model output files.")
    parser.add_argument("-B", "--baselabel", dest="base_label", default="Base Model", help="Label for base model.")
    parser.add_argument("-c", "--comppath", dest="comp_path", required=True, help="Path to comparison model output files.")
    parser.add_argument("-C", "--complabel", dest="comp_label", default="Comp Model", help="Label for comparison model.")
    parser.add_argument("-i", "--ids", dest="ids", default=None,
                        help="Also plot timeseries comparisons for these feature ids (as a comma separated list)")
    parser.add_argument("-n", "--nodask", dest="use_dask", default=True, action="store_false",
                        help="Don't use dask. Useful to avoid HDF5 chunking errors for small datasets.")

    parser.add_argument("-t", "--thresholds", dest="thresholds", default=None, help="Use monimum thresholds from this file.")

    parser.add_argument("-d", dest="debug", action="store_true", default=False, help="Print debug messages")

    options = parser.parse_args()

    if not os.path.isdir(options.base_path):
        print(f"ERROR: {options.base_path} does not exist!")
        return False

    if not os.path.isdir(options.comp_path):
        print(f"ERROR: {options.comp_path} does not exist!")
        return False

    if options.thresholds:
        try:
            thresholds = {}
            for line in open(options.thresholds).readlines():
                (type, var, threshold) = line.replace("\n","").split(",")

                if type not in thresholds:
                    thresholds[type] = {}

                thresholds[type][var] = float(threshold)

            options.thresholds = thresholds

        except Exception as e:
            print(f"ERROR: Could not read thresholds file: {e}")
            return None

    filetypes = {}
    for ft in options.filetype:
        toks = ft.split(":")
        type = toks[0]
        if type not in FILE_TYPES:
            print(f"ERROR: Unknown file type {type}")
            return None

        try:
            # find one file and pull out variables and long names
            files = os.listdir(options.base_path)
            found = False
            for f in files:
                if FILE_TYPES[type]['pattern'] not in f:
                    continue
                ds = xr.open_dataset("%s/%s" % (options.base_path, f))
                variables = {}
                for v in ds.variables:
                    if v in ds.coords or v == "crs":
                        continue

                    label = ds.variables[v].attrs['long_name'] if 'long_name' in ds.variables[v].attrs else v
                    units = ds.variables[v].attrs['units'] if 'units' in ds.variables[v].attrs else ""

                    variables[v] = {'label': label, 'units': units}

                    if options.thresholds:
                        if type in options.thresholds and v in options.thresholds[type]:
                            variables[v]['threshold'] = options.thresholds[type][v]

                FILE_TYPES[type]['variables'] = variables
                found=True
                break
            if not found:
                print("ERROR: No files of type '%s' found in '%s'" % (type, options.base_path))
                return None
        except Exception as e:
            print("Problem getting varnames from '%s' files: %s" % (type, e))
            return None

        # now, get requested variables and match them against what we found in the file
        vars = []
        if len(toks) > 1:
            toks = toks[1].split(",")
            for v in toks:
                if v not in FILE_TYPES[type]['variables']:
                    print(f"ERROR: Unknown variable {v} for file type {type}. Skipping.")
                else:
                    vars.append(v)
        else:
            vars = FILE_TYPES[type]['variables']

        filetypes[type] = vars

    options.filetypes = filetypes

    if options.ids is not None:
        options.ids = options.ids.split(",")

    if not os.path.isdir(options.outdir):
        try:
            os.makedirs(options.outdir, exist_ok=True)
        except Exception as e:
            print(f"ERROR: Could not create output directory: {e}")
            return None

    logger.setLevel(logging.DEBUG if options.debug is True else logging.INFO)
    return options


def get_datasets(base_path, comp_path, filepattern, useDask=True):
    """
    Get base and comparison datasets by concatenating all output files
    across the time dimension. If a variable does not have a time dimension,
    one is added before concatenation.

    Args:
        base_path: The path to the base model output
        comp_path: The path to the comparison model output
        filepattern: The type of file to compare (e.g. LDAS, CHANOBS). Must be a unique
                     filenme pattern shared by these file types

    Returns: A dict {'base': base_dataset, 'comp': comp_dataset}, or None

    """
    logger.info("Opening datasets...")

    datasets = {}

    for c in [base_path, comp_path]:
        logger.debug("Opening %s" % c)

        if useDask:
            try:
                ds = xr.open_mfdataset("%s/*%s*" % (c, filepattern), combine="nested", concat_dim=TIME)
            except:
                ds = None
        else:
            a = []
            for f in sorted(os.listdir(c)):
                if filepattern not in f: continue
                ds = xr.open_dataset('%s/%s' % (c, f))

                for v in ds.variables:
                    if v not in ds.coords:
                        if TIME not in ds[v].dims:
                            ds[v] = ds[v].expand_dims(dim=TIME)

                a.append(ds)

            # merge files along the time axis
            try:
                ds = xr.merge(a)
            except:
                ds = None

        if ds is None:
            logger.info("No datasets found")
            return None

        if c == base_path and 'base' not in datasets:
            datasets['base'] = ds
        else:
            datasets['comp'] = ds

        logger.debug("Found %s times" % len(ds[TIME]))

    return datasets


def create_diff(datasets, variable, threshold=0):
    """
    Subtract the comparison dataset from the base dataset
    at each gridpoint, for the given variable,
    Args:
        datasets: A dict containing base and comp datasets
        variable: The variable to diff
        threshold: The minimum absolute difference to be considered a difference.
             Diff values below the threshold will be considered equal (i.e. diff=0)

    Returns: A dataset containing the diff
             TODO also include projection info?

    """
    logger.debug(f"Getting diffs for {variable}")

    base = datasets['base']
    comp = datasets['comp']
    diff = base
    basev = base[variable]
    compv = comp[variable]

    # choose top layer if there's an extra dimension
    for dim in basev.dims:
        if dim not in [TIME, XCOORD, YCOORD, FEATURE_ID]:
            basev = basev.sel({dim:0})
            compv = compv.sel({dim:0})

    d = basev.data - compv.data
    diff[f"{variable}_diff"] = basev
    diff[f"{variable}_diff"].data = xr.where(np.abs(d) < threshold, 0, d)

    return diff[[f"{variable}_diff"]]


def get_stat(dataset, variable, stat):
    """
    Get the given statistic over all model differences at each gridpoint, across the time dimension
    Args:
        dataset: The dataset containing the calculated model difference
        variable: The variable name to analyze
        stat: The name of the stat to calculate. One of [ min, max, sum, abssum, mean, std ]
    Returns:
        A dataset containing the given statistic of all differences at each gridpoint, across the time dimension
    """
    da = None
    # minimum non-NAN values at a point required for sum statistics
    times = dataset.dims[TIME]-1

    if stat == "min":
        da = dataset[variable].min(dim=TIME)
    elif stat == "max":
        da = dataset[variable].max(dim=TIME)
    elif stat == "sum":
        da = dataset[variable].sum(dim=TIME, min_count=times)
    elif stat == "abssum":
        da = dataset[variable]
        da.data = np.abs(da.data)
        da = da.sum(dim=TIME, min_count=times)
    elif stat == "mean":
        da = dataset[variable].mean(dim=TIME)
    elif stat == "std":
        da = dataset[variable].std(dim=TIME)
    else:
        logger.error(f"Unknown statistic: {stat}")

    return da


def plot_diffs(dataset, file_type, variable, var_label, outpath, type=GRIDDED, base="Base Model", comp="Comp Model",
               range=None, value_range=None):
    """
    Plot statistics of the model differences, and save to disk as a PNG file
    Args:
        dataset: The dataset containing the diff of (base-comp).
        file_type: The type of file (ldas, chrt, etc)
        variable: The name of the analyzed variable, without '_diff' appended
                  (Expects the dataset to contain {variable}_diff)
        var_label: The long name of the variable
        outpath: The path where the plots should be written (as a .png file)
        type: The type of data (gridded, feature)
        base: The long name of the base model, for plot labeling
        comp: The long name of the comparison model, for plot labeling
        range: The start/end times of the analysis, as strings. Optional
        value_range: The min/max of the variable. Optional
    Returns:

    """
    varname = variable + "_diff"
    logger.info(f"Creating plots for {variable}")

    if type not in [ GRIDDED, FEATURE]:
        logger.error(f"Plots not implemented for datatype '{type}'")
        return False

    # average across non-coordinate variables
    for c in dataset.dims:
        if c not in dataset.coords:
            dataset = dataset.mean(dim=c)

    plt.rcParams.update({'font.size': 22, 'axes.labelsize': 22})
    cols = len(STATS)
    if cols % 2 > 0:
        cols += 1
    cols = int(cols / 2)

    if type == GRIDDED:
        fig, axes = plt.subplots(2, cols, sharex=True, sharey=True)
    elif type == FEATURE:
        fig, axes = plt.subplots(2, cols)
    else:
        logger.error(f"plot_diffs: Unknown type '{type}'")
        return False

    fig.set_size_inches(20*cols, 35)
    title = f"{var_label}\n{base} minus {comp}"
    if range:
        start, end = (range[0], range[1])
        title += f"\n{start} to {end}"
    if value_range and value_range[1] != value_range[0]:
        df = float(dataset[varname].max().compute())
        max_err = "{0:.4f}".format(df / (value_range[1] - value_range[0]) * 100)
        title += f"\nValue range {value_range[0]} to {value_range[1]}, Max error {max_err}%"
    plt.suptitle(title, fontsize=40, weight='bold')

    row = 0
    col = 0
    for stat in STATS.keys():
        label = STATS[stat]

        logger.debug(f" --- Calculating {stat}")
        ds = get_stat(dataset, varname, stat)

        if type == GRIDDED:
            # plot the data as a heatmap
            ds.plot(cmap=cm.coolwarm, ax=axes[row, col], cbar_kwargs={'label': ""}) # ,vmin=-1, vmax=1 )
        elif type == FEATURE:
            # drop NaN's so we can do a boxplot. Report # of NaN's (if there are any)
            data = ds.where(~np.isnan(ds),drop=True)
            axes[row, col].boxplot(data)
            num_nans = len(ds) - len(data)

            if num_nans > 0:
                y = 0 if len(data) == 0 else data.min()
                axes[row,col].text(1, y, f"                   # NaNs: {num_nans}")

        axes[row,col].set_title(label, fontsize=30, weight='bold')

        col += 1
        if col >= cols:
            col = 0
            row += 1

    if type == GRIDDED:
        for ax in axes.flat:
            ax.set(xlabel="X coordinate (m)", ylabel="Y coordinate (m)")
        for ax in axes.flat:
            ax.label_outer()

    outf = f"{outpath}/{file_type}_{variable}_diff_stats.png"
    logger.info(f"Saving plot to {outf}")
    plt.savefig(outf, bbox_inches = 'tight')

    return True


def get_range(datasets, variable):
    """
    Get the min/max of the selected variable across both datasets
    @param datasets: The loaded datasets1
    @param variable: The variable to inspect
    @return: (min, max)
    """
    min1 = datasets['base'][variable].min()
    max1 = datasets['base'][variable].max()
    min2 = datasets['comp'][variable].min()
    max2 = datasets['comp'][variable].max()

    min = min1 if min1 < min2 else min2
    max = max1 if max1 > max2 else max2

    # round the values to two significant digits
    range = []
    for a in [min, max]:
        a = float(a)
        if 'e' in str(a):
            (n,e) = str(a).split("e")
            n = "{0:.2f}".format(float(n))
            a = float(f"{n}e{e}")
        else:
            a = float("{0:.2f}".format(a))

        range.append(a)

    return range


def process_variable(type, variable, datasets, outdir, base_label, comp_label, ids=None):
    """
    Create difference statistics for a single variable
    Args:
        type: The filetype containing the variable
        variable: The variable name
        datasets: The dict containing the base and comp model datasets
        outdir: The directory to write plots to
        base_label: The label of the base model, to use in the plots
        comp_label: The label of the comparison model, to use in the plots
        ids: A list of feature ids to plot timeseries for

    Returns: True on success, false on error

    """
    logger.info(f"Processing type {type}, variable {variable}")
    tp = FILE_TYPES[type]

    threshold = FILE_TYPES[type]['variables'][variable]['threshold'] \
        if 'threshold' in FILE_TYPES[type]['variables'][variable] else 0

    diff = create_diff(datasets, variable, threshold)

    df = diff[f"{variable}_diff"].max().compute()
    if df == 0:
        logger.info(f"No differences found for type {type}, variable {variable}. Skipping plots")
        return True

    range = get_range(datasets, variable)

    start = str(diff[TIME].values[0])[:19]
    end = str(diff[TIME].values[-1])[:19]
    label = FILE_TYPES[type]['variables'][variable]['label']
    units = FILE_TYPES[type]['variables'][variable]['units']

    var_label = f"{label} ({variable}) - {units}"

    datatype = tp['datatype']
    success = plot_diffs(diff, type, variable, var_label, outdir, datatype, base=base_label, comp=comp_label,
                                  range=[start, end], value_range=range)

    if ids is not None and tp['datatype'] == FEATURE \
            and FEATURE_ID in datasets['base'].variables and FEATURE_ID in datasets['comp'].variables:
        for i in ids:
            if int(i) in datasets['base'][FEATURE_ID]:
                plot_timeseries(datasets, int(i), outdir, type, variable, base_label, comp_label)

    return success


def plot_timeseries(datasets, feature_id, outpath, file_type, variable, base_label, comp_label):
    """
    Plot feature timeseries for base and comparison models
    Args:
        datasets: A dict containing base and comparison model datasets
        feature_id: The integer feature id to plot
        outpath: The path to write output images to
        file_type: The file type, e.g. chanobs, gwout, etc.
        variable: The name of the variable to plot
        base_label: The label for the base model
        comp_label: The label for the comparison model

    Returns: True on success

    """
    logger.debug(f"Plotting timeseries for feature id '{feature_id}'")

    base = datasets['base'][variable].sel({FEATURE_ID: feature_id})
    comp = datasets['comp'][variable].sel({FEATURE_ID: feature_id})

    start = str(base[TIME].values[0])[:19]
    end = str(base[TIME].values[-1])[:19]
    label = FILE_TYPES[file_type]['variables'][variable]['label']
    units = FILE_TYPES[file_type]['variables'][variable]['units']

    var_label = f"{label} ({variable}) - {units}"

    fig = plt.figure()
    plt.clf()

    fig.set_size_inches(30,20)

    base.plot(label=base_label)
    comp.plot(label=comp_label)
    plt.legend()

    title = f"{var_label}\n{base_label} minus {comp_label}\nTimeseries {start} to {end}" + \
        f"\nFeature id: {feature_id}"
    plt.title(title)

    outf = f"{outpath}/{file_type}_{variable}_timeseries_feature_id_{feature_id}.png"
    logger.debug(f"Saving plot to {outf}")
    plt.savefig(outf, bbox_inches = 'tight')

    return True


def run(options=None):
    """
    Create plots based on the command line options
    Returns: True on success, False on error

    """
    if not options:
        options = get_options()
    if not options:
        return False

    for type in options.filetypes.keys():
        datasets = get_datasets(options.base_path, options.comp_path, FILE_TYPES[type]['pattern'], useDask=options.use_dask)

        if datasets is None:
            continue

        for variable in options.filetypes[type]:
            success = process_variable(type, variable, datasets, options.outdir, options.base_label,
                                       options.comp_label, options.ids)

            if not success:
                return False

        datasets['base'].close()
        datasets['comp'].close()

    return True


if __name__ == "__main__":
    if run():
        exit(0)
    exit(-1)

