import os
from pathlib import Path
import time


def plot_diffs(output_dir, candidatename, referencename, testname, feature_ids=None):
    """
    Function to create diff plots
    Args:
        output_dir: The output directory for this configuration
        candidatename: The name of the candidate run. Must match the name of the
                       model output directory
        referencename: The name of the reference run. Must match the name of the
                       model output directory
        testname: The name of the test being run. Plots will be placed in this
                       named directory
        feature_ids: A dict containing lists of channel, lake, and groundwater bucket ids
                       to also plot timeseries for, or None. If defined, should be
                       {'channels': [], 'lakes': [], 'gwout': []}. Difference statistics
                       plots for CHRTOUT, LAKEOUT and GWOUT are made either way.
    """
    output_dir = Path(output_dir)
    candidatename = Path(candidatename)
    referencename = Path(referencename)
    testname = Path(testname)

    candidate = output_dir / candidatename
    reference = output_dir / referencename
    plots = output_dir / "diff_plots" / testname
    # script_dir = output_dir / "../candidate_can_pytest/tests/local/utils"
    # output_dir is at build/Run/output
    script_dir = output_dir / "../../../tests/local/utils"
    if not script_dir.is_dir():
        script_dir.mkdir(parents=True)

    gen_script = script_dir / "generate_diff_plots.py"
    thresholds = script_dir / "thresholds.csv"


    cmd = f"python {gen_script} -o {plots} -d " + \
        f"-b {reference} -B {referencename} -c {candidate} -C {candidatename} " + \
        f"-n -t {thresholds}"

    ldas_vars = ['ACCET','SFCRNOFF','UGDRNOFF','SOIL_M','SNEQV','FSA','FIRA','TRAD','GRDFLX','LH','HFX']

    # (-f argument, filename pattern, feature_ids key for timeseries plots)
    file_types = [
        ("ldas:" + ",".join(ldas_vars), ".LDASOUT_DOMAIN", None),
        ("rtout", ".RTOUT_DOMAIN", None),
        ("chrtout", ".CHRTOUT_DOMAIN", "channels"),
        ("lakeout", ".LAKEOUT_DOMAIN", "lakes"),
        ("gwout", ".GWOUT_DOMAIN", "gwout"),
    ]

    # One call per file type: generate_diff_plots.py aborts the whole run if
    # any requested type has no files, and not every configuration writes
    # every type (e.g. LAKEOUT needs lakes).
    for file_type, pattern, ids_key in file_types:
        name = file_type.split(":")[0]
        if not any(reference.glob(f"*{pattern}*")):
            print(f"\nNo *{pattern}* files in {reference}, skipping {name} plots")
            continue

        cmd_type = cmd + f" -f {file_type}"
        if feature_ids and ids_key in feature_ids:
            cmd_type += " -i '" + ",".join(feature_ids[ids_key]) + "'"

        print(f"\nPlotting {name} model diffs...")
        os.system(cmd_type)
