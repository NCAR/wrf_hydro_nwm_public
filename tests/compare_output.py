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
        feature_ids: A dict containing a list of channels and lake ids to create diff
                       plots for, or None. If defined, should be {'channels': [], 'lakes': []}
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

    print("CMAD DDD + =", cmd)
    ldas_vars = ['ACCET','SFCRNOF','UGDRNOFF','SOIL_M','SNEQV','FSA','FIRA','TRAD','GRDFLX','LH','HFX']

    cmd_gridded = cmd + " -f ldas:" + (",".join(ldas_vars)) + " -f rtout"

    print("\nPlotting gridded model diffs...")
    os.system(cmd_gridded)

    if feature_ids is None:
        return

    if 'channels' in feature_ids:
        cmd_feature = cmd + " -f chrtout -i '" + ",".join(feature_ids['channels']) + "'"
        print("\nPlotting channel model diffs...")
        os.system(cmd_feature)

    if 'lakes' in feature_ids:
        cmd_feature = cmd + " -f lakeout -i '" + ",".join(feature_ids['lakes']) + "'"
        print("\nPlotting lake model diffs...")
        os.system(cmd_feature)

    if 'gwout' in feature_ids:
        cmd_feature = cmd + " -f gwout -i '" + ",".join(feature_ids['gwout']) + "'"
        print("\nPlotting gwout model diffs...")
        os.system(cmd_feature)
