import sys
import time
import os

from wrfhydropy import *


def wait_job(sim):
    """
    Function to wait for job completion
    Args:
        sim: A wrfhydropy simulation object
    """
    file = sim.jobs[0].job_dir.joinpath('WrfHydroJob_postrun.pkl')
    while True:
        if file.exists():
            break
        time.sleep(5)


def eprint(*args, **kwargs):
    """
    Function to mimic print function but to standard error instead of standard out
    Args:
        args: Arguments to standard print function
        kwargs: keyword arguments to standard print function
    """
    print(*args, file=sys.stderr, **kwargs)


def print_diffs(diffs):
    """
    Function to print all diffs to stderr in a log-friendly manner
    Args:
        diffs: A wrfhydropy.output.OutputDiffs object
    """
    eprint(diffs.diff_counts)
    for key, value in diffs.diff_counts.items():
        if value != 0:
            diff_set = getattr(diffs, key)
            eprint('\n' + key + '\n')
            for a_diff in diff_set:
                eprint(a_diff)


def plot_diffs(output_dir, candidatename, referencename, testname):
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
    """
    candidate = output_dir / candidatename
    reference = output_dir / referencename
    plots = output_dir / "diff_plots" / testname
    script_dir = output_dir / "../candidate_can_pytest/tests/local/utils"

    gen_script = script_dir / "generate_diff_plots.py"
    thresholds = script_dir / "thresholds.csv"

    cmd = f"python {gen_script} -f ldas:ACCET,SNOWH,FIRA -f rtout -o {plots} " + \
        f"-b {reference} -B {referencename} -c {candidate} -C {candidatename} " + \
        f"-n -t {thresholds}"

    print(f"\nPlotting model diffs using command {cmd}\n", end="")

    os.system(cmd)


def make_sim(domain_dir,
             compiler,
             source_dir,
             configuration,
             option_suite,
             ncores,
             nnodes,
             scheduler,
             account,
             walltime,
             queue,
             channel_only = False):
    # model
    model = Model(
        compiler=compiler,
        source_dir=source_dir,
        model_config=configuration
    )

    # domain
    domain = Domain(
        domain_top_dir=domain_dir,
        domain_config=configuration
    )

    # simulation
    sim = Simulation()
    sim.add(model)
    sim.add(domain)

    # Update base namelists with option suite if specified
    if option_suite is not None:
        pass

    if scheduler:
        sim.add(schedulers.PBSCheyenne(account=account,
                                       nproc=int(ncores),
                                       nnodes=int(nnodes),
                                       walltime=walltime,
                                       queue=queue))

    # Channel and bucket mode is forc_typ = 10.
    if channel_only:
        sim.base_hrldas_namelist['wrf_hydro_offline']['forc_typ'] = 10

    return sim