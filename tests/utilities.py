import sys
import datetime
import time
from pathlib import Path

import pytest
from wrfhydropy import *

COMPILE_TIME = 60 * 2       # 2 minutes
MODEL_RUN_TIME = 60 * 5    # 45 minutes
DEFAULT_LOOP_WAIT = 0.5


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
             channel_only=False):
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


def wait_on_file(file: Path, timeout_s: int, timeout_msg: str, error_on_timeout: bool = False,
                 loop_wait: float = DEFAULT_LOOP_WAIT):
    start_wait = datetime.datetime.now()
    while not file.exists():
        time.sleep(loop_wait)
        if (datetime.datetime.now() - start_wait).seconds > timeout_s:
            if error_on_timeout:
                raise TimeoutError(timeout_msg)
            else:
                pytest.skip(timeout_msg)
    time.sleep(0.1)  # let filesystem settle


def wait_for_output_files(output_dir: Path, count: int):
    output_files_expected = 4  # TODO: verify this?
    sim_files = []
    start_wait = datetime.datetime.now()
    while len(sim_files) < output_files_expected:
        time.sleep(DEFAULT_LOOP_WAIT)
        if (datetime.datetime.now() - start_wait).seconds > MODEL_RUN_TIME:
            pytest.skip("test_output_has_nans timed out waiting for runs to complete")

        sim_files = list(output_dir.rglob('WrfHydroSim_collected.pkl'))
