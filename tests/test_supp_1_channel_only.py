import copy
import datetime as dt
import os
import pathlib
import pickle
import sys
import warnings

import pandas as pd
import pytest
import wrfhydropy

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from test_1_fundamental import wait_job


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


# Channel-only Run
def test_run_candidate_channel_only(candidate_sim,
                                    candidate_channel_only_sim,
                                    output_dir,
                                    ncores):
    if candidate_sim.model.model_config.lower().find('nwm') < 0:
        pytest.skip('Channel-only test only applicable to nwm_ana config')

    print("\nQuestion: The candidate channel-only mode runs successfully?\n", end='')

    # Dont recompile the model, just use the candidate's model.
    candidate_channel_only_sim.model = candidate_sim.model

    # Set the forcing directory

    # Set run directory and go for execution.
    run_dir = output_dir / 'run_candidate_channel_only'
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    # Job
    exe_command = ('mpirun -np {0} ./wrf_hydro.exe').format(str(ncores))
    job = wrfhydropy.Job(job_id='run_candidate_channel_only', exe_cmd=exe_command)
    candidate_channel_only_sim.add(job)

    candidate_channel_only_sim.jobs[0]._hrldas_namelist['noahlsm_offline']['indir'] = \
        str(output_dir / 'run_candidate')

    # Run
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_channel_only_sim.compose()

    print('\nwaiting for job to complete...', end='')
    candidate_channel_only_sim.run()
    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    wait_job(candidate_channel_only_sim)

    candidate_channel_only_sim.collect()
    candidate_channel_only_sim.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check job run statuses
    for job in candidate_channel_only_sim.jobs:
        assert job.exit_status == 0, \
            "Candidate channel-only code run exited with non-zero status"


# Channel-only matches full-model?
def test_channel_only_matches_full(candidate_channel_only_sim, output_dir):
    if candidate_channel_only_sim.model.model_config.lower().find('nwm') < 0:
        pytest.skip('Channel-only test only applicable to nwm_ana config')

    print("\nQuestion: The candidate channel-only run output files match those of the full "
          "model?\n", end="")

    # Check for existence of simobjects
    candidate_run_file = \
        output_dir / 'run_candidate' / 'WrfHydroSim_collected.pkl'
    candidate_channel_only_run_file = \
        output_dir / 'run_candidate_channel_only' / 'WrfHydroSim_collected.pkl'

    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')
    if candidate_channel_only_run_file.is_file() is False:
        pytest.skip('candidate_channel_only run object not found, skipping test')

    # Load run objects
    candidate_run_expected = pickle.load(candidate_run_file.open("rb"))
    candidate_channel_only_run_expected = pickle.load(candidate_channel_only_run_file.open("rb"))

    exclude_vars = [
        'stc1',
        'smc1',
        'sh2ox1',
        'stc2',
        'smc2',
        'sh2ox2',
        'stc3',
        'smc3',
        'sh2ox3',
        'stc4',
        'smc4',
        'sh2ox4',
        'infxsrt',
        'soldrain',
        'sfcheadrt',
        'QBDRYRT',
        'infxswgt',
        'sfcheadsubrt',
        'sh2owgt1',
        'sh2owgt2',
        'sh2owgt3',
        'sh2owgt4',
        'qstrmvolrt',
        'hlink',
        'lake_inflort'
    ]

    # We still compare these:
    # 'qlink1'
    # 'qlink2'
    # 'resht'
    # 'qlakeo'
    # 'z_gwsubbas'

    # Dont compare metadata in this case, there are different dimensions
    # in the files that always result in a return code of 1.
    nccmp_options = ['--data', '--force', '--quiet']  # , '--metadata']

    # Check diffs
    # Run
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        diffs = wrfhydropy.outputdiffs.OutputDataDiffs(
            candidate_run_expected.output,
            candidate_channel_only_run_expected.output,
            nccmp_options=nccmp_options,
            exclude_vars=exclude_vars
        )

    has_diffs = any(value != 0 for value in diffs.diff_counts.values())
    if has_diffs:
        eprint(diffs.diff_counts)
        for key, value in diffs.diff_counts.items():
            if value != 0:
                diffs = getattr(diffs, key)
                eprint('\n' + key + '\n')
                for diff in diffs:
                    eprint(diff)
    assert has_diffs == False, \
        'Outputs for candidate_channel_only run do not match outputs from candidate run'


# Channel-only ncores question
def test_ncores_candidate_channel_only(output_dir):
    candidate_channel_only_sim_file = \
        output_dir / 'run_candidate_channel_only' / 'WrfHydroSim.pkl'
    candidate_channel_only_collected_file = \
        output_dir / 'run_candidate_channel_only' / 'WrfHydroSim_collected.pkl'

    if candidate_channel_only_collected_file.is_file() is False:
        pytest.skip('candidate_channel_only collected run object not found, skipping test.')

    print("\nQuestion: The candidate_channel-only output files from an ncores runmatch those "
          "from an ncores-1 run?\n", end='')

    candidate_channel_only_sim = \
        pickle.load(candidate_channel_only_sim_file.open("rb"))
    candidate_channel_only_sim_expected = \
        pickle.load(candidate_channel_only_collected_file.open("rb"))
    candidate_channel_only_sim_ncores = copy.deepcopy(candidate_channel_only_sim)

    run_dir = output_dir / 'ncores_candidate_channel_only'
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    old_job = candidate_channel_only_sim.jobs[0]
    new_job = wrfhydropy.Job(job_id='ncores_candidate', exe_cmd=old_job._exe_cmd)

    # Remove old job and add new job
    candidate_channel_only_sim_ncores.jobs.pop(0)
    candidate_channel_only_sim_ncores.add(new_job)

    # Edit the sim object number of cores
    if candidate_channel_only_sim_ncores.scheduler is not None:
        candidate_channel_only_sim_ncores.scheduler.nproc = \
            candidate_channel_only_sim_ncores.scheduler.nproc - 1
    else:
        orig_exe_cmd = candidate_channel_only_sim_ncores.jobs[0]._exe_cmd
        orig_exe_cmd = orig_exe_cmd.replace('-np 2', '-np 1')

    # Recompose into new directory and run
    # catch warnings related to missing start and end job times
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_channel_only_sim_ncores.compose(force=True)

    print('\nwaiting for job to complete...', end='')
    candidate_channel_only_sim_ncores.run()

    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    wait_job(candidate_channel_only_sim_ncores)

    candidate_channel_only_sim_ncores.collect()
    candidate_channel_only_sim_ncores.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check outputs
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        diffs = wrfhydropy.outputdiffs.OutputDataDiffs(candidate_channel_only_sim_ncores.output,
                                                       candidate_channel_only_sim_expected.output)

    # Assert all diff values are 0 and print diff stats if not
    has_diffs = any(value != 0 for value in diffs.diff_counts.values())
    if has_diffs:
        eprint(diffs.diff_counts)
        for key, value in diffs.diff_counts.items():
            if value != 0:
                diffs = getattr(diffs, key)
                eprint('\n' + key + '\n')
                for diff in diffs:
                    eprint(diff)
    assert has_diffs is False, \
        'Outputs for candidate_channel_only run with ncores do not match outputs with ncores-1'


def test_perfrestart_candidate_channel_only(output_dir):
    candidate_channel_only_sim_file = \
        output_dir / 'run_candidate_channel_only' / 'WrfHydroSim.pkl'
    candidate_channel_only_collected_file = \
        output_dir / 'run_candidate_channel_only' / 'WrfHydroSim_collected.pkl'

    if candidate_channel_only_collected_file.is_file() is False:
        pytest.skip('candidate_channel_only run object not found, skipping test')

    print("\nQuestion: The candidate_channel_only outputs from a restart run match the outputs "
          "from standard run?\n", end='')

    # Load initial run model object and copy
    candidate_channel_only_sim = \
        pickle.load(candidate_channel_only_sim_file.open(mode="rb"))
    candidate_channel_only_sim_expected = \
        pickle.load(candidate_channel_only_collected_file.open(mode="rb"))
    candidate_channel_only_sim_restart = copy.deepcopy(candidate_channel_only_sim)

    # Set run directory
    run_dir = output_dir / 'restart_candidate_channel_only'
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    # Get a new start time 1 hour later
    restart_job = candidate_channel_only_sim_restart.jobs[0]
    restart_job.model_start_time = restart_job.model_start_time + dt.timedelta(hours=2)

    # Get restart files from previous run and symlink into restart sim dir
    # Hydro: Use actual time listed in meta data not filename or positional list index
    for restart_file in candidate_channel_only_sim_expected.output.restart_hydro:
        restart_time = restart_file.open().Restart_Time
        restart_time = pd.to_datetime(restart_time, format='%Y-%m-%d_%H:%M:%S')
        if restart_time == restart_job.model_start_time:
            candidate_hydro_restart_file = pathlib.Path(restart_file.name)
            candidate_hydro_restart_file.symlink_to(restart_file)

    # Nudging: Use actual time listed in meta data not filename or positional list index
    for restart_file in candidate_channel_only_sim_expected.output.restart_nudging:
        restart_time = restart_file.open().modelTimeAtOutput
        restart_time = pd.to_datetime(restart_time, format='%Y-%m-%d_%H:%M:%S')
        if restart_time == restart_job.model_start_time:
            candidate_nudging_restart_file = pathlib.Path(restart_file.name)
            candidate_nudging_restart_file.symlink_to(restart_file)

    # Compose and run
    # Catch warnings related to missing start and end job times
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_channel_only_sim_restart.compose(force=True)

    print('\nwaiting for job to complete...', end='')
    candidate_channel_only_sim_restart.run()

    # Wait to collect until job has finished. All test runs are performed on a single job with
    wait_job(candidate_channel_only_sim_restart)

    candidate_channel_only_sim_restart.collect()
    candidate_channel_only_sim_restart.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check outputs
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        diffs = wrfhydropy.outputdiffs.OutputDataDiffs(candidate_channel_only_sim_restart.output,
                                                       candidate_channel_only_sim_expected.output)

    # Assert all diff values are 0 and print diff stats if not
    has_diffs = any(value != 0 for value in diffs.diff_counts.values())
    if has_diffs:
        eprint(diffs.diff_counts)
        for key, value in diffs.diff_counts.items():
            if value != 0:
                diffs = getattr(diffs, key)
                eprint('\n' + key + '\n')
                for diff in diffs:
                    eprint(diff)
    assert has_diffs is False, \
        'Outputs for candidate run do not match outputs from candidate restart run'
