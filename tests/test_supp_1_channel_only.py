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
from utilities import print_diffs, wait_job

EXCLUDE_VARS_CHAN_ONLY = [
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

#List variabls to ignore in tests, primarily accumulation variables
EXCLUDE_VARS = ['ACMELT',
                'ACSNOW',
                'SFCRUNOFF',
                'UDRUNOFF',
                'ACCPRCP',
                'ACCECAN',
                'ACCEDIR',
                'ACCETRAN',
                'qstrmvolrt',
                'reference_time',
                'lake_inflort']

EXCLUDE_VARS_CHAN_ONLY = [
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

#List variabls to ignore in tests, primarily accumulation variables
EXCLUDE_VARS = ['ACMELT',
                'ACSNOW',
                'SFCRUNOFF',
                'UDRUNOFF',
                'ACCPRCP',
                'ACCECAN',
                'ACCEDIR',
                'ACCETRAN',
                'qstrmvolrt',
                'reference_time',
                'lake_inflort']

# Channel-only Run
def test_run_candidate_channel_only(
        candidate_sim,
        candidate_channel_only_sim,
        output_dir,
        ncores,
        exe_cmd
):

    if candidate_sim.model.model_config.lower().find('nwm_ana') < 0:
        pytest.skip('Channel-only test only applicable to nwm_ana config')

    print("\nQuestion: The candidate channel-only mode runs successfully?\n", end='')
    print('\n')

    # re-run candidate at shorter duration since requires hourly outputs

    # Set run directory and change working directory to run dir for simulation
    run_dir = output_dir / 'channel_only_candidate_full_model_run'
    if run_dir.exists():
        pytest.skip('Candidate channel-only run dir exists, skipping candidate channel-only run')
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    candidate_sim_copy = copy.deepcopy(candidate_sim)
    candidate_sim_copy.base_hydro_namelist['hydro_nlist']['output_channelbucket_influx'] = 2
    candidate_channel_only_sim_copy = copy.deepcopy(candidate_channel_only_sim)

    # Job
    exe_command = exe_cmd.format(str(ncores))
    job = wrfhydropy.Job(
        job_id='run_candidate',
        exe_cmd=exe_command,
        restart_freq_hr=6,
        output_freq_hr=1
    )
    candidate_sim_copy.add(job)

    start_time, end_time = candidate_sim_copy.jobs[0]._solve_model_start_end_times()
    candidate_sim_copy.jobs[0].model_start_time = start_time
    candidate_sim_copy.jobs[0].model_end_time = start_time + dt.timedelta(hours=24)

    # Run, catch warnings related to missing start and end job times
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_sim_copy.compose()

    print('\nwaiting for job to complete...', end='')
    candidate_sim_copy.run()
    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    wait_job(candidate_sim_copy)

    candidate_sim_copy.collect()
    candidate_sim_copy.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check job run statuses
    for job in candidate_sim.jobs:
        assert job.exit_status == 0, \
            "Candidate code run (for channel-only reference) exited with non-zero status"

    #########################
    # Run channel only

    # Dont recompile the model, just use the candidate's model.
    candidate_channel_only_sim_copy.model = copy.deepcopy(candidate_sim.model)

    # Set run directory and go for execution.
    run_dir = output_dir / 'channel_only_candidate_run'
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    # Set the forcing directory
    candidate_channel_only_sim_copy.base_hrldas_namelist['noahlsm_offline']['indir'] = \
        str(output_dir / 'channel_only_candidate_full_model_run')

    # Job
    exe_command = exe_cmd.format(str(ncores))
    job = wrfhydropy.Job(
        job_id='run_candidate_channel_only',
        exe_cmd=exe_command,
        restart_freq_hr=6,
        output_freq_hr=1
    )
    candidate_channel_only_sim_copy.add(job)

    start_time, end_time = candidate_channel_only_sim_copy.jobs[0]._solve_model_start_end_times()
    candidate_channel_only_sim_copy.jobs[0].model_start_time = start_time
    candidate_channel_only_sim_copy.jobs[0].model_end_time = start_time + dt.timedelta(hours=24)

    # Run
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_channel_only_sim_copy.compose()

    print('\nwaiting for job to complete...', end='')
    candidate_channel_only_sim_copy.run()
    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    wait_job(candidate_channel_only_sim_copy)

    candidate_channel_only_sim_copy.collect()
    candidate_channel_only_sim_copy.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check job run statuses
    for job in candidate_channel_only_sim_copy.jobs:
        assert job.exit_status == 0, \
            "Candidate channel-only code run exited with non-zero status"


# Channel-only matches full-model?
def test_channel_only_matches_full(
    candidate_channel_only_sim,
    output_dir,
    xrcmp_n_cores
):

    if candidate_channel_only_sim.model.model_config.lower().find('nwm') < 0:
        pytest.skip('Channel-only test only applicable to nwm_ana config')

    print("\nQuestion: The candidate channel-only run output files match those of the full "
          "model?\n", end="")
    print('\n')

    # Check for existence of simobjects
    candidate_run_file = \
        output_dir / 'channel_only_candidate_full_model_run' / 'WrfHydroSim_collected.pkl'
    candidate_channel_only_run_file = \
        output_dir / 'channel_only_candidate_run' / 'WrfHydroSim_collected.pkl'

    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')
    if candidate_channel_only_run_file.is_file() is False:
        pytest.skip('candidate_channel_only run object not found, skipping test')

    # Load run objects
    candidate_run_expected = pickle.load(candidate_run_file.open("rb"))
    candidate_channel_only_run_expected = pickle.load(candidate_channel_only_run_file.open("rb"))

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
            exclude_vars=EXCLUDE_VARS_CHAN_ONLY,
            xrcmp_n_cores=xrcmp_n_cores
        )

    has_diffs = any(value != 0 for value in diffs.diff_counts.values())
    if has_diffs:
        print_diffs(diffs)
    assert has_diffs is False, \
        'Outputs for candidate_channel_only run do not match outputs from candidate run'


# Channel-only ncores question
def test_ncores_candidate_channel_only(
    output_dir,
    ncores,
    exe_cmd,
    xrcmp_n_cores
):

    candidate_channel_only_sim_file = \
        output_dir / 'channel_only_candidate_run' / 'WrfHydroSim.pkl'
    candidate_channel_only_collected_file = \
        output_dir / 'channel_only_candidate_run' / 'WrfHydroSim_collected.pkl'

    if candidate_channel_only_collected_file.is_file() is False:
        pytest.skip('candidate_channel_only collected run object not found, skipping test.')

    print("\nQuestion: The candidate_channel-only output files from an ncores runmatch those "
          "from an ncores-1 run?\n", end='')
    print('\n')

    candidate_channel_only_sim_expected = pickle.load(
        candidate_channel_only_collected_file.open("rb"))

    run_dir = output_dir / 'channel_only_candidate_ncores'

    if not run_dir.exists():
        run_dir.mkdir(parents=True)
        os.chdir(str(run_dir))

        candidate_channel_only_sim = \
            pickle.load(candidate_channel_only_sim_file.open("rb"))
        candidate_channel_only_sim_ncores = copy.deepcopy(candidate_channel_only_sim)
        candidate_channel_only_sim_ncores. \
            base_hydro_namelist['hydro_nlist']['output_channelbucket_influx'] = 2

        old_job = candidate_channel_only_sim.jobs[0]
        new_job = wrfhydropy.Job(
            job_id='ncores_candidate',
            model_start_time=old_job._model_start_time,
            model_end_time=old_job._model_end_time,
            exe_cmd=old_job._exe_cmd,
            restart_freq_hr=6,
            output_freq_hr=1
        )

        # Remove old job and add new job
        candidate_channel_only_sim_ncores.jobs.pop(0)
        candidate_channel_only_sim_ncores.add(new_job)

        # Edit the sim object number of cores
        if candidate_channel_only_sim_ncores.scheduler is not None:
            candidate_channel_only_sim_ncores.scheduler.nproc = \
                candidate_channel_only_sim_ncores.scheduler.nproc - 1
        else:
            candidate_channel_only_sim_ncores.jobs[0]._exe_cmd = exe_cmd.format(str(int(ncores)-1))

        # Recompose into new directory and run
        # catch warnings related to missing start and end job times
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            candidate_channel_only_sim_ncores.compose(force=True)

        print('\nwaiting for job to complete...', end='')
        candidate_channel_only_sim_ncores.run()

        wait_job(candidate_channel_only_sim_ncores)

        candidate_channel_only_sim_ncores.collect()
        candidate_channel_only_sim_ncores.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    else:
        print('Candidate channel-only n_cores run dir exists, '
              'skipping n_cores candidate channel-only run...')
        candidate_channel_only_sim_ncores = pickle.load(
            open(run_dir.joinpath('WrfHydroSim_collected.pkl'), 'rb'))

    for job in candidate_channel_only_sim_ncores.jobs:
        assert job.exit_status == 0, \
            "Candidate channel-only ncores run exited with non-zero status"

    # Check outputs
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        diffs = wrfhydropy.outputdiffs.OutputDataDiffs(
            candidate_channel_only_sim_ncores.output,
            candidate_channel_only_sim_expected.output,
            exclude_vars=EXCLUDE_VARS,
            xrcmp_n_cores=xrcmp_n_cores
        )

    # Assert all diff values are 0 and print diff stats if not
    has_diffs = any(value != 0 for value in diffs.diff_counts.values())
    if has_diffs:
        print_diffs(diffs)
    assert has_diffs is False, \
        'Outputs for candidate_channel_only run with ncores do not match outputs with ncores-1'


def test_perfrestart_candidate_channel_only(output_dir, xrcmp_n_cores):

    candidate_channel_only_sim_file = \
        output_dir / 'channel_only_candidate_run' / 'WrfHydroSim.pkl'
    candidate_channel_only_collected_file = \
        output_dir / 'channel_only_candidate_run' / 'WrfHydroSim_collected.pkl'

    if candidate_channel_only_collected_file.is_file() is False:
        pytest.skip('candidate_channel_only run object not found, skipping test')

    print("\nQuestion: The candidate_channel_only outputs from a restart run match the outputs "
          "from standard run?\n", end='')
    print('\n')

    candidate_channel_only_sim_expected = \
        pickle.load(candidate_channel_only_collected_file.open(mode="rb"))

    run_dir = output_dir / 'channel_only_candidate_restart'

    if not run_dir.exists():

        run_dir.mkdir(parents=True)
        os.chdir(str(run_dir))

        candidate_channel_only_sim = \
            pickle.load(candidate_channel_only_sim_file.open(mode="rb"))
        candidate_channel_only_sim_restart = copy.deepcopy(candidate_channel_only_sim)

        # Get a new start time halfway along the run, make sure the restart frequency accomodates
        restart_job = candidate_channel_only_sim_restart.jobs[0]
        duration = restart_job.model_end_time - restart_job.model_start_time
        delay_restart_hr = int((duration.total_seconds() / 3600)/2)

        # Want matching restart frequencies for this test...
        assert \
            candidate_channel_only_sim.jobs[0].restart_freq_hr_hydro == \
            candidate_channel_only_sim.jobs[0].restart_freq_hr_hrldas, \
            "Hydro and HRLDAS components do not have the same restart frequencies."

        assert delay_restart_hr % candidate_channel_only_sim.jobs[0].restart_freq_hr_hydro == 0, \
            "The restart delay is not a multiple of the restart frequency."
        restart_job.model_start_time = \
            restart_job.model_start_time + dt.timedelta(hours=delay_restart_hr)

        # Get restart files from previous run and symlink into restart sim dir
        # Hydro: Use actual time listed in meta data not filename or positional list index
        for restart_file in candidate_channel_only_sim_expected.output.restart_hydro:
            restart_time = restart_file.open().Restart_Time
            restart_time = pd.to_datetime(restart_time, format='%Y-%m-%d_%H:%M:%S')
            if restart_time == restart_job.model_start_time:
                candidate_hydro_restart_file = pathlib.Path(restart_file.name)
                candidate_hydro_restart_file.symlink_to(restart_file)
                key1 = 'hydro_nlist'
                key2 = 'restart_file'
                restart_job._hydro_namelist[key1][key2] = candidate_hydro_restart_file

        # Nudging: Use actual time listed in meta data not filename or positional list index
        if candidate_channel_only_sim_expected.output.restart_nudging is not None:
            for restart_file in candidate_channel_only_sim_expected.output.restart_nudging:
                restart_time = restart_file.open().modelTimeAtOutput
                restart_time = pd.to_datetime(restart_time, format='%Y-%m-%d_%H:%M:%S')
                if restart_time == restart_job.model_start_time:
                    candidate_nudging_restart_file = pathlib.Path(restart_file.name)
                    candidate_nudging_restart_file.symlink_to(restart_file)
                    key1 = 'nudging_nlist'
                    key2 = 'nudginglastobsfile'
                    restart_job._hydro_namelist[key1][key2] = candidate_nudging_restart_file

        # Compose and run
        # Catch warnings related to missing start and end job times
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            candidate_channel_only_sim_restart.compose(force=True)

        print('\nwaiting for job to complete...', end='')
        candidate_channel_only_sim_restart.run()

        wait_job(candidate_channel_only_sim_restart)

        candidate_channel_only_sim_restart.collect()
        candidate_channel_only_sim_restart.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    else:
        candidate_channel_only_sim_restart = pickle.load(
            open(run_dir.joinpath('WrfHydroSim_collected.pkl'), 'rb'))

    for job in candidate_channel_only_sim_restart.jobs:
        assert job.exit_status == 0, \
            "Candidate channel-only ncores run exited with non-zero status"

    # Check outputs
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        diffs = wrfhydropy.outputdiffs.OutputDataDiffs(
            candidate_channel_only_sim_restart.output,
            candidate_channel_only_sim_expected.output,
            exclude_vars=EXCLUDE_VARS,
            xrcmp_n_cores=xrcmp_n_cores
        )

    # Assert all diff values are 0 and print diff stats if not
    has_diffs = any(value != 0 for value in diffs.diff_counts.values())
    if has_diffs:
        print_diffs(diffs)
    assert has_diffs is False, \
        'Outputs for candidate run do not match outputs from candidate restart run'
