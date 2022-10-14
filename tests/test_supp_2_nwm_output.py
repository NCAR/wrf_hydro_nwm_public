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


def test_run_reference_nwm_output_sim(
    reference_sim,
    reference_nwm_output_sim,
    output_dir,
    ncores
):

    if reference_nwm_output_sim.model.model_config.lower().find('nwm') < 0:
        pytest.skip('NWM Output test only applicable to nwm configs')

    print("\nQuestion: The reference nwm ouput configuration runs successfully?\n", end='')
    print('\n')

    # Set run directory and change working directory to run dir for simulation
    run_dir = output_dir / 'nwm_output_reference'
    if run_dir.exists():
        pytest.skip('Reference nwm output run exists, skipping nwm reference output run.')
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    reference_nwm_output_sim_copy = copy.deepcopy(reference_nwm_output_sim)
    # Dont recompile the model, just use the reference's model.
    reference_nwm_output_sim_copy.model = copy.deepcopy(reference_sim.model)

    # Job
    exe_command = 'mpirun -np {0} ./wrf_hydro.exe'.format(str(ncores))
    job = wrfhydropy.Job(
        job_id='run_reference',
        exe_cmd=exe_command,
        restart_freq_hr=1,
        output_freq_hr=1
    )
    reference_nwm_output_sim_copy.add(job)

    # start_time, end_time = reference_nwm_output_sim_copy.jobs[0]._solve_model_start_end_times()
    # reference_nwm_output_sim_copy.jobs[0].model_start_time = start_time
    # reference_nwm_output_sim_copy.jobs[0].model_end_time = start_time + dt.timedelta(hours=24)
    # reference_nwm_output_sim_copy.jobs[0].restart_freq_hr = 1

    # Run, catch warnings related to missing start and end job times
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        reference_nwm_output_sim_copy.compose()

    print('\nwaiting for job to complete...', end='')
    reference_nwm_output_sim_copy.run()
    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    wait_job(reference_nwm_output_sim_copy)

    reference_nwm_output_sim_copy.collect()
    reference_nwm_output_sim_copy.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check job run statuses
    for job in reference_nwm_output_sim.jobs:
        assert job.exit_status == 0, \
            "Reference run exited with non-zero status"


def test_run_candidate_nwm_output_sim(
    candidate_sim,
    candidate_nwm_output_sim,
    output_dir,
    ncores
):

    if candidate_nwm_output_sim.model.model_config.lower().find('nwm') < 0:
        pytest.skip('NWM Output test only applicable to nwm configs')

    print("\nQuestion: The candidate nwm ouput configuration runs successfully?\n", end='')
    print('\n')

    # Set run directory and change working directory to run dir for simulation
    run_dir = output_dir / 'nwm_output_candidate'
    if run_dir.exists():
        pytest.skip('Candidate nwm output run exists, skipping nwm candidate output run.')
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    candidate_nwm_output_sim_copy = copy.deepcopy(candidate_nwm_output_sim)
    # Dont recompile the model, just use the candidate's model.
    candidate_nwm_output_sim_copy.model = copy.deepcopy(candidate_sim.model)

    # Job
    exe_command = 'mpirun -np {0} ./wrf_hydro.exe'.format(str(ncores))
    job = wrfhydropy.Job(
        job_id='run_candidate',
        exe_cmd=exe_command,
        restart_freq_hr=1,
        output_freq_hr=1
    )
    candidate_nwm_output_sim_copy.add(job)

    # start_time, end_time = candidate_nwm_output_sim_copy.jobs[0]._solve_model_start_end_times()
    # candidate_nwm_output_sim_copy.jobs[0].model_start_time = start_time
    # candidate_nwm_output_sim_copy.jobs[0].model_end_time = start_time + dt.timedelta(hours=24)
    # candidate_nwm_output_sim_copy.jobs[0].restart_freq_hr = 1

    # Run, catch warnings related to missing start and end job times
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_nwm_output_sim_copy.compose()

    print('\nwaiting for job to complete...', end='')
    candidate_nwm_output_sim_copy.run()
    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    wait_job(candidate_nwm_output_sim_copy)

    candidate_nwm_output_sim_copy.collect()
    candidate_nwm_output_sim_copy.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check job run statuses
    for job in candidate_nwm_output_sim.jobs:
        assert job.exit_status == 0, \
            "Candidate run exited with non-zero status"


# regression question
def test_regression_metadata_nwm_output(output_dir):
    print("\nQuestion: The NWM output candidate metadata match those of the reference run?\n", end="")
    print('\n')

    # Check for existence of sim objects
    candidate_run_file = output_dir / 'nwm_output_candidate' / 'WrfHydroSim_collected.pkl'
    reference_run_file = output_dir / 'nwm_output_reference' / 'WrfHydroSim_collected.pkl'

    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')
    if reference_run_file.is_file() is False:
        pytest.skip('Reference run object not found, skipping test')

    # Load run objects
    candidate_run_expected = pickle.load(candidate_run_file.open(mode="rb"))
    reference_run_expected = pickle.load(reference_run_file.open(mode="rb"))

    # Check regression
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        meta_data_diffs = wrfhydropy.outputdiffs.OutputMetaDataDiffs(
            candidate_run_expected.output,
            reference_run_expected.output
        )

    # Assert all diff values are 0 and print diff stats if not
    has_metadata_diffs = any(value != 0 for value in meta_data_diffs.diff_counts.values())
    if has_metadata_diffs:
        print_diffs(meta_data_diffs)
    assert has_metadata_diffs is False, \
        'NWM output metadata and attributes of candidate run do not match those of the reference run.'

