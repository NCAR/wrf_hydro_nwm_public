import copy
import datetime as dt
import pickle
import pytest
import warnings
import wrfhydropy
import os
import shutil
import pathlib
import time

##################################
# Setup the test with a domain, a candidate, and a reference.
# Get domain, reference, candidate, and optional output directory from command line arguments
# Setup a domain

#Utility function to wait for job completion
def wait_job(sim):
    file = sim.jobs[0].job_dir.joinpath('WrfHydroJob_postrun.pkl')
    while True:
        if file.exists():
            break
        time.sleep(5)

##################################
# Define tests


def test_compile_candidate(candidate_sim,output_dir,capsys):
    with capsys.disabled():
        print("\nQuestion: The candidate compiles?", end='')

    compile_dir = output_dir / 'compile_candidate'

    # Compile the model, catch warnings related to non-existant compile directory
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_sim.model.compile(compile_dir=compile_dir)

    # Check compilation status
    assert candidate_sim.model.compile_log.returncode == 0, \
        "Candidate code did not compile correctly."


def test_compile_reference(reference_sim,output_dir,capsys):
    with capsys.disabled():
        print("\nQuestion: The reference compiles?", end='')

    compile_dir = output_dir / 'compile_reference'

    # Compile the model, catch warnings related to non-existant compile directory
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        reference_sim.model.compile(compile_dir=compile_dir)

    # Check compilation status
    assert reference_sim.model.compile_log.returncode == 0, \
        "Reference code did not compile correctly"


def test_run_candidate(candidate_sim, output_dir, ncores, capsys):
    with capsys.disabled():
        print("\nQuestion: The candidate runs successfully?", end='')

    # Set run directory and change working directory to run dir for simulation
    run_dir = output_dir / 'run_candidate'
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    # Job
    exe_command = ('mpirun -np {0} ./wrf_hydro.exe').format(str(ncores))
    job = wrfhydropy.Job(job_id='run_candidate',exe_cmd=exe_command)
    candidate_sim.add(job)

    # Run, catch warnings related to missing start and end job times
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_sim.compose()

    candidate_sim.run()
    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    with capsys.disabled():
        print('\nwaiting for job to complete...', end='')
    wait_job(candidate_sim)

    candidate_sim.collect()
    candidate_sim.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check job run statuses
    for job in candidate_sim.jobs:
        assert job.exit_status == 0, \
            "Candidate code run exited with non-zero status"


# Run questions
def test_run_reference(reference_sim, output_dir, ncores, capsys):
    with capsys.disabled():
        print("\nQuestion: The reference runs successfully?", end='')

    # Set run directory and change working directory to run dir for simulation
    run_dir = output_dir / 'run_reference'
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    # Job
    exe_command = ('mpirun -np {0} ./wrf_hydro.exe').format(str(ncores))
    job = wrfhydropy.Job(job_id='run_reference',exe_cmd=exe_command)
    reference_sim.add(job)

    # Run, catch warnings related to missing start and end job times
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        reference_sim.compose()

    reference_sim.run()

    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    with capsys.disabled():
        print('\nwaiting for job to complete...', end='')
    wait_job(reference_sim)

    reference_sim.collect()
    reference_sim.pickle(run_dir.joinpath('WrfHydroSim_collected.pkl'))

    # Check job run statuses
    for job in reference_sim.jobs:
        assert job.exit_status == 0, \
            "Reference code run exited with non-zero status"

#Ncores question
def test_ncores_candidate(output_dir, capsys):
    with capsys.disabled():
        print("\nQuestion: The candidate outputs from a ncores run match outputs from"
              " ncores-1 run?\n", end='')

    candidate_sim_file = output_dir / 'run_candidate' / 'WrfHydroSim.pkl'
    candidate_collected_file = output_dir / 'run_candidate' / 'WrfHydroSim_collected.pkl'
    if candidate_collected_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test.')

    # Load initial sim object, collect sim_object and copy for makign new sims
    candidate_sim = pickle.load(candidate_sim_file.open(mode="rb"))
    candidate_sim_expected = pickle.load(candidate_collected_file.open(mode="rb"))
    candidate_sim_ncores = copy.deepcopy(candidate_sim)

    # Set run directory
    run_dir = output_dir.joinpath('ncores_candidate')
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    # Make a new job based on the old job but with a new job ID
    old_job = candidate_sim.jobs[0]
    new_job = wrfhydropy.Job(job_id='ncores_candidate', exe_cmd=old_job._exe_cmd)

    # Remove old job and add new job
    candidate_sim_ncores.jobs.pop(0)
    candidate_sim_ncores.add(new_job)

    # Edit the sim object number of cores
    if candidate_sim_ncores.scheduler is not None:
        candidate_sim_ncores.scheduler.nproc = candidate_sim_ncores.scheduler.nproc - 1
    else:
        orig_exe_cmd = candidate_sim_ncores.jobs[0]._exe_cmd
        orig_exe_cmd = orig_exe_cmd.replace('-np 2','-np 1')

    # Recompose into new directory and run
    # catch warnings related to missing start and end job times
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_sim_ncores.compose()
    candidate_sim_ncores.run()

    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    wait_job(candidate_sim_ncores)
    candidate_sim_ncores.collect()

    # Check outputs
    diffs = wrfhydropy.outputdiffs.OutputDiffs(candidate_sim_ncores.output,
                                               candidate_sim_expected.output)

    # Assert all diff values are 0 and print diff stats if not
    has_diffs = any(value != 0 for value in diffs.diff_counts.values())
    if has_diffs:
        with capsys.disabled():
            print(diffs.diff_counts)
        for key, value in diffs.diff_counts.items():
            if value != 0:
                with capsys.disabled():
                    print(getattr(diffs, key))
    assert has_diffs == False, \
        'Outputs for candidate run with ncores do not match outputs with ncores-1'


#Perfect restarts question
def test_perfrestart_candidate(
    output_dir,
    capsys
):
    with capsys.disabled():
        print("\nQuestion: The candidate restarts from a restart run match the restarts"
              " from standard run?\n", end='')

    candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroSim_collected.pkl'
    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test.')

    # Load initial run model object and copy
    candidate_sim_expected = pickle.load(candidate_run_file.open(mode="rb"))
    candidate_sim_restart = copy.deepcopy(candidate_sim_expected)

    # Set run directory
    run_dir = output_dir.joinpath('restart_candidate')
    run_dir.mkdir(parents=True)
    os.chdir(str(run_dir))

    # Edit the sim object job start time to be one hour earlier
    restart_job = candidate_sim_restart.jobs[0]
    restart_job.model_start_time = restart_job.model_start_time + \
                                   dt.timedelta(hours=1)

    # Get restart files from previous run and symlink into restart sim dir
    ## Hydro
    expected_hydro_restart_file = candidate_sim_expected.output.restart_hydro[1]
    candidate_hydro_restart_file = pathlib.Path(expected_hydro_restart_file.name)
    candidate_hydro_restart_file.symlink_to(expected_hydro_restart_file)

    ## LSM
    expected_lsm_restart_file = candidate_sim_expected.output.restart_lsm[1]
    candidate_lsm_restart_file = pathlib.Path(expected_lsm_restart_file.name)
    candidate_lsm_restart_file.symlink_to(expected_lsm_restart_file)

    ## Nudging
    if len(candidate_sim_expected.output.restart_nudging) > 1:
        expected_nudging_restart_file = candidate_sim_expected.output.restart_nudging[1]
        candidate_nudging_restart_file = pathlib.Path(expected_nudging_restart_file.name)
        candidate_nudging_restart_file.symlink_to(expected_nudging_restart_file)


    # Make a new job based on the old job but with a new job ID
    old_job = candidate_sim_restart.jobs[0]
    new_job = wrfhydropy.Job(job_id='restart_candidate', exe_cmd=old_job._exe_cmd)

    # Remove old job and add new job
    candidate_sim_restart.jobs.pop(0)
    candidate_sim_restart.add(new_job)

    # Compose and run
    candidate_sim_restart.compose(force=True)
    candidate_sim_restart.run()

    # collect

    # Wait to collect until job has finished. All test runs are performed on a single job with
    # job_id='test_job'
    wait_job(candidate_sim_restart)
    candidate_sim_restart.collect()

    # Check outputs
    diffs = wrfhydropy.outputdiffs.OutputDiffs(candidate_sim_restart.output,
                                               candidate_sim_expected.output)

    # Assert all diff values are 0 and print diff stats if not
    has_diffs = any(value != 0 for value in diffs.diff_counts.values())
    if has_diffs:
        with capsys.disabled():
            print(diffs.diff_counts)
        for key, value in diffs.diff_counts.items():
            if value != 0:
                with capsys.disabled():
                    print('\n' + key + '\n')
                    print(getattr(diffs, key))
    assert has_diffs == False, \
        'Outputs for candidate run do not match outputs from candidate restart run'
