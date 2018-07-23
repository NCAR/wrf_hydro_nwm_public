import copy
import datetime as dt
import json
import pickle
import pytest
import shutil
import time
import warnings
import wrfhydropy

##################################
# Setup the test with a domain, a candidate, and a reference.
# Get domain, reference, candidate, and optional output directory from command line arguments
# Setup a domain


##################################
# Define tests


###Compile questions compiler,
def test_compile_candidate(
    candidate_setup,
    output_dir,
    compiler,
    capsys
):
    with capsys.disabled():
        print("\nQuestion: The candidate compiles?", end='')

    compile_dir = output_dir / 'compile_candidate'

    # Compile the model
    candidate_setup.model.compile(
        compiler=compiler,
        compile_dir=compile_dir,
        overwrite=True
    )

    # Check compilation status
    assert candidate_setup.model.compile_log.returncode == 0, \
        "Candidate code did not compile correctly."


def test_compile_reference(
    reference_setup,
    output_dir,
    compiler,
    capsys
):
    with capsys.disabled():
        print("\nQuestion: The reference compiles?", end='')

    compile_dir = output_dir / 'compile_reference'

    # Compile the model
    reference_setup.model.compile(
        compiler=compiler,
        compile_dir=compile_dir,
        overwrite=True
    )

    # Check compilation status
    assert reference_setup.model.compile_log.returncode == 0, \
        "Reference code did not compile correctly"



# Run questions
def test_run_candidate(
    candidate_setup,
    output_dir,
    job_default,
    scheduler,
    capsys
):
    with capsys.disabled():
        print("\nQuestion: The candidate runs successfully?", end='')

    # Set run directory
    run_dir = output_dir / 'run_candidate'

    candidate_job = job_default
    candidate_job.scheduler = scheduler

    # Run
    candidate_run = wrfhydropy.WrfHydroRun(
        wrf_hydro_setup=candidate_setup,
        run_dir=run_dir,
        jobs=candidate_job
    )
    check_run_dir = candidate_run.run_jobs()

    if scheduler is not None:
        candidate_run = wrfhydropy.job_tools.restore_completed_scheduled_job(check_run_dir)

    # Check subprocess and model run status
    assert candidate_run.jobs_completed[0].exit_status == 0, \
        "Candidate code run exited with non-zero status"
    assert candidate_run.jobs_completed[0].job_status == 'completed success', \
        "Candidate code run did not complete"


def test_run_reference(
    reference_setup,
    output_dir,
    job_default,
    scheduler,
    capsys
):
    with capsys.disabled():
        print("\nQuestion: The reference runs successfully?", end='')

    #Set run directory
    run_dir = output_dir / 'run_reference'

    reference_job = job_default
    reference_job.scheduler = scheduler

    # Run
    reference_run = wrfhydropy.WrfHydroRun(
        wrf_hydro_setup=reference_setup,
        run_dir=run_dir,
        jobs=reference_job
    )
    check_run_dir = reference_run.run_jobs()

    if scheduler is not None:
        reference_run = wrfhydropy.job_tools.restore_completed_scheduled_job(check_run_dir)

    # Check subprocess and model run status
    assert reference_run.jobs_completed[0].exit_status == 0, \
        "Reference code run exited with non-zero status"
    assert reference_run.jobs_completed[0].job_status == 'completed success', \
        "Reference code run did not complete"


#Ncores question
def test_ncores_candidate(
    candidate_setup,
    output_dir,
    job_ncores,
    scheduler,
    capsys
):
    with capsys.disabled():
        print("\nQuestion: The candidate restarts from a 1 core run match restarts from standard run?",
              end='')

    candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroRun.pkl'
    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test.')

    # Load initial run model object
    candidate_run_expected = pickle.load(open(candidate_run_file, "rb"))
    # Set run directory
    run_dir = output_dir.joinpath('ncores_candidate')

    candidate_ncores_job = job_ncores
    candidate_ncores_job.scheduler = scheduler

    # Run
    candidate_ncores_run = wrfhydropy.WrfHydroRun(
        wrf_hydro_setup=candidate_setup,
        run_dir=run_dir,
        jobs=candidate_ncores_job
    )
    check_run_dir = candidate_ncores_run.run_jobs()

    if scheduler is not None:
        candidate_ncores_run = wrfhydropy.job_tools.restore_completed_scheduled_job(check_run_dir)
    
    #Check against initial run
    ncores_restart_diffs = wrfhydropy.RestartDiffs(candidate_ncores_run, candidate_run_expected)

    ## Check hydro restarts
    for diff in ncores_restart_diffs.hydro:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff == None, "Candidate hydro restart files do not match when run with different number of cores"

    ## Check lsm restarts
    for diff in ncores_restart_diffs.lsm:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff == None, "Candidate lsm restart files do not match when run with different number of cores"

    ## Check nudging restarts
    for diff in ncores_restart_diffs.nudging:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff == None, "Candidate nudging restart files do not match when run with different number of cores"


#Perfect restarts question
def test_perfrestart_candidate(
    candidate_setup,
    output_dir,
    job_default,
    scheduler,
    capsys
):
    with capsys.disabled():
        print("\nQuestion: The candidate restarts from a restart run match the restarts from standard run?",
              end='')

    candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroRun.pkl'
    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')

    # Load initial run model object
    candidate_run_expected = pickle.load(open(output_dir / 'run_candidate' / 'WrfHydroRun.pkl',
                                              "rb"))

    #Make deep copy since changing namelist optoins
    perfrestart_setup = copy.deepcopy(candidate_setup)

    # Set run directory
    run_dir = output_dir / 'restart_candidate'

    # Establish the run (run after setting external files)
    candidate_perfrestart_job = job_default
    # TODO(JLM): edit scheduler names
    candidate_perfrestart_job.scheduler = scheduler

    # Add the jobs after determining the restart time.
    candidate_perfrestart_run = wrfhydropy.WrfHydroRun(
        wrf_hydro_setup=perfrestart_setup,
        run_dir=run_dir,
        mode='r'
    )

    # Symlink restarts files to new directory and modify namelistrestart files
    # Hydro
    hydro_rst = candidate_run_expected.restart_hydro[0]
    new_hydro_rst_path = run_dir.joinpath(hydro_rst.name)
    new_hydro_rst_path.unlink()
    new_hydro_rst_path.symlink_to(hydro_rst)

    perfrestart_setup.hydro_namelist['hydro_nlist'].update(
        {'restart_file': str(new_hydro_rst_path)})

    # LSM
    lsm_rst = candidate_run_expected.restart_lsm[0]
    new_lsm_rst_path = run_dir.joinpath(lsm_rst.name)
    new_lsm_rst_path.unlink()
    new_lsm_rst_path.symlink_to(lsm_rst)

    perfrestart_setup.namelist_hrldas['noahlsm_offline'].update(
        {'restart_filename_requested': str(run_dir.joinpath(lsm_rst.name))})

    # Nudging
    if candidate_run_expected.restart_nudging is not None and \
       len(candidate_run_expected.restart_nudging) > 0:
        nudging_rst = candidate_run_expected.restart_nudging[0]
        new_nudging_rst_path = run_dir.joinpath(nudging_rst.name)
        new_nudging_rst_path.unlink()
        new_nudging_rst_path.symlink_to(nudging_rst)

        perfrestart_setup.hydro_namelist['nudging_nlist'].update(
            {'nudginglastobsfile': str(run_dir.joinpath(nudging_rst.name))})


    # Setup the restart in the run.
    orig_start_time, orig_end_time = wrfhydropy.job_tools.solve_model_start_end_times(
        None,
        None,
        candidate_perfrestart_run.setup
    )

    restart_dt = hydro_rst.open()        
    restart_time = dt.datetime.strptime(restart_dt.Restart_Time,'%Y-%m-%d_%H:%M:%S')

    candidate_perfrestart_job.model_start_time = restart_time
    candidate_perfrestart_job.model_end_time = orig_end_time

    # Run
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_perfrestart_run.add_jobs(candidate_perfrestart_job)
        check_run_dir = candidate_perfrestart_run.run_jobs()
        if scheduler is not None:
            candidate_perfrestart_run = \
                wrfhydropy.job_tools.restore_completed_scheduled_job(check_run_dir)

    #Check against initial run
    perfstart_restart_diffs = wrfhydropy.RestartDiffs(
        candidate_perfrestart_run,
        candidate_run_expected
    )
    ## Check hydro restarts
    for diff in perfstart_restart_diffs.hydro:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff is None, \
            "Candidate hydro restart files do not match when starting from a restart"

    ## Check lsm restarts
    for diff in perfstart_restart_diffs.lsm:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff is None, \
            "Candidate lsm restart files do not match when starting from a restart"

    ## Check nudging restarts
    for diff in perfstart_restart_diffs.nudging:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff is None, \
            "Candidate nudging restart files do not match when starting from a restart"
