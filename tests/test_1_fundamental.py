import sys
from wrfhydropy import *
import shutil
import pickle
import datetime as dt
import copy
import warnings
import pytest

#docker@da16b70279a1:~/wrf_hydro_py/wrfhydro$ pytest wrf_hydro_nwm_test_refactor --domain_dir
# /home/docker/wrf_hydro_py/wrfhydro/tests/data/domain --candidate_dir /home/docker/wrf_hydro_py/wrfhydro/tests/data/wrf_hydro_nwm/source --reference_dir /home/docker/wrf_hydro_py/wrfhydro/tests/data/wrf_hydro_nwm/source --output_dir /home/docker/tests

##################################
####Setup the test with a domain, a candidate, and a reference
# Get domain, reference, candidate, and optional output directory from command line arguments
# Setup a domain


##################################
# Define tests

###Compile questionscompiler,
def test_compile_candidate(candidate_sim,output_dir,capsys):
    with capsys.disabled():
        print("Question: The candidate compiles?")

    compile_dir = output_dir / 'compile_candidate'

    # Compile the model
    candidate_sim.model.compile(compiler = 'gfort',
                                compile_dir = compile_dir,
                                overwrite=True)

    # Check compilation status
    assert candidate_sim.model.compile_log.returncode == 0, "Candidate code did not compile correctly"


def test_compile_reference(reference_sim,output_dir,capsys):
    with capsys.disabled():
        print("Question: The reference compiles?")

    compile_dir = output_dir / 'compile_reference'

    # Compile the model
    reference_sim.model.compile(compiler = 'gfort',
                                compile_dir = compile_dir,
                                overwrite=True)

    # Check compilation status
    assert reference_sim.model.compile_log.returncode == 0, "Reference code did not compile correctly"



###Run questions
def test_run_candidate(candidate_sim,output_dir,capsys):
    with capsys.disabled():
        print("Question: The candidate will run without error, i.e. standard run?")

    # Set simulation directory
    simulation_dir = output_dir / 'run_candidate'

    # Run the simulation
    candidate_run = candidate_sim.run(simulation_dir=simulation_dir,
                                      num_cores=2,
                                      mode='w')

    # Check subprocess and model run status
    assert candidate_run.run_log.returncode == 0, "Candidate code run exited with non-zero status"
    assert candidate_run.run_status == 0, "Candidate code run did not complete"

def test_run_reference(reference_sim,output_dir,capsys):
    with capsys.disabled():
        print("Question: The reference will run without error, i.e. standard run?")

    #Set simulation directory
    simulation_dir = output_dir / 'run_reference'

    # Run the simulation
    reference_run = reference_sim.run(simulation_dir=simulation_dir,
                                      num_cores=2,
                                      mode='w')

    # Check subprocess and model run status
    assert reference_run.run_log.returncode == 0, "Reference code run exited with non-zero status"
    assert reference_run.run_status == 0, "Reference code run did not complete"


#Ncores question
def test_ncores_candidate(candidate_sim,output_dir,capsys):
    with capsys.disabled():
        print("Question: The candidate restarts from a 1 core run match restarts from standard run?")

    candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroRun.pkl'
    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')

    # Load initial run model object
    candidate_run_expected = pickle.load(open(candidate_run_file, "rb"))
    # Set simulation directory
    simulation_dir = output_dir.joinpath('ncores_candidate')

    # Run the simulation
    candidate_ncores_run = candidate_sim.run(simulation_dir=simulation_dir,
                                      num_cores=1,
                                             mode='w')

    #Check against initial run
    ncores_restart_diffs = RestartDiffs(candidate_ncores_run,candidate_run_expected)

    ## Check hydro restarts
    for diff in ncores_restart_diffs.hydro:
        assert diff == None, "Candidate hydro restart files do not match when run with different number of cores"

    ## Check lsm restarts
    for diff in ncores_restart_diffs.lsm:
        assert diff == None, "Candidate lsm restart files do not match when run with different number of cores"

    ## Check nudging restarts
    for diff in ncores_restart_diffs.nudging:
        assert diff == None, "Candidate nudging restart files do not match when run with different number of cores"


#Perfect restarts question
def test_perfrestart_candidate(candidate_sim,output_dir,capsys):
    with capsys.disabled():
        print("Question: The candidate restarts from a restart run match the restarts from standard run?")

    candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroRun.pkl'
    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')

    # Load initial run model object
    candidate_run_expected = pickle.load(open(output_dir / 'run_candidate' / 'WrfHydroRun.pkl',
                                              "rb"))

    #Make deep copy since changing namelist optoins
    perfrestart_sim = copy.deepcopy(candidate_sim)

    # Set simulation directory
    simulation_dir = output_dir / 'restart_candidate'

    #Make directory so that symlinks can be placed
    if simulation_dir.is_dir() is True:
        shutil.rmtree(str(simulation_dir))
    simulation_dir.mkdir(parents=True)

    # Symlink restarts files to new directory and modify namelistrestart files

    # Hydro
    hydro_rst = candidate_run_expected.restart_hydro[0]
    new_hydro_rst_path = simulation_dir.joinpath(hydro_rst.name)
    new_hydro_rst_path.symlink_to(hydro_rst)

    perfrestart_sim.hydro_namelist['hydro_nlist'].update(
        {'restart_file': str(new_hydro_rst_path)})

    # LSM
    lsm_rst = candidate_run_expected.restart_lsm[0]
    new_lsm_rst_path = simulation_dir.joinpath(lsm_rst.name)
    new_lsm_rst_path.symlink_to(lsm_rst)

    perfrestart_sim.namelist_hrldas['noahlsm_offline'].update(
        {'restart_filename_requested': str(simulation_dir.joinpath(lsm_rst.name))})

    # Nudging
    if len(candidate_run_expected.restart_nudging) > 0:
        nudging_rst = candidate_run_expected.restart_nudging[0]
        new_nudging_rst_path = simulation_dir.joinpath(nudging_rst.name)
        new_nudging_rst_path.symlink_to(nudging_rst)

        perfrestart_sim.hydro_namelist['nudging_nlist'].update(
            {'nudginglastobsfile': str(simulation_dir.joinpath(nudging_rst.name))})

    #Move simulation start time to restart time in hydro restart file
    start_dt = hydro_rst.open()
    start_dt = dt.datetime.strptime(start_dt.Restart_Time,'%Y-%m-%d_%H:%M:%S')
    perfrestart_sim.namelist_hrldas['noahlsm_offline'].update(
        {'start_year': start_dt.year,
         'start_month': start_dt.month,
         'start_day': start_dt.day,
         'start_hour': start_dt.hour,
         'start_min': start_dt.minute})

    #Adjust duration to be shorter by restart time delta in days
    hydro_rst_dt = perfrestart_sim.hydro_namelist['hydro_nlist']['rst_dt']
    previous_duration =  candidate_run_expected.simulation.namelist_hrldas['noahlsm_offline'][
        'kday']
    new_duration = int(previous_duration - hydro_rst_dt/60/24)
    perfrestart_sim.namelist_hrldas['noahlsm_offline'].update({'kday':new_duration})

    # Run the simulation
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        candidate_perfrestart_run = perfrestart_sim.run(simulation_dir=simulation_dir,
                                                        num_cores=2,
                                                        mode='a')

    #Check against initial run
    perfstart_restart_diffs = RestartDiffs(candidate_perfrestart_run,candidate_run_expected)
    ## Check hydro restarts
    for diff in perfstart_restart_diffs.hydro:
        assert diff == None, "Candidate hydro restart files do not match when starting from a restart"

    ## Check lsm restarts
    for diff in perfstart_restart_diffs.lsm:
        assert diff == None, "Candidate lsm restart files do not match when starting from a restart"

    ## Check nudging restarts
    for diff in perfstart_restart_diffs.nudging:
        assert diff == None, "Candidate nudging restart files do not match when starting from a restart"


