import sys
import wrfhydropy
import shutil
import pickle
import datetime as dt
import copy
import warnings
import pytest


# regression question
def test_regression(output_dir, capsys):
    with capsys.disabled():
        print("\nQuestion: The candidate standard run restarts match the reference standard restarts?",
              end="")

    # Check for existence of run objects
    candidate_run_file =  output_dir / 'run_candidate' / 'WrfHydroRun.pkl'
    reference_run_file =  output_dir / 'run_reference' / 'WrfHydroRun.pkl'

    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')
    if reference_run_file.is_file() is False:
        pytest.skip('Reference run object not found, skipping test')

    # Load run objects
    candidate_run_expected = pickle.load(open(candidate_run_file,"rb"))
    reference_run_expected = pickle.load(open(reference_run_file,"rb"))

    #Check regression
    regression_diffs = wrfhydropy.RestartDiffs(candidate_run_expected,reference_run_expected)

    ## Check hydro restarts
    for diff in regression_diffs.hydro:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff == None, "Candidate hydro restart files do not regress on reference restart files"

    ## Check lsm restarts
    for diff in regression_diffs.lsm:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff == None, "Candidate lsm restart files do not regress on reference restart files"

    ## Check nudging restarts
    for diff in regression_diffs.nudging:
        if diff is not None:
            with capsys.disabled():
                print(diff)
        assert diff == None, "Candidate nudging restart files do not regress on reference restart files"

def test_chrtout(output_dir,capsys):
    compare_ncfiles = wrfhydropy.utilities.compare_restarts #TODO remove when compare_restarts is
    # deprecated
    with capsys.disabled():
        print("\nQuestion: The candidate CHRTOUT files match the reference CHRTOUT files?", end="")

    # Check for existence of run objects
    candidate_run_file =  output_dir / 'run_candidate' / 'WrfHydroRun.pkl'
    reference_run_file =  output_dir / 'run_reference' / 'WrfHydroRun.pkl'

    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')
    if reference_run_file.is_file() is False:
        pytest.skip('Reference run object not found, skipping test')

    # Load objects
    candidate_run = pickle.load(open(candidate_run_file,"rb"))
    reference_run = pickle.load(open(reference_run_file,"rb"))

    # Check CHRTOUT files
    chrtout_diffs = compare_ncfiles(candidate_run.channel_rt, reference_run.channel_rt)

    if all(v is None for v in chrtout_diffs):
        has_diffs = False
    else:
        has_diffs = True
        #chrtout_diffs.remove(None)
        for the_diff in chrtout_diffs[0:5]:
            with capsys.disabled():
                print(the_diff)

    assert has_diffs == False, 'CHRTOUT output files differ between candidate and reference'

def test_chanobs(output_dir, capsys):
    compare_ncfiles = wrfhydropy.utilities.compare_restarts #TODO remove when compare_restarts is deprecated
    with capsys.disabled():
        print("\nQuestion: The candidate CHANOBS files match the reference CHANOBS files?", end="")

    # Check for existence of run objects
    candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroRun.pkl'
    reference_run_file = output_dir / 'run_reference' / 'WrfHydroRun.pkl'

    if candidate_run_file.is_file() is False:
        pytest.skip('Candidate run object not found, skipping test')
    if reference_run_file.is_file() is False:
        pytest.skip('Reference run object not found, skipping test')

    # Load objects
    candidate_run = pickle.load(open(candidate_run_file, "rb"))
    reference_run = pickle.load(open(reference_run_file, "rb"))

    # Check CHRTOUT files
    chanobs_diffs = compare_ncfiles(candidate_run.chanobs, reference_run.chanobs)

    if all(v is None for v in chanobs_diffs):
        has_diffs = False
    else:
        has_diffs = True
        #chanobs_diffs.remove(None)
        for the_diff in chanobs_diffs[0:5]:
            with capsys.disabled():
                print(the_diff)

    assert has_diffs == False, 'CHANOBS output files differ between candidate and reference'
