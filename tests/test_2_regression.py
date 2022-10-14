import pathlib
import pickle
import sys
import warnings

import pytest
import wrfhydropy

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from utilities import print_diffs

# Ignore reference time in regression test because it depends on wall time of model run
EXCLUDE_VARS = ['reference_time']

# regression question
def test_regression_data(output_dir, xrcmp_n_cores):
    print("\nQuestion: The candidate run data values match the reference run?\n", end="")
    print('\n')

    # Check for existence of sim objects
    candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroSim_collected.pkl'
    reference_run_file = output_dir / 'run_reference' / 'WrfHydroSim_collected.pkl'

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
        data_diffs = wrfhydropy.outputdiffs.OutputDataDiffs(
            candidate_output=candidate_run_expected.output,
            reference_output=reference_run_expected.output,
            exclude_vars=EXCLUDE_VARS,
            xrcmp_n_cores=xrcmp_n_cores
        )

    # Assert all diff values are 0 and print diff stats if not
    has_data_diffs = any(value != 0 for value in data_diffs.diff_counts.values())
    if has_data_diffs:
        print_diffs(data_diffs)
    assert has_data_diffs is False, \
        'Data values in outputs for candidate run do not match reference run'


# regression question
def test_regression_metadata(output_dir, xrcmp_n_cores):
    print("\nQuestion: The candidate run output metadata match the reference run?\n", end="")
    print('\n')

    # Check for existence of sim objects
    candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroSim_collected.pkl'
    reference_run_file = output_dir / 'run_reference' / 'WrfHydroSim_collected.pkl'

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
            reference_run_expected.output,
            exclude_vars=EXCLUDE_VARS,
            xrcmp_n_cores=xrcmp_n_cores
        )

    # Assert all diff values are 0 and print diff stats if not
    has_metadata_diffs = any(value != 0 for value in meta_data_diffs.diff_counts.values())
    if has_metadata_diffs:
        print_diffs(meta_data_diffs)
    assert has_metadata_diffs is False, \
        'Metadata and attributes in outputs for candidate run do not match reference run'
