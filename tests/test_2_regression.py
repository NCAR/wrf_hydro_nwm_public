import pickle
import pytest
import wrfhydropy


#regression question
def test_regression(output_dir, capsys):
    with capsys.disabled():
        print("\nQuestion: The candidate run outputs match the reference run outputs?", end="")

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

    #Check regression
    diffs = wrfhydropy.outputdiffs.OutputDiffs(candidate_run_expected.output,
                                               reference_run_expected.output)

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
        'Outputs for candidate run do not match outputs from reference run'
