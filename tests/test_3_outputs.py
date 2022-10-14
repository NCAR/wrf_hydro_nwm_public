import pathlib
import pickle

candidate_run_file = pathlib.Path('run_candidate/WrfHydroSim_collected.pkl')


# regression question
def test_output_has_nans(output_dir, xrcmp_n_cores):
    print("\nQuestion: Outputs from all tests are free of nans in data and attributes\n", end="")
    print('\n')

    sim_files = output_dir.rglob('WrfHydroSim_collected.pkl')

    for file in sim_files:
        sim = pickle.load(file.open('rb'))
        sim_nans = sim.output.check_output_nans(n_cores=xrcmp_n_cores)
        assert sim_nans is None, \
            'nans found in the following files ' + sim_nans['file'].unique()
