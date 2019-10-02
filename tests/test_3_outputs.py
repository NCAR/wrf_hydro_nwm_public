import datetime
import pathlib
import pickle
import time

import pytest

from tests.utilities import DEFAULT_LOOP_WAIT, MODEL_RUN_TIME, wait_for_output_files

candidate_run_file = pathlib.Path('run_candidate/WrfHydroSim_collected.pkl')


# regression question
def test_output_has_nans(output_dir, xrcmp_n_cores):
    print("\nQuestion: Outputs from all tests are free of nans in data and attributes\n", end="")
    print('\n')

    # TODO: there's probably a better way to do this
    wait_for_output_files(output_dir, 4)

    sim_files = output_dir.rglob('WrfHydroSim_collected.pkl')
    for file in sim_files:
        sim = pickle.load(file.open('rb'))
        sim_nans = sim.output.check_output_nans(n_cores=xrcmp_n_cores)
        assert sim_nans is None, \
            'nans found in the following files ' + sim_nans['file'].unique()
