import pytest
import sys
import pathlib
from wrfhydropy import *

def pytest_addoption(parser):
    parser.addoption('--domain_dir', action='store', help='domain directory')
    parser.addoption('--candidate_dir', action='store', help='candidate directory')
    parser.addoption('--reference_dir', action='store', help='reference directory')
    parser.addoption('--output_dir', action='store', help='test output directory')

@pytest.fixture(scope="session")
def candidate_sim(request):
    # Setup a candidate model
    domain_dir = request.config.getoption("--domain_dir")
    candidate_dir = request.config.getoption("--candidate_dir")

    # Setup a candidate model
    candidate_model = WrfHydroModel(candidate_dir)

    # Setup a domain
    domain = WrfHydroDomain(domain_top_dir=domain_dir,
                            domain_config='NWM',
                            model_version=candidate_model.version)

    # Setup a candidate simulation
    candidate_sim = WrfHydroSim(candidate_model, domain)

    return candidate_sim

@pytest.fixture(scope="session")
def reference_sim(request):
    # Setup a candidate model
    domain_dir = request.config.getoption("--domain_dir")
    reference_dir = request.config.getoption("--reference_dir")

    # Setup a candidate model
    reference_model = WrfHydroModel(reference_dir)

    # Setup a domain
    domain = WrfHydroDomain(domain_top_dir=domain_dir,
                            domain_config='NWM',
                            model_version=reference_model.version)

    # Setup a candidate simulation
    reference_sim = WrfHydroSim(reference_model, domain)

    return reference_sim

@pytest.fixture(scope="session")
def output_dir(request):
    output_dir = pathlib.Path(request.config.getoption("--output_dir"))
    if output_dir.is_dir() is False:
        output_dir.mkdir()
    return output_dir