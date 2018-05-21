import pytest
import sys
import pathlib
from wrfhydropy import *

def pytest_addoption(parser):
    parser.addoption("--config", default=['NWM','Gridded','Reach'], nargs='+',
        help="List of model configurations to test, options are 'NWM','Gridded',and'Reach'")
    parser.addoption('--domain_dir', required=True, action='store', help='domain directory')
    parser.addoption('--candidate_dir', required=True, action='store', help='candidate directory')
    parser.addoption('--reference_dir', required=True, action='store', help='reference directory')
    parser.addoption('--output_dir', required=True, action='store', help='test output directory')

def pytest_generate_tests(metafunc):
    if 'configuration' in metafunc.fixturenames:
        metafunc.parametrize("configuration",
                             metafunc.config.option.config,
                             scope="session")

@pytest.fixture(scope="session")
def candidate_sim(request,configuration):
    # Setup a candidate model
    domain_dir = request.config.getoption("--domain_dir")
    candidate_dir = request.config.getoption("--candidate_dir")

    # Setup a candidate model
    candidate_model = WrfHydroModel(source_dir=candidate_dir,model_config=configuration)

    # Setup a domain
    domain = WrfHydroDomain(domain_top_dir=domain_dir,
                            domain_config=configuration,
                            model_version=candidate_model.version)

    # Setup a candidate simulation
    candidate_sim = WrfHydroSim(candidate_model, domain)

    return candidate_sim

@pytest.fixture(scope="session")
def reference_sim(request,configuration):
    # Setup a candidate model
    domain_dir = request.config.getoption("--domain_dir")
    reference_dir = request.config.getoption("--reference_dir")

    # Setup a candidate model
    reference_model = WrfHydroModel(source_dir=reference_dir,model_config=configuration)

    # Setup a domain
    domain = WrfHydroDomain(domain_top_dir=domain_dir,
                            domain_config=configuration,
                            model_version=reference_model.version)

    # Setup a candidate simulation
    reference_sim = WrfHydroSim(reference_model, domain)

    return reference_sim

@pytest.fixture(scope="session")
def output_dir(request,configuration):
    output_dir = request.config.getoption("--output_dir")
    output_dir = pathlib.Path(output_dir)
    output_dir = output_dir / configuration
    if output_dir.is_dir() is False:
        output_dir.mkdir(parents=True)
    return output_dir