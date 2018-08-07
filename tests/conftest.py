import pytest
import shutil
import pathlib
from wrfhydropy import *


def pytest_addoption(parser):

    # Required args:

    parser.addoption('--domain_dir',
                     required=True,
                     action='store',
                     help='domain directory'
                     )

    parser.addoption('--output_dir',
                     required=True,
                     action='store',
                     help='test output directory'
                     )

    parser.addoption('--candidate_dir',
                     required=True,
                     action='store',
                     help='candidate model directory'
                     )

    parser.addoption('--reference_dir',
                     required=True,
                     action='store',
                     help='reference model directory'
                     )

    # Optional args:
    parser.addoption("--config",
                     required=True,
                     action='store',
                     help=("The configuration to test, "
                             "must be one listed in trunk/NDHMS/hydro_namelist.json keys.")
                     )

    # Optional args

    parser.addoption('--compiler',
                     default='gfort',
                     required=False,
                     action='store',
                     help='compiler, options are intel or gfort'
                     )

    parser.addoption("--option_suite",
                     required=False,
                     action='store',
                     help=("An option suite to test on top of the specified configuration,"
                           "must be one listed in hydro_option_suites.json")
                     )

    parser.addoption( '--ncores',
                      default='2',
                      required=False,
                      action='store',
                      help='Number of cores to use for testing'
                      )

    parser.addoption('--scheduler',
                     action='store_true',
                     help='Use PBS scheduler on cheyenne')

    parser.addoption('--nnodes',
                     default='2',
                     required=False,
                     help='Number of nodes to use for testing if running on scheduler')

    parser.addoption('--account',
                     default='NRAL0017',
                     required=False,
                     action='store',
                     help='Account number to use if using a scheduler.')

def _make_sim(domain_dir,
              source_dir,
              configuration,
              option_suite,
              ncores,
              nnodes,
              scheduler,
              account):
    # model
    model = Model(
        source_dir=source_dir,
        model_config=configuration
    )

    # domain
    domain = Domain(
        domain_top_dir=domain_dir,
        domain_config=configuration)

    # Job
    # exe_command = ('mpirun -np {0} ./wrf_hydro.exe').format(str(ncores))
    # job = Job(job_id='test_job',exe_cmd=exe_command)

    # simulation
    sim = Simulation()
    sim.add(model)
    sim.add(domain)

    # Update base namelists with option suite if specified
    if option_suite is not None:
        pass

    if scheduler:
        sim.add(schedulers.PBSCheyenne(account=account,
                                       nproc=int(ncores),
                                       nnodes=nnodes,
                                       walltime='00:45:00'))


    return sim

@pytest.fixture(scope="session")
def candidate_sim(request):

    domain_dir = request.config.getoption("--domain_dir")
    candidate_dir = request.config.getoption("--candidate_dir")
    configuration = request.config.getoption("--config")
    option_suite = request.config.getoption("--option_suite")
    ncores = request.config.getoption("--ncores")
    nnodes = request.config.getoption("--nnodes")
    scheduler = request.config.getoption("--scheduler")
    account = request.config.getoption("--account")

    candidate_sim = _make_sim(domain_dir = domain_dir,
                              source_dir= candidate_dir,
                              configuration=configuration,
                              option_suite=option_suite,
                              ncores = ncores,
                              nnodes=nnodes,
                              scheduler = scheduler,
                              account = account)

    return candidate_sim

@pytest.fixture(scope="session")
def reference_sim(request):

    domain_dir = request.config.getoption("--domain_dir")
    reference_dir = request.config.getoption("--reference_dir")
    configuration = request.config.getoption("--config")
    option_suite = request.config.getoption("--option_suite")
    ncores = request.config.getoption("--ncores")
    nnodes = request.config.getoption("--nnodes")
    scheduler = request.config.getoption("--scheduler")
    account = request.config.getoption("--account")

    reference_sim = _make_sim(domain_dir = domain_dir,
                              source_dir= reference_dir,
                              configuration=configuration,
                              option_suite=option_suite,
                              ncores = ncores,
                              nnodes=nnodes,
                              scheduler = scheduler,
                              account = account)

    return reference_sim

@pytest.fixture(scope="session")
def output_dir(request):
    configuration = request.config.getoption("--config")
    output_dir = request.config.getoption("--output_dir")

    output_dir = pathlib.Path(output_dir)
    output_dir = output_dir / configuration

    if output_dir.is_dir() is True:
        shutil.rmtree(str(output_dir))

    output_dir.mkdir(parents=True)
    return output_dir

@pytest.fixture(scope="session")
def ncores(request):
    ncores = request.config.getoption("--ncores")

    return ncores

