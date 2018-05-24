import json
import pathlib
import pytest
import sys
import warnings
from wrfhydropy import *


def pytest_addoption(parser):

    # Required args:

    parser.addoption(
        '--domain_dir',
        required=True,
        action='store',
        help='domain directory'
    )

    parser.addoption(
        '--output_dir',
        required=True,
        action='store',
        help='test output directory'
    )

    parser.addoption(
        '--candidate_dir',
        required=True,
        action='store',
        help='candidate model directory'
    )

    parser.addoption(
        '--reference_dir',
        required=True,
        action='store',
        help='reference model directory'
    )

    # Optional args:

    parser.addoption(
        "--config",
        default=['NWM', 'Gridded', 'Reach'],
        nargs='+',
        help=("List of model configurations to test, options are 'NWM'," +
              "'Gridded',and 'Reach'")
    )

    parser.addoption(
        '--compiler',
        default='gfort',
        required=False,
        action='store',
        help='compiler key from config'
    )

    parser.addoption(
        '--job_default',
        default=None,
        required=False,
        action='store',
        help='candidate job'
    )

    parser.addoption(
        '--job_ncores',
        default=None,
        required=False,
        action='store',
        help='ncores job'
    )

    parser.addoption(
        '--scheduler',
        default=None,
        required=False,
        action='store',
        help='scheduler for all jobs')


def pytest_generate_tests(metafunc):
    if 'configuration' in metafunc.fixturenames:
        metafunc.parametrize(
            "configuration",
            metafunc.config.option.config,
            scope="session"
        )


@pytest.fixture(scope="session")
def candidate_setup(request, configuration):

    domain_dir = request.config.getoption("--domain_dir")
    candidate_dir = request.config.getoption("--candidate_dir")

    # Candidate model
    candidate_model = WrfHydroModel(
        source_dir=candidate_dir,
        model_config=configuration
    )

    # Candidate domain
    domain = WrfHydroDomain(
        domain_top_dir=domain_dir,
        domain_config=configuration,
        model_version=candidate_model.version
    )

    # Candidate setup
    candidate_setup = WrfHydroSetup(candidate_model, domain)

    return candidate_setup


@pytest.fixture(scope="session")
def reference_setup(request, configuration):

    domain_dir = request.config.getoption("--domain_dir")
    reference_dir = request.config.getoption("--reference_dir")

    # Reference model
    reference_model = WrfHydroModel(
        source_dir=reference_dir,
        model_config=configuration
    )

    # Reference domain
    domain = WrfHydroDomain(
        domain_top_dir=domain_dir,
        domain_config=configuration,
        model_version=reference_model.version
    )

    # Reference setup
    reference_setup = WrfHydroSetup(reference_model, domain)

    return reference_setup


@pytest.fixture(scope="session")
def compiler(request):
    return request.config.getoption("--compiler")


@pytest.fixture(scope="session")
def job_default(request, configuration):
    job_in = request.config.getoption("--job_default")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        job_dum = Job(nproc=-1)
    if job_in is not None:
        job_in = json.loads(job_in)
        job_dum.__dict__.update(job_in)
    return job_dum


@pytest.fixture(scope="session")
def job_ncores(request, configuration):
    job_in = request.config.getoption("--job_ncores")
    print("job_in: ", job_in)

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        job_dum = Job(nproc=-1)
    if job_in is not None:
        job_in = json.loads(job_in)
        job_dum.__dict__.update(job_in)
    return job_dum


@pytest.fixture(scope="session")
def scheduler(request, configuration):
    sched_in = request.config.getoption("--scheduler")
    if sched_in is None or sched_in == 'null':
        return None
    else:
        sched = json.loads(sched_in)
        sched_dum = Scheduler(
            job_name='default',
            account='DUMDUM',
            walltime='00:01:00',
            queue='none',
            nproc=1,
            ppn=1
        )
        sched_dum.__dict__.update(sched)
        return sched_dum


@pytest.fixture(scope="session")
def output_dir(request,configuration):
    output_dir = request.config.getoption("--output_dir")
    output_dir = pathlib.Path(output_dir)
    output_dir = output_dir / configuration
    if output_dir.is_dir() is False:
        output_dir.mkdir(parents=True)
    return output_dir
