import pathlib
import shutil
import pytest
import wrfhydropy


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

    parser.addoption(
        "--config",
        required=True,
        action='store',
        help=("The configuration to test, "
              "must be one listed in trunk/NDHMS/hydro_namelist.json keys.")
    )

    # Optional args

    parser.addoption(
        '--compiler',
        default='gfort',
        required=False,
        action='store',
        help='compiler, options are ifort or gfort'
    )

    parser.addoption(
        "--option_suite",
        required=False,
        action='store',
        help=("An option suite to test on top of the specified configuration,"
              "must be one listed in hydro_option_suites.json")
    )

    parser.addoption(
        '--ncores',
        default='2',
        required=False,
        action='store',
        help='Number of cores to use for testing'
    )

    parser.addoption(
        '--scheduler',
        action='store_true',
        help='Use PBS scheduler on cheyenne'
    )

    parser.addoption(
        '--nnodes',
        default='2',
        required=False,
        help='Number of nodes to use for testing if running on scheduler'
    )

    parser.addoption(
        '--account',
        default='NRAL0017',
        required=False,
        action='store',
        help='Account number to use if using a scheduler.'
    )

    parser.addoption(
        '--walltime',
        default='02:00:00',
        required=False,
        action='store',
        help='Wall clock time for each test run in hh:mm:ss format'
    )

    parser.addoption(
        '--queue',
        default='regular',
        required=False,
        action='store',
        help='Queue to use if running on NCAR Cheyenne, options are regular, '
        'premium, or shared'
    )

    parser.addoption(
        '--exe_cmd',
        default='mpirun -np {0} ./wrf_hydro.exe',
        required=False,
        action='store',
        help='The MPI-dependent model execution command. Default is best guess. '
        'The first/zeroth variable is set to the total number of cores. The '
        'wrf_hydro_py convention is that the exe is always named wrf_hydro.exe.'
    )

    parser.addoption(
        '--use_existing_test_dir',
        default=False,
        required=False,
        action='store_true',
        help='Use existing compiles and runs, only perform output comparisons.'
    )

    parser.addoption(
        '--xrcmp_n_cores',
        default=0,
        required=False,
        help='Use xrcmp if > 0, and how many cores if so?'
    )


def _make_sim(
    domain_dir,
    compiler,
    source_dir,
    configuration,
    option_suite,
    ncores,
    nnodes,
    scheduler,
    account,
    walltime,
    queue
):

    model = wrfhydropy.Model(
        compiler=compiler,
        source_dir=source_dir,
        model_config=configuration
    )

    domain = wrfhydropy.Domain(
        domain_top_dir=domain_dir,
        domain_config=configuration
    )

    sim = wrfhydropy.Simulation()
    sim.add(model)
    sim.add(domain)

    # Update base namelists with option suite if specified
    if option_suite is not None:
        pass

    if scheduler:
        sim.add(
            wrfhydropy.schedulers.PBSCheyenne(
                account=account,
                nproc=int(ncores),
                nnodes=int(nnodes),
                walltime=walltime,
                queue=queue
            )
        )

    return sim


@pytest.fixture(scope="session")
def exe_cmd(request):
    return request.config.getoption("--exe_cmd")


@pytest.fixture(scope="session")
def candidate_sim(request):

    domain_dir = request.config.getoption("--domain_dir")
    compiler = request.config.getoption("--compiler")
    candidate_dir = request.config.getoption("--candidate_dir")
    configuration = request.config.getoption("--config")
    option_suite = request.config.getoption("--option_suite")
    ncores = request.config.getoption("--ncores")
    nnodes = request.config.getoption("--nnodes")
    scheduler = request.config.getoption("--scheduler")
    account = request.config.getoption("--account")
    walltime = request.config.getoption("--walltime")
    queue = request.config.getoption("--queue")

    candidate_sim = _make_sim(
        domain_dir=domain_dir,
        compiler=compiler,
        source_dir=candidate_dir,
        configuration=configuration,
        option_suite=option_suite,
        ncores=ncores,
        nnodes=nnodes,
        scheduler=scheduler,
        account=account,
        walltime=walltime,
        queue=queue
    )

    return candidate_sim


# Since we want to leverage the NWM compiles (both candidate and reference, ptoentially),
# we create additional simulation fixtures where we hard-code the alternate configurations,
# those are "channel-only" and "nwm_output_ana", currently.
@pytest.fixture(scope="session")
def candidate_channel_only_sim(request):

    domain_dir = request.config.getoption("--domain_dir")
    compiler = request.config.getoption("--compiler")
    candidate_dir = request.config.getoption("--candidate_dir")
    option_suite = request.config.getoption("--option_suite")
    ncores = request.config.getoption("--ncores")
    nnodes = request.config.getoption("--nnodes")
    scheduler = request.config.getoption("--scheduler")
    account = request.config.getoption("--account")
    walltime = request.config.getoption("--walltime")
    queue = request.config.getoption("--queue")

    configuration = "nwm_channel-only"

    candidate_channel_only_sim = _make_sim(
        domain_dir=domain_dir,
        compiler=compiler,
        source_dir=candidate_dir,
        configuration=configuration,
        option_suite=option_suite,
        ncores=ncores,
        nnodes=nnodes,
        scheduler=scheduler,
        account=account,
        walltime=walltime,
        queue=queue
    )

    return candidate_channel_only_sim


@pytest.fixture(scope="session")
def candidate_nwm_output_sim(request):

    domain_dir = request.config.getoption("--domain_dir")
    compiler = request.config.getoption("--compiler")
    candidate_dir = request.config.getoption("--candidate_dir")
    option_suite = request.config.getoption("--option_suite")
    ncores = request.config.getoption("--ncores")
    nnodes = request.config.getoption("--nnodes")
    scheduler = request.config.getoption("--scheduler")
    account = request.config.getoption("--account")
    walltime = request.config.getoption("--walltime")
    queue = request.config.getoption("--queue")

    configuration = request.config.getoption("--config")
    if configuration == 'nwm_long_range':
        configuration = "nwm_output_long_range"
    else:
        configuration = "nwm_output_ana"

    candidate_nwm_output_sim = _make_sim(
        domain_dir=domain_dir,
        compiler=compiler,
        source_dir=candidate_dir,
        configuration=configuration,
        option_suite=option_suite,
        ncores=ncores,
        nnodes=nnodes,
        scheduler=scheduler,
        account=account,
        walltime=walltime,
        queue=queue
    )

    return candidate_nwm_output_sim


@pytest.fixture(scope="session")
def reference_sim(request):

    domain_dir = request.config.getoption("--domain_dir")
    compiler = request.config.getoption("--compiler")
    reference_dir = request.config.getoption("--reference_dir")
    configuration = request.config.getoption("--config")
    option_suite = request.config.getoption("--option_suite")
    ncores = request.config.getoption("--ncores")
    nnodes = request.config.getoption("--nnodes")
    scheduler = request.config.getoption("--scheduler")
    account = request.config.getoption("--account")
    walltime = request.config.getoption("--walltime")
    queue = request.config.getoption("--queue")

    reference_sim = _make_sim(
        domain_dir=domain_dir,
        compiler=compiler,
        source_dir=reference_dir,
        configuration=configuration,
        option_suite=option_suite,
        ncores=ncores,
        nnodes=nnodes,
        scheduler=scheduler,
        account=account,
        walltime=walltime,
        queue=queue
    )

    return reference_sim


# Since we want to leverage the NWM compiles (both candidate and reference, ptoentially),
# we create additional simulation fixtures where we hard-code the alternate configurations,
# those are "channel-only" and "nwm_output_ana", currently.
@pytest.fixture(scope="session")
def reference_nwm_output_sim(request):

    domain_dir = request.config.getoption("--domain_dir")
    compiler = request.config.getoption("--compiler")
    reference_dir = request.config.getoption("--reference_dir")
    option_suite = request.config.getoption("--option_suite")
    ncores = request.config.getoption("--ncores")
    nnodes = request.config.getoption("--nnodes")
    scheduler = request.config.getoption("--scheduler")
    account = request.config.getoption("--account")
    walltime = request.config.getoption("--walltime")
    queue = request.config.getoption("--queue")

    configuration = request.config.getoption("--config")
    if configuration == 'nwm_long_range':
        configuration = "nwm_output_long_range"
    else:
        configuration = "nwm_output_ana"

    reference_nwm_output_sim = _make_sim(
        domain_dir=domain_dir,
        compiler=compiler,
        source_dir=reference_dir,
        configuration=configuration,
        option_suite=option_suite,
        ncores=ncores,
        nnodes=nnodes,
        scheduler=scheduler,
        account=account,
        walltime=walltime,
        queue=queue
    )

    return reference_nwm_output_sim


@pytest.fixture(scope="session")
def output_dir(request):
    configuration = request.config.getoption("--config")
    output_dir = request.config.getoption("--output_dir")
    use_existing_test_dir = request.config.getoption("--use_existing_test_dir")

    output_dir = pathlib.Path(output_dir)
    output_dir = output_dir / configuration
    if not use_existing_test_dir:
        output_dir.mkdir(parents=True)

    return output_dir


@pytest.fixture(scope="session")
def ncores(request):
    return int(request.config.getoption("--ncores"))


@pytest.fixture(scope="session")
def xrcmp_n_cores(request):
    return int(request.config.getoption("--xrcmp_n_cores"))
