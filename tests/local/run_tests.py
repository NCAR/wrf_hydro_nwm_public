import pathlib
import shutil
import socket
import subprocess
import sys
from argparse import ArgumentParser

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from utils.releaseapi import get_release_asset
from utils.gdrive_download import download_file_from_google_drive

def run_tests(
    config: str,
    compiler: str,
    domain_dir: str,
    candidate_dir: str,
    reference_dir: str,
    output_dir: str,
    scheduler: bool = False,
    exe_cmd: str = None,
    ncores: int = 216,
    nnodes: int = 6,
    account: str = 'NRAL0017',
    walltime: str = '02:00:00',
    queue: str = 'regular',
    print_log: bool = False,
    pdb: bool = False,
    pdb_x: bool = False,
    use_existing_test_dir: bool = False,
    xrcmp_n_cores: int = 0
):

    """Function to run wrf_hydro_nwm pytests
        Args:
            config: The config(s) to run, must be listed in hydro_namelist.json keys.
            E.g. nwm_ana gridded
            compiler: The compiler to use, options are 'ifort' or 'gfort'
            domain_dir: The domain directory to use
            candidate_dir: The wrf-hydro code candidate directory to use, e.g. wrf_hydro_nwm_public
            reference_dir: The wrf-hydro code directory to use, e.g. wrf_hydro_nwm_public
            output_dir: The directory to hold test outputs
            scheduler: Use PBSCheyenne scheduler?
            exe_cmd: Optional. The MPI dependent run command which zeroth variable for ncores. 
            ncores: Optional. The number of cores to use if running on cheyenne
            nnodes: Optional. The number of nodes to use if running on cheyenne
            account: Options. The account number to use if running on cheyenne
            walltime: Optional. Walltime for scheduler
            queue: Optional, queue to use for scheduler
            print_log: Optional, print text logs instead of HTML logs
            pdb: Drop down to python debugger in pytest?
            pdb_x: Exit the debugger on success?
            use_existing_test_dir: Run just output comparisions on existing test dir?
    """

    # Pytest wants the actual source code directory, not the top level repo directory
    candidate_source_dir = candidate_dir + '/trunk/NDHMS'
    reference_source_dir = reference_dir + '/trunk/NDHMS'

    # Load modules and override nnodes/ncores if running on cheyenne
    hostname = socket.gethostname()
    module_cmd = ''
    if 'cheyenne' in hostname:
        module_cmd = 'echo; echo "Using the following modules for testing:" ; module list; echo;'

    # HTML report
    html_report = 'wrfhydro_testing' + '-' + compiler + '-' + config + '.html'
    html_report = str(pathlib.Path(output_dir).joinpath(html_report))

    pytest_cmd = "pytest -vv --tb=no --ignore=local -p no:cacheprovider "

    if pdb:
        if pdb_x:
            pytest_cmd += " -x --pdb"
        else:
            pytest_cmd += " --pdb"

    if print_log:
        pytest_cmd += " -s"

    # Ignore section: for cleaner tests with less skipps!

    # NWM Supplementals.
    # If it is not NWM, ignore channel-only and nwm_output tests.
    # (This is likely not the right way to do this.)
    if config != 'nwm_ana':
        pytest_cmd += " --ignore=tests/test_supp_1_channel_only.py "

    if config.lower().find('nwm') < 0:
        pytest_cmd += " --ignore=tests/test_supp_2_nwm_output.py "

    pytest_cmd += " --html=" + str(html_report) + " --self-contained-html"
    pytest_cmd += " --config=" + config.lower()
    pytest_cmd += " --compiler=" + compiler.lower()
    pytest_cmd += " --domain_dir=" + domain_dir
    pytest_cmd += " --candidate_dir=" + candidate_source_dir
    pytest_cmd += " --reference_dir=" + reference_source_dir
    pytest_cmd += " --output_dir=" + output_dir
    pytest_cmd += " --exe_cmd=" + exe_cmd
    pytest_cmd += " --ncores=" + str(ncores)
    pytest_cmd += " --xrcmp_n_cores=" + str(xrcmp_n_cores)

    if scheduler:
        pytest_cmd += " --scheduler "
        pytest_cmd += " --nnodes=" + str(nnodes)
        pytest_cmd += " --account=" + account
        pytest_cmd += " --walltime=" + walltime
        pytest_cmd += " --queue=" + queue

    if use_existing_test_dir:
        pytest_cmd += " --use_existing_test_dir"

    subprocess_cmd = module_cmd + pytest_cmd
    print(subprocess_cmd)
    tests = subprocess.run(subprocess_cmd, shell=True, cwd=candidate_dir)

    return tests


def main():
    parser = ArgumentParser(
        description='Run WRF-Hydro test suite locally',
        epilog='Example usage: python -B run_tests.py '
        '--config nwm_ana nwm_long_range '
        '--compiler gfort '
        '--candidate_dir '
        '/glade/scratch/jmills/wrf_hydro_nwm_public_origin '
        '--reference_dir '
        '/glade/scratch/jmills/wrf_hydro_nwm_public_upstream '
        '--domain_dir /glade/scratch/jmills/CONUS_V2 '
        '--scheduler '
        '--output_dir /glade/scratch/jmills/conus_v2_testing_gfort'
    )

    parser.add_argument(
        "--config",
        required=True,
        nargs='+',
        help="<Required> The configuration(s) to test, "
        "must be one listed in trunk/NDHMS/hydro_namelist.json keys."
    )

    parser.add_argument(
        '--compiler',
        required=True,
        help='<Required> compiler, options are intel or gfort'
    )

    parser.add_argument(
        '--output_dir',
        required=True,
        help='<Required> test output directory'
    )
    
    parser.add_argument(
        '--candidate_dir',
        required=True,
        help='<Required> candidate model directory'
    )

    parser.add_argument(
        '--reference_dir',
        required=True,
        help='<Required> reference model directory'
    )

    parser.add_argument(
        '--domain_dir',
        required=False,
        help='optional domain directory'
    )

    parser.add_argument(
        "--domain_tag",
        required=False,
        help="The release tag of the domain to retrieve, e.g. v5.0.1. or dev. If "
             "specified, a small test domain will be retrieved and placed in the "
             "specified output_dir and used for the testing domain"
    )

    parser.add_argument(
        '--exe_cmd',
        default="'mpirun -np {0} ./wrf_hydro.exe'",
        required=False,
        help='The MPI-dependent model execution command. Default is best guess. '
        'The first/zeroth variable is set to the total number of cores (ncores). The '
        'wrf_hydro_py convention is that the exe is always named wrf_hydro.exe.'
    )

    parser.add_argument(
        '--ncores',
        default='2',
        required=False,
        help='Number of cores to use for testing'
    )

    parser.add_argument(
        '--scheduler',
        required=False,
        action='store_true',
        help='Scheduler to use for testing, options are PBSCheyenne or do not '
        'specify for no scheduler'
    )

    parser.add_argument(
        '--nnodes',
        default='6',
        required=False,
        help='Number of nodes to use for testing if running on scheduler'
    )

    parser.add_argument(
        '--account',
        default='NRAL0017',
        required=False,
        action='store',
        help='Account number to use if using a scheduler.'
    )

    parser.add_argument(
        '--walltime',
        default='02:00:00',
        required=False,
        action='store',
        help='Account number to use if using a scheduler.'
    )

    parser.add_argument(
        '--queue',
        default='regular',
        required=False,
        action='store',
        help='Queue to use if running on NCAR Cheyenne, options are regular, '
        'premium, or shared'
    )

    parser.add_argument(
        '--print',
        required=False,
        action='store_true',
        help='Print log to stdout instead of html'
    )

    parser.add_argument(
        '--pdb',
        required=False,
        action='store_true',
        help='pdb (debug) in pytest')

    parser.add_argument(
        '-x',
        required=False,
        action='store_true',
        help='Exit pdb on first failure.'
    )

    parser.add_argument(
        '--use_existing_test_dir',
        default=False,
        required=False,
        action='store_true',
        help='Use existing compiles and runs, only perform output comparisons.'
    )

    parser.add_argument(
        '--xrcmp_n_cores',
        default=0,
        required=False,
        help='Use xrcmp if > 0, and how many cores if so?'
    )

    args = parser.parse_args()

    # Make all directories pathlib objects
    output_dir = pathlib.Path(args.output_dir)
    candidate_dir = pathlib.Path(args.candidate_dir)
    reference_dir = pathlib.Path(args.reference_dir)
    domain_dir = args.domain_dir

    if domain_dir is not None:
        domain_dir = pathlib.Path(domain_dir)

    # Get other args
    config_list = args.config

    compiler = args.compiler
    domain_tag = args.domain_tag
    exe_cmd = args.exe_cmd
    ncores = int(args.ncores)
    nnodes = int(args.nnodes)
    scheduler = args.scheduler
    account = args.account
    walltime = args.walltime
    queue = args.queue
    print_log = args.print
    pdb = args.pdb
    pdb_x = args.x
    use_existing_test_dir = args.use_existing_test_dir
    xrcmp_n_cores = args.xrcmp_n_cores

    # Make output dir if does not exist
    if not use_existing_test_dir:
        if output_dir.is_dir():
            raise(IsADirectoryError('Output directory ' + str(output_dir) + ' already exists'))
        else:
            output_dir.mkdir(parents=True)

    # Get the domain if asked for
    if domain_tag is not None:
        # Reset domain dir to be the downlaoded domain in the output dir
        domain_dir = output_dir.joinpath('example_case')

        if domain_tag == 'dev':
            file_id = '1xFYB--zm9f8bFHESzgP5X5i7sZryQzJe'
            download_file_from_google_drive(file_id, str(output_dir.joinpath(
                'gdrive_testcase.tar.gz')))

            # untar the test case
            untar_cmd = 'tar -xf *testcase*.tar.gz'
            subprocess.run(untar_cmd,
                           shell=True,
                           cwd=str(output_dir))
        else:
            get_release_asset(download_dir=str(output_dir),
                              repo_name='NCAR/wrf_hydro_nwm_public',
                              tag=domain_tag,
                              asset_name='testcase')
            # untar the test case
            untar_cmd = 'tar -xf *testcase*.tar.gz'
            subprocess.run(untar_cmd,
                           shell=True,
                           cwd=str(output_dir))

    # Make copy paths
    candidate_copy = output_dir.joinpath(candidate_dir.name + '_can_pytest')
    reference_copy = output_dir.joinpath(reference_dir.name + '_ref_pytest')
    # copy directories to avoid polluting user source code directories
    if not candidate_copy.exists() or not use_existing_test_dir:
        shutil.copytree(str(candidate_dir), str(candidate_copy), symlinks=True)
    if not reference_copy.exists() or not use_existing_test_dir:
        shutil.copytree(str(reference_dir), str(reference_copy), symlinks=True)

    # run pytest for each supplied config
    has_failure = False
    print("\n\n---------------- Starting WRF-Hydro Testing ----------------")
    print("Testing the configs: " + ', '.join(config_list), flush=True)
    for config in config_list:
        extra_spaces = 29
        total_len = len(config) + extra_spaces
        print('\n\n' + ('#' * total_len))
        print('### TESTING:  ---  ' + config + '  ---  ###')
        print(('#' * total_len) + '\n', flush=True)

        test_result = run_tests(
            config=config,
            compiler=compiler,
            domain_dir=str(domain_dir),
            candidate_dir=str(candidate_copy),
            reference_dir=str(reference_copy),
            output_dir=str(output_dir),
            scheduler=scheduler,
            exe_cmd=exe_cmd,
            ncores=ncores,
            nnodes=nnodes,
            account=account,
            walltime=walltime,
            queue=queue,
            print_log=print_log,
            pdb=pdb,
            pdb_x=pdb_x,
            use_existing_test_dir=use_existing_test_dir,
            xrcmp_n_cores=xrcmp_n_cores
        )

        if test_result.returncode != 0:
            has_failure = True

    # Exit with 1 if failure
    if has_failure:
        print('\n\n'
              '##################################')
        print('###  ---  TESTING FAILED  ---  ###')
        print('##################################\n\n', flush=True)
        exit(1)
    else:
        print('\n\n'
              '##################################')
        print('###  ---  TESTING PASSED  ---  ###')
        print('##################################\n\n', flush=True)
        exit(0)


if __name__ == '__main__':
    main()
