import argparse
import code
import copy
import json
import logging
import os
import pathlib
from pprint import pprint, pformat
import pytest
import shutil
import sys
import wrfhydropy

this_script = __file__
this_script_path = os.path.dirname(os.path.realpath(this_script))
this_repo_path = os.path.dirname(os.path.realpath(this_script_path))

sys.path.insert(0, this_script_path+'/toolbox/')
from color_logs import log
from establish_repo import *
from establish_specs import *
from log_boilerplate import log_boilerplate

machine_name = wrfhydropy.core.job_tools.get_machine()

# ######################################################
# Preamble/overview/Help/docstring.

# ######################################################
# Agruments
parser = argparse.ArgumentParser(
    description='A WRF-Hydro candidate takes a test.'
)

parser.add_argument(
    '--domain',
    metavar='path to domain directory',
    help='Path to the domain directory.',
    default=None
)

parser.add_argument(
    '--candidate_spec_file',
    metavar='candidate spec file',
    type=str, 
    help='The candidate specification file.',
    default=this_script_path + '/default_candidate_spec_' + machine_name + '.yaml'
)

# For the following, the defaults of None trigger the defaults set in conftest.

parser.add_argument(
    '--config',
    metavar='model configuration key',   
    help='Key for model configuration (default is all)',
    default = None # None is handled as all by pytest currently.
)

parser.add_argument(
   '--test_spec',
   metavar='test specification key',
   help='Key for specifying the desired tests',
   default=None
)

args = parser.parse_args()
candidate_spec_file = args.candidate_spec_file
domain = args.domain
config= args.config
test_spec = args.test_spec
# TODO(JLM): these items need to go into the boilerplate.

if domain is None:
    if machine_name is 'cheyenne':
        domain = '/glade/p/work/jamesmcc/domains/public/croton_NY'
    else :
        domain = '/home/docker/domain/croton_NY'

if test_spec is None:
    test_spec = 'fundamental'

# TODO JLM: want to get the log file name from the candidate_spec_file but want to be
#           logging to file before that. COPY the log file at the end.

# TODO JLM: generate default log file name. Hide it in a dot file and copy at end?
# TODO JLM: deal with relative and absolute paths, eg log file... 

# ######################################################
# Logging setup.
# This coloring approach may only allow one log.
log.setLevel(logging.DEBUG)

stdout = logging.StreamHandler()
stdout.setLevel(logging.DEBUG)
log.addHandler(stdout)

log_file = "take_test.log"
log_file_handler = logging.FileHandler(log_file, mode='w')
log_file_handler.setLevel(logging.DEBUG)
log.addHandler(log_file_handler)

horiz_bar = '================================================================='
log.info(horiz_bar)
log.info("*** take_test.py: A wrf_hydro candidate takes a test. ***")
log.debug('')


# ######################################################
# Specification files to dictionaries.
log.info(horiz_bar )
log.info( "Setup the specifictions (specs):")

env_vars       = os.environ.copy()

candidate_spec = establish_candidate(candidate_spec_file)
# The default candidate path is solved here based on the this_script_path.
if not pathlib.PosixPath(candidate_spec['candidate_repo']['local_path']).exists():
    candidate_spec['candidate_repo']['local_path'] = \
        candidate_spec['candidate_repo']['local_path'].format(
            **{'this_repo_path': this_repo_path}
        )

user_spec      = establish_user_spec(candidate_spec, env_vars)
candidate_spec['machine_spec_file'] = this_script_path + '/machine_spec.yaml'
machine_spec   = establish_machine_spec(candidate_spec, user_spec, env_vars)
log.debug('')

# #################################
if pathlib.PosixPath(candidate_spec['test_dir']).exists():
    raise FileExistsError("Exiting: the testing run directory already exists " +
                          candidate_spec['test_dir'])

# ######################################################
# Log boilerplate info

candidate_spec['config'] = config
candidate_spec['domain'] = domain

log.info(horiz_bar )
log.info("Boilerplate:")
log_boilerplate(candidate_spec, user_spec, env_vars, horiz_bar, this_script_path)
log.debug('')


# ######################################################
# Repos setup
log.info(horiz_bar )
log.info("Establish repositories:")
establish_repo('candidate_repo', candidate_spec, user_spec)
establish_repo('reference_repo', candidate_spec, user_spec)
log.debug('')

# ###################################
log.info(horiz_bar )
log.info("Establish jobs and scheduler:")

if machine_name == 'docker':
    default_scheduler = None
else:
    if candidate_spec['queue'] is None or candidate_spec['queue'] == 'None':
        default_scheduler = None
    else:
        default_scheduler = wrfhydropy.Scheduler(
            job_name='default',
            account=user_spec['PBS']['account'],
            walltime=candidate_spec['wall_time'],
            queue=candidate_spec['queue'],
            nproc=candidate_spec['n_cores']['default'],
            ppn=machine_spec[machine_name]['cores_per_node']
        ).__dict__

default_scheduler = json.dumps(default_scheduler)
job_default = wrfhydropy.Job(nproc=candidate_spec['n_cores']['default'])
job_ncores=copy.deepcopy(job_default)
job_ncores.nproc=candidate_spec['n_cores']['test']
job_default = json.dumps(job_default.__dict__)
job_ncores = json.dumps(job_ncores.__dict__)

log.debug('')
# ###################################
log.info(horiz_bar)
log.info("Calling pytest:")

pytest_cmd = [
    '--rootdir', str(candidate_spec['candidate_repo']['local_path']) + '/tests/' ,
    '--ignore', 'take_test.py',
    '--ignore', 'toolbox/',
    '--compiler', candidate_spec['compiler'],
    '--domain_dir', domain,
    '--output_dir',  candidate_spec['test_dir'],
    '--candidate_dir', str(candidate_spec['candidate_repo']['local_path']) + '/trunk/NDHMS',
    '--reference_dir', str(candidate_spec['reference_repo']['local_path']) + '/trunk/NDHMS',
    '--job_default', job_default,
    '--job_ncores', job_ncores,
    '--scheduler', default_scheduler
]

if config is not None:
    pytest_cmd = pytest_cmd + [ '--config', config,]
# TODO JLM: do this for all the arguments.

log.debug('')
log.debug(pytest_cmd)
log.debug('')

pytest_return = pytest.main(pytest_cmd)

log.debug('')

# ######################################################
# Tear down if success
log.info('=================================================================')
if pytest_return == 0:
    log.info('All tests successful: tear down test.')
    log.debug('')
    shutil.rmtree(candidate_spec['repos_dir'])
    shutil.rmtree(candidate_spec['test_dir'])
    log.debug('')
else:
    if pathlib.PosixPath(candidate_spec['repos_dir']).exists():
        log.info('Some tests failed: leaving tests and repos in:')
        log.info('Repos: ' + candidate_spec['repos_dir'])
    else:
        log.info('Some tests failed: leaving tests in:')
        
    log.info('Tests: ' + candidate_spec['test_dir'])
    log.debug('')


# ######################################################
# Echo specs to log files
log.info('=================================================================')
log.info('*** take_test.py: Finished. ***')
log.debug('')
log.debug('Writing working specifications to ' + log_file +'.')
log.debug('')

# Kill the 'stdout' handler.
log.removeHandler(stdout)

log.info('*****************************************************************')
log.debug('')

# Protect the authtoken from printing to log files.
if not user_spec['github']['authtoken'] is None:
    user_spec['github']['authtoken'] = '*************************'

def log_spec(spec, name):
    log.info(horiz_bar)
    log.info(name+' spec: ')
    log.debug(pformat(spec))
    log.debug('')

all_specs = { 'Candidate': candidate_spec,
              'User': user_spec,
              'Machine': machine_spec }

for key, value in all_specs.items():
    log_spec(value, key)
