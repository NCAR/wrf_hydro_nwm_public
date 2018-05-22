import os
import yaml
from pathlib import Path
from boltons.iterutils import remap
from color_logs import log

# A few generic utils followed by functions for individual spec files which
# can acommodate customization.
# The user, machine, and candidate specs are very similar. The test spec is it's own thing.

# ######################################################
# Remapping nested values
# http://sedimental.org/remap.html

# These visit all levels of a nested dictionary and expand ${} and ~.


def visit_expand(path, key, value):
    if isinstance(value, str):
        return key, os.path.expanduser(os.path.expandvars(value))
    return key, value


def remap_vars(spec_file):
    return(remap(spec_file, visit_expand))


# These visit all levels of a nested dictionary and transform '' to None.


def visit_blanks(path, key, value):
    if isinstance(value, str):
        if value == '':
            return key, None
    return key, value


def remap_blanks(spec_file):
    return(remap(spec_file, visit_blanks))


# ######################################################
# Generic spec establishment = YAML + remap_spec

def establish_spec(spec_file):
    """Parse YAML and expand  ~ and $
    """
    with open(spec_file) as ff:
        spec_dict = yaml.safe_load(ff)

    spec = remap_vars(spec_dict)
    spec = remap_vars(spec)

    return(spec)


# ######################################################
# User spec

def establish_user_spec(candidate_spec, env_vars):
    log.debug('Establish user spec.')

    user_spec_file = None
    if ('wrf_hydro_tests' in candidate_spec) and \
       ('user_spec_file'  in candidate_spec):
        user_spec_file = candidate_spec['user_spec_file']
        candidate_spec['user_spec_setby'] = 'candidate spec'

    if user_spec_file == '' or user_spec_file is None:
        default_files = establish_default_files()
        user_spec_file = default_files[0]
        candidate_spec['user_spec_setby'] = 'env var'

    candidate_spec['user_spec_file'] = user_spec_file
    # TODO JLM: indicate in the candidate_spec how the user_spec_file was set.
    # TODO JLM: WARN if DNE

    user_spec = establish_spec(user_spec_file)

    return(user_spec)


# ######################################################
# Machine spec

def establish_machine_spec(candidate_spec, user_spec, env_vars):
    log.debug('Establish machine spec.')

    # TODO JLM: indicate in the candidate_spec how the machine_spec_file was set.
    machine_spec = establish_spec(candidate_spec['machine_spec_file'])
    candidate_spec['machine_spec_setby'] = '__file__'
    
    # TODO JLM: User spec is supposed to allow overrides to machine spec.
    # Apply overrides from user_spec
    return(machine_spec)


# ######################################################
# Candidate spec

def establish_candidate(candidate_spec_file):
    log.debug('Establish candidate spec.')
    candidate_spec = establish_spec(candidate_spec_file)
    candidate_spec['candidate_spec_file'] = candidate_spec_file
    return(candidate_spec)


# ######################################################
# Test spec
# This one is very different as there's no yaml file to parse, just
# a command line arg.

# def establish_test(test_spec, candidate_spec, user_spec):
#     log.debug("Establish test spec.")
#     test_spec_file = Path(test_spec)
#     candidate_spec['test_spec_setby'] = 'command line path/file'
#     if not test_spec_file.exists():
#         candidate_spec['test_spec_setby'] = 'command line key'
#         test_spec_list = \
#             list(Path(user_spec['wrf_hydro_tests_dir']+'/tests').glob(test_spec+'.py'))
#         if len(test_spec_list) != 1:
#             log.error('The test specification argument does not identify a unique test.')
#         else:
#             test_spec_file = str(test_spec_list[0])

#     candidate_spec['test_spec_file'] = test_spec_file
#     return(True)


# This is to support external calls... should probably enforce this above... 
def establish_default_files():
    default_user_spec_file = os.environ['WRF_HYDRO_TESTS_USER_SPEC']
    default_user_spec = establish_spec(default_user_spec_file)
    default_machine_spec_file = default_user_spec['wrf_hydro_tests_dir'] + '/machine_spec.yaml'
    return default_user_spec_file, default_machine_spec_file
