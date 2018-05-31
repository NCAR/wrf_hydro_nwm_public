import os
#from pprint import pprint
import re
import socket
import sys
import warnings


home = os.path.expanduser("~/")
sys.path.insert(0, home + '/WRF_Hydro/wrf_hydro_tests/toolbox/')
from establish_specs import establish_spec, establish_default_files


def get_job_args_from_specs(
    job_name: str=None,
    mode: str='r',
    nnodes: int=None,
    nproc: int=None,
    scheduler_name: str='',
    machine_spec_file: str=None,
    user_spec_file: str=None,
    candidate_spec_file: str=None
):

    # The candidate comes first and can replace the machine and user specs files.
    # The user spec can overrides parts of the machine spec.
    # So, importance: candidate_spec_file > user_spec_file > machine_spec_file
    
    # Candidate and its overrides    
    if candidate_spec_file:
        candidate_spec = establish_spec(candidate_spec_file)
    #pprint(candidate_spec)
    
    machine_spec_file_from_candiate = candidate_spec['wrf_hydro_tests']['machine_spec']
    if machine_spec_file_from_candiate:
        machine_spec_file = machine_spec_file_from_candiate
        warnings.warn("WARNING: candidate spec_file is overriding machine_spec_file with file " + 
              machine_spec_file)
            
    user_spec_file_from_candiate = candidate_spec['wrf_hydro_tests']['user_spec']
    if user_spec_file_from_candiate:
        user_spec_file = user_spec_file_from_candiate
        warnings.warn("WARNING: candidate spec_file is overriding user_spec_file with file " + 
              user_spec_file)
    
    default_user_file, default_machine_file = establish_default_files()
    
    if not user_spec_file:
        user_spec_file = default_user_file

    # TODO JLM: should probably be in a try catch.    
    user_spec = establish_spec(user_spec_file)

    if not machine_spec_file:
        machine_spec_file = default_machine_file

    # TODO JLM: should probably be in a try catch.    
    machine_spec = establish_spec(machine_spec_file)

    # From least to most important
    spec=machine_spec
    spec.update(user_spec)
    spec.update(candidate_spec)

    # Extract relevant information for the scheduler
    machine = socket.gethostname()
    if re.search('cheyenne',machine):
        machine = 'cheyenne'

    if machine not in spec.keys():
        machine = 'docker'
        warnings.warn("Machine not found in the machine_spec_file.yaml, using docker.")

    if scheduler_name == '':
        if spec[machine]['scheduler'] is not None:
            scheduler_name = spec[machine]['scheduler']['name']        
        else:
            scheduler_name = None
            
    compiler_name = spec['compiler']

    sched_args_dict = {}
    job_args_dict = {}
    # sad alias for mutable container
    sad = sched_args_dict
    jad = job_args_dict
    
    # From optional arguments
    if job_name:
        sad['job_name'] = job_name
    if nnodes:
        sad['nnodes'] = nnodes
    if nproc:
        sad['nproc'] = nproc
        jad['nproc'] = nproc

    #jad['machine'] = machine

    # From spec files.
    if scheduler_name is not None: 
        sad['account']    = spec[scheduler_name]['account']
        sad['email_when'] = spec[scheduler_name]['email']['when']
        sad['email_who']  = spec[scheduler_name]['email']['who']
        sad['queue']      = spec['queue']
        sad['walltime']   = spec['wall_time']
        sad['ppn']        = spec[machine]['cores_per_node']
        jad['scheduler']  = sad
        jad['exe_cmd']    = spec[machine]['exe_cmd'][scheduler_name]
    else:
        jad['exe_cmd']    = spec[machine]['exe_cmd']['default']

    if spec[machine]['modules'] is not None: 
        jad['modules']    = spec[machine]['modules'][compiler_name]
        if 'base' in spec[machine]['modules'].keys():
            jad['modules'] += ' ' + spec[machine]['modules']['base']


    return(job_args_dict)
