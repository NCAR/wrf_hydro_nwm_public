import subprocess
import os
from datetime import datetime
from color_logs import log

def log_boilerplate(candidate_spec, user_spec, env_vars, horiz_bar, script_path):

    log.debug( "Date                  : " + datetime.now().strftime('%Y %h %d %H:%M:%S %Z') )

    if not 'USER' in env_vars:
        user = subprocess.Popen(["whoami"], stdout=subprocess.PIPE).communicate()[0]
        env_vars['USER'] = user.decode('utf-8').replace("\n",'')
    log.debug( "User                  : " + env_vars['USER'] )

    if not 'HOSTNAME' in env_vars:
        hostname = subprocess.Popen(["hostname"], stdout=subprocess.PIPE).communicate()[0]
        env_vars['HOSTNAME'] = hostname.decode('utf-8').replace("\n",'')
    log.debug( "Machine               : " + env_vars['HOSTNAME'] )

    proc = subprocess.run(
        ['git', 'rev-parse', 'HEAD'],
        stdout=subprocess.PIPE,
        cwd=script_path
    )
    the_commit = proc.stdout.decode('utf-8').split()[0]
    log.debug( "take_test.py location : " + script_path )
    # TODO(JLM): should we check for uncommiteed changes in the script_path? Just
    # ones in the test dir?

    #is_uncommitted = \
    #    subprocess.run(['git', 'diff-index', '--quiet', 'HEAD', '--']).returncode
    #if is_uncommitted != 0:
    #    log.warning( "There are uncommitted changes to the testing repo (" + script_path + ")")

    log.debug("Domain argument       : " + str(candidate_spec['domain']))
    log.debug("Config argument       : " + str(candidate_spec['config']))

    log.debug("Tests run in          : " + candidate_spec['test_dir'] )
    log.debug("Cloned repos in       : " + candidate_spec['repos_dir'] )
    
    log.debug("Candidate spec file   : " + candidate_spec['candidate_spec_file'] )

    log.debug("Machine spec file     : " + candidate_spec['machine_spec_file'] )
    log.debug("Machine spec set by   : " +
              candidate_spec['machine_spec_setby'] )

    log.debug("User spec file        : " + candidate_spec['user_spec_file'] )
    log.debug("User spec set by      : " +
              candidate_spec['user_spec_setby'] )

    #log.debug( "Test spec file        : " + candidate_spec['test_spec_file'] )
    #log.debug( "Test spec set by      : " +
    #           candidate_spec['test_spec_setby'] )

    log.debug("Log file              : " + script_path + '/take_test.log')
    log.debug("Will echo specs to log at end.")
    return(True)
