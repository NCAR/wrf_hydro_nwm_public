# It's annoyingly hard to run the CI locally and to have to run/test
# code which is not actually part of the .travi.yml file.
# So just parse the .travis.yml as much as possible and run it on docker locally!

import os
this_script = __file__
this_script_path = os.path.dirname(os.path.realpath(this_script))
this_repo_path = os.path.dirname(os.path.dirname(os.path.realpath(this_script_path)))

docker_repo_path = '/home/docker/the_repo_to_test'

import yaml
travis_yaml_file = this_repo_path + '/.travis.yml'
with open(travis_yaml_file) as ff:
    travis_cmds = yaml.safe_load(ff)

import re
before_cmds = [bb.replace('sudo ', '')
               for bb in travis_cmds['before_install'] if not re.match('docker', bb)]
before_cmds='; '.join(before_cmds)
script_cmds = travis_cmds['script'][0].split('"')[1]
the_cmd = before_cmds + '; ' + script_cmds

data_container_name = "croton_NY_for_CI_local"
mk_data_container_cmd = "docker create --name " + data_container_name + " wrfhydro/domains:croton_NY"
rm_data_container_cmd = "docker rm -v " + data_container_name

invoke_docker_cmd = "docker run -it "
invoke_docker_cmd += " -e TRAVIS_BUILD_DIR=" + docker_repo_path
invoke_docker_cmd += " --volumes-from  " + data_container_name
invoke_docker_cmd += " -v " + this_repo_path + ":" + docker_repo_path 
invoke_docker_cmd += " wrfhydro/dev:conda "
invoke_docker_cmd += " /bin/bash -c \"" + the_cmd + "\""

import subprocess
import shlex
from pprint import pprint
subprocess.run(shlex.split(mk_data_container_cmd))
subprocess.run(shlex.split(invoke_docker_cmd))
subprocess.run(shlex.split(rm_data_container_cmd))
