#!/bin/bash
# Purpose:
#   This script is the general pupose launching script for wrf_hydro_nwm_public
#   and wrf_hydro_nwm testing.
#   This script takes care of logging the tests.
#   This script handles launchin in docker and on known machines. Other
#   machines will cause an error.
#           
# These args are passed to take_test.py:
#   domain
#   config
#   candidate_spec_file
#   test_spec


# #################################
# Determine the path to this file, allowing for a symlink.
#https://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
this_dir="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
this_repo=$(dirname $this_dir)
echo "Testing: $this_dir"


# #################################
# Collect some parameters... 
if [ $(which docker 2> /dev/null | wc -l) != 0 ]; then
    docker_avail=0
else
    docker_avail=1
fi
#echo "docker_avail: $docker_avail"


grep docker /proc/1/cgroup -qa 2> /dev/null
in_docker=$?
#echo "in docker: $in_docker"

machine_spec_file=$this_dir/machine_spec.yaml
machines_in_spec=`cat $machine_spec_file | egrep -e $'^[a-z]' | cut -d':' -f1`
#echo "machines_in_spec:" $machines_in_spec


known_machine=1
for mm in ${machines_in_spec}; do
    if [ $(echo ${HOSTNAME} | grep ${mm} | wc -l 2> /dev/null) -gt 0 ]; then
        known_machine=0
        #echo $mm
    fi
done
#echo "known_machine: $known_machine"


# #################################
# TODO(JLM): Construct the passed options
# domain
# config
# candidate_spec_file
# test_spec

if [ -z $domain ]; then
    domain=croton_NY
fi

args_to_pass=''


# #################################
# Known Machine (this includes docker)

if [[ $known_machine == 0 ]] || [[ $in_docker == 0 ]]; then

    python ${this_dir}/take_test.py ${args_to_pass}

else

# #################################
# UnKnown Machine

    # #################################
    # Docker
    if [[ $docker_avail != 0 ]]; then

        echo "This machine is not known to $machine_spec_file and "
        echo "docker does not seem to be available. Exiting."
        exit 1
        
    else

        echo "Using Docker."
        
        echo "Refresh the primay container and the domain container."
        docker pull wrfhydro/dev:conda
        docker pull wrfhydro/domains:${domain}

        # Dummy, hopefully untaken name...
        domain_tmp_vol=${domain}_tmp_vol    
        docker create --name ${domain_tmp_vol} wrfhydro/domains:${domain} || exit 1

        # Need the user spec in a mountable place.
        host_spec_dir=/tmp/user_spec_dir/
        docker_spec_dir=/home/docker/.user_spec_dir
        rm -rf ${host_spec_dir}
        mkdir ${host_spec_dir}
        if [ ! -z $WRF_HYDRO_TESTS_USER_SPEC ]; then
            echo cp $WRF_HYDRO_TESTS_USER_SPEC ${host_spec_dir}/.
            echo docker_user_spec=${docker_spec_dir}/$(basename $WRF_HYDRO_TESTS_USER_SPEC)
        else
            cp ${this_dir}/template_user_spec.yaml ${host_spec_dir}/.
            docker_user_spec=${docker_spec_dir}/template_user_spec.yaml
        fi
        
        # Use mount this repo to /home/docker
        this_repo_name=$(basename $this_repo)

        docker_cmd="cd /home/docker/wrf_hydro_py; pip uninstall -y wrfhydropy; "
        docker_cmd=${docker_cmd}" python setup.py install; pip install termcolor; "
        docker_cmd=${docker_cmd}" cd /home/docker/${this_repo_name}/tests/; ./take_test.sh"
        docker_cmd=${docker_cmd}" exit $?"
        # TODO(JLM): I have not verified passing of the return value...
        
        echo "Starting the docker image."
        # -e establishes env variables in docker.
        # -v mounts directories host:docker 
        # TODO (JLM): Remove the -i? Or do we want it to be interactive if it fails?
        docker run -it \
               -e USER=docker \
               -e GITHUB_AUTHTOKEN=$GITHUB_AUTHTOKEN \
               -e GITHUB_USERNAME=$GITHUB_USERNAME \
               -e WRF_HYDRO_TESTS_USER_SPEC=${docker_user_spec} \
               -v ${host_spec_dir}:${docker_spec_dir} \
               -v ${this_repo}:/home/docker/${this_repo_name} \
               -v /Users/jamesmcc/WRF_Hydro/wrf_hydro_py:/home/docker/wrf_hydro_py \
               --volumes-from ${domain_tmp_vol} \
               wrfhydro/dev:conda /bin/bash -c "${docker_cmd}"

        # TODO (JLM): Dont remove this if the test failed? May want different name.
        echo "Tearing down the data container: " $(docker rm -v ${domain_tmp_vol})
        
    fi # Trying docker
    
fi # Known machine else unknown machine
    
exit 0
