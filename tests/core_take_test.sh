#!/bin/bash
# Purpose:
#   This script is the general pupose launching script for wrf_hydro_nwm_public
#   and wrf_hydro_nwm testing.
#   This script takes care of logging the tests.
#   This script handles launchin in docker and on known machines. Other
#   machines will cause an error.
#           
# Arguments are passed to take_test.py, please run
#   ./take_test.sh --help
# For complete information on the following arguments:
#   -- domain
#   -- config
#   -- candidate_spec_file
#   -- test_spec

if [[ ${@} == *"--help"* ]]; then
    echo
    echo "Retrieving the help from take_test.py..."
    echo
    python take_test.py --help
    echo
    echo "take_test.sh notes: "
    echo "  When using docker, the domain argument becomes the key which is the basename of the path."
    echo
    exit 0
fi

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
#echo "Testing: $this_dir"


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

# Eliminate '=' and condense spaces
args_to_pass="${@}"
args_to_parse=$(echo "${args_to_pass}" | tr -s ' ' | tr -d '=' | tr -d '-')
#echo "args_to_pass: $args_to_pass"

# The domain arg and the candidate spec file args are needed
# by this script. Eschew getopt and do it manually for portability.

origIFS=${IFS}
IFS=' ' read -r -a array <<< "$args_to_parse"

wh_domain=-1
wh_candidate_spec=-1
for index in "${!array[@]}"
do
    if [[ "${array[index]}" == domain ]]; then
        wh_domain=$((index+1))
    fi
    if [[ "${array[index]}" == candidate_spec_file ]]; then
        wh_candidate_spec=$((index+1))
    fi
done

if [ $wh_domain != -1 ]; then
    domain=${array[$wh_domain]}
    #echo "domain: $domain"
fi
if [ -z $domain ]; then
    domain=croton_NY
    #echo "domain: $domain"
fi

if [ $wh_candidate_spec != -1 ]; then
    candidate_spec_file=${array[$wh_candidate_spec]}
    #echo "candidate_spec_file: $candidate_spec_file"
fi

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

        echo 
        echo "Using Docker."
        echo
        
        echo "Refresh the wrfhydro/dev:conda container"
        docker pull wrfhydro/dev:conda
        echo
        echo "Refresh the wrfhydro/domains:${domain} container"
        docker pull wrfhydro/domains:${domain}
        echo
        
        # Dummy, hopefully untaken name...
        domain_tmp_vol=${domain}_tmp_vol    
        docker create --name ${domain_tmp_vol} wrfhydro/domains:${domain} || exit 1
 
        # Need the user and candidate specs in a mountable place.
        host_spec_dir=/tmp/user_spec_dir/
        docker_spec_dir=/home/docker/.test_spec_dir
        rm -rf ${host_spec_dir}
        mkdir ${host_spec_dir}

        # User spec 
        if [ ! -z $WRF_HYDRO_TESTS_USER_SPEC ]; then
            cp $WRF_HYDRO_TESTS_USER_SPEC ${host_spec_dir}/.
            docker_user_spec=${docker_spec_dir}/$(basename $WRF_HYDRO_TESTS_USER_SPEC)
        else
            # TODO(JLM): is this handled by take_test.py? may not be necessary.
            echo "Using default user specification file: ${this_dir}/template_user_spec.yaml"
            echo
            cp ${this_dir}/template_user_spec.yaml ${host_spec_dir}/.
            docker_user_spec=${docker_spec_dir}/template_user_spec.yaml
        fi

        # Candidate spec 
        if [ $wh_candidate_spec != -1 ]; then
            cp candidate_spec_file ${host_spec_dir}/.
            
        fi
        
        # Use mount this repo to /home/docker
        this_repo_name=$(basename $this_repo)

        docker_cmd=""
        #docker_cmd=${docker_cmd}"cd /home/docker/wrf_hydro_py; pip uninstall -y wrfhydropy; "
        #docker_cmd=${docker_cmd}" python setup.py install; pip install termcolor; "
        docker_cmd=${docker_cmd}" cd /home/docker/${this_repo_name}/tests/; ./take_test.sh ${args_to_pass};"
        docker_cmd=${docker_cmd}" exit $?"
        # TODO(JLM): I have not verified passing of the return value...
        
        echo "Starting the docker image."
        echo
        # -e establishes env variables in docker.
        # -v mounts directories host:docker
        # The GITHUB variables are used only for the private, wrf_hydro_nwm repo.
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
