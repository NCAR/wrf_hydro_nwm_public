#!/bin/bash
# TODO(JLM): I should have written this in python... or not, would require python whereas
#            docker has python. This only require bash on the host machine.
# Purpose:
#   This script is the general pupose launching script for wrf_hydro_nwm_public
#   and wrf_hydro_nwm testing.
#   This script takes care of logging the tests.
#   This script handles launchin in docker and on known machines. Other
#   machines will cause an error.
#
# Only argument take for docker usage:
#   -i : enters docker interactively.
#
# All other arguments are passed to take_test.py, please run
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
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
this_dir="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
this_repo=$(dirname $this_dir)
this_repo_name=$(basename $this_repo)
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
    domain=wrfhydro/domains:croton_NY
    args_to_pass="${args_to_pass} --domain $domain"
    #echo "domain: $domain"
fi

if [ $wh_candidate_spec != -1 ]; then
    candidate_spec_file=${array[$wh_candidate_spec]}
    #echo "candidate_spec_file: $candidate_spec_file"
fi

# #################################
# Known Machine (this includes docker)

if [[ $known_machine == 0 ]] || [[ $in_docker == 0 ]]; then

    if [[ $in_docker == 0 ]]; then

	# Deal with possibility of case-insensitive mounted file system
	rm -f abcdefghijklmnop ABCDEFGHIJKLMNOP
	touch abcdefghijklmnop ABCDEFGHIJKLMNOP &> /dev/null
	rm -f ABCDEFGHIJKLMNOP
	case_insensitive=$(ls abcdefghijklmnop &> /dev/null | wc -l)
	#echo "case_insensitive: $case_insensitive"
	rm -f abcdefghijklmnop ABCDEFGHIJKLMNOP
	
	if [[ $case_insensitive -eq 0 ]]; then
	    cp -r ${this_repo} ${this_repo}_case_sensitive
	    cd ${this_repo}_case_sensitive/tests
	    # Edit the candidate spec file for the above. 
	    for ff in ~/.test_spec_dir/*yaml; do
		sed -i "s|${this_repo}\$|${this_repo}_case_sensitive|g" $ff
		#echo $this_repo
		#echo $ff
	    done
	    # Also deal with local_paths in the candidate spec...
	    for ii in `egrep 'local_path.*:' ~/.test_spec_dir/*.yaml | tr -d ' ' | cut -d':' -f3`; do
		#echo $ii
		if [[ "$ii" == *"_case_sensitive" ]]; then
		    continue
		fi
		cp -r $ii ${ii}_case_sensitive
		sed -i "s|$ii\$|${ii}_case_sensitive|g"  ~/.test_spec_dir/*.yaml
	    done
		
	fi

    fi

    python take_test.py ${args_to_pass}
    return_code=$?
    
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
        
        # Is the domain a container or a path?
        if [[ ${domain} == "wrfhydro/domains:"* ]]; then

            echo "Refresh the ${domain} container"
            docker pull ${domain}

            # Have to edit args_to_pass to the correct location.
            domain_tag=$(echo $domain | cut -d':' -f2)
            args_to_pass=$(echo "$args_to_pass" | sed "s|${domain}|/home/docker/domain/${domain_tag}|")
            # Dummy, hopefully untaken name...
            domain_tmp_vol="${domain_tag}"_tmp_vol
            docker create --name $domain_tmp_vol ${domain} || exit 1

        else

            # Set the mount.
            host_domain_dir=$domain
            domain_tag=$(basename $domain)
            docker_domain_dir=/home/docker/domain/$domain_tag
            args_to_pass=$(echo "$args_to_pass" | sed "s|${domain}|/home/docker/domain/${domain_tag}|")
            
        fi
        echo
       
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
            cp ${candidate_spec_file} ${host_spec_dir}/.
	    can_spec_file_copy=${host_spec_dir}$(basename ${candidate_spec_file})
	    can_spec_file_docker=$docker_spec_dir/$(basename ${candidate_spec_file})
            # Have to mount and edit
	    
	    args_to_pass=$(echo "$args_to_pass" | \
			   sed "s|${candidate_spec_file}|$can_spec_file_docker|")
	    candidate_spec_mounts=""
	    for ii in `egrep 'local_path.*:' ${candidate_spec_file} | tr -d ' ' | cut -d':' -f2`; do
		if [ -z $ii ]; then
		    continue
		fi
		#echo ----
		# SOURCE="$ii"
		# while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlnk
		#     echo bar
		#     DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
		#     SOURCE="$(readlink "$SOURCE")"
		#     [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
		# done
		# the_path=$SOURCE
		the_path=$ii
		#echo "the_path: $the_path"
		#echo "this_repo: $this_repo"
		#echo "this_repo_name: $this_repo_name"
		if [[ "$the_path" != "$this_repo" ]] && \
		       [[ "$the_path" != *"$this_repo_name" ]]; then
		    # If the local_path is the testing repo (though full path is not 
		    # in this case have to mount the repos
		    # and have to edit the copied candidate spec to reflect docker locn
		    rep_str="${the_path}:/home/docker/$(basename ${the_path})"
		    candidate_spec_mounts=$(echo "${candiate_spec_mounts} -v ${rep_str}")
		fi
		sed -i '' "s|${the_path}|/home/docker/$(basename ${the_path})|g" $can_spec_file_copy		    
	    done
	    
        fi

	
        # Use mount this repo to /home/docker
        this_repo_name=$(basename $this_repo)

        not_interactive=$(echo "$args_to_pass" | grep '\-i' | wc -l)
        #args_to_pass=$(echo "$args_to_pass" | sed "s|-i||")
        
        docker_cmd=""
        #docker_cmd=${docker_cmd}"cd /home/docker/wrf_hydro_py; pip uninstall -y wrfhydropy; "
        #docker_cmd=${docker_cmd}" python setup.py install; pip install termcolor; "
        docker_cmd=$(echo \
"${docker_cmd} \
cd /home/docker/${this_repo_name}/tests/; \
./take_test.sh ${args_to_pass}; \
if [ \\\$? -ne 0 ]; then cd /home/docker/take_test; /bin/bash; fi;")

        if [[ "$not_interactive" -eq 1 ]]; then
            echo "Interactive docker requested at end of test."
            docker_cmd=$(echo "${docker_cmd} cd /home/docker/take_test; /bin/bash ")
	else
            docker_cmd=$(echo "${docker_cmd} exit \\\$? ")	    
        fi

        #echo "docker_cmd: $docker_cmd"
        
        echo "Starting the docker image."
        echo
        # -e establishes env variables in docker.
        # -v mounts directories host:docker
        # The GITHUB variables are used only for the private, wrf_hydro_nwm repo.
        invoke_docker=$(echo \
"docker run -it \
     -e USER=docker \
     -e GITHUB_AUTHTOKEN=$GITHUB_AUTHTOKEN \
     -e GITHUB_USERNAME=$GITHUB_USERNAME \
     -e WRF_HYDRO_TESTS_USER_SPEC=${docker_user_spec} \
")
        
        if [[ ! -z $host_domain_dir ]]; then
            invoke_docker=$(echo \
"${invoke_docker} \
     -v ${host_domain_dir}:${docker_domain_dir} \
")
        fi

        invoke_docker=$(echo \
"${invoke_docker} \
     -v ${host_spec_dir}:${docker_spec_dir} \
     -v ${this_repo}:/home/docker/${this_repo_name} \
     ${candidate_spec_mounts}")

        # May want the custom ability to mount wrf_hydro_py at some point. 
#     -v /Users/jamesmcc/WRF_Hydro/wrf_hydro_py:/home/docker/wrf_hydro_py \

        if [[ ! -z $domain_tmp_vol ]]; then
            invoke_docker=$(echo \
"${invoke_docker} \
     --volumes-from ${domain_tmp_vol} \
")
        fi

        invoke_docker=$(echo \
"${invoke_docker} \
     wrfhydro/dev:conda /bin/bash -c \"${docker_cmd}\"")

        #echo "$args_to_pass"
        #echo "invoke_docker: $invoke_docker"
        
        eval $invoke_docker
        return_code=$?
        
        if [[ ${domain} == "wrfhydro/domains:"* ]]; then
            echo "Tearing down the data container: " $(docker rm -v ${domain_tmp_vol})
        fi
        
    fi # Trying docker
    
fi # Known machine else unknown machine

exit $return_code
