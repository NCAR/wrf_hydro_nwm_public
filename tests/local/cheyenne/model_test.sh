#!/bin/bash
# A first productiony test script for cheyenne.
# James McCreight
# April 5, 2019
# Why bash?
#    1) might need to establish the python env from scratch,
#    2) set the modules is much easier
#    3) git is easier in bash.
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

the_help="\
Model Testing

Required
    -c: 
        Candidate directory path. The code to test. 
    -r:
        Reference directory path. The code to test against.

Options:
    --reference_update: 
       default=true
       In the reference repo, fetch upstream (NCAR) and checkout branch matching 
       that of the candidate?
    --compiler: 
        [default=ifort, gfort, <You can try whatever is available in modules>] 
        Currently:  ifort -> intel/17.0.1   and  gnu -> gnu/7.1.0
        We should try to keep this update along with what is here:
        https://github.com/NCAR/wrf_hydro_nwm_public/wiki/Compiler-requirements-by-version
    --mpi: 
        [default=impi, <You can try whatever is available in modules.>]
    --exe_cmd:
        [default=\"mpirun -np \\\$ncores ./wrf_hydro.exe\", <Whatever you need for your mpi.>]
        See examples for MPT in the shared queue (other MPI distros apparently dont need this).
    --config: 
        [default=nwm_ana, nwm_long_range, <Whatever is available in the .json files for the domain.>]
    ---ncores: 
        default=360
    --nnodes:
        default=ceiling ncores 36
    --queue:
        default=regular
    --account:
        default=NRAL0017
    --walltime:
        default=01:00:00
    --domain_dir:
        default=/glade/work/jamesmcc/domains/private/CONUS
        The domain must be properly constructed with domain-side json namelist patch files and a
        .version file. Most domains in /glade/work/jamesmcc/domains fit that criteria.
    --use_existing_test_dir
        default is not included. Lets testing proceede with existing output in place.
    --xrcmp_n_cores
        default is 0. Values < 2 mean \"use nccmp\", otherwise the number of cores xrcmp
        and xrnan will use.


Usage Examples: 
# A CONUS test of nwm_ana
./model_test.sh -c /path/to/candidate_dir -r /path/to/reference_dir

# A CONUS test of nwm_long_range
./model_test.sh -c /path/to/candidate_dir -r /path/to/reference_dir \\
    --compiler=ifort --mpi=mpt --config=nwm_long_range --ncores=288

# A croton test of all test configurations. Could swap the domain.
./model_test.sh \\
    -c ~/WRF_Hydro/wrf_hydro_nwm_public \\
    -r ~/WRF_Hydro/.wrf_hydro_nwm_public_REFERENCE \\
    --compiler=ifort --mpi=impi \\
    --config='nwm_ana nwm_long_range gridded reach' \\
    --ncores=6 --queue=share \\
    --domain_dir /glade/work/jamesmcc/domains/public/croton_NY

# Non-standard mpi/exe_cmd example. A croton test of nwm_ana configuration.
# An strenuous exercise in bash escaping.
./model_test.sh \\
    -c ~/WRF_Hydro/wrf_hydro_nwm_public \\
    -r ~/WRF_Hydro/.wrf_hydro_nwm_public_REFERENCE \\
    --compiler=ifort --mpi=mpt \\
    --exe_cmd=\"mpiexec_mpt $'\\\$(hostname)' -np \\\$ncores ./wrf_hydro.exe\" \\
    --config='nwm_ana' \\
    --ncores=6 --queue=share \\
    --domain_dir /glade/work/jamesmcc/domains/public/croton_NY
"

## Default options
compiler=ifort
mpi=impi
exe_cmd="mpirun -np \$ncores ./wrf_hydro.exe"
config=nwm_ana
ncores=360
nnodes=to_calculate
queue=regular
account=NRAL0017
walltime=01:00:00
reference_update=true
domain_dir=/glade/work/jamesmcc/domains/private/CONUS
use_existing_test_dir=''
xrcmp_n_cores=0

# Getops
TEMP=\
`getopt \
    -o h,c:,r: \
    --long help,compiler:,mpi:,exe_cmd:,config:,ncores:,nnodes:,queue:,account:,walltime:,reference_update:,domain_dir:,use_existing_test_dir,xrcmp_n_cores: \
    -n 'Model Testing' -- "$@"`

if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi
# Note the quotes around `$TEMP': they are essential!
eval set -- "$TEMP"
while true; do
    case "$1" in
        -h | --help ) echo "$the_help"; exit 0 ;;
        -c ) candidate_dir="$2"; shift 2;;
        -r ) reference_dir="$2"; shift 2;;
        --reference_update ) reference_update="$2"; shift 2;;
        --compiler ) compiler="$2"; shift 2;;
        --mpi ) mpi="$2"; shift 2;;
        --exe_cmd ) exe_cmd="$2"; shift 2;;
        --config ) config="$2"; shift 2;;
        --ncores ) ncores="$2"; shift 2;;
        --nnodes ) nnodes="$2"; shift 2;;
        --queue ) queue="$2"; shift 2;;
        --account ) account="$2"; shift 2;;
        --walltime ) walltime="$2"; shift 2;;
        --domain_dir ) domain_dir="$2"; shift 2;;
        --use_existing_test_dir ) use_existing_test_dir="--use_existing_test_dir " ; shift 1;;
        --xrcmp_n_cores ) xrcmp_n_cores="$2"; shift 2;;
        -- ) shift; break ;;
        * ) break ;;
    esac
done

# Required variables.
if [ -z $candidate_dir ] | [ -z $reference_dir ]; then
    echo "The candidate_dir and reference_dir arguments are both required but not found. Exiting."
    exit 1
fi

# Perform any needed substitution/evaluation.
if [[ "$nnodes" == to_calculate ]]; then
    function ceiling() { echo "($1 + $2 - 1)/$2" | bc; }
    nnodes=$(ceiling $ncores 36)
fi

exe_cmd=`eval "echo $exe_cmd"`


#-------------------------------------------------------
# Modules.
# Default intel and gnu compiler versions if the generics are passed
if [ "$compiler" == ifort ]; then
    compiler_module=intel/18.0.5
elif [ "$compiler" == gfort ]; then
    compiler_module=gnu/8.3.0
else
    compiler_module=$compiler
    if [[ "$compiler" == *intel* ]]; then
        compiler=ifort
    elif [[ "$compiler" == *gnu* ]]; then
        compiler=gfort
    else
        "Unsure how to se the ./configure compiler from the selected compiler=$compiler"
    fi
fi

echo
printf "\e[7;49;94mModule information\e[0m\n"
module purge
# Is this strict enough in the sense that things might be changing?
module load  $compiler_module  $mpi  ncarcompilers  netcdf  ncarenv nccmp || exit 4
module list

#-------------------------------------------------------
# Python Env
deactivate > /dev/null 2>&1
source /glade/p/cisl/nwc/model_testing_env/wrf_hydro_nwm_test/bin/activate || exit 9
#-------------------------------------------------------
## Candidates branch to tag the test directory and optionally update the reference.
cd $candidate_dir
branch_name="$(git symbolic-ref HEAD 2>/dev/null)" || branch_name="$(git rev-parse --short HEAD)"
branch_name=${branch_name##refs/heads/}
printf "\e[7;49;94mTesting branch: $branch_name in $candidate_dir\e[0m\n"
output_dir=/glade/scratch/`whoami`/take_test_$(basename $domain_dir)_${branch_name}

#-------------------------------------------------------
# "Update" the reference dir? This makes the branch match that of
# the candidate and it fetches the NCAR:branch_name
# There are two potential pitfalls here,
# 1) the branch name may not match
# 2) you may not want to update. (take care of)
if [[ $reference_update == 'true' ]]; then
    echo; echo
    printf "\e[7;49;94mUpdate the reference repository with NCAR/$branch_name\e[0m\n"
    cd $reference_dir || exit 9
    if [[ `hostname` != *cheyenne* ]]; then
        ssh cheyenne1 "cd $reference_dir && git fetch upstream" || exit 9
    else
        cd $reference_dir || exit 9
        git fetch upstream || exit 9
    fi             
    git checkout origin/$branch_name  || exit 9
    cd - 2> /dev/null cd 1> /dev/null
fi

#-------------------------------------------------------
echo; echo
printf "\e[7;49;94mStarting tests in $output_dir\e[0m\n"
echo

run_test_path=$(dirname $script_dir)
python3 $run_test_path/run_tests.py \
       --config $config \
       --compiler $compiler \
       --exe_cmd="'$exe_cmd'" \
       --scheduler \
       --output_dir $output_dir \
       --candidate_dir $candidate_dir \
       --reference_dir $reference_dir \
       --domain_dir $domain_dir \
       --ncores $ncores \
       --nnodes $nnodes \
       --account $account \
       --queue $queue \
       --walltime $walltime \
       --xrcmp_n_cores $xrcmp_n_cores \
       $use_existing_test_dir

exit $?
