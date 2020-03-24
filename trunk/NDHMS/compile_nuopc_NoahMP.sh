#!/bin/bash

## This script takes three optional arguments:
## a file which sets the environment variables
## to use in the compile (clearning any inherited
## from the calling envionrment first). The template
## for this file is trunk/NDHMS/setEnvar.sh. Please
## copy that file to trunk/NDHMS, make copies
## for your favorite compile configurations, and
## pass the appropriate file name to this script
## as desired.
## a file, esmf-impi-env.sh, which sets the esmf 
## environment variables.
## copy this file from CPL/NUOPC_cpl if doesn't exists
## in NDHMS directory.
## compiler option for configure script

env_file=$1
esmf_env=$2
comp_opt=$3

# ###########################
# Include ESMFMKFILE fragment
# ###########################

if [[ ! -z $comp_opt ]]; then
    echo "configure: Running configure script with $comp_opt for the compile options."
    ./configure $comp_opt
else
    echo "configure: You must configure compiler option! Run ./configure"
fi

if [[ ! -z $esmf_env ]]; then
    echo "configure: Sourcing $esmf_env for the compile options."
    source $esmf_env
else
    echo "configure: Using the compile options in the calling environment for ESMFMKFILE."
fi


if [[ ! -z $env_file ]]; then

    ## unset these in the env so we are not mixing
    ## and matching env vars and the sourced file.
    unset WRF_HYDRO
    unset HYDRO_D
    unset RESERVOIR_D
    unset SPATIAL_SOIL
    unset WRF_HYDRO_RAPID
    unset WRFIO_NCD_LARGE_FILE_SUPPORT
    unset NCEP_WCOSS
    unset NWM_META
    unset WRF_HYDRO_NUDGING

    echo "configure: Sourcing $env_file for the compile options."
    source $env_file
    
else
    echo "configure: Using the compile options in the calling environment."
fi

if [[ "$WRF_HYDRO" -ne 1 ]]; then
    echo
    echo "The WRF_HYDRO compile option is required to be 1 for compile_nuopc_NoahMP.sh"
    exit 1
fi

if [[ -e "Makefile.nuopc" ]]; then  rm -f  Makefile.nuopc; fi
cp CPL/NUOPC_cpl/Makefile.nuopc Makefile.nuopc
cd Land_models/NoahMP
if [[ -e "Makefile.NoahMP.nuopc" ]]; then  rm -f Makefile.NoahMP.nuopc ; fi
cp ../../CPL/NUOPC_cpl/Makefile.NoahMP.nuopc Makefile.NoahMP.nuopc
if [[ -e "IO_code/Makefile.IO_code.nuopc" ]]; then  rm -f  IO_code/Makefile.IO_code.nuopc; fi
cp ../../CPL/NUOPC_cpl/Makefile.IO_code.nuopc IO_code/Makefile.IO_code.nuopc
if [[ -e "MPP" ]]; then  rm -rf  MPP; fi
ln -sf ../../MPP .
cd ../..

cat macros Land_models/NoahMP/hydro/user_build_options.bak  > Land_models/NoahMP/user_build_options

# make -f Makefile.nuopc nuopcclean
make -f Makefile.nuopc

if [[ $? -eq 0 ]]; then
    echo
    echo '*****************************************************************'
    echo "Make was successful"
else 
    echo
    echo '*****************************************************************'
    echo "Make NOT successful"
    exit 1
fi

cp template/NoahMP/namelist.hrldas .
cp template/HYDRO/hydro.namelist .

if [ "$(cat .nwm_version)" == "none" ]; then
    # if it is not an nwm version, use the stock ones.
    cp Land_models/NoahMP/run/*TBL .
    cp template/HYDRO/HYDRO.TBL .
    cp template/HYDRO/CHANPARM.TBL .
else
    # If it's an nwm version (nwm release branch), then use these
    cp template/WCOSS/TBLS/* .
fi
    

echo
echo '*****************************************************************'
echo "The environment variables used in the compile:"
grepStr="(WRF_HYDRO)|(HYDRO_D)|(SPATIAL_SOIL)|(WRF_HYDRO_RAPID)|(WRFIO_NCD_LARGE_FILE_SUPPORT)|(HYDRO_REALTIME)|(NCEP_WCOSS)|(WRF_HYDRO_NUDGING)|(NETCDF)"
printenv | egrep -w "${grepStr}" | sort

exit 0
