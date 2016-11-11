#!/bin/sh

SORCnwm=`pwd`
EXECnwm=`pwd`/../../exec
#MODULEFILESnwm=`pwd`/../../modulefiles
#export MODULEPATH=$MODULEPATH:$MODULEFILESnwm
#
#. $MODULESHOME/etc/modules.sh
module purge
module load intel/15.3.187 impi
module load netcdf/4.3.0
module load hdf5

export NETCDF_INC=$NETCDF/include
export NETCDF_LIB=$NETCDF/lib

set -aux

cd $SORCnwm

# ============= wrf-hydro ===================

# Run configuration file
./configure <<RUNME
10
\r
RUNME

# Offline compiling
make clean

./compile_offline_NoahMP.sh

cp -p Run/wrf_hydro.exe $EXECnwm/nwm.exe


# ============= wrf-hydro nudging  ===================
# Turn on Nudging model
export WRF_HYDRO_NUDGING=1

# Run configuration file
./configure <<RUNME
10
\r
RUNME

# Nudging compiling
make clean

./compile_offline_NoahMP.sh

cp -p Run/wrf_hydro.exe $EXECnwm/nwm_nudging.exe

rm -f LandModel LandModel_cpl Land_models/NoahMP/MPP macros Makefile.comm

