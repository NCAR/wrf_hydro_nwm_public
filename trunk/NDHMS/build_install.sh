#!/bin/sh

SORCnwm=`pwd`
EXECnwm=`pwd`/../../exec
MODULEFILESnwm=`pwd`/../../modulefiles
export MODULEPATH=$MODULEPATH:$MODULEFILESnwm

if [ ! -e $EXECnwm ]; then mkdir $EXECnwm; fi

. $MODULESHOME/etc/modules.sh
module purge
module load nwm/v1.1 
module list

set -aux

cd $SORCnwm

# ============= wrf-hydro nudging  ===================
# Turn on Nudging model
export WRF_HYDRO_NUDGING=1

# Run configuration file
./configure <<RUNME
9
\r
RUNME

# Nudging compiling
make clean

./compile_offline_NoahMP.sh

cp -p Run/wrf_hydro.exe $EXECnwm/nwm.exe

rm -f LandModel LandModel_cpl Land_models/NoahMP/MPP macros Makefile.comm

