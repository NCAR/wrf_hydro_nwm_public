#!/bin/bash

. ./setEnvar.sh

if [ "$WRF_HYDRO" -ne 1 ] 
then
   echo "Please set WRF_HYDRO to be 1 from setEnvar.sh"
   exit
fi

rm -f  LandModel LandModel_cpl
cp arc/Makefile.NoahMP Makefile

cd Land_models/NoahMP

cp hydro/Makefile.hydro Makefile

if [ -e "MPP" ]
then  
  rm -rf  MPP
fi

ln -sf ../../MPP .

cd ../..

ln -sf Land_models/NoahMP LandModel

cat macros LandModel/hydro/user_build_options.bak  > LandModel/user_build_options

ln -sf CPL/NoahMP_cpl LandModel_cpl

make clean; rm -f Run/wrf_hydro_NoahMP.exe

make

cd Run

mv  wrf_hydro.exe wrf_hydro_NoahMP.exe; ln -sf wrf_hydro_NoahMP.exe wrf_hydro.exe
