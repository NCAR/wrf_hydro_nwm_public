#!/bin/csh -f

source setEnvar.csh

if($WRF_HYDRO != 1) then
   echo "Please set WRF_HYDRO to be 1 from setEnvar.csh"
   exit
endif
rm -f  LandModel LandModel_cpl
cp arc/Makefile.NoahMP Makefile
## if(! -e Land_models/NoahMP/MPP) then
  cd Land_models/NoahMP
  cp hydro/Makefile.hydro Makefile
  if(-e MPP) rm -rf  MPP
  ln -sf ../../MPP .
  cd ../..
## endif
ln -sf Land_models/NoahMP LandModel
cat macros LandModel/hydro/user_build_options.bak  > LandModel/user_build_options
ln -sf CPL/NoahMP_cpl LandModel_cpl
make clean; rm -f Run/wrf_hydro_NoahMP.exe

make
cd Run
mv  wrf_hydro.exe wrf_hydro_NoahMP.exe; ln -sf wrf_hydro_NoahMP.exe wrf_hydro.exe
