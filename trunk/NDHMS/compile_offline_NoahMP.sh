#!/bin/bash

echo
echo "IMPORTANT: When compiling using this script, environment variables for"
echo "WRF-Hydro are sourced from the setEnvar.sh script in this directory."
echo 

sleep 5

source ./setEnvar.sh

if [[ "$WRF_HYDRO" -ne 1 ]]; then
    echo
    echo "Please set WRF_HYDRO to be 1 from setEnvar.sh"
    exit
fi

rm -f  LandModel LandModel_cpl
cp arc/Makefile.NoahMP Makefile
cd Land_models/NoahMP
cp hydro/Makefile.hydro Makefile
if [[ -e "MPP" ]]; then  rm -rf  MPP; fi
ln -sf ../../MPP .
cd ../..

ln -sf Land_models/NoahMP LandModel
cat macros LandModel/hydro/user_build_options.bak  > LandModel/user_build_options
ln -sf CPL/NoahMP_cpl LandModel_cpl
make clean; rm -f Run/wrf_hydro_NoahMP.exe ; rm -f Run/*TBL ; rm -f Run/*namelist*

make

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

cd Run
mv  wrf_hydro.exe wrf_hydro_NoahMP.exe; ln -sf wrf_hydro_NoahMP.exe wrf_hydro.exe
cp ../Land_models/NoahMP/run/*TBL .
cp ../template/NoahMP/namelist.hrldas .
cp ../template/HYDRO/HYDRO.TBL .
cp ../template/HYDRO/CHANPARM.TBL .
cp ../template/HYDRO/hydro.namelist .

echo
echo '*****************************************************************'
echo "The environment variables used in the compile:"
grepStr="(WRF_HYDRO)|(HYDRO_D)|(SPATIAL_SOIL)|(WRF_HYDRO_RAPID)|(WRFIO_NCD_LARGE_FILE_SUPPORT)|(HYDRO_REALTIME)|(NCEP_WCOSS)|(WRF_HYDRO_NUDGING)|(NETCDF)"
printenv | egrep -w "${grepStr}" | sort

exit 0
