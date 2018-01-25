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

rm -f LandModel LandModel_cpl
cp arc/Makefile.Noah Makefile
ln -sf CPL/Noah_cpl LandModel_cpl
ln -sf Land_models/Noah LandModel
cat macros LandModel/user_build_options.bak  > LandModel/user_build_options
make clean ; rm -f Run/wrf_hydro_Noah.exe ; rm -f Run/*TBL ; rm -f Run/*namelist*

cat macros LandModel/user_build_options.bak > LandModel/user_build_options

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
mv wrf_hydro.exe wrf_hydro_Noah.exe ; ln -sf wrf_hydro_Noah.exe wrf_hydro.exe
cp ../Land_models/Noah/Run/*TBL .
cp ../template/Noah/namelist.hrldas .
cp ../template/HYDRO/HYDRO.TBL .
cp ../template/HYDRO/CHANPARM.TBL .
cp ../template/HYDRO/hydro.namelist .

echo
echo '*****************************************************************'
echo "The environment variables used in the compile:"
grepStr="(WRF_HYDRO)|(HYDRO_D)|(SPATIAL_SOIL)|(WRF_HYDRO_RAPID)|(WRFIO_NCD_LARGE_FILE_SUPPORT)|(HYDRO_REALTIME)|(NCEP_WCOSS)|(WRF_HYDRO_NUDGING)|(NETCDF)"
printenv | egrep -w "${grepStr}" | sort

exit 0
