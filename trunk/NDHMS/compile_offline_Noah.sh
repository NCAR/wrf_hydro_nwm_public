#!/bin/bash

source ./setEnvar.sh

if [[ "$WRF_HYDRO" -ne 1 ]]; then
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
cp ../template/HYDRO/hydro.namelist .

echo
echo '*****************************************************************'
echo "The environment variables used in the compile:"
printenv | egrep -i "(HYDRO|NUDG|PRECIP|CHAN_CONN|^NETCDF|REALTIME|SOIL|WRFIO)" | egrep -v PWD

exit 0
