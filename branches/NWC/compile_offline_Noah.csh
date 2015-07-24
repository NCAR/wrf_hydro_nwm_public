#!/bin/csh -f

rm -f LandModel LandModel_cpl
ln -sf CPL/Noah_cpl LandModel_cpl
ln -sf Land_models/Noah LandModel
make clean ; rm -f Run/wrf_hydro_Noah.exe


cat macros LandModel/user_build_options.bak > LandModel/user_build_options
make
cd Run
mv wrf_hydro.exe wrf_hydro_Noah.exe ; ln -sf wrf_hydro_Noah.exe wrf_hydro.exe
