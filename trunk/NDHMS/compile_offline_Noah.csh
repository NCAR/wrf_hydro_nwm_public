#!/bin/csh -f

rm -f LandModel LandModel_cpl
ln -sf CPL/Noah_cpl LandModel_cpl
ln -sf Land_models/Noah_LandModel LandModel
make clean


cat macros LandModel/user_build_options.bak > LandModel/user_build_options
make
