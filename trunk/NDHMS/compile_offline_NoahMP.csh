#!/bin/csh -f
rm -f  LandModel LandModel_cpl
ln -sf Land_models/NoahMP_LandModel LandModel
ln -sf CPL/NoahMP_cpl LandModel_cpl
make clean

cat macros LandModel/user_build_options.bak > LandModel/user_build_options
make
