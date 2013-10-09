#!/bin/csh -f
rm -f  LandModel LandModel_cpl
ln -sf land_models/NoahMP_LandModel LandModel
ln -sf cpl/NoahMP_cpl LandModel_cpl
make clean

cat macros LandModel/user_build_options.bak > LandModel/user_build_options
make
