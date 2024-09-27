#!/bin/bash

./model_test.sh \
    -c  path/to/wrf_hydro_nwm_public \
    -r  path/to/wrf_hydro_nwm_public_REFERENCE \
    --compiler=ifort \
    --mpi=impi \
    --config='nwm_ana' \
    --ncores=6 --queue=share \
    --reference_update=false \
    --domain_dir path/to/croton_NY

# Can be added
#    --use_existing_test_dir
#    --xrcmp_n_cores 4

exit $?
