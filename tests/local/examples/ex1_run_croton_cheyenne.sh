#!/usr/bin/env bash

# These paths point to a preconfigured conda environment. Likewise, you can install your own
# using the utils/nwm_testing.yml conda environment file.
export PATH="/glade/work/jmills/nwm_testing/miniconda3/envs/nwm_testing/bin:$PATH"
export PYTHONPATH="/glade/work/jmills/nwm_testing/miniconda3/envs/nwm_testing/lib/python3.7/site-packages"

# Run the tests interactively, no scheduler
python /glade/scratch/$USER/wrf_hydro_nwm_public/tests/local/run_tests.py \
--config nwm_ana nwm_long_range gridded reach \
--compiler ifort \
--output_dir /glade/scratch/$USER/test_out \
--candidate_dir /glade/scratch/$USER/wrf_hydro_nwm_public/ \
--reference_dir /glade/scratch/$USER/wrf_hydro_nwm_public/ \
--domain_tag dev \
--ncores 3
