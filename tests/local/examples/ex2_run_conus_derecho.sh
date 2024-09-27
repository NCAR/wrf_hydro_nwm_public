#!/usr/bin/env bash

# Load conda environment

# Run the tests using a scheduler 
python /glade/scratch/$USER/wrf_hydro_nwm_public/tests/local/run_tests.py \
--config nwm_ana nwm_long_range gridded reach \
--compiler ifort \
--output_dir /glade/derecho/scratch/$USER/test_out \
--candidate_dir /glade/derecho/scratch/$USER/wrf_hydro_nwm_public/ \
--reference_dir /glade/derecho/scratch/$USER/wrf_hydro_nwm_public/ \
--domain_dir path/to/CONUS_domain \
--scheduler
