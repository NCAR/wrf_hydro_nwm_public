#!/usr/bin/env bash
export PATH="/glade/p/work/jmills/nwm_testing/miniconda3/envs/nwm_data_pipeline/bin:$PATH"
export PYTHONPATH="/glade/p/work/jmills/nwm_testing/miniconda3/envs/nwm_data_pipeline/lib/python3.6/site-packages/"

python /glade/scratch/jmills/wrf_hydro_nwm_public/tests/local/run_tests.py \
--config nwm_ana nwm_long_range gridded reach \
--compiler ifort \
--output_dir /glade/scratch/jmills/test_out \
--candidate_dir /glade/scratch/jmills/wrf_hydro_nwm_public/ \
--reference_dir /glade/scratch/jmills/wrf_hydro_nwm_public/ \
--domain_tag dev \
--ncores 3
