#!/bin/bash
# Shell script to save files from a previous run before starting a new run.
# Script will also clean up directory in preparation for next run...
# Usage:  ./run_save.sh <save_directory>
# Notes:
#
# Modified by: D. Gochis, Nov. 5, 2014

save_dir=$1
cd $WORKDIR
rm *LSMOUT* qstrmvolrt_accum.txt 
cp frxst_pts_out.txt diag_hydro.* GW_*txt $save_dir
cp CHANPARM.TBL GENPARM.TBL namelist.hrldas hydro.namelist GWBUCKPARM.TBL SOILPARM.TBL $save_dir
mv 201* HYDRO_RST* $save_dir
#mv HYDRO_RST* RESTART* $save_dir
