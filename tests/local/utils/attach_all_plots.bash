#!/bin/bash

PR=$1
CONFIG=$2
cwd=`pwd`

cd $GITHUB_WORKSPACE/test_report/$CONFIG/diff_plots

for d in `ls -1`
do
        if [[ -d $d && `ls -1 $d` ]]; then
            python $cwd/attach_plots_to_pr.py -r andygaydos/wrf_hydro_nwm_public -p $PR -d $d/* -t "$GITHUB_TOKEN"
        fi
done