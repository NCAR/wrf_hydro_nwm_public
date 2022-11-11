#!/bin/bash

PR=$1
CONFIG=$2
cwd=`pwd`
diffs=$GITHUB_WORKSPACE/test_report/$CONFIG/diff_plots

if [[ ! -d $diffs ]]; then
    echo "No diff plots to attach!"
    exit 0
fi

cd $diffs

for d in `ls -1`
do
        if [[ -d $d && `ls -1 $d` ]]; then
            python $cwd/attach_plots_to_pr.py -r andygaydos/wrf_hydro_nwm_public -p $PR -d $d/* 
        fi
done