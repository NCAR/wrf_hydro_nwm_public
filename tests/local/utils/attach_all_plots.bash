#!/bin/bash

PR=$1
CONFIG=$2
cwd=`pwd`
diffs=$GITHUB_WORKSPACE/test_report/$CONFIG

if [[ ! -d $diffs ]]; then
    echo "No diff plots to attach at ${diffs}"
    exit 0
fi

title="Difference plots for configuration '$CONFIG'"

# set for repo authentication
git config --global user.email "model.tester@ucar.edu"
git config --global user.name "Model Tester"

TOKEN=`wget -q -O- https://hydro.rap.ucar.edu/HydroInspector/token.html`
REPO=NCAR/wrf_hydro_nwm_public

cd $diffs

set -e -x

if [[ `ls -1 $d` ]]
then
    echo "attach_all_plots.py:28"
    python $cwd/attach_plots_to_pr.py -r $REPO -p $PR -d -t "$TOKEN" --title "$title" ./*
fi
