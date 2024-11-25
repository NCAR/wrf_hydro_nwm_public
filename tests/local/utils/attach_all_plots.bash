#!/bin/bash

PR=$1
CONFIG=$2
TEST_TYPE=$3
cwd=`pwd`
diffs=$GITHUB_WORKSPACE/test_report/$CONFIG

if [[ ! -d $diffs ]]; then
    echo "No diff plots to attach at ${diffs}"
    exit 0
fi

title="Difference plots for configuration '$CONFIG' and $TEST_TYPE test type"

# set for repo authentication
git config --global user.email "model.tester@ucar.edu"
git config --global user.name "Model Tester"

TOKEN=`wget -q -O- https://hydro.rap.ucar.edu/HydroInspector/token.html`
REPO=NCAR/wrf_hydro_nwm_public

cd $diffs

# if $diffs directory is not empty, attach files in it
if [[ `ls -1 ./` ]]
then
    python $cwd/attach_plots_to_pr.py -r $REPO -p $PR -d -t "$TOKEN" --title "$title" ./*
fi
