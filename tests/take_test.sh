#!/bin/bash

# This is a wrapper on take_test_core.sh which handles the logging. 

log_file=take_test.log

./core_take_test.sh "${@}" 2>&1 | tee $log_file

## The following is how you get a return status in spite of tee.
exitValue=${PIPESTATUS[0]}

exit $exitValue
