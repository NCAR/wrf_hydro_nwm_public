#!/usr/bin/env bash

# Pull the docker container for model testing
docker pull wrfhydro/dev:modeltesting

# Run testing
docker run -it \
-v /Volumes/d1/jmills/temp/wrf_hydro_nwm_public:/home/docker/candidate \
-v /Volumes/d1/jmills/temp/wrf_hydro_nwm_public_upstream:/home/docker/reference \
wrfhydro/dev:modeltesting \
--config nwm_ana nwm_long_range gridded reach \
--domain_tag dev

# See https://hub.docker.com/r/wrfhydro/dev/ for more info
# To get help on command line use 'docker run wrfhydro/dev:modeltesting --help'
