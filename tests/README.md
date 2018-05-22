# Testing wrf\_hydro\_nwm\_public & wrf\_hydro\_nwm

## Status
For the community 5.0 release: testing is in something of a crude
state, but it is funcitonal. Improvements are on their way. 


## Purposes: 
* Protect production code
* Distribute responsibility
* Reproducibility and communication: log files
* Support your development: boost confidence, find bugs faster

The mantra is: *Test with every compile.*


## Overview
Conceptually a *candidate* takes a *test*. The take\_test name
emphasizes that there are two parts: the taker and the test. The
candidate is the state of this (or some other) repository. The tests
are encoded in the `tests/` directory. The tests refere to another
repository state called the "reference", this is a blessed state of
the repository for the candidate's results.


## Usage
Currently:  
`python take_test.py <options>`

In the future, we aim to provide a more general script which will 
choose and run in docker if not on a known machine. 

Supported machines: cheyenne and docker. There are sections below 
about both of these. More machines can be added. 


Options are currently described by:
`python take_test.py --help`:


```
usage: take_test.py [-h] [--domain path to domain directory]
                    [--candidate_spec_file candidate spec file]
                    [--config model configuration key]
                    [--test_spec test specification key]

A WRF-Hydro candidate takes a test.

optional arguments:
  -h, --help            show this help message and exit
  --domain path to domain directory
                        Path to the domain directory.
  --candidate_spec_file candidate spec file
                        The candidate specification file.
  --config model configuration key
                        Key for model configuration (default is all)
  --test_spec test specification key
                        Key for specifying the desired tests
```


## Configuration files
### User spec file 
The user specification file is set by the following environment variable, for
example in bash:

`export WRF_HYDRO_TESTS_USER_SPEC=~/wrf_hydro_tests_user_spec.yaml`

The `wrf_hydro_nwm_public/tests/tests/template_user_spec.yaml` should
be copied a new location and edited to meet your needs.

### Candidate spec file
The `wrf_hydro_nwm_public/tests/template_candidate_spec.yaml` should
be copied and modified for specific tests. This file allows for the
maximum testing flexibility. 

### Machine spec file
This file is update for new machines (your cluster or your desktop) to
run the tests. These changes should come back via version control so
that each machine only needs specified just once. 


## Requirements:
Broadly:  
python3.6.4 with [wrfhydropy](https://github.com/NCAR/wrf_hydro_py)
installed. Generally, the latest which installs from pip should work.

Specifically:  
Requirements are documented in this docker file
[wrfhydro/dev:conda](https://github.com/NCAR/wrf_hydro_docker/blob/master/dev/conda/Dockerfile)


## CI Domain
The testing is mean to work with arbitrary domains as long as they
adopt the correct conventions. The Croton, NY, domain is used for
testing: wrfhydro/domains:croton_NY. We will shortly provide a better
way to pull this domain and other domains outside the docker context.


## Cheyenne Example



## Docker Example

## Get docker images
docker pull wrfhydro/dev:conda
docker pull wrfhydro/domains:croton_NY

## Create data volume
docker create --name croton_NY wrfhydro/domains:croton_NY

## Start docker
docker run --volumes-from croton_NY -v YOUR_WRF_HYDRO_NWM_CODE_DIRECTORY:/home/docker/wrf_hydro_nwm_public -it wrfhydro/dev:conda

### Running inside of docker issue the following commands
#### Install correct version of wrfhydropy
pip uninstall -y wrfhydropy
pip install wrfhydropy

#### Change directory to wrf_hydro_nwm and run tests
cd /home/docker/wrf_hydro_nwm_public
pytest -v --domain_dir=/home/docker/domain/croton_NY 
          --candidate_dir=YOUR_REFERENCE_CODE_DIRECTORY/trunk/NDHMS
          --reference_dir=YOUR_CANDIDATE_CODE_DIRECTORY/trunk/NDHMS 
          --output_dir=/home/docker/test_out"
