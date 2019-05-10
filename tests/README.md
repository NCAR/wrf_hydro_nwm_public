# Testing wrf\_hydro\_nwm\_public & wrf\_hydro\_nwm

## Status
For the community 5.0 release: testing is in something of a crude
state, but it is funcitonal. Improvements are on their way. 


## Purposes: 
* Protect production code
* Distribute responsibility
* Reproducibility and communication: log files
* Support your development: boost confidence, find bugs faster

The mantra is: *Test with every compile on small domains.*


## Overview
Conceptually a *candidate* takes a *test*. The names of the `take\_test.sh` 
and and `take_test.py` scripts emphasize that there are two parts: the taker 
and the test. The candidate which takes the test is the state of this (or 
potentially some other) repository. The tests are encoded in the `tests/` 
directory. The tests referr to another repository state called the 
"reference", this is a blessed state of the repository for the candidate's 
results.

By default: 
* Candidate is the current (potentially uncommitted) state of the repo from which 
`take_test` is invoked. 
* Reference is upstream/master (either NCAR/wrf_hydro_nwm_public or 
NCAR/wrf_hydro_nwm).
* The tests are defined by the `tests/test_*` files. 

## Usage
Currently there are 2 ways to invoke the testing. The fundamental way:
`python take_test.py <options>`. This works fine on linux. But if you want to 
test on a non-linux machine using docker, the following script is meant to be 
a machine independent interface: `take_test.sh <options>`. This later script is 
still under development.

Both of the "take_test" scripts may be preceeded with a path or
invoked in the `tests/` directory, as shown. 

The options are described below.

Currently supported machines: cheyenne and docker. There are sections below 
about both of these. More machines can be added. 

Options (all optional) are described by:
`python take_test.py --help` and `./take_test.sh --help`:

```
Retrieving the help from take_test.py...

usage: take_test.py [-h] [--domain /path/to/domain/directory]
                    [--candidate_spec_file path/to/candidate_spec_file]
                    [--config [key [key ...]]] [--test_spec [key [key ...]]]

A WRF-Hydro candidate takes a test.

optional arguments:
  -h, --help            show this help message and exit
  --domain /path/to/domain/directory
                        Path to the domain directory.
  --candidate_spec_file path/to/candidate_spec_file
                        The YAML candidate specification file.
  --config [key [key ...]]
                        Zero or more keys separated by whitespace for model
                        configuration selection (no keys runs all
                        configurations).
  --test_spec [key [key ...]]
                        Zero or more keys separated by whitespace for
                        specifying the desired tests. These keys are grepped
                        against the test_*py files in the tests/ directory.

take_test.sh notes: 
  When using docker, the domain argument becomes the key which is the basename of the path.
```



## Configuration files
### User spec file 
The user specification file is set by the following environment variable, for
example in bash:

`export WRF_HYDRO_TESTS_USER_SPEC=~/wrf_hydro_tests_user_spec.yaml`

The `wrf_hydro_nwm_public/tests/tests/template_user_spec.yaml` should
be copied a new location and edited to meet your needs (do not put
your edits under version control).

### Candidate spec file
The `wrf_hydro_nwm_public/tests/template_candidate_spec.yaml` should
be copied and modified for specific tests (do not put your edits under
version control). This file allows for the maximum testing flexibility. 

### Machine spec file
This file is to be updated for new machines (your cluster or your desktop) to
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


## Cheyenne Setup
1. Setup the python 3.6.4 virutal env per cisl instructions.
   Python 3.6.4+ is required.
   Both sections   
      [https://www2.cisl.ucar.edu/resources/computational-systems/cheyenne/software/python#modules]  
      [https://www2.cisl.ucar.edu/resources/computational-systems/cheyenne/software/python#library]  
   including "Creating your own clone of the NCAR package library".

3. Install the wrfhydropy prequisites.  
   These are currently summarized here:  
      [https://github.com/NCAR/wrf_hydro_docker/blob/160da2458e9be7313636910051fa8887776fb7be/dev/conda/Dockerfile#L40-L43]  
   Currently (may be out of date) this is, for example:  
      `pip install jupyter cartopy rasterio netcdf4 dask f90nml deepdiff xarray plotnine boltons pytest pytest-datadir-ng wrfhydropy`
   If a development version of wrfhydropy is needed, you'll need to clone that repository to cheyenne, then do the following:  
      `cd /path/to/wrf_hydro_py/; pip uninstall -y wrfhydropy; python setup.py install`

## Docker Example
The docker setup is given in `take_test.sh`. Comments are provided for
the docker commands.
