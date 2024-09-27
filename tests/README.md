tests/
===============================

# Testing: Just do it.
Please use and help to improve the testing, including this documentation.

You are responsible for your code passing these tests on multiple domains,
you should use it on machines where you run, not just via Travis CI.


# Why testing?
* Protect production code
* Distribute responsibility
* Reproducibility and communication: log files, common tests
* Support your development: boost confidence, find bugs faster

The mantra is: *Test with every compile on small domains (... and then
test CONUS with PRs).*


# Conceptual overview
* Candidate: The repository state to be merged with the reference.
* Reference: The accepted target for merging the candidate.

A *candidate* takes a *test*. When a candidate "passes" all its tests,
it generally becomes the new reference for the next candidate.

The *reference* is commonly known as 'upstream' in git parlance, at
least when testing is applied for merging code to upstream. But the
tests can compare any two repo states, including uncommitted states.

In most cases the candidate will not change the output of model relative to the
reference. In some cases the candidate will change the output of the model
relative to the reference, that is the candidate does not pass regression
testing. When this happens, evidence, justification, discussion, and sound
judgement are required to accept such changes as the new reference.

Regression is optional, the other tests are not.

## Summary of Basic tests:
* Compile: does the candidate compile?
* Run: does the candidate run?
* N-cores test: are the results independent of the number of proceses used by
MPI?
* Perfect restart: Can candidate model state written to and retrieved from disk
without affecting the model state at a later time? Illustration:
```
state1   ->   state2     ->  state3
                 \                  =?
	              (restart)->  state3'
```
* Regression: Does the candidate output match that of the reference?
* Metadata Regression: Does the candidate metadata match that of the reference?
* NaN Check: Check for NaNs in output.


# Technical overview
Core: `pytest` & `wrfhydropy`
User Interface: `tests/local/run_tests.py`

## `pytest`
`pytest` is the engine which carries out testing. You can call pytest directly
in `tests/` but it is not the easiest thing to interact with directly, at least
not for the testing in this repo which is not directly on python code. Calls
to `pytest` is generated and printed prior to calling it by the standard user
interface explained below `tests/local/run_tests.py`, this provides hints for
when tweaking direct calls to pytest are necessary (not normal).


## `wrfhydropy`
The python API for wrf-hydro, facilitates building objects/classes like
"simulations", "jobs", and "schedulers" which can be reused. Classes also
provide methods for evaluation and comparing outputs. The model-side and
domain-side JSON namelist files used by `wrfhydropy` are key to establishing
model "configurations" which can be applied to any domain. These are key
capabilities for flexible testing.

TODO: Explain the JSON namelists.

## `tests/local/run_tests.py`
This is the main user interface to the testing, to be called directly by users.
Examples are provided in `tests/local/examples`.

At this time:
```
$ python run_tests.py --help
usage: run_tests.py [-h] --config CONFIG [CONFIG ...] --compiler COMPILER
                    --output_dir OUTPUT_DIR --candidate_dir CANDIDATE_DIR
                    --reference_dir REFERENCE_DIR [--domain_dir DOMAIN_DIR]
                    [--domain_tag DOMAIN_TAG] [--exe_cmd EXE_CMD]
                    [--ncores NCORES] [--scheduler] [--nnodes NNODES]
                    [--account ACCOUNT] [--walltime WALLTIME] [--queue QUEUE]
                    [--print] [--pdb] [-x] [--use_existing_test_dir]
                    [--xrcmp_n_cores XRCMP_N_CORES]

Run WRF-Hydro test suite locally

optional arguments:
  -h, --help            show this help message and exit
  --config CONFIG [CONFIG ...]
                        <Required> The configuration(s) to test, must be one
                        listed in src/hydro_namelist.json keys.
  --compiler COMPILER   <Required> compiler, options are intel or gfort
  --output_dir OUTPUT_DIR
                        <Required> test output directory
  --candidate_dir CANDIDATE_DIR
                        <Required> candidate model directory
  --reference_dir REFERENCE_DIR
                        <Required> reference model directory
  --domain_dir DOMAIN_DIR
                        optional domain directory
  --domain_tag DOMAIN_TAG
                        The release tag of the domain to retrieve, e.g.
                        v5.0.1. or dev. If specified, a small test domain will
                        be retrieved and placed in the specified output_dir
                        and used for the testing domain
  --exe_cmd EXE_CMD     The MPI-dependent model execution command. Default is
                        best guess. The first/zeroth variable is set to the
                        total number of cores (ncores). The wrf_hydro_py
                        convention is that the exe is always named
                        wrf_hydro.exe.
  --ncores NCORES       Number of cores to use for testing
  --scheduler           Scheduler to use for testing, options are PBSDerecho
                        or do not specify for no scheduler
  --nnodes NNODES       Number of nodes to use for testing if running on
                        scheduler
  --account ACCOUNT     Account number to use if using a scheduler.
  --walltime WALLTIME   Account number to use if using a scheduler.
  --queue QUEUE         Queue to use if running on NCAR Derecho, options are
                        regular, premium, or shared
  --print               Print log to stdout instead of html
  --pdb                 pdb (debug) in pytest
  -x                    Exit pdb on first failure.
  --use_existing_test_dir
                        Use existing compiles and runs, only perform output
                        comparisons.
  --xrcmp_n_cores XRCMP_N_CORES
                        Use xrcmp if > 0, and how many cores if so?
```

## Croton example
Many docker-related details aside, this is essentially how the Croton Continuous-Inegration domain is run inside a docker container:
```
$ cd ~/wrf_hydro_nwm_public/tests/local
$ python run_tests.py \
    --config nwm_ana nwm_long_range reach gridded
    --compiler gfort \
    --output_dir /home/docker/test_out \
    --candidate_dir /home/docker/wrf_hydro_nwm_public \
    --reference_dir /home/docker/wrf_hydro_nwm_public_upstream \
    --domain_dir /croton_NY
```
This can be adapted to other platorms....


# Requirements / Software Stack
In addition to needing to compile and run the model, python3 is needed with
specific libraries which are encapsulated in `tests/local/requirements.txt`. One
notable piece of software used specifically for comparing output files is
[`nccmp`](https://gitlab.com/remikz/nccmp). For large domains, we rolled a
version of this tool using [`xarray`](https://github.com/pydata/xarray), another
notable piece in the testing stack.

The following two envionments come "ready to go":


## Docker
The two containers [`wrfhydro/dev:conda`](https://github.com/NCAR/wrf_hydro_docker/blob/main/dev/conda/Dockerfile) and  [`wrfhydro/dev:modeltesting`](https://github.com/NCAR/wrf_hydro_docker/blob/main/dev/modeltesting/Dockerfile) contain the full software stack required to run testing.


## Derecho
To activate a common python virtual envionment for model testing on Derecho:
```
$ deactivate
$ source /glade/p/cisl/nwc/model_testing_env/wrf_hydro_nwm_test/bin/activate
```
Because Whole new levels of testing complexity open up on Derecho, there is a
special script to handle this with minimal pain:
`test/local/derecho/model_test.sh`. This script provides flexibility to
switch compilers, MPI distributions, and domains. With MPI distributions,
different model execution commands may be required. Furthermore, output
comparison on large domains is better handled by `xrcmp` in `wrfhydropy`.


# The Croton domain
A lovely watershed with some very lovely lakes, I am sure as I hope to visit
it some day. As a test domain, it has served us marvelously. To pull the domain
from the WRF-Hydro release:
```
$ wget https://github.com/NCAR/wrf_hydro_nwm_public/releases/download/v5.4.0/croton_NY_training_example_v5.4.tar.gz
$ tar zxf croton_NY_training_example_v5.4.tar.gz
$ mv example_case croton_NY  ## we thought the generic name would be useful.
```
