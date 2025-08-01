# Prerequisites
Turn on Python coupling during CMake initialization.
Make sure Conda environment is loaded before modules.

## Load Python Environment
```
$ conda create --prefix /path/to/wrf-h-ml
$ conda activate wrf-h-ml
$ conda install -c conda-forge scikit-learn xgboost pandas joblib
```

## Load Modules
```
$ conda activate wrf-h-ml
$ module purge
$ module load ncarenv gcc cray-mpich netcdf cmake
$ module load conda
```

# Build
```
$ mkdir build
$ cd build
$ cmake ../ -DPYTHON_CPL=1
$ make -j 4
```

# Run
From `build` or `bin` directory
```
$ ./bin/call_python
```

# Flowchart
```mermaid
flowchart TD
  driver([call_python_test_driver.f90]) -- "call Fortran routine" --> bind_c([call_python_bind_c.f90])
  bind_c -- "call C routine" --> c([call_python.c])
  c -- "call Python routine" --> python([get_weights.py <br />put weights in array])
  python -- "return array" --> c
  c -- "return array" --> bind_c
  bind_c -- "return array"--> driver
```
