# Prerequisites
```
$ conda create --prefix /path/to/wrf-h-ml
$ conda activate wrf-h-ml
$ conda install -c conda-forge scikit-learn xgboost pandas joblib
```

# Build
Turn on Python coupling during CMake initialization.
Make sure Conda environment is loaded before modules.

```
$ conda activate wrf-h-ml
$ module purge
$ module load ncarenv gcc cray-mpich netcdf cmake
```

# Run
From `build` or `bin` directory
```
$ ./bin/call_python
```
