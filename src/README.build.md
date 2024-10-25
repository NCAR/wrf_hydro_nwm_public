# Standalone WRF-Hydro Build Instructions
Details regarding the model as well as documentation and user guides can be
found on the project website:
https://ral.ucar.edu/projects/wrf_hydro

## Requirements
* A supported Fortran compiler:
  - GNU (gfortran)
  - Cray (ftn)
  - Intel (ifort)
  - NVidia (nvfortran)
* MPI library (MPICH, Open MPI, etc.)
* NetCDF C & Fortran libraries v4.0+

Please note that these libraries need to be compiled with the same set of
compilers that will be used to compile WRF-Hydro

## Building WRF-Hydro

1. Obtain the source code

The source code for the latest WRF-Hydro release can be obtained here:
https://github.com/NCAR/wrf_hydro_nwm_public/releases/latest

Download and unpack the source code and navigate to the directory where you
will compile the code:

```
$ wget https://github.com/NCAR/wrf_hydro_nwm_public/archive/refs/tags/v5.3.0.tar.gz
$ tar zxf v5.3.0.tar.gz
$ cd wrf_hydro_nwm_public-5.3.0/src
```

2. Compile using CMake

Create build directory, configure with CMake, and then compile.
See the [CMake Build link](https://github.com/NCAR/wrf_hydro_nwm_public/blob/main/docs/BUILD.md#cmake-build)
for a table of WRF-Hydro specific configuration options.
The user can enable debug mode, nudging, etc.

```
$ mkdir build
$ cd build
$ cmake ..
$ make -j 4
```

This should result in the creation of a 'Run' directory populated with the
appropriate template parameter tables and namelists for the land surface model
selected as well as a model executable that is then symlinked to wrf_hydro.exe.

Note that, as mentioned above, passing the environment variable file as an
argument to the compile script is optional. However, if this is not passed the
desired environment variables must be set prior to running the compile script.
