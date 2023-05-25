# Quick Build
Here are quick build instructions.
For more in-depth instructions see the [project website](https://ral.ucar.edu/projects/wrf_hydro/technical-description-user-guide).

## Requirements

| Compiler   | Version |
|------------|---------|
| GNU        | 7.4+    |
| Intel      | 2018+   |
| NVidia/PGI | 20.4+   |
| Cray       | 8+      |

| Libraries/Software | Version |
|--------------------|---------|
| MPI                | 3.x+    |
| Fortran NetCDF     | 4.4+    |
| CMake              | 3.12+   |


## CMake Build

```
$ mkdir build
$ cd build
$ cmake ..
$ make -j 4
```

The executables, required namelists and tables are now in the `Run` directory
under `build`.
Testcases with domain setups can be found [here](https://ral.ucar.edu/projects/wrf_hydro/testcases).
To build with additional functionality, enter `cmake .. -DFOO=1` where the
available options are described in the following table.


| CMake WRF-Hydro Specific Options   | Functionality                                                                 |
|------------------------------------|-------------------------------------------------------------------------------|
| `-DWRF_HYDRO=1`                    | Default: turn on WRF-Hydro                                                    |
| `-DHYDRO_D=1`                      | Enhanced diagnostic output for debugging                                      |
| `-DSPATIAL_SOIL=1`                 | Spatially distributed parameters for NoahMP                                   |
| `-DWRF_HYDRO_NUDGING=1`            | Enable the streamflow nudging routines for Muskingum-Cunge Routing            |
| `-DNWM_META=1`                     | Output NWM Metadata                                                           |
| `-DOUTPUT_CHAN_CONN=1`             | For gridded channel routing, write the channel connectivity to a netcdf file  |
| `-DPRECIP_DOUBLE=1`                | Double precipitation from hydro forcing                                       |
| `-DNCEP_WCOSS=1`                   | Do not use unless working on the WCOSS machines                               |


| Unsupported Functionality          |                                                                               |
|------------------------------------|-------------------------------------------------------------------------------|
| `-DWRF_HYDRO_NUOPC=1`              | Coupling with NUOPC, this option is not currently supported                   |
| `-DWRF_HYDRO_RAPID=1`              | Coupling with the RAPID routing model, this option is not currently supported |
