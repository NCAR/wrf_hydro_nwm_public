#!/bin/csh -f
### This will called by either compile_offline_NoahMP.csh
### or compile_offline_Noah.csh

### turn on WRF_HYDRO for NoahMP
if(! $?WRF_HYDRO) setenv WRF_HYDRO 1

### turn on output information during running time.
if(! $?HYDRO_D) setenv HYDRO_D 1

### turn on distributed parameters for NoahMP
# if(! $?SPATIAL_SOIL) setenv SPATIAL_SOIL 1  

### turn on RAPID model
# if(! $?WRF_HYDRO_RAPID) setenv WRF_HYDRO_RAPID 1

### using large netcdf file definition.
# if(! $?WRFIO_NCD_LARGE_FILE_SUPPORT) setenv WRFIO_NCD_LARGE_FILE_SUPPORT 1

### running in REALTIME mode (with reduced output).
# if(! $?HYDRO_REALTIME) setenv HYDRO_REALTIME 1

