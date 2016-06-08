#!/bin/csh -f
### This will called by either compile_offline_NoahMP.csh
### or compile_offline_Noah.csh
## THIS SETS THE DEFAULT VALUES WHEN THE ENV VARS ARE NOT ALREADY SET

### turn on WRF_HYDRO for NoahMP
setenv WRF_HYDRO 1

### turn on output information during running time.
setenv HYDRO_D 1

### turn on distributed parameters for NoahMP
setenv SPATIAL_SOIL 1  

### turn on RAPID model
# if(! $?WRF_HYDRO_RAPID)  setenv WRF_HYDRO_RAPID 0

### using large netcdf file definition.
setenv WRFIO_NCD_LARGE_FILE_SUPPORT 1

### running in REALTIME mode (with reduced output).
 setenv HYDRO_REALTIME 1

### use streamflow nudging?
# setenv WRF_HYDRO_NUDGING 0
