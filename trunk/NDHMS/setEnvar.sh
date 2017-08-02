#!/bin/bash

### This will called by either compile_offline_NoahMP.csh
### or compile_offline_Noah.csh

### turn on WRF_HYDRO for NoahMP
export WRF_HYDRO=1

### turn on output information during running time.
export HYDRO_D=1

### turn on distributed parameters for NoahMP
#export SPATIAL_SOIL=1  

### turn on RAPID model
export WRF_HYDRO_RAPID=0

### using large netcdf file definition.
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

### running in REALTIME mode (with reduced output).
export HYDRO_REALTIME=0

### turn on wcoss flag 
export NCEP_WCOSS=0

### turn nudging off
export WRF_HYDRO_NUDGING=0
