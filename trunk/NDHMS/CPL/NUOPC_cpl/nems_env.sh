# 09/13/2019 - COASTAL ACT Project
# This is a script to set the environment for nems appliction
# How to use: from linux command line type:
# source nems_env.sh


module load intel/18.0.5.274
module load szip/2.1
module load hdf5/1.10.4
module load impi/2018.0.4
module load netcdf/4.6.1

# Environment for ESMF v8.0.0 beta snapshot 48g
# /scratch1/NCEPDEV/nems/emc.nemspara/soft/esmf/8.0.0bs48g-intel18.0.5.274-impi2018.0.4-netcdf4.6.1
module use /home/emc.nemspara/SOFT-hera/modulefiles
module load esmf/8.0.0bs48g


# Using conda for python3.7
module load contrib anaconda/latest

