# to set env for esmf/modules
# source this file: esmf-impi-env.sh
#

# Path to libraries and includes and bins
module load intel/18.0.5.274 
module load szip/2.1  
module load hdf5/1.10.4 
module load impi/2018.0.4  
module load netcdf/4.6.1

# /apps/intel/compilers_and_libraries_2018.3.222/linux/bin/intel64
# module load intel/14.0.2

# /apps/szip/2.1/lib
# module load szip

# /apps/hdf5/1.8.14-intel-impi
# module load hdf5

# /apps/netcdf/4.4.0/intel/15.6.233
# module load netcdf/4.4.0

# /apps/intel/impi/5.1.2.150/intel64/bin
# module load impi/5.1.2.150

# Environment for ESMF v8.0.0 beta snapshot 40
module use /home/emc.nemspara/SOFT-hera/modulefiles
# module load intel/15.1.133 impi/5.1.1.109 netcdf/4.3.0 
# module load impi/5.1.1.109
#module load yaml-cpp 
module load esmf/8.0.0bs48g

# latest version on Here
# module load yaml-cpp
# module --ignore-cache load "esmf/8.0.0bs42"
# module load esmf/7.1.0r

# Environment for ESMF 
# export ESMF_DIR=/scratch2/COASTAL/coastal/save/Beheen.M.Trimble/esmf_v8.0_beta/INSTALL/esmf8.0.beta_impi18.0.4
# export ESMF_BOPT='g'
# export ESMF_COMM=intelmpi      # mpich, mpich2,lam, openmpi or intelmpi
# export ESMF_COMPILER='intel'
# export ESMF_ABI='64'
# export ESMF_OS='Linux'         # uname -s

# export ESMFMKFILE=$ESMF_DIR/lib/esmf.mk

# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/scratch2/COASTAL/coastal/save/Beheen.M.Trimble/yaml-cpp-0.6.2/lib:$ESMF_DIR/lib

