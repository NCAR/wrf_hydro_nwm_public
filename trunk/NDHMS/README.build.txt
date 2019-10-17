This README provides a brief description of how to build WRF-Hydro as a standalone
modeling system.

More details regarding the model as well as documentation and user guides can be found on 
the project website:
https://ral.ucar.edu/projects/wrf_hydro

Requirements:
   * A supported Fortran compiler:
     - PGI (pgfortran) 
     - Intel (ifort) 
     - GNU (gfortran)
   * MPI libraries (MPICH or Open MPI)
   * netCDF C & Fortran libraries version 4 or greater

   Please note that these libraries need to be compiled with the same set of compilers 
   that will be used to compile WRF-Hydro

Steps for building WRF-Hydro:

1. Obtain the source code

   The source code for the latest WRF-Hydro release can be obtained here:
   https://github.com/NCAR/wrf_hydro_nwm_public/releases/latest

   Download and unpack the source code and navigate to the directory where you will
   compile the code:

   cd  wrf_hydro*/trunk/NDHMS

2. Set the required environment variables

   Note: If the required netCDF libraries are not already available on your system
   (these are commonly available as a module on high performance computing environments),
   you will need to install them now.

   First ensure the environment variables describing where your netCDF libraries live
   are set appropriately so that the compiler can find them.  For a bash shell the
   following should work with <PATH> replaced by the installation prefix for your
   netCDF libraries.  
  
   export NETCDF_INC"<PATH>/include"
   export NETCDF_LIB="<PATH>/lib"
      
   Then copy over the setEnvar.sh script from the 'template' directory and edit
   the WRF-Hydro environment variables / compile time options in the file as needed.
   This file can then be passed as an argument (see step #4) to the compile scripts 
   below which will source the environment variables for you. Alternatively these 
   environment variables can be set by the user.

3. Configure

   To configure the model run the following and select the appropriate option for
   your system / compiler:

   ./configure

4. Compile

   To compile the model run the compile script with the name corresponding to the
   land surface model (i.e. Noah or NoahMP) you would like to be utilized within
   WRF-Hydro and pass the file containing the WRF-Hydro environment variables /
   compile time options as an argument:

   ./compile_offline_Noah.sh setEnvar.sh
   OR
   ./compile_offline_NoahMP.sh setEnvar.sh

   This should result in the creation of a 'Run' directory populated with the
   appropriate template parameter tables and namelists for the land surface model
   selected as well as a model executable that is then symlinked to wrf_hydro.exe.

   Note that, as mentioned above, passing the environment variable file as an argument
   to the compile script is optional. However, if this is not passed the desired 
   environment variables must be set prior to running the compile script.

