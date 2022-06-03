This file describes how to build WRF-Hydro both as a standalone modeling system 
(Section #1) and as a library coupled within the Weather Research and Forecasting
(WRF) atmospheric modeling system (Section #2).

Section #1: Building the standalone WRF-Hydro system

The following steps describe the process of building WRF-Hydro as a standalone
modeling system.

1. Obtain the source code for WRF-Hydro

   The source code for the latest WRF-Hydro release can be obtained here:
   https://github.com/NCAR/wrf_hydro/releases/latest

   Download and unpack the source code and navigate to the directory where you will
   compile the code:

   cd  wrf_hydro/trunk/NDHMS

2. Set the required environment variables

   First ensure the environment variables describing where your netCDF libraries live
   are set appropriately so that the compiler can find them.  For a bash shell the
   following should work with <PATH> replaced by the installation prefix for your
   netCDF libraries.  
  
   export NETCDF_INC"<PATH>/include"
   export NETCDF_LIB="<PATH>/lib"
      
   Then copy over the setEnvar.sh script from the 'template' directory and edit
   the WRF-Hydro environment variables / compile time options in the file as needed.
   This file can then be passed to the compile scripts below which will source the
   environment variable for you or alternatively these environment variables can be set
   by the user.

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
   appropriate parameter tables and namelist for the land surface model selected
   as well as the hydro namelist and a model executable that is then symlinked to 
   wrf_hydro.exe.

   Note that as mentioned above passing the environment variable file as an argument
   is optional. However, if this is not passed the desired environment variables 
   must be set prior to running the compile script.


Section #2: Building the coupled WRF | WRF-Hydro system

The following steps describe the process of building WRF-Hydro as a library that is
coupled into the Weather Research and Forecasting (WRF) atmospheric modeling system.
The calling of WRF-Hydro within the WRF modeling system is controlled by a macro 
definition (WRF_HYDRO) that is specified as an environment variable prior to the
compilation process.  When WRF-Hydro is not activated via this setting, only the
standard WRF model will be built.  

1. Obtain the source code for WRF and WRF-Hydro

   The source code for the latest WRF-Hydro release can be obtained here:
   https://github.com/NCAR/wrf_hydro/releases/latest

   The source code for WRF and WPS can be obtained here:
   http://www2.mmm.ucar.edu/wrf/users/downloads.html

2. Unpack the code and move the WRF-Hydro source code to the proper location within the
   WRF directory structure

   Unpack the code for WRF and WPS as you normally would to build the WRF model. Then 
   unpack the WRF-Hydro code and move the top directory into the WRFV3 directory. 

2. Set the required environment variables

   Edit the WRF-Hydro environment variables as needed in a copy of the 
   wrf_hydro/trunk/NDHMS/template/setEnvar.sh script and source this script to set the
   necessary variables:

   source setEnvar.sh

3. Build the coupled WRF | WRF-Hydro executable 

   Now build the WRF model as you normally would.  

Note that when running in coupled model you will need to copy the hydro.namelist and
required parameter files into the WRF run directory.  More details are included in the 
Technical Description and User's Guide that can be found on the project webpage: 
https://ral.ucar.edu/projects/wrf_hydro

