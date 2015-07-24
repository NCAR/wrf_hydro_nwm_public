~~~~~~~~~~~~~~~~~~~~~~~~~~~
Purpose
~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Converts an XMRG precipitation, XMRG like parameter file to an Arc/Info 
    ASCII format file 

~~~~~~~~~~~~~~~~~~~~~~~~~~~
System Requirements
~~~~~~~~~~~~~~~~~~~~~~~~~~~
    1. Red Hat Linux 7.3 or later
    2. g++ 3.2 or later
    4. Boost 1.32 or later, the Boost library can be built without root
       privilege, see www.boost.org for details

~~~~~~~~~~~~~~~~~~~~~~~~~~~
How to Build
~~~~~~~~~~~~~~~~~~~~~~~~~~~
    1. Install the boost library if boost is not installed
    2. Set the BOOSTDIR and BOOSTINCLUDEDIR environmental either in the
       Makefile.am file or in the environmental variable
    3. Build the libutil.a
    4. "cd" (change directory) to the directory "xmrgtoasc"
    5. Open the "Makefile.am" file using a text editor
    6. Reset the path to the libutil.a file  accordingly
    7. Type "./bootstrap" then "./configure" on the command prompt 
    8. Type "make".

~~~~~~~~~~~~~~~~~~~~~~~~~~~
How To Run
~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xmrgtoasc will print the following help message on the screen when run
    without any options:

Convert XMRG file to Arc/Info ASCII Raster file:
  --help                    Print this message(optional)
  -i [ --input ] arg        input XMRG filename
  -o [ --output ] arg       output ASCII filename
  -p [ --proj ] arg (=HRAP) projection key word of the ASCII file, can be
                            "ster" or "HRAP", default = HRAP
  -n [ --nodata ] arg (=-1) NODATAVALUE of the XMRG file default = -1.0

~~~~~~~~~~~~~~~~~~~~~~~~~~~
Output
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the output file name is missing, the output Arc/Info ASCII file has the 
same file name as the input file with the ".asc" appended at the end

~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example
~~~~~~~~~~~~~~~~~~~~~~~~~~~
xmrgtoasc -i xmrg1226199700z.Z

The output is xmrg1226199700z.Z.asc

~~~~~~~~~~~~~~~~~~~~~~~~~~~
Bugs
~~~~~~~~~~~~~~~~~~~~~~~~~~~
     Good bug reports make us serve you better. Please send bug reports and
     comments to Zhengtao.Cui@noaa.gov.
