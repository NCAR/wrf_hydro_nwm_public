# Development

# WRF-Hydro v5.1.2

Version 5.1.2 of the Community WRF-Hydro® modeling system is a bug-fix release that corrects several small but critical errors in version 5.1.1:
* Water class value for output masking is now set to the appropriate water class code instead of being hard-coded to "16". (Addresses issue #455)
* WRF-Hydro can now be linked to versions of netCDF newer than v4.6 without crashing. (Addresses issue #382)

Beyond these fixes, no additional answer-changing modifications are present, nor are different configuration or input forcing files required from version 5.1.1.

# WRF-Hydro v5.1.1

Version 5.1.1 of the Community WRF-Hydro® source code is consistent with the NOAA National Water Model (NWM) v2.0 code in operations plus the following additions and fixes:
* Lakes in the NCAR reach configuration:
  *  When running the NCAR Reach configuration, Lakes will now work as expected (similarly to the NWM configuration).
* Compound channel:
  * In order to represent a simplification of the behavior of a flood wave when it exceeds the channel bank, a compound channel formulation was added to the NWM code. 
  * The compound channel formulation is currently only functional in the NWM configuration.  
* RSURF_EXP parameter exposed in MPTABLE.TBL:
  * The previously hard coded soil shape function exponent parameter in Noah-MP was added to the MPTABLE.TBL parameter table as RSURF_EXP and spatial soil parameter file as rsurfexp. 
  * The parameter RSURF_EXP controls the shape of the rsurf resistance curve as it relates to soil moisture: lower rsurfexp will result in higher resistance for a given soil moisture and hence less soil evaporation, higher rsurfexp will result in lower resistance and more soil evaporation.
  * *Backwards Incompatibility*: for backwards compatibility with older versions of WRF-Hydro, users specifying a spatial soil parameter file (in namelist.hrldas -> noahlsm_offline -> spatial_filename) will need to update this file. The file can be updated, for example, by the following NCO command where the existing file is named "soil_properties.nc": ```ncap2 -O -s "rsurfexp=slope*0.0+5.0" soil_properties.nc soil_properties.nc```
* BATS albedo parameters were exposed in MPTABLE.TBL
* The maximum snow water equivalent was changed to 5000mm
* Bug fix to snow liquid water movement
* Broader usage of the netCDF4/HDF5 format: 
  * NetCDF3 outputs were converted to NetCDF4 and the compile time option for large file support was removed. 
* Enhanced testing (additional model configurations and tests)
* Output: additional direct and diffuse snow albedo variables and minor updates to output file metadata
* Makefiles no longer generate intermediate preprocessed source files, which eases debugging (line numbers displayed will now match actual line numbers in the source code), and permits compilation on case-insensitive file systems (such as those used with macOS)
* Bugfix to reservoir physics:
  * The uniform lake surface area was set to be consistent with the vertical wall assumption.
  * Qlateral inputs were “zeroed out” for reach-based methods PR #161 
* Bugfix - soil moisture check:
  * The `smc smcmax` check threshold was reduced from 10-6 to 10-5 due to some conflicts still triggering at the lower threshold.  PR #111 
  * * Bugfix - runoff mapping:
Surface runoff that was mapped through groundwater (GW) buckets in non-UDMP configurations now behaves as pass-through only PR #306 

# WRF-Hydro v5.0.3

## Bug fixes
* Gridded routing lake inflows were not preserved in restarts, therefore solution to the level-pool scheme on intial timestep after model restart had slightly different initial conditions. This was fixed by preserving last timestep lake inflows in the restart files.
* The lake level-pool scheme had a number of internal bugs fixed, which should improve lake level and outflow behavior.
* Hard-coded values for minutes and seconds in output metadata were removed to allow for correct sub-hourly outputs.

# WRF-Hydro v5.0.2
* Minor updates to model testing

# WRF-Hydro v5.0.1
* Minor updates to model testing

# WRF-Hydro v5.0.0

## High-Level Highlights:
* New capability to aggregate and route flow with user-defined mapping over NHDPLUS catchments and reaches
* New capability to specify key hydrologic and vegetation parameters in 2 dimensions (and 3 in the case of soil properties)
* New Noah-MP surface resistance formulation that improves snowpack simulation
* Updates to Noah-MP infiltration scheme to better handle high intensity rainfall events in fast-draining soils
* Significant improvements to model output routines, including full CF compliance, new capabilities for applying internal scale/offset and compression to reduce file sizes, and built-in coordinate information to allow outputs to be read natively in GIS environments
* New capability for streamflow nudging data-assimilation for the Muskingum-Cunge method
* New capability for engineering and regression testing is now available for WRF-Hydro. More information can be found [here](tests/README.md).

## Details:
### Namelists:
The hydro.namelist and namelist.hrldas files used to control model run options have changed.  The existing v3.0 namelists should be replaced with the new files in the /template directory in the tarball.  The major changes are specified below.

#### Changes in hydro.namelist
* A new 2-dimensional file option for specifying hydrologic terrain routing parameters (HYDROTBL_F), which allows these parameter to be freely distributed across the domain as opposed to tied to a lookup table.
* The option to provide a Geospatial land metadata file (LAND_SPATIAL_META_FLNM) to write out CF compliant files.  This can be generated by the WRF-Hydro ArcGIS Pre-Processing Toolkit under “Build Spatial Metadata file”. Note: any spatial metadata file created before November, 2016 will need to be re-created due to an added attribute in the x/y variables.  If this file is not provided, the output will not be CF compliant but the model will still run.
* io_form_outputs option: file outputs can now provide flexibility in the format of output (compressed, scale factor/add offset, etc.). 
* The io_config_outputs option is used for switching between different predefined output variable sets
* New option to differentiate channel inflow components into surface and groundwater bucket model contributions in the model output (output_channelBucket_influx).
* New capability to specify distinct timesteps for channel (DTRT_CH) and terrain (DTRT_TER) routing modules.
* The UDMP_OPT and associated udmap_file option for allowing user-defined mapping such as the NHDPlus network used in the National Water Model (NWM).
* New options to support streamflow nudging data assimilation.

#### Output options: new features in the hydro.namelist
* frxst_pts_out: An option to turn on and off forecast points for gridded routing has been created.  When frxstpts = 1 AND the user has specified frxstpts in the Fulldom_hires file generation (using the WRF-Hydro GIS Pre-processing Tool), then a `frxstpts_out.txt` file will be generated.  
* CHANOBS_DOMAIN: An option to output frxstpts in netCDF format has been created.  Control this option by specifying the CHANOBS = 1 (will output) and CHANOBS = 0 (off).  The CHANOBS file will output streamflow at the frxstpts specified in the `Fulldom_hires.nc` file.
* output_gw: GW files can be output independently. 
* The RTOUT files now have geospatial metadata appended, making it much easier to work with these files in GIS.  

#### Changes in namelist.hrldas
* New option to specify key vegetation and surface properties in 2 dimensions and soil properties in 3 dimensions. This allows much more user flexibility in how these parameters are assigned and calibrated.
* Specification (dependency) of the geogrid file has been removed

#### Changes to parameter files
* The supplemental domain parameter files are new or have updates since the last release: `wrfinput_d0x.nc`, `soil_properties.nc`, and `hydro2dtbl.nc` 
* New `soil_properties.nc` file, which can be created using the provided R script `create_soilProperties.R`.  Note: the `soil_properties.nc` file is only active (and required) if the SPATIAL_SOIL compile option is selected.  If the compile option is set to 0, the model will read from `SOILPARM.TBL`. The creation of this file has a dependency on “R” and several of its libraries. 
* The model can now read a `hydro2dtbl.nc` file (HYDROTBL_F) instead of the `HYDRO.TBL` file, which enables easy manipulation of parameters.  The model will create one for you if it is not provided, or if you use the R script to generate the `soil_properties.nc` file, it will also create the `hydro2dtbl.nc` file. 
* Changes to the `wrfinput_d0x.nc` file to ensure consistency between the geogrid and the initial land surface model states. A new `wrfinput_d0x.nc` file is required due to some variable changes and can be generated using the `create_Wrfinput.R` script. Note: the v3.0 shell script for creating the `wrfinput_d0x.nc` file from the geogrid file will not work for v5; use the v5 script provided on the [WRF-Hydro website](https://ral.ucar.edu/projects/wrf_hydro/pre-processing-tools).  The creation of this file has a dependency on “R” and the “netCDF4” library. This script is currently only compatible with the Noah-MP land surface model.
* The water and urban land cover type flags are now read from geogrid file global attributes, allowing flexibility to use classification systems other than the previously supported USGS (e.g., MODIS).

#### Lakes / reservoirs
* For gridded routing (channel_option = 3) or Muskingum-Cunge routing with user-defined mapping active (channel_option = 2 and UDMP_OPT = 1), a LAKEPARM file can now be read in either in .TBL format or .nc format.  The WRF-Hydro GIS Pre-processing Toolkit will generate either or both. Note that variable names have changed and to convert to the new format, use the `convert_LAKEPARM_to_V5.0.sh` script available on the website.  Note: that lakes will not run at this time using reach-based routing (channel_option=2) unless paired with user-defined mapping (UDMP_OPT=1).  Namelist checks will prevent users from running with a LAKEPARM supplied and channel_option=2 selected.

#### Groundwater
* The model now reads in netCDF versions of the GWBUCKPARM file and `gw_basins_geogrid.txt` file. These are required for running groundwater (GWBASESWCRT options 1 and 2). The WRF-Hydro GIS Pre-processing Toolkit can generate these files using a tool in the Utilities called “Build GWBUCKPARM Table”: See details in the WRF-Hydro GIS Pre-processing Toolkit documentation for options for how to generate this file.  

### Bug fixes
* A bug in the gridded lake code was fixed to properly translate inflows and outflows.
* An error in the calculation of the trapezoidal celerity for Muskingum-Cunge was corrected.
* An error in soil moisture accounting in the subsurface flow module was fixed.
* Accumulated variables in a few locations in the code were converted to single timestep accounting to minimize machine truncation errors on large values.
* Fixed issue with energy calculations that was leading to above freezing temperatures for very shallow snowpacks.

