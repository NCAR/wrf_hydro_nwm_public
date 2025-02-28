# WRF-Hydro® v5.4.0 Release Notes

Please note that starting with WRF-Hydro v5.4, CMake is the preferred
build system. The legacy configure/compile scripts are still present,
but should be considered deprecated and will not be supported in
future releases.

WRF-Hydro model documentation has been migrated from individual Word and
PDF files to an online documentation system that will be hosted alongside
the model code in the GitHub repository. Please see the docs/ directory for
more details and view the documentation on the web at:

https://wrf-hydro.readthedocs.io/en/latest

For specific updates, reference the PRs listed in the following sections:

## Model Improvements
  - PR#756: Add initial support for gage-assisted diversions in channel
    routing, which requires a new optional Diversion netCDF parameter file.
    This also adds a C compiler dependency
  - PR#725: `lake_option` added to `&hydro_namelist` to override lake physics
    options (or turn off lakes completely). Reservoir options have been moved
    to a new, separate `&reservoir_nlist` namelist
  - PR#743: liquid water fraction (or snow) added as optional forcing input variables
    Forcing variables names can now be supplied as namelist inputs
  - PR#782: documentation converted to readthedocs, also PR#786, PR#785,
    PR#774, PR#789, PR#795, PR#796, PR#792, PR#799, PR#798, PR#791, PR#799,
    PR#798, PR#797, PR#805, PR#804, PR#809, PR#810

## Bugfixes:
  - PR#803, PR#808: `lake_option` bugfixes
  - PR#813: Finding NetCDF fix for new Derecho environment
  - PR#811: Adding routing diversion Makefiles
  - PR#785: Support Fedora MPI environmental variables for NetCDF
  - PR#752: LSM accumulations not reset if `RSTRT_SWC` equals `no reset`
  - PR#729: Crocus glacier arrays changed to `optional` from `allocatable`
  - Bugfixes: CMake nudging parallel build

## General cleanup and misc.
  - PR#802: increment version numbers
  - PR#794: input namelists and parametere tables
  - PR#790: PR template updates
  - PR#764: adding deprecation warning to configure build, CMake preferred
  - PR#739: general CMake improvements, tabs to spaces
  - PR#720: header info from files removed
  - PR#724: redundant return statements removed
  - PR#723: MPI case style fixed
  - PR#733: .F -> .F90 file renaming
  - PR#717: Added citation file