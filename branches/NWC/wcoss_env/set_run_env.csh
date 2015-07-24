#!/bin/sh

#
#Set evnironmental variables for wrf-hydro
#

# Define unit numbers for all input files
 export FORT11="namelist.hrldas"
 export FORT12="hydro.namelist"
 export FORT13="HYDRO.TBL"
 export FORT15="MPTABLE.TBL"
 export FORT16="LAKEPARM.TBL"
 # FORT17 filename 'route_link_f' is read from FORT12, hydro.namelist
 # FORT18 filename 'gwbasmskfil' is read from FORT12, hydro.namelist
 # FORT19 restart filename is read from FORT12, hydro.namelist
 export FORT20="VEGPARM.TBL"
 export FORT21="SOILPARM.TBL"
 export FORT22="GENPARM.TBL"
 export FORT23="CHANPARM.TBL"
 export FORT24="GWBUCKPARM.TBL"
 export FORT25="URBPARM.TBL"

 export FORT51="GW_inflow.txt"
 export FORT52="GW_outflow.txt"
 export FORT53="GW_zlev.txt"
 export FORT54="qstrmvolrt_accum.txt"
 export FORT55="frxst_pts_out.txt"
 export FORT78="diag_hydro."
#
