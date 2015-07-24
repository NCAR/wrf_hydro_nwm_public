#!/bin/csh -f
set srcDir = "$1"
set dstGeo = "$2"
ln -sf $dstGeo wrf_geo_em.nc
set wkDir = `pwd`
if(! -e regrid_data) mkdir regrid_data

cd $srcDir
foreach file (`ls -1 *.nc`)
   cd wkDir
   ln -sf $srcDir/$file xmrg_netcdf.nc

   if(! -e xmrg2WRF_weight_file.nc) then
     ncl < ESMF_genWeight_for_xmrg2WRF.ncl
   endif

   ncl  < xmrg2wrf_ESMFregrid.ncl

   set tmp = `echo $file |wc`
   set len = $tmp[3]
   @ len = $len - 4
   set nFile = `echo $file | cut -c1-$len`

   mv xmrg2wrf_out.nc regrid_data/${nFile}.nc
end

rm -f *.nc *.Log
