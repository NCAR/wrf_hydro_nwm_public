#!/bin/csh -f
if($#argv <1) then
   echo "Usage: mxrg_asc2nc.csh dir_contains_xmrg_ascii_files"
   exit
endif
set srcDir = "$1"

if(! -e asc2nc.exe) then
   pgf90 -o asc2nc.exe read_xmrg_f.F -I $HOME/netcdf/include \
         -L $HOME/netcdf/lib -lnetcdf -lnetcdff
endif

if(! -e data_xmrg_nc) mkdir data_xmrg_nc
foreach file (`ls -1 $srcDir/*.asc`)
  ln -sf $file tmp_in
  ./asc2nc.exe

  set tmp = `echo $file |wc`
  set len = $tmp[3]
  @ len = $len - 5
  set nFile = `echo $file | cut -c1-$len`

  mv out.nc data_xmrg_nc/${nFile}.nc
end
  rm -f tmp_in
