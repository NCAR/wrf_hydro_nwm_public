#!/bin/csh -f
if($#argv < 0) then
  echo "Usage: dir_contain_smrg_data"
  exit
endif
set srcDir = "$1"
set wkDir = `pwd`

if(! -e xmrgtoasc) then
  cd ../xmrgtoasc_ind
  make
  cp xmrgtoasc $wkDir
  cd $wkDir
endif

if(! -e data_asc) mkdir data_asc

cd $srcDir
foreach file(xmrg*)
  cd $wkDir
  ln -sf $srcDir/$file .
  ./xmrgtoasc  $file
  mv *.asc data_asc
  rm -f $file
end
