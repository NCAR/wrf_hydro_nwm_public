#!/bin/csh -f
#argument 1: GMODJOB DIR 2: date of the forecast, 3: "restart file Dir"
if($#argv < 3) then
   echo "error of less input arguments."
   exit
endif
set gmodjobdir = "$1"
set date = "$2"
set rstDir = "$3"

#step 1 copy all of rt information to exe Dir
cp -r $gmodjobdir/NDHMS/* .

#step 2. change the namelist
   set yy = `echo $date |cut -c1-4`
   set mm = `echo $date |cut -c5-6`
   set dd = `echo $date |cut -c7-8`
   set hh = `echo $date |cut -c9-10`
#   set mm = `echo $date |cut -c11-12`

sed -e 's/NDHMS_YY/'$yy'/g' \
    -e 's/NDHMS_MM/'$mm'/g' \
    -e 's/NDHMS_DD/'$dd'/g' \
    -e 's/NDHMS_HH/'$hh'/g'  noah_wrfcode.namelist > noah_wrfcode.namelist.tmp
mv noah_wrfcode.namelist.tmp noah_wrfcode.namelist
    
#step 3. link the restart file
ls -1 $rstDir/RESTART.${yy}-${mm}-${dd}_${hh}:00_DOMAIN*
if($status == 0) then
    ln -sf $rstDir/RESTART.${yy}-${mm}-${dd}_${hh}:??_DOMAIN* .
    sed -e 's/NDHMS_RST/''/g' noah_wrfcode.namelist > noah_wrfcode.namelist.tmp
else
    sed -e 's/NDHMS_RST/'!'/g' noah_wrfcode.namelist > noah_wrfcode.namelist.tmp
endif
    mv noah_wrfcode.namelist.tmp noah_wrfcode.namelist
