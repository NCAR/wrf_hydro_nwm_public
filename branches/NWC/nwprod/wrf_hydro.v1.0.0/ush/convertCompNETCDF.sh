################################################################
# This script will compare results of the NCAR and WCOSS.
# 
# syntax: ./convertCompNETCDF.sh
#
# Note: Need to correct NCARdir path and WCOSSdir to your area 
#      
# ChamP - 07/15/2015
################################################################

PDY=`/bin/date +%Y%m%d`

echo $PDY

NCARdir=/ohd/noscrub/$LOGNAME/com/CASES/runatNCARbenchmark
WCOSSdir=/ohd/noscrub/$LOGNAME/com/wrf_hydro/prod/wrf_hydro.${PDY}


cd $WCOSSdir

rm -rf 2013*.txt

# compare text format files
for infile in GW_inflow.txt GW_outflow.txt GW_zlev.txt frxst_pts_out.txt
do
  diff "$infile" "$NCARdir/$infile.benchmark"
  if [ $? = 1 ]; then
    echo -e "$infile are DIFFER ==========\n"
  else
    echo -e "$infile\n"
  fi
done

# compare NETCDF format files
for file in `ls -1 2013*`
do
 ncdump $file | awk 'f;/data:/{f=1}' > $file.txt
 if [ -e "$NCARdir/$file.benchmark.txt" ]; then
    diff "$file.txt" "$NCARdir/$file.benchmark.txt"
    if [ $? = 1 ]; then
      echo -e "$file 'are DIFFER ==========\n"
    else
      echo -e "$file\n"
    fi
 else
    echo -e "WCOSS generated an extra output $file\n"
 fi
done


