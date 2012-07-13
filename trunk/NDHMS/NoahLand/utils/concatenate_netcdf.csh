#!/usr/bin/csh -f
### instruction by Wei Yu
### Usage: csh concatenate_netcdf.csh file1 file2 ... filen out.nc
### Note: 1. out.nc must be a new file for output
###       2. The list of the file has the common dimension name and size.
###
###

if($#argv < 2 ) exit
set OUT_FILE = "$argv[$#argv]"
if(-e $OUT_FILE) then
   echo "Error: $OUT_FILE exist."
   exit
endif
#echo "hello" >$OUT_FILE 
set NFILES = $#argv
set NUMBERS = 1000

echo "NFILES = $NFILES"

cat >! script.ncl <<EOF1
; This file is used to create the grid nudging file.
; time_level1 : tmp1.nc
; time_level2 : tmp2.nc
; fdda.nc     : the file need to be replaced.
  count = -1
begin
  in_files = systemfunc("ls $argv")
  system("/bin/rm -f $OUT_FILE")
  new_file     = addfile("$OUT_FILE" ,"c")
;  make time and UNLIMITED dimension      ; recommended  for most applications
;  filedimdef(new_file,"time",-1,True)

;  new_file->x = file1->x
;  new_file->y = file1->y
;  new_file->flodir = file1->flodir
;  new_file->lambert_conformal_conic = file1->lambert_conformal_conic
   namelist = new((/$NUMBERS/),string)

do nfile = 0, $NFILES - 2
   file1 = addfile(in_files(nfile),"r")
   vNames = getfilevarnames(file1)
   nNames = dimsizes(vNames)
;      print ("file = "+in_files(nfile)+" nNames="+nNames)
   do n = 0, nNames - 1
      v = vNames(n)
      flag = 99
      do i = 0,count
        if(v .eq. namelist(i))  then
	   flag = -99
	end if     
      end do
      if( flag .gt. 0 ) then
        print("in_File="+in_files(nfile)+" vNames="+v) 
        new_file->\$v\$ = file1->\$v\$
	count = count + 1
	namelist(count) = v
      end if
   end do

   delete(vNames)
   delete(nNames)
end do
   new_file@Conventions = file1@Conventions
   new_file@Source_Software = file1@Source_Software
end
EOF1

ncl script.ncl
rm script.ncl
