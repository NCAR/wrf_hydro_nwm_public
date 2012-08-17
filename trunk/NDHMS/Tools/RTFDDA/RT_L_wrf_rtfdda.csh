#!/bin/csh -f

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- WRF FDDA and FCST start -----------------------------"
echo  "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
#     " ------------------Yubao Liu 2005.2 - 2007.2---------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM WRF
setenv RM "rm -rf"

set DOMS = ( 1 2 3 4 )
if($?NUM_DOMS) then
 if ($NUM_DOMS == 5) then
  set DOMS = ( 1 2 3 4 5 )
 else if ($NUM_DOMS == 4) then
  set DOMS = ( 1 2 3 4)
 else if ($NUM_DOMS == 3) then 
  set DOMS = ( 1 2 3)
 else if ($NUM_DOMS == 2) then 
  set DOMS = ( 1 2 )
 else if ($NUM_DOMS == 1) then 
  set DOMS = ( 1 )
 endif
endif

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif

source ${CFILE}user.mm5sys.${MM5HOST};    


if ( ${#argv} != 9) then
	echo "usage: wrf.csh this_cycle fcst_id fcst_h start_date cpun d4start out_int NODE ETA"
	echo "where start_date and this_cycle is ccyymmddhh"
	echo "fcst_id: = 1  Cold start; = 2 Restart for last cycle; = 3 restart in this cycle"
	echo "FCST_H are fcst length in hour"
	echo "cpun is the number of parallel processors"
	echo "d4start is the time to begin domain 4 (minutes)"
	echo "out_int is the time interval of MMOUT "
	echo "NODE: 0 -mpp; 1 - 64 starting node for mpp jobs"
	echo "NAMGFS: ETA/NAM or AVN/GFS IC/BC"
	exit ( 1 )
endif

set this_cycle = $1
set fcst_id    = $2
set FCST_H     = $3
set start_date = $4
set cpun       = $5
set d4start    = $6
set out_int    = $7
set NODE       = $8
set NAMGFS     = $9

#	Get to the right directory

set CYCDIR = $RUNDIR/$this_cycle

if ( ($fcst_id == 1) || ($fcst_id == 2)) then   # Final FDDA cycle
set tmp_work = WRF_F
else
set tmp_work = WRF_P
endif

# Will work on main disk
set up_dir = $CYCDIR
set dir_work = $up_dir/$tmp_work
rm -rf $dir_work

# Unless a workdir on compute nodes has been created
if (-d $GEAPSTMP/1) then # $GEAPSTMP/1: mpirun does not like local disk
     # Will work on local disk
     set up_dir = $GEAPSTMP
     set dir_work = $up_dir/$tmp_work
     ln -sf $dir_work $CYCDIR/$tmp_work
endif

if (! -e $dir_work) $MustHaveDir $dir_work

# Go to Work dir
echo
echo "WORKDIR is $dir_work"
cd  $dir_work


# ES: These are MM env-vars. 
# BATCH_SYSTEM can be set to PBS, LSF, INTER. If it not set, default to INTER:
# MM is set to MM. If not set, default to "notMM"
if (! $?BATCH_SYSTEM) setenv BATCH_SYSTEM "INTER"
if (! $?MM ) setenv MM "notMM"

setenv D1_LENGTH $FCST_H
if ( ! $?D2_LENGTH) setenv D2_LENGTH $FCST_H
if ( ! $?D3_LENGTH) setenv D3_LENGTH $FCST_H
if ( ! $?D4_LENGTH) setenv D4_LENGTH 13
if ( ! $?D5_LENGTH) setenv D5_LENGTH 13
set DOM_LENGTH = ( $D1_LENGTH $D2_LENGTH $D3_LENGTH $D4_LENGTH $D5_LENGTH )

if ( ! $?D1_OUT_INT) setenv D1_OUT_INT $OUT_INT
if ( ! $?D2_OUT_INT) setenv D2_OUT_INT $OUT_INT
if ( ! $?D3_OUT_INT) setenv D3_OUT_INT $OUT_INT
if ( ! $?D4_OUT_INT) setenv D4_OUT_INT $OUT_INT
if ( ! $?D5_OUT_INT) setenv D5_OUT_INT $OUT_INT
set out_intv = ( $D1_OUT_INT $D2_OUT_INT $D3_OUT_INT $D4_OUT_INT $D5_OUT_INT)

if($fcst_id == 2) set FCST_H = $CYC_INT

set i=1
set end_date=$start_date
while ( $i <= 216 && $i <= $FCST_H) 
#echo "$end_date , 1" >! input
#${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
#set end_date = `cat output`
 set end_date = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $end_date 1`
 @ i ++
end

echo "start=$start_date end=$end_date FH=$FCST_H"

set y_start = `echo $start_date | cut -b 1-4`
set m_start = `echo $start_date | cut -b 5-6`
set d_start = `echo $start_date | cut -b 7-8`
set h_start = `echo $start_date | cut -b 9-10`

set BC_INT = 10800

set y_end = `echo  $end_date | cut -b 1-4`
set m_end = `echo  $end_date | cut -b 5-6`
set d_end = `echo  $end_date | cut -b 7-8`
set h_end = `echo  $end_date | cut -b 9-10`

# Original capability: D4 end-time only...
if ( $fcst_id == 3) then
# echo "$start_date , $D4_LENGTH" >! input
# ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
# set d4end_date = `cat output`
  set d4end_date = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $start_date $D4_LENGTH`
else
 set d4end_date = $end_date
endif
set d4y_end = `echo $d4end_date | cut -b 1-4`
set d4m_end = `echo $d4end_date | cut -b 5-6`
set d4d_end = `echo $d4end_date | cut -b 7-8`
set d4h_end = `echo $d4end_date | cut -b 9-10`

echo "Done with D4 settings  $D4_LENGTH"
# New capability: end-time specified for each domain

set dy_end = ( 0 0 0 0 0 0)
set dm_end = ( 0 0 0 0 0 0)
set dd_end = ( 0 0 0 0 0 0)
set dh_end = ( 0 0 0 0 0 0)

foreach i ($DOMS)
 if ( $fcst_id == 3) then
  set dom_end_date=$start_date
# echo "$dom_end_date , $DOM_LENGTH[$i]" >! input
# ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
# set dom_end_date = `cat output`
  set dom_end_date = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $dom_end_date $DOM_LENGTH[$i]`
 else
  set dom_end_date = $end_date
 endif
 set dy_end[$i] = `echo $dom_end_date | cut -b 1-4`
 set dm_end[$i] = `echo $dom_end_date | cut -b 5-6`
 set dd_end[$i] = `echo $dom_end_date | cut -b 7-8`
 set dh_end[$i] = `echo $dom_end_date | cut -b 9-10`
echo "Done with $i settings  $DOM_LENGTH[$i]"
end

set NODEMEM = ""
set ETKFMEM = "NO"
set ONEFILE = `pwd`
if(! $?ENSEMBLE ) set ENSEMBLE = "no"
echo "ENSEMBLE = ${ENSEMBLE}"
if(! $?ETKF ) set ETKF = "no"
echo "ETKF = ${ETKF}"
if(! $?MPPJOB ) set MPPJOB = "no"
echo "MPPJOB = ${MPPJOB}"
if(! $?CYC_INT) set CYC_INT = 3

set OUTTAG = $MM5HOST

if( $ENSEMBLE == "yes") then
  foreach i (15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50)
   @ ip = $i + 3
   @ iq = $i + 6
   @ is = $i + 4
   @ ie = $i + 8
   set ccy4 = `echo $ONEFILE |cut -c $i-$ip`
   set ccy3 = `echo $ONEFILE |cut -c $is-$iq`
   set ccy5 = `echo $ONEFILE |cut -c $is-$ie`
   if($ccy4 == "NAM_" || $ccy4 == "GFS_" || $ccy4 == "ECM_")   then
    set NAMGFS  = $ccy4
    set NODEMEM = $ccy5
    if($ccy3 == "MET") set ETKFMEM = "YES"
   endif
  end
  set OUTTAG = $NAMGFS$NODEMEM
endif

#	Bring wrf namelist and executables  over that we need

 if(-e $GSJOBDIR/namelists/wrf.nl.template.$NODE) then
  cp $GSJOBDIR/namelists/wrf.nl.template.$NODE wrf.nl
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/wrf.nl.template.$NODE"
 else if ( -e $GSJOBDIR/namelists/wrf.nl.template) then
  cp $GSJOBDIR/namelists/wrf.nl.template wrf.nl
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/wrf.nl.template"
 else
  echo "${SUBSYSTEM} -- Missing wrf.nl -> exiting"
  exit (4)
 endif

 if ($MPPJOB == "no") then
  if(-e $GSJOBDIR/executables/wrf.exe.$NODE) then
   cp $GSJOBDIR/executables/wrf.exe.$NODE wrf.exe
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/wrf.exe.$NODE"
  else if ( -e $GSJOBDIR/executables/wrf.exe) then
   cp $GSJOBDIR/executables/wrf.exe wrf.exe
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/wrf.exe"
  else if ( -e ${WRF_EXE}) then
   cp ${WRF_EXE} wrf.exe 
   echo "${SUBSYSTEM} -- Using ${WRF_EXE}"
  else
   echo "${SUBSYSTEM} -- Missing wrf.executable --  exiting"
   exit (4)
  endif
 else 
  if(-e $GSJOBDIR/executables/wrf.mpich.$NODE) then
   cp $GSJOBDIR/executables/wrf.mpich.$NODE wrf.mpich
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/wrf.mpich.$NODE"
  else if ( -e $GSJOBDIR/executables/wrf.mpich) then
   cp $GSJOBDIR/executables/wrf.mpich wrf.mpich
   echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/wrf.mpich"
  else if ( -e ${WRF_MPICH}) then
   cp ${WRF_MPICH} wrf.mpich 
   echo "${SUBSYSTEM} -- Using ${WRF_MPICH}"
  else
   echo "${SUBSYSTEM} -- Missing wrf.executable --  exiting"
   exit (4)
  endif
 endif


if ( $fcst_id == 1 ) then          # cold start for Final FDDA
 set ifrest = "FALSE"
 @ savefrq  = $FCST_H * 60
else if ( $fcst_id == 2 ) then      # Normal Final FDDA
 set ifrest = "TRUE"
 @ savefrq  = $FCST_H * 60
else if ( $fcst_id == 3 ) then      # Primary FDDA + FCST
 set ifrest = "TRUE"
 set savefrq  = 44600                # No savefile for restart is needed
endif

set metl  = 27
if($NAMGFS == ETA || $NAMGFS == NAM) set metl = 40

# build the wrf namelist

echo $y_start $m_start $d_start $h_start 
echo $y_end $m_end $d_end $h_end $NUM_DOMS
echo $FCST_H $savefrq $ifrest $out_int $d4y_end $d4m_end $d4d_end $d4h_end

set NDOM = $NUM_DOMS
ed wrf.nl << EOF > /dev/null
g/SYY/s//$y_start/g
g/SMM/s//$m_start/g
g/SDD/s//$d_start/g
g/SHH/s//$h_start/g
g/EYY/s//$y_end/g
g/EMM/s//$m_end/g
g/EDD/s//$d_end/g
g/EHH/s//$h_end/g
g/D4YY/s//$d4y_end/g
g/D4MM/s//$d4m_end/g
g/D4DD/s//$d4d_end/g
g/D4HH/s//$d4h_end/g
g/E1YY/s//$dy_end[1]/g
g/E1MM/s//$dm_end[1]/g
g/E1DD/s//$dd_end[1]/g
g/E1HH/s//$dh_end[1]/g
g/E2YY/s//$dy_end[2]/g
g/E2MM/s//$dm_end[2]/g
g/E2DD/s//$dd_end[2]/g
g/E2HH/s//$dh_end[2]/g
g/E3YY/s//$dy_end[3]/g
g/E3MM/s//$dm_end[3]/g
g/E3DD/s//$dd_end[3]/g
g/E3HH/s//$dh_end[3]/g
g/E4YY/s//$dy_end[4]/g
g/E4MM/s//$dm_end[4]/g
g/E4DD/s//$dd_end[4]/g
g/E4HH/s//$dh_end[4]/g
g/E5YY/s//$dy_end[5]/g
g/E5MM/s//$dm_end[5]/g
g/E5DD/s//$dd_end[5]/g
g/E5HH/s//$dh_end[5]/g
g/DOM/s//$NDOM/g
g/FCSTH/s//$FCST_H/g
g/SaveFrq/s//$savefrq/g
g/OUTINT/s//$out_int/g
g/OUT1INT/s//$out_intv[1]/g
g/OUT2INT/s//$out_intv[2]/g
g/OUT3INT/s//$out_intv[3]/g
g/OUT4INT/s//$out_intv[4]/g
g/OUT5INT/s//$out_intv[5]/g
g/IfRest/s//$ifrest/g
g/METL/s//$metl/g
w
q
EOF

cp -f wrf.nl $CYCDIR/.
mv -f wrf.nl namelist.input

##yw begin added by Wei Yu
#yw if($?HYBRID) then
if("$tmp_work" == "WRF_F") then
if(-e "$MM5HOME/cycle_code/HY3DVAR/scripts/hy3dvarDriver.pl") then
       echo "3DVAR analysis start ....."
       if(! $?DATADIR) set DATADIR = "$DATA_DIR"
       if(! $?RANGE) set RANGE = "$MM5HOST"
       if(! $?BATCH_SYS) set BATCH_SYS = "$BATCH_SYSTEM"
   echo "perl $MM5HOME/cycle_code/HY3DVAR/scripts/hy3dvarDriver.pl $this_cycle $CYC_INT  $RUNDIR $GSJOBDIR $RANGE $NUM_PROCS $DATADIR $BATCH_SYS $MM5HOME/cycle_code $#DOMS"
       perl $MM5HOME/cycle_code/HY3DVAR/scripts/hy3dvarDriver.pl $this_cycle $CYC_INT  $RUNDIR $GSJOBDIR $RANGE $NUM_PROCS $DATADIR $BATCH_SYS $MM5HOME/cycle_code $#DOMS $fcst_id
endif
endif
##yw end

#added for NDHMS
  echo "yyww start_date = $start_date  this_cycle= $this_cycle"
if("$tmp_work" == "WRF_F") then
  echo "yyww start_date = $start_date"
  csh $GSJOBDIR/NDHMS/rtfdda_ndhms_drv.csh $GSJOBDIR $start_date $RUNDIR/$start_date/WRF_F  
else
  csh $GSJOBDIR/NDHMS/rtfdda_ndhms_drv.csh $GSJOBDIR $this_cycle $RUNDIR/$this_cycle/WRF_F  
endif


#Now prepare data to run wrf

# link proper input and bc files for wrf runs 

foreach i ( $DOMS )
 if($cpun < 2) then
  ln -s $up_dir/${this_cycle}_wrfinput_d0${i}_cold wrfinput_d0$i
  if (-e $up_dir/${this_cycle}_wrffdda_d0${i}) then
   ln -s $up_dir/${this_cycle}_wrffdda_d0${i} wrffdda_d0${i}
  endif
 else
  ln -s $CYCDIR/${this_cycle}_wrfinput_d0${i}_cold wrfinput_d0$i
  if (-e $CYCDIR/${this_cycle}_wrffdda_d0${i}) then
   ln -s $CYCDIR/${this_cycle}_wrffdda_d0${i} wrffdda_d0${i}
  endif
 endif 
end
  ln -s $CYCDIR/${this_cycle}_wrfbdy_d01 wrfbdy_d01

# OBS nudging files 

foreach i ( $DOMS )
  set ii = 0
  foreach f  (`/bin/ls  $CYCDIR/*_s`)
  @ ii ++
  if($ii < 10) then
   #ln -f -s $f  MM5OBS_DOMAIN${i}0${ii}
   ln -f -s $f  OBS_DOMAIN${i}0${ii}
  else
   #ln -f -s $f  MM5OBS_DOMAIN${i}${ii}
   ln -f -s $f  OBS_DOMAIN${i}${ii}
  endif
  end
end

# Restart file

foreach i ( $DOMS )
 if ($fcst_id == 2) then 
  ln -s $RUNDIR/restart_files/${this_cycle}.RESTART_DOMAIN$i wrfrst_d0${i}_${y_start}-$m_start-${d_start}_${h_start}:00:00
 else if ($fcst_id == 3) then 
  ln -s $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN$i wrfrst_d0${i}_${y_start}-$m_start-${d_start}_${h_start}:00:00
 endif
end

#ETKF Grid-nudging files

if($ETKFMEM == "YES") then
 foreach i ( $DOMS )
  if ( ( -l ETKFINPUT_DOMAIN$i ) || ( -e ETKFINPUT_DOMAIN$i ) ) ${RM} ETKFINPUT_DOMAIN$i
  if ( -l $RUNDIR/../etkf_out/${this_cycle}_ETKFINPUT_DOMAIN${i}.$NODE) then
    ln -s -f $RUNDIR/../etkf_out/${this_cycle}_ETKFINPUT_DOMAIN${i}.$NODE ETKFINPUT_DOMAIN$i
  endif
 end
endif

#	Run the wrf program

if( -e $GSJOBDIR/wrfrun.$NODE ) then
 echo "Using tables in $GSJOBDIR/wrfrun.$NODE"
 cp $GSJOBDIR/wrfrun.$NODE/* .
else if( -e $GSJOBDIR/wrfrun ) then
 echo "Using tables in $GSJOBDIR/wrfrun"
 cp $GSJOBDIR/wrfrun/* .
else
 echo "Using tables in ${CONSTANT_FILES}/wrfrun"
 cp ${CONSTANT_FILES}/wrfrun/* .
endif

if ( ! -e wrf.exe && ! -e wrf.mpich) then
   echo "${SUBSYSTEM} -- No WRF executable - wrf.exe or wrf.mpich exit..."
   exit
endif

if( -e wrf.exe ) then   
  #(ssh node$NODE "setenv OMP_NUM_THREADS $cpun;setenv MPSTKZ 64M;limit stacksize unlimited;cd $dir_work; ./wrf.exe " ) >&! wrf_print.out
  if ($BATCH_SYSTEM == "INTER") then 
    (ssh node$NODE "setenv OMP_NUM_THREADS $cpun; limit stacksize unlimited;cd $dir_work; ./wrf.exe " ) >&! wrf_print.out
  else if ($BATCH_SYSTEM == "PBS" || $BATCH_SYSTEM == "LSF") then
    setenv OMP_NUM_THREADS $cpun
    limit stacksize unlimited
    cd $dir_work
    ./wrf.exe  >&! wrf_print.out
  endif
else 
  rm rsl* show*;
  if ($BATCH_SYSTEM == "INTER") then
    if ($NODE == 0) then
      echo \
     "$MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $GMPICONF wrf.mpich >&! wrf_print.out"
      $MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $GMPICONF wrf.mpich >&! wrf_print.out
    else
      #mpirun -np $cpun wrf.mpich >&! wrf_print.out 
      if(-e $GSJOBDIR/machinefile) then
        cp $GSJOBDIR/machinefile $RUNDIR/hosts
      endif
      echo \
     "$MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $RUNDIR/hosts wrf.mpich >&! wrf_print.out"
      $MPICMD_BIN_DIR/mpirun -np $cpun -machinefile $RUNDIR/hosts wrf.mpich >&! wrf_print.out
      #/opt/mpich-gm.orig/bin/mpirun -np $cpun -machinefile $RUNDIR/hosts wrf.mpich >&! wrf_print.out
      #/opt/mpich/bin/mpirun -np $cpun -machinefile $GMPICONF wrf.mpich >&! wrf_print.out
    endif
  else if ($BATCH_SYSTEM == "PBS") then
    # run it using mpiexec-0.80:
    # per mpiexec-0.80's man page:
    #   Run the executable a.out as a parallel mpi code on each process allocated by pbs
    #   mpiexec a.out
    ### The PBS directive: we may not need it here.
    #PBS -l nodes=${NUM_NODES}:ppn=${PPN}
    echo \
   "$MPICMD_BIN_DIR/mpiexec wrf.mpich >&! wrf_print.out"
    $MPICMD_BIN_DIR/mpiexec wrf.mpich >&! wrf_print.out
  else if ($BATCH_SYSTEM == "LSF") then
     $GSJOBDIR/mpirun.lsf ./wrf.mpich  #>&! wrf_print.out
     exit (0)
  else
     echo
     echo "ERROR: Unknown value BATCH_SYSTEM = $BATCH_SYSTEM!"
     echo "       BATCH_SYSTEM must be PBS, LSF or INTER (interactive)"
     echo
  endif
endif

if (! $?MM || $MM != "MM") then
# In MM we are doing the moving and cleaning in post_process_clean.pl

# Move the important files around
# Save files

foreach i ( $DOMS )
 if ($fcst_id <= 2) then 
  echo \
 "cp wrfrst_d0${i}_${y_end}-${m_end}-${d_end}_${h_end}:00:00 $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN$i"
  cp wrfrst_d0${i}_${y_end}-${m_end}-${d_end}_${h_end}:00:00 $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN$i
  #mv wrfrst_d0${i}*  $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN${1}_F$FCST_HH
  #ln -s $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN${i}_F$FCST_HH wrfrst_d0${i}_${y_end}-${m_end}-${d_end}_${h_end}:00:00
 endif
end

# Output 
@ HOURLY_OUT =  60 / $OUT_INT
foreach d ( $DOMS )

  if( ($fcst_id == 1) || ($fcst_id == 2) ) then
    set cnt = 0
    set cnt2 = 0
    foreach f (`/bin/ls wrfout_d0{$d}*`)
     @ cnt ++
     if( $cnt == $HOURLY_OUT ) then
      # keep hourly D1 analysis only at GEAPSTMP directory (INTER only)
      if ($BATCH_SYSTEM == "INTER") then 
      if( -d $GEAPSTMP && $d == 1 ) cp $f $GEAPSTMP/ 
      endif
      set cnt = 0
      @ cnt2 ++
      if( $cnt2 == 3 && $ETKF == "yes" && $NODEMEM == "CTRL") then 
       cp $f $RUNDIR/../etkf_in/${this_cycle}_final$OUTTAG.WRFOUT_DOMAIN${d}
      endif
     endif
     echo \
    "mv  $f $CYCDIR/${f}.${OUTTAG}_F"   #save all final analysis for all domains
     mv  $f $CYCDIR/${f}.${OUTTAG}_F    #save all final analysis for all domains 
     ln -s $CYCDIR/${f}.${OUTTAG}_F $f  #preserve a soft link of the file

    end
  else if ($fcst_id == 3) then          
    set cnt = 0
    set cnt1 = 0
    set cnt2 = 0
    # keep hourly fcsts only
    foreach f (`/bin/ls wrfout_d0{$d}*`)
     @ cnt ++
     if( $cnt == $HOURLY_OUT ) then
      # keep hourly D1 FCST only at /d1/GEAPSTMP with short cut-off (INTER only)
      if ($BATCH_SYSTEM == "INTER") then 
      if( -d $GEAPSTMP && $d == 1 && $cnt1 < $CYC_INT + 2 ) cp $f $GEAPSTMP/ 
      endif
      set cnt = 0
      @ cnt1 ++
      @ cnt2 ++
     endif
     #if( $ETKF == "yes" &&  ($ETKFMEM == "YES" || $NODEMEM == "CRTL")) then
     if( $ETKF == "yes" ) then
     if( $cnt2 == 6 ) cp $f $RUNDIR/../etkf_in/${this_cycle}_06hfcst$OUTTAG.WRFOUT_DOMAIN${d}
     #if( $cnt2 == 12) cp $f $RUNDIR/../etkf_in/${this_cycle}_12hfcst$OUTTAG.WRFOUT_DOMAIN${d}
     endif
     echo \
    "mv $f $CYCDIR/${f}.${OUTTAG}_P+FCST" #all domains all time 
     mv $f $CYCDIR/${f}.${OUTTAG}_P+FCST  #all domains all time 

     ln -s $CYCDIR/${f}.${OUTTAG}_P+FCST $f #preserve a soft link of the file

    end
  endif
end

  ls -l >> wrf_print.out
  echo "  --------------- wrf namelist------------ " >> wrf_print.out
  cat namelist.input >> wrf_print.out
  echo "  --------------- OBS   ------------------ " >> wrf_print.out
  cat obs_* >> wrf_print.out
  echo "  --------------- PRINT OUT   ------------------ " >> wrf_print.out
  cat rsl.out.0000 >> wrf_print.out

# Move the print out from the workdir to the cycle dir
  cp -f wrf_print.out $CYCDIR/wrf_print_${tmp_work}.out

else
 set here = `pwd`
 echo
 echo "Content of run directory ${here}:"
 ls -alh
 echo

endif

# Clean up

if ($BATCH_SYSTEM != "INTER") then 

 cd $CYCDIR

 if($cpun > 1 && $CLEAN_GEAPSTMP > 0 && -e $GEAPSTMP) then
     echo
     echo "Removing  $GEAPSTMP"
     rm -rf $GEAPSTMP
 endif

 if($cpun > 1 && $CLEAN_GEAPSKEP > 0 && -e $GEAPSKEP) then
     echo
     echo "Removing  $GEAPSKEP"
     rm -rf $GEAPSKEP
 endif

endif

exit ( 0 ) 

