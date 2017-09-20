! Module for handling National Water Model streamflow, land surface,
! gridded routing, lake, and groundwater output.

! Logan Karsten
! National Center for Atmospheric Research
! Research Applications Laboratory
! karsten@ucar.edu
! 303-497-2693

module module_NWM_io

implicit none

! Module-wide variables
integer, private :: ftnNoahMP ! Private NetCDF file handle since output routine
                              ! called multiple times for one file.

contains

!==============================================================================
! Program Name: output_chrt_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for channel points for the National Water Model.
! History Log:
! 3/6/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_chrt_NWM(domainId)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
   use module_mpp_land
   use module_mpp_reachls,  only: ReachLS_write_io
#endif
   implicit none

   ! Pass in "did" value from hydro driving program. 
   integer, intent(in) :: domainId

   ! Derived types.
   type(chrtMeta) :: fileMeta

   ! Local variables
   integer :: nudgeFlag, mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(3) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: featureVarId, elevVarId, orderVarId ! Misc NetCDF variable id values
   integer :: latVarId, lonVarId ! Lat/lon NetCDF variable id values.
   integer :: varRange(2) ! Local storage of min/max valid range values.
   integer :: gSize ! Global size of channel point array. 
   integer :: indVarId,ftnRt ! values related to extraction of ascending order index values from the RouteLink file.
   integer :: iTmp, indTmp ! Misc integer values. 
   integer :: ierr, myId ! MPI return status, process ID
   integer :: ascFlag ! Flag for if ascendingIndex is present
   ! Establish local, allocatable arrays
   ! These are used to hold global output arrays, and global output arrays after
   ! sorting has taken place by ascending feature_id value. 
   real, allocatable, dimension(:) :: strFlowLocal,velocityLocal
   real, allocatable, dimension(:,:) :: g_qlink
   integer, allocatable, dimension(:) :: g_linkid,g_order
   real, allocatable, dimension(:) :: g_chlat,g_chlon,g_hlink,g_zelev
   real, allocatable, dimension(:) :: g_QLateral,g_velocity
   real, allocatable, dimension(:) :: g_nudge,g_qSfcLatRunoff
   real, allocatable, dimension(:) :: g_qBucket,g_qBtmVertRunoff,g_accBucket
   real*8, allocatable, dimension(:) :: g_accSfcLatRunoff
   real, allocatable, dimension(:,:) :: g_qlinkOut
   integer, allocatable, dimension(:) :: g_orderOut,g_linkidOut
   real, allocatable, dimension(:) :: g_chlatOut,g_chlonOut,g_hlinkOut,g_zelevOut
   real, allocatable, dimension(:) :: g_QLateralOut,g_velocityOut
   real, allocatable, dimension(:) :: g_nudgeOut,g_qSfcLatRunoffOut
   real, allocatable, dimension(:) :: g_qBucketOut,g_qBtmVertRunoffOut,g_accBucketOut
   real*8, allocatable, dimension(:) :: g_accSfcLatRunoffOut
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset have been applied.
   integer, allocatable, dimension(:) :: chIndArray ! Array of index values for
   !each channel point. feature_id will need to be sorted in ascending order once
   !data is collected into the global array. From there, the index values are
   !re-sorted, and used to re-sort output arrays. 

   ! Initialize the ascFlag to 1
   ascFlag = 1

   ! Establish macro variables to hlep guide this subroutine. 
#ifdef WRF_HYDRO_NUDGING
   nudgeFlag = 1
#else
   nudgeFlag = 0
#endif

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   if(nlst_rt(domainId)%CHRTOUT_DOMAIN .eq. 0) then
      ! No output requested here, return to parent calling program/subroutine.
      return
   endif

   ! To keep things simple, only allow CHRTOUT_DOMAIN option of 2 since this is NWM output.
   ! We don't write frxstpts, but that could easily be put in here in the future. 
   !if(nlst_rt(domainId)%CHRTOUT_DOMAIN .ne. 2) then
   !   call nwmCheck(diagFlag,1,'ERROR: CHRTOUT_DOMAIN value of 0 or 2 only allowed.')
   !endif

   ! Also, we are only allowing a split_output_count of 1.
   !if(nlst_rt(domainId)%split_output_count .ne. 1) then
   !   call nwmCheck(diagFlag,1,'ERROR: split_output_count of 1 only allowed.')
   !endif

   ! Since this is NWM output, we are assuming only channel_option 2 is acceptable.
   !if(nlst_rt(domainId)%channel_option .ne. 2) then
   !   call nwmCheck(diagFlag,1,'ERROR: Only channel_option of 2 allowed.')
   !endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initChrtDict(fileMeta,diagFlag,myId)
  
   ! Depending on the NWM forecast config, we will be outputting different
   ! varibles. DO NOT MODIFY THESE ARRAYS WITHOUT CONSULTING NCAR OR
   ! OWP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   if(nlst_rt(1)%iocFlag .eq. 0) then
      ! Diagnostics
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%iocFlag .eq. 1) then
      ! Analysis and Assimilation 
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%iocFlag .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%iocFlag .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%iocFlag .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%iocFlag .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [1,0,1,1,0,0,0,0,0,0]
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! First step is to collect and assemble all data that will be written to the 
   ! NetCDF file. If we are not using MPI, we bypass the collection step through
   ! MPI. 
   if(mppFlag .eq. 1) then
      gSize = rt_domain(domainId)%gnlinks
      if(rt_domain(domainId)%gnlinksl .gt. rt_domain(domainId)%gnlinks) then
         gSize = rt_domain(domainId)%gnlinksl
      endif

      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      if(myId .eq. 0) then
         ! Allocate memory for output.
         allocate(g_chlon(gsize))
         allocate(g_chlat(gsize))
         allocate(g_hlink(gsize))
         allocate(g_zelev(gsize))
         allocate(g_qlink(gsize,2))
         allocate(g_order(gsize))
         allocate(g_linkid(gsize))
         allocate(g_QLateral(gsize))
         allocate(g_velocity(gsize))
         allocate(g_nudge(gsize))
         allocate(g_qSfcLatRunoff(gsize))
         allocate(g_qBucket(gsize))
         allocate(g_qBtmVertRunoff(gsize))
         allocate(g_accSfcLatRunoff(gsize))
         allocate(g_accBucket(gsize))
         allocate(g_chlonOut(gsize))
         allocate(g_chlatOut(gsize))
         allocate(g_hlinkOut(gsize))
         allocate(g_zelevOut(gsize))
         allocate(g_qlinkOut(gsize,2))
         allocate(g_orderOut(gsize))
         allocate(g_QLateralOut(gsize))
         allocate(g_velocityOut(gsize))
         allocate(g_nudgeOut(gsize))
         allocate(g_qSfcLatRunoffOut(gsize))
         allocate(g_qBucketOut(gsize))
         allocate(g_qBtmVertRunoffOut(gsize))
         allocate(g_accSfcLatRunoffOut(gsize))
         allocate(g_accBucketOut(gsize))
         allocate(chIndArray(gsize))
         allocate(g_linkidOut(gsize))
      else
         allocate(g_chlon(1))
         allocate(g_chlat(1))
         allocate(g_hlink(1))
         allocate(g_zelev(1))
         allocate(g_qlink(1,2))
         allocate(g_order(1))
         allocate(g_linkid(1))
         allocate(g_QLateral(1))
         allocate(g_velocity(1))
         allocate(g_nudge(1))
         allocate(g_qSfcLatRunoff(1))
         allocate(g_qBucket(1))
         allocate(g_qBtmVertRunoff(1))
         allocate(g_accSfcLatRunoff(1))
         allocate(g_accBucket(1))
         allocate(g_chlonOut(1))
         allocate(g_chlatOut(1))
         allocate(g_hlinkOut(1))
         allocate(g_zelevOut(gsize))
         allocate(g_qlinkOut(1,2))
         allocate(g_orderOut(1))
         allocate(g_QLateralOut(1))
         allocate(g_velocityOut(1))
         allocate(g_nudgeOut(1))
         allocate(g_qSfcLatRunoffOut(1))
         allocate(g_qBucketOut(1))
         allocate(g_qBtmVertRunoffOut(1))
         allocate(g_accSfcLatRunoffOut(1))
         allocate(g_accBucketOut(1))
         allocate(chIndArray(1))
         allocate(g_linkidOut(1))
      endif

      ! Allocate local streamflow and velocity arrays. We need to do a check to
      ! for lake_type 2. However, we cannot set the values in the global array 
      ! to missing as this causes the model to crash.
      allocate(strFlowLocal(RT_DOMAIN(domainId)%NLINKS))
      allocate(velocityLocal(RT_DOMAIN(domainId)%NLINKS))
      strFlowLocal = RT_DOMAIN(domainId)%QLINK(:,1)
      velocityLocal = RT_DOMAIN(domainId)%velocity
 
      ! Sync everything up before the next step.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Loop through all the local links on this processor. For lake_type
      ! of 2, we need to manually set the streamflow and velocity values
      ! to the model NDV value.
      if (RT_DOMAIN(domainId)%NLAKES .gt. 0) then
         do iTmp=1,RT_DOMAIN(domainId)%NLINKS
            if (RT_DOMAIN(domainId)%TYPEL(iTmp) .eq. 2) then
               strFlowLocal(iTmp) = fileMeta%modelNdv
               velocityLocal(iTmp) = fileMeta%modelNdv
            endif
         end do
      endif

      ! Collect arrays from various processors through MPI, and 
      ! assemble into global arrays previously allocated.
      if(nlst_rt(domainId)%channel_option .ne. 3) then
         ! Reach-based routing collection
#ifdef MPP_LAND
         call ReachLS_write_io(strFlowLocal,g_qlink(:,1))
         call ReachLS_write_io(RT_DOMAIN(domainId)%QLINK(:,2),g_qlink(:,2))
         call ReachLS_write_io(RT_DOMAIN(domainId)%ORDER,g_order)
         call ReachLS_write_io(RT_DOMAIN(domainId)%linkid,g_linkid)
         call ReachLS_write_io(RT_DOMAIN(domainId)%CHLAT,g_chlat)
         call ReachLS_write_io(RT_DOMAIN(domainId)%CHLON,g_chlon)
         call ReachLS_write_io(RT_DOMAIN(domainId)%ZELEV,g_zelev)
         call ReachLS_write_io(RT_DOMAIN(domainId)%QLateral,g_QLateral)
         call ReachLS_write_io(velocityLocal,g_velocity)
         call ReachLS_write_io(RT_DOMAIN(domainId)%HLINK,g_hlink)
         ! Optional outputs
         if(nudgeFlag .eq. 1)then
#ifdef WRF_HYDRO_NUDGING
            fileMeta%outFlag(2) = 1 ! Set output flag to on. 
            call ReachLS_write_io(RT_DOMAIN(domainID)%nudge,g_nudge)
#endif
         endif
         if(nlst_rt(domainId)%output_channelBucket_influx .eq. 1 .or. &
            nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
            fileMeta%outFlag(6) = 1
            fileMeta%outFlag(7) = 1
            call ReachLS_write_io(RT_DOMAIN(domainId)%qSfcLatRunoff,g_qSfcLatRunoff)
            call ReachLS_write_io(RT_DOMAIN(domainId)%qBucket,g_qBucket)
         endif
         if(nlst_rt(domainId)%output_channelBucket_influx .eq. 2 .and. &
            nlst_rt(domainId)%channel_only                .eq. 0         ) then
            fileMeta%outFlag(8) = 1
            call ReachLS_write_io(RT_DOMAIN(domainId)%qin_gwsubbas,g_qBtmVertRunoff)
         endif
         if(nlst_rt(domainId)%output_channelBucket_influx .eq. 3) then
            !! JLM: unsure the following will work... but this is caveated in namelist.
            fileMeta%outFlag(9) = 1
            fileMeta%outFlag(10) = 1
            call ReachLS_write_io(RT_DOMAIN(domainId)%accSfcLatRunoff,g_accSfcLatRunoff)
            call ReachLS_write_io(RT_DOMAIN(domainId)%qBucket,g_accBucket)
         endif
#endif
      else
         ! Gridded routing collection
         call write_chanel_real(strFlowLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,1))
         call write_chanel_real(RT_DOMAIN(domainId)%QLINK(:,2),rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,2))
         call write_chanel_real(RT_DOMAIN(domainId)%CHLAT,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlat)
         call write_chanel_real(RT_DOMAIN(domainId)%CHLON,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlon)
         call write_chanel_real(RT_DOMAIN(domainId)%HLINK,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_hlink)
         call write_chanel_int(RT_DOMAIN(domainId)%ORDER,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_order)
         call write_chanel_int(RT_DOMAIN(domainId)%linkid,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_linkid)
         call write_chanel_real(RT_DOMAIN(domainId)%ZELEV,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_zelev)
         call write_chanel_real(RT_DOMAIN(domainId)%QLateral,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_QLateral)
         call write_chanel_real(velocityLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_velocity)
         ! PLUG TO CHECK FOR CHANNEL ONLY OUTPUTS
      endif

   else
      gSize = rt_domain(domainId)%nlinksl
      ! No MPI - We are running on a single processor
      allocate(g_chlon(gsize))
      allocate(g_chlat(gsize))
      allocate(g_hlink(gsize))
      allocate(g_zelev(gsize))
      allocate(g_qlink(gsize,2))
      allocate(g_order(gsize))
      allocate(g_linkid(gsize))
      allocate(g_QLateral(gsize))
      allocate(g_velocity(gsize))
      allocate(g_nudge(gsize))
      allocate(g_qSfcLatRunoff(gsize))
      allocate(g_qBucket(gsize))
      allocate(g_qBtmVertRunoff(gsize))
      allocate(g_accSfcLatRunoff(gsize))
      allocate(g_accBucket(gsize))
      allocate(g_chlonOut(gsize))
      allocate(g_chlatOut(gsize))
      allocate(g_hlinkOut(gsize))
      allocate(g_zelevOut(gsize))
      allocate(g_qlinkOut(gsize,2))
      allocate(g_orderOut(gsize))
      allocate(g_QLateralOut(gsize))
      allocate(g_velocityOut(gsize))
      allocate(g_nudgeOut(gsize))
      allocate(g_qSfcLatRunoffOut(gsize))
      allocate(g_qBucketOut(gsize))
      allocate(g_qBtmVertRunoffOut(gsize))
      allocate(g_accSfcLatRunoffOut(gsize))
      allocate(g_accBucketOut(gsize))
      allocate(chIndArray(gsize))
      allocate(g_linkidOut(gsize))
      g_chlon = RT_DOMAIN(domainId)%CHLON
      g_chlat = RT_DOMAIN(domainId)%CHLAT
      g_zelev = RT_DOMAIN(domainId)%ZELEV
      g_order = RT_DOMAIN(domainId)%ORDER
      g_linkid = RT_DOMAIN(domainId)%linkid
      g_hlink = RT_DOMAIN(domainId)%HLINK
      g_qlink = RT_DOMAIN(domainId)%QLINK 
      g_QLateral = RT_DOMAIN(domainId)%QLateral
      g_velocity = RT_DOMAIN(domainId)%velocity
      ! Optional outputs
      if(nudgeFlag .eq. 1)then
#ifdef WRF_HYDRO_NUDGING
         fileMeta%outFlag(2) = 1 ! Set output flag to on. 
         g_nudge = RT_DOMAIN(domainID)%nudge
#endif
      endif
      if(nlst_rt(domainId)%output_channelBucket_influx .eq. 1 .or. &
         nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
         fileMeta%outFlag(6) = 1
         fileMeta%outFlag(7) = 1
         g_qSfcLatRunoff = RT_DOMAIN(domainId)%qSfcLatRunoff
         g_qBucket = RT_DOMAIN(domainId)%qBucket
      endif
      if(nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
         fileMeta%outFlag(8) = 1
         g_qBtmVertRunoff = RT_DOMAIN(domainId)%qin_gwsubbas
      endif
      if(nlst_rt(domainId)%output_channelBucket_influx .eq. 3) then
         fileMeta%outFlag(9) = 1
         fileMeta%outFlag(10) = 1
         g_accSfcLatRunoff = RT_DOMAIN(domainId)%accSfcLatRunoff
         g_accBucket = RT_DOMAIN(domainId)%qBucket
      endif
   endif

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim  
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00') 
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)
 
   ! Compose output file name.
   write(output_flnm, '(A12,".CHRTOUT_DOMAIN",I1)')nlst_rt(domainId)%olddate(1:4)//&
         nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
         nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16), nlst_rt(domainId)%igrid

   ! Only run NetCDF library calls to output data if we are on the master
   ! processor.
   if(myId .eq. 0) then
      if(nlst_rt(domainId)%channel_option .ne. 3) then
         ! Read in index values from Routelink that will be used to sort output
         ! variables by ascending feature_id.
         iret = nf90_open(trim(nlst_rt(1)%route_link_f),NF90_NOWRITE,ncid=ftnRt)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open RouteLink file for index extraction')
         iret = nf90_inq_varid(ftnRt,'ascendingIndex',indVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: ascendingIndex not found in RouteLink file. No resorting will take place.')
            ascFlag = 0
         endif
         if(ascFlag .eq. 1) then 
            iret = nf90_get_var(ftnRt,indVarId,chIndArray)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to extract ascendingIndex from RouteLink file.')
         endif
         iret = nf90_close(ftnRt)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close RouteLink file.')
      else
         ascFlag = 0
      endif
      ! Place all output arrays into one real array that will be looped over
      ! during conversion to compressed integer format.
      allocate(varOutReal(fileMeta%numVars,gSize))
      allocate(varOutInt(gSize))
      if(ascFlag .eq. 1) then
         ! Sort feature_id values by ascending values using the index array
         ! extracted from the RouteLink file. 
         do iTmp=1,gSize
            indTmp = chIndArray(iTmp)
            indTmp = indTmp + 1 ! Python starts index values at 0, so we need to add one.
            g_linkidOut(iTmp) = g_linkid(indTmp)
            g_qlinkOut(iTmp,1) = g_qlink(indTmp,1)
            g_nudgeOut(iTmp) = g_nudge(indTmp)
            g_QLateralOut(iTmp) = g_QLateral(indTmp)
            g_velocityOut(iTmp) = g_velocity(indTmp)
            g_hlinkOut(iTmp) = g_hlink(indTmp)
            g_qSfcLatRunoffOut(iTmp) = g_qSfcLatRunoff(indTmp)
            g_qBucketOut(iTmp) = g_qBucket(indTmp)
            g_qBtmVertRunoffOut(iTmp) = g_qBtmVertRunoff(indTmp)
            g_accSfcLatRunoffOut(iTmp) = g_accSfcLatRunoff(indTmp)
            g_accBucketOut(iTmp) = g_accBucket(indTmp)
            g_chlatOut(iTmp) = g_chlat(indTmp)
            g_chlonOut(iTmp) = g_chlon(indTmp)
            g_orderOut(iTmp) = g_order(indTmp)
            g_zelevOut(iTmp) = g_zelev(indTmp)
         end do
      else
         g_linkidOut = g_linkid
         g_qlinkOut(:,1) = g_qlink(:,1)
         g_nudgeOut = g_nudge
         g_QLateralOut = g_QLateral
         g_velocityOut = g_velocity
         g_hlinkOut = g_hlink
         g_qSfcLatRunoffOut = g_qSfcLatRunoff
         g_qBucketOut = g_qBucket
         g_qBtmVertRunoffOut = g_qBtmVertRunoff
         g_accSfcLatRunoffOut = g_accSfcLatRunoff
         g_accBucketOut = g_accBucket
         g_chlatOut = g_chlat
         g_chlonOut = g_chlon
         g_orderOut = g_order
         g_zelevOut = g_zelev
      endif
      varOutReal(1,:) = g_qlinkOut(:,1)
      varOutReal(2,:) = g_nudgeOut
      varOutReal(3,:) = g_QLateralOut
      varOutReal(4,:) = g_velocityOut
      varOutReal(5,:) = g_hlinkOut
      varOutReal(6,:) = g_qSfcLatRunoffOut
      varOutReal(7,:) = g_qBucketOut
      varOutReal(8,:) = g_qBtmVertRunoffOut
      varOutReal(9,:) = g_accSfcLatRunoffOut
      varOutReal(10,:) = g_accBucketOut

      ! Mask out missing values
      where ( varOutReal == fileMeta%modelNdv ) varOutReal = -9999.0

      ! Open output NetCDF file for writing.
      iret = nf90_create(trim(output_flnm),cmode=nf90_hdf5,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create CHRTOUT NetCDF file.')

      ! Write global attributes.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"featureType",trim(fileMeta%fType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create featureType attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_initialization_time",trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model init attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"station_dimension",trim(fileMeta%stDim))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create st. dimension attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_valid_time",trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model valid attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"stream_order_output",fileMeta%stOrder)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create order attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"cdm_datatype",trim(fileMeta%cdm))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create CDM attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"esri_pe_string",trim(fileMeta%esri))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create ESRI attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"Conventions",trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create conventions attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_version",trim(fileMeta%outVersion))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_version attribute')

      ! Create global attributes specific to running output through the
      ! channel-only configuration of the model.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_OVRTSWCRT",nlst_rt(domainId)%OVRTSWCRT)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_OVRTSWCRT attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_NOAH_TIMESTEP",int(nlst_rt(domainId)%dt))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_NOAH_TIMESTEP attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_channel_only",nlst_rt(domainId)%channel_only)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_channel_only attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_channelBucket_only",nlst_rt(domainId)%channelBucket_only)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_channelBucket_only attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'dev','dev_ prefix indicates development/internal meta data')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,"feature_id",gSize,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id dimension')
      iret = nf90_def_dim(ftn,"time",NF90_UNLIMITED,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time dimension')
      iret = nf90_def_dim(ftn,"reference_time",1,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(2),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(3),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create feature_id variable
      iret = nf90_def_var(ftn,"feature_id",nf90_int,dimId(1),featureVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id variable.')
      iret = nf90_put_att(ftn,featureVarId,'long_name',trim(fileMeta%featureIdLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'comment',trim(fileMeta%featureIdComment))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'cf_role',trim(fileMeta%cfRole))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place cf_role attribute into feature_id variable')

      ! Create channel lat/lon variables
      iret = nf90_def_var(ftn,"latitude",nf90_float,dimId(1),latVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create latitude variable.')
      iret = nf90_put_att(ftn,latVarId,'long_name',trim(fileMeta%latLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'standard_name',trim(fileMeta%latStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'units',trim(fileMeta%latUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into latitude variable')
      iret = nf90_def_var(ftn,"longitude",nf90_float,dimId(1),lonVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create longitude variable.')
      iret = nf90_put_att(ftn,lonVarId,'long_name',trim(fileMeta%lonLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'standard_name',trim(fileMeta%lonStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'units',trim(fileMeta%lonUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into longitude variable')

      ! Create channel order variable
      iret = nf90_def_var(ftn,"order",nf90_int,dimId(1),orderVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create order variable.')
      iret = nf90_put_att(ftn,orderVarId,'long_name',trim(fileMeta%orderLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into order variable')
      iret = nf90_put_att(ftn,orderVarId,'standard_name',trim(fileMeta%orderStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into order variable')

      ! Create channel elevation variable
      iret = nf90_def_var(ftn,"elevation",nf90_float,dimId(1),elevVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create elevation variable.')
      iret = nf90_put_att(ftn,elevVarId,'long_name',trim(fileMeta%elevLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into elevation variable')
      iret = nf90_put_att(ftn,elevVarId,'standard_name',trim(fileMeta%elevStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into elevation variable')

      ! Define deflation levels for these meta-variables. For now, we are going to
      ! default to a compression level of 2. Only compress if nwmIo is set to 1.
      if(nlst_rt(1)%nwmIo .eq. 1) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,latVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for latitude.')
         iret = nf90_def_var_deflate(ftn,lonVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for longitude.') 
         iret = nf90_def_var_deflate(ftn,orderVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for order.')  
         iret = nf90_def_var_deflate(ftn,elevVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for elevation.')  
      endif

      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout the
      ! output looping below for conversion to compressed integer values.
      ! Loop through and create each output variable, create variable attributes,
      ! and insert data.
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! First create variable
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,dimId(1),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing
            ! output. Only compress if nwmIo is set to 1.
            if(nlst_rt(1)%nwmIo .eq. 1) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'coordinates',trim(fileMeta%coordNames(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place coordinates attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
         endif
      end do 

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take CHRTOUT file out of definition mode')

      ! Loop through all possible output variables, and convert floating points
      ! to integers via prescribed scale_factor/add_offset, then write to the
      ! NetCDF variable. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer. If this is time zero, check to see if we
            ! need to convert all data to NDV
            if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               varOutInt(:) = fileMeta%fillComp(iTmp)
            else
               varOutInt(:) = NINT((varOutReal(iTmp,:)-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
            endif
            ! Get NetCDF variable id.
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))
 
            ! Put data into NetCDF file
            iret = nf90_put_var(ftn,varId,varOutInt)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do

      ! Place link ID values into the NetCDF file
      iret = nf90_inq_varid(ftn,'feature_id',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_linkidOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')
      
      iret = nf90_inq_varid(ftn,'latitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate latitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_chlatOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into latitude output variable.')

      iret = nf90_inq_varid(ftn,'longitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate longitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_chlonOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into longitude output variable.')

      iret = nf90_inq_varid(ftn,'order',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate order in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_orderOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into order output variable.')

      iret = nf90_inq_varid(ftn,'elevation',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate elevation in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_zelevOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into elevation output variable.')

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close CHRTOUT file.')

   endif ! End if we are on master processor. 
  
   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Deallocate all memory.
   if(myId .eq. 0) then
      deallocate(varOutReal)
      deallocate(varOutInt)
   endif
   deallocate(g_chlonOut)
   deallocate(g_chlatOut)
   deallocate(g_hlinkOut)
   deallocate(g_zelevOut)
   deallocate(g_qlinkOut)
   deallocate(g_orderOut)
   deallocate(g_QLateralOut)
   deallocate(g_velocityOut)
   deallocate(g_nudgeOut)
   deallocate(g_qSfcLatRunoffOut)
   deallocate(g_qBucketOut)
   deallocate(g_qBtmVertRunoffOut)
   deallocate(g_accSfcLatRunoffOut)
   deallocate(g_accBucketOut)
   deallocate(chIndArray)
   deallocate(g_linkidOut)
   deallocate(g_chlon)
   deallocate(g_chlat)
   deallocate(g_hlink)
   deallocate(g_zelev)
   deallocate(g_qlink)
   deallocate(g_order)
   deallocate(g_linkid)
   deallocate(g_QLateral)
   deallocate(g_nudge)
   deallocate(g_qSfcLatRunoff)
   deallocate(g_qBucket)
   deallocate(g_qBtmVertRunoff)
   deallocate(g_accSfcLatRunoff)
   deallocate(g_accBucket)
   deallocate(strFlowLocal)
   deallocate(velocityLocal)

end subroutine output_chrt_NWM

!==============================================================================
! Program Name: output_NoahMP_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for NoahMP grids for the National Water Model.
! History Log:
! 3/6/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_NoahMP_NWM(outDir,iGrid,output_timestep,startdate,date,ixPar,jxPar,zNum,varReal,vegTyp,varInd,nwmIoLsm)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
#endif
   implicit none

   ! Subroutine arguments
   character(len=*), intent(in) :: outDir ! Output directory to place output.
   integer, intent(in) :: iGrid ! Grid number
   integer, intent(in) :: output_timestep ! Output timestep we are on.
   character(len=19),intent(in) :: startdate ! Model simulation start date
   character(len=19),intent(in) :: date ! Current model date
   integer, intent(in) :: ixPar,jxPar ! I/J dimensions of local grid.
   integer, intent(in) :: zNum ! Number of vertical layers (most of the time 1)
   real, intent(in) :: varReal(ixPar,zNum,jxPar) ! Variable data to be written. 
   integer, intent(inout) :: vegTyp(ixPar,jxPar) ! Vegetation type grid used to mask out variables.
   integer, intent(in) :: varInd ! Variable index used to extact meta-data from.
   integer, intent(in) :: nwmIoLsm ! nwmIo flag specified in namelist.hrldas

   ! Derived types.
   type(ldasMeta) :: fileMeta

   ! Local variables
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDateTmp ! Start of model simulation, represented as a string. 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: mppFlag, diagFlag
   character(len=1024) :: output_flnm ! Output file name
   integer :: iret ! NetCDF return status
   integer :: ftn  ! NetCDF file handle
   integer :: dimId(6) ! NetCDF dimension ID values
   integer :: varId ! NetCDF variable ID value
   integer :: timeId ! NetCDF time variable ID
   integer :: refTimeId ! NetCDF reference_time variable ID
   integer :: coordVarId ! NetCDF coordinate variable ID
   integer :: xVarId,yVarId ! NetCDF x/y variable ID
   integer :: ierr, myId ! MPI related values
   integer :: varRange(2) ! Local storage of valid min/max values
   integer :: iTmp,jTmp,zTmp,jTmp2
   integer :: ftnGeo,geoXVarId,geoYVarId
   integer :: waterVal ! Value in HRLDAS in WRFINPUT file used to define water bodies for masking
   ! Allocatable arrays to hold global output arrays, and local arrays for
   ! conversion to integers. 
   integer, allocatable, dimension(:,:) :: localCompTmp, globalCompTmp
   integer, allocatable, dimension(:,:,:) :: globalOutComp
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2
   real, allocatable, dimension(:,:,:) :: varRealTmp

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! Sync up processes. 
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize water type to 16.
   ! NOTE THIS MAY CHANGE IN THE FUTURE!!!!!
   waterVal = 16

   ! Initialize NWM dictionary derived type containing all the necessary
   ! metadata for the output file.
   call initLdasDict(fileMeta,myId,diagFlag)

   ! Calculate necessary datetime information that will go into the output file.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDateTmp = trim(nlst_rt(1)%startdate(1:4)//"-"//&
                       nlst_rt(1)%startdate(6:7)//&
                       &"-"//nlst_rt(1)%startdate(9:10)//" "//&
                       nlst_rt(1)%startdate(12:13)//":"//&
                       nlst_rt(1)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDateTmp,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim  
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(1)%olddate(1:4)//'-'//&
                    nlst_rt(1)%olddate(6:7)//'-'//&
                    nlst_rt(1)%olddate(9:10)//'_'//&
                    nlst_rt(1)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(1)%startdate(1:4)//'-'//&
                  nlst_rt(1)%startdate(6:7)//'-'//&
                  nlst_rt(1)%startdate(9:10)//'_'//&
                  nlst_rt(1)%startdate(12:13)//&
                  &':00:00') 
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)
 
   ! Depending on the NWM forecast config, we will be outputting different
   ! varibles. DO NOT MODIFY THESE ARRAYS WITHOUT CONSULTING NCAR OR
   ! OWP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if(nlst_rt(1)%iocFlag .eq. 0) then
      ! Diagnostics
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,1,&
                             1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,&
                             0,0,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,1,1,1,1,1]
   else if(nlst_rt(1)%iocFlag .eq. 1) then
      ! Analysis and Assimilation
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,0,0,1,0,1]
   else if(nlst_rt(1)%iocFlag .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,0,0,1,0,1]
   else if(nlst_rt(1)%iocFlag .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,1,&
                             0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,&
                             0,0,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,1,1,1,0,1]
   else if(nlst_rt(1)%iocFlag .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
                             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,1,0,1,1,0]
   else if(nlst_rt(1)%iocFlag .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,1,& !1-20
                             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,& !21-40
                             0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,& !41-60
                             1,0,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,& !61-80
                             0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]            !81-95
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   if(varInd .eq. 1) then
      ! We are on the first variable, we need to create the output file with
      ! attributes first.
      if(myId .eq. 0) then
         ! We are on the I/O node. Create output file.
         if (mod(output_timestep,3600) == 0) then
            write(output_flnm, '(A,"/",A12,".LDASOUT_DOMAIN",I1)') outdir,date(1:4)//&
                  date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
         elseif (mod(output_timestep,60) == 0) then
            write(output_flnm, '(A,"/",A12,".LDASOUT_DOMAIN",I1)') outdir,date(1:4)//&
                  date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
         else
            write(output_flnm, '(A,"/",A14,".LDASOUT_DOMAIN",I1)') outdir,date(1:4)//&
                  date(6:7)//date(9:10)//date(12:13)//date(15:16)//date(18:19), igrid
         endif

         iret = nf90_create(trim(output_flnm),cmode=nf90_hdf5,ncid = ftn)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create LDASOUT NetCDF file.')
         ftnNoahMP = ftn

         ! Write global attributes
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'TITLE',trim(fileMeta%title))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place TITLE attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'model_output_valid_time',trim(fileMeta%validTime))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'Conventions',trim(fileMeta%conventions))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into LDASOUT file.')
         iret = nf90_put_att(ftn,NF90_GLOBAL,"model_version",trim(fileMeta%outVersion))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_version attribute')

         ! Create dimensions
         iret = nf90_def_dim(ftnNoahMP,'time',NF90_UNLIMITED,dimId(1))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
         iret = nf90_def_dim(ftnNoahMP,'x',global_nx,dimId(2))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
         iret = nf90_def_dim(ftnNoahMP,'y',global_ny,dimId(3))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
         iret = nf90_def_dim(ftnNoahMP,'soil_layers_stag',fileMeta%numSoilLayers,dimId(4))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define soil_layers_stag dimension')
         iret = nf90_def_dim(ftnNoahMP,'snow_layers',fileMeta%numSnowLayers,dimId(5))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define snow_layers dimension')
         iret = nf90_def_dim(ftnNoahMP,'reference_time',1,dimId(6))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define reference_time dimension')

         ! Create and populate reference_time and time variables.
         iret = nf90_def_var(ftnNoahMP,"time",nf90_int,dimId(1),timeId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
         iret = nf90_put_att(ftnNoahMP,timeId,'long_name',trim(fileMeta%timeLName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
         iret = nf90_put_att(ftnNoahMP,timeId,'standard_name',trim(fileMeta%timeStName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
         iret = nf90_put_att(ftnNoahMP,timeId,'units',trim(fileMeta%timeUnits))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
         iret = nf90_def_var(ftnNoahMP,"reference_time",nf90_int,dimId(6),refTimeId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
         iret = nf90_put_att(ftnNoahMP,refTimeId,'long_name',trim(fileMeta%rTimeLName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
         iret = nf90_put_att(ftnNoahMP,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
         iret = nf90_put_att(ftnNoahMP,refTimeId,'units',trim(fileMeta%rTimeUnits))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

         ! Create x/y coordinate variables
         iret = nf90_def_var(ftnNoahMP,'x',nf90_double,dimId(2),xVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create x coordinate variable')
         iret = nf90_put_att(ftnNoahMP,xVarId,'long_name',trim(fileMeta%xLName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into x coordinate variable')
         iret = nf90_put_att(ftnNoahMP,xVarId,'standard_name',trim(fileMeta%xStName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into x coordinate variable')
         iret = nf90_put_att(ftnNoahMP,xVarId,'_CoordinateAxisType',trim(fileMeta%xAxisType))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxisType attribute into x coordinate variable')
         iret = nf90_put_att(ftnNoahMP,xVarId,'units',trim(fileMeta%xUnits))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into x coordinate variable')
         iret = nf90_put_att(ftnNoahMP,xVarId,'resolution',fileMeta%xRes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place resolution attribute into x coordinate variable')
         iret = nf90_def_var(ftnNoahMP,'y',nf90_double,dimId(3),yVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create y coordinate variable')
         iret = nf90_put_att(ftnNoahMP,yVarId,'long_name',trim(fileMeta%yLName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into y coordinate variable')
         iret = nf90_put_att(ftnNoahMP,yVarId,'standard_name',trim(fileMeta%yStName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into y coordinate variable')
         iret = nf90_put_att(ftnNoahMP,yVarId,'_CoordinateAxisType',trim(fileMeta%yAxisType))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxisType attribute into y coordinate variable')
         iret = nf90_put_att(ftnNoahMP,yVarId,'units',trim(fileMeta%yUnits))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into y coordinate variable')
         iret = nf90_put_att(ftnNoahMP,yVarId,'resolution',fileMeta%yRes)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place resolution attribute into y coordinate variable')

         ! Define compression for meta-variables. Only compress if nwmIo is set
         ! to 1.
         if(nwmIoLsm .eq. 1) then
            iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
            iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
            iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
            iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')
         endif

         ! Create ProjectionCoordinateSystem variable
         iret = nf90_def_var(ftnNoahMP,'ProjectionCoordinateSystem',nf90_char,varid=coordVarId)
         iret = nf90_put_att(ftnNoahMP,coordVarId,'_CoordinateTransformType',trim(fileMeta%projTransform))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateTransformType attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'transform_name',trim(fileMeta%projTransformName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place transform_name attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'grid_mapping_name',trim(fileMeta%projGridMappingName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping_name attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'_CoordinateAxes',trim(fileMeta%projCoordAxes))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxes attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'esri_pe_string',trim(fileMeta%projEsri))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'standard_parallel',fileMeta%stParallel)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_parallel attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'longitude_of_central_meridian',fileMeta%lonCtrlMer)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place longitude_of_central_meridian attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'latitude_of_projection_origin',fileMeta%latOrig)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place latitude_of_projection_origin attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'false_easting',fileMeta%falseEast)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to false_easting place attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'false_northing',fileMeta%falseNorth)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place false_northing attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'earth_radius',fileMeta%radius)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place earth_radius attribute into ProjectionCoordinateVariable')
         iret = nf90_put_att(ftnNoahMP,coordVarId,'proj4',trim(fileMeta%proj4))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into ProjectionCoordinateVariable')

         ! Loop through all possible variables and create them, along with their
         ! metadata attributes. 
         do iTmp=1,fileMeta%numVars
            if(fileMeta%outFlag(iTmp) .eq. 1) then
               if(fileMeta%numLev(iTmp) .eq. fileMeta%numSoilLayers) then
                  iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(4),dimId(3),dimId(1)/),varId)
               else if(fileMeta%numLev(iTmp) .eq. fileMeta%numSnowLayers) then
                  iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(5),dimId(3),dimId(1)/),varId)
               else if(fileMeta%numLev(iTmp) .eq. 1) then
                  iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
               endif
               call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(iTmp)))            
            
               ! Extract valid range into a 1D array for placement.
               varRange(1) = fileMeta%validMinComp(iTmp)
               varRange(2) = fileMeta%validMaxComp(iTmp)

               ! Establish a compression level for the variables. For now we are using a
               ! compression level of 2. In addition, we are choosing to turn the shuffle
               ! filter off for now. Kelley Eicher did some testing with this and
               ! determined that the benefit wasn't worth the extra time spent writing output.
               ! Only compress if nwmIo is set to 1.
               if(nwmIoLsm .eq. 1) then
                  iret = nf90_def_var_deflate(ftnNoahMP,varId,0,1,2)
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
               endif

               ! Create variable attributes
               iret = nf90_put_att(ftnNoahMP,varId,'_FillValue',fileMeta%fillComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'missing_value',fileMeta%missingComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'long_name',trim(fileMeta%longName(iTmp)))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'units',trim(fileMeta%units(iTmp)))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'add_offset',fileMeta%addOffset(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'valid_range',varRange)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'proj4',trim(fileMeta%proj4))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into variable: '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'grid_mapping',trim(fileMeta%gridMapping))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'esri_pe_string',trim(fileMeta%projEsri))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into variable: '//trim(fileMeta%varNames(iTmp))) 
            endif ! End if output flag is on
         end do ! end looping through variable output list.

         ! Remove NetCDF file from definition mode.
         iret = nf90_enddef(ftnNoahMP)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to take LDASOUT file out of definition mode')
 
         ! Read in coordinates from GeoGrid file. These will be placed into the
         ! output file coordinate variables. 
         allocate(xCoord(global_nx))
         allocate(yCoord(global_ny))
         allocate(yCoord2(global_ny))
         iret = nf90_open(trim(nlst_rt(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnGeo)
         if(iret .ne. 0) then
            ! Spatial metadata file not found for land grid. Warn the user no
            ! file was found, and set x/y coordinates to -9999.0
            call postDiagMsg(diagFlag,'WARNING: Unable to find LAND spatial metadata file')
            xCoord = -9999.0
            yCoord = -9999.0
            yCoord2 = -9999.0
         else
            iret = nf90_inq_varid(ftnGeo,'x',geoXVarId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find x coordinate in geoGrid file')
            iret = nf90_get_var(ftnGeo,geoXVarId,xCoord)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to extract x coordinate from geoGrid file')
            iret = nf90_inq_varid(ftnGeo,'y',geoYVarId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find y coordinate in geoGrid file')
            iret = nf90_get_var(ftnGeo,geoYVarId,yCoord)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to extract y coordinate from geoGrid file')
            iret = nf90_close(ftnGeo)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to close geoGrid file.')
            ! Reverse Y coordinates. They are read in reverse. 
            jTmp2 = 0
            do jTmp = global_ny,1,-1
               jTmp2 = jTmp2 + 1
               yCoord2(jTmp2) = yCoord(jTmp)
            end do
         endif         

         ! Place coordinate values into output file
         iret = nf90_inq_varid(ftnNoahMP,'x',varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to locate x coordinate variable.')
         iret = nf90_put_var(ftnNoahMP,varId,xCoord)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into x coordinate variable')
         iret = nf90_inq_varid(ftnNoahMP,'y',varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to locate y coordinate variable')
         iret = nf90_put_var(ftnNoahMP,varId,yCoord2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into y coordinate variable')
         deallocate(xCoord)
         deallocate(yCoord)
         deallocate(yCoord2)

         ! Place time values into time variables.
         iret = nf90_inq_varid(ftnNoahMP,'time',varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
         iret = nf90_put_var(ftnNoahMP,varId,minSinceEpoch)
         call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
         iret = nf90_inq_varid(ftnNoahMP,'reference_time',varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
         iret = nf90_put_var(ftnNoahMP,varId,minSinceEpoch1)
         call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      end if ! End if we are on the I/O processor.
   endif ! End if we are on the first variable

   ! Sync up all processes
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Place data into NetCDF file. This involves a few steps:
   ! 1.) Allocate an integer array of local grid size.
   ! 2.) Allocate an integer array of global grid size.
   ! 3.) Make a copy of the floating point grid so it can be
   !     masked out where water bodies exist, or missing NoahMP values
   !     exist.
   ! 4.) Loop through real local grid, convert floating point
   !     values to integer via scale_factor/add_offset. If
   !     missing value found, assign FillValue caluclated 
   !     in the dictionary. 
   ! 5.) Use MPP utilities to collect local integer arrays
   !     into global integer array. 
   ! 6.) Write global integer array into output file.
   if(fileMeta%outFlag(varInd) .eq. 1) then
      ! Output flag on for this variable. 
      ! Allocate memory
      if(myId .eq. 0) then
         allocate(globalOutComp(global_nx,fileMeta%numLev(varInd),global_ny))
         allocate(globalCompTmp(global_nx,global_ny))
      else
         allocate(globalOutComp(1,1,1))
         allocate(globalCompTmp(1,1))
      endif
      allocate(localCompTmp(ixPar,jxPar))
      allocate(varRealTmp(ixPar,fileMeta%numLev(varInd),jxPar))
      globalOutComp = fileMeta%fillComp(varInd)
      

      ! Sync up processes
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      varRealTmp = varReal
      ! Reset any missing values that may exist.
      where ( varRealTmp .eq. fileMeta%modelNdv ) varRealTmp = fileMeta%fillReal(varInd)
      where ( varRealTmp .eq. fileMeta%modelNdvInt ) varRealTmp = fileMeta%fillReal(varInd)
      where ( varRealTmp .eq. fileMeta%modelNdv2 ) varRealTmp = fileMeta%fillReal(varInd)
      where ( varRealTmp .eq. fileMeta%modelNdv3 ) varRealTmp = fileMeta%fillReal(varInd)
      do zTmp = 1,fileMeta%numLev(varInd)
         localCompTmp = fileMeta%fillComp(varInd)
         globalCompTmp = fileMeta%fillComp(varInd)
         where ( vegTyp .eq. waterVal) varRealTmp(:,zTmp,:) = fileMeta%fillReal(varInd)
         ! Check to see if we are on time 0. If the flag is set to 0 for time 0
         ! outputs, convert all data to a fill. If we are time 0, make sure we
         ! don't need to fill the grid in with NDV values. 
         if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(varInd) .eq. 0) then
            localCompTmp = fileMeta%fillComp(varInd)
         else
            localCompTmp = NINT((varRealTmp(:,zTmp,:)-fileMeta%addOffset(varInd))/fileMeta%scaleFactor(varInd))
         endif
         ! Sync all processes up.
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call write_IO_int(localCompTmp,globalCompTmp)
#endif
         else
            globalCompTmp = localCompTmp
         endif
         ! Sync all processes up.
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif
         ! Place output into global array to be written to NetCDF file.
         if(myId .eq. 0) then
            globalOutComp(:,zTmp,:) = globalCompTmp
         endif
      end do
 
      ! Sync up processes
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Write array out to NetCDF file.
      if(myId .eq. 0) then
         iret = nf90_inq_varid(ftnNoahMP,trim(fileMeta%varNames(varInd)),varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(varInd)))
         if(fileMeta%numLev(varInd) .eq. 1) then
            iret = nf90_put_var(ftnNoahMP,varId,globalOutComp,(/1,1,1/),(/global_nx,global_ny,1/))
         else
            iret = nf90_put_var(ftnNoahMP,varId,globalOutComp,(/1,1,1,1/),(/global_nx,fileMeta%numLev(varInd),global_ny,1/))
         endif
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(varInd)))
      endif

      ! Deallocate memory for this variable. 
      deallocate(globalOutComp)
      deallocate(globalCompTmp)
      deallocate(localCompTmp)
      deallocate(varRealTmp)

   endif

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   if(myId .eq. 0) then
      ! Only close the file if we are finished with the very last variable. 
      if(varInd .eq. fileMeta%numVars) then
         ! Close the output file
         iret = nf90_close(ftnNoahMP)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LDASOUT file.')
      endif
   endif


end subroutine output_NoahMP_NWM

!==============================================================================
! Program Name: output_rt_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for terrain routing variables 
!           for the National Water Model.
! History Log:
! 3/6/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_rt_NWM(domainId,iGrid)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
#endif
   implicit none

   ! subroutine arguments
   integer, intent(in) :: domainId
   integer, intent(in) :: iGrid

   ! Derived types.
   type(rtDomainMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(4) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: xVarId,yVarId,coordVarId ! Coordinate variable NC ID values
   integer :: varRange(2) ! Local storage for valid min/max ranges
   integer :: ierr, myId ! MPI return status, process ID
   integer :: iTmp,jTmp,jTmp2,iTmp2
   real :: varRealTmp ! Local copy of floating point lake value
   integer :: ftnGeo,geoXVarId,geoYVarId
   ! Allocatable arrays to hold either x/y coordinate information,
   ! or the grid of output values to be converted to integer via scale_factor
   ! and add_offset. 
   integer, allocatable, dimension(:,:) :: localCompTmp
   integer, allocatable, dimension(:,:) :: globalOutComp
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2

! Establish macro variables to hlep guide this subroutine. 
#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initRtDomainDict(fileMeta,myId,diagFlag)

   ! Depending on the NWM forecast config, we will be outputting different
   ! varibles. DO NOT MODIFY THESE ARRAYS WITHOUT CONSULTING NCAR OR
   ! OWP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if(nlst_rt(domainId)%iocFlag .eq. 0) then
      ! Diagnostics
      fileMeta%outFlag(:) = [1,1,0,0]
   else if(nlst_rt(domainId)%iocFlag .eq. 1) then
      ! Analysis and Assimilation
      fileMeta%outFlag(:) = [1,1,0,0]
   else if(nlst_rt(domainId)%iocFlag .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [1,1,0,0]
   else if(nlst_rt(domainId)%iocFlag .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [1,1,0,0]
   else if(nlst_rt(domainId)%iocFlag .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [1,1,0,0]
   else if(nlst_rt(domainId)%iocFlag .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [1,1,0,0]
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! Create output filename
   write(output_flnm, '(A12,".RTOUT_DOMAIN",I1)') nlst_rt(domainId)%olddate(1:4)//&
                       nlst_rt(domainId)%olddate(6:7)//&
                       nlst_rt(domainId)%olddate(9:10)//&
                       nlst_rt(domainId)%olddate(12:13)//&
                       nlst_rt(domainId)%olddate(15:16), igrid

   if(myId .eq. 0) then
      ! Create output NetCDF file for writing. 
      iret = nf90_create(trim(output_flnm),cmode=nf90_hdf5,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create RT_DOMAIN NetCDF file.')

      ! Write global attributes
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_output_valid_time',trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'Conventions',trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_version",trim(fileMeta%outVersion))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_version attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,'time',NF90_UNLIMITED,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
      iret = nf90_def_dim(ftn,'x',RT_DOMAIN(domainId)%g_ixrt,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
      iret = nf90_def_dim(ftn,'y',RT_DOMAIN(domainId)%g_jxrt,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
      iret = nf90_def_dim(ftn,'reference_time',1,dimId(4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define reference_time dimension') 

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(1),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(4),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create x/y coordinate variables
      iret = nf90_def_var(ftn,'x',nf90_double,dimId(2),xVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'long_name',trim(fileMeta%xLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'standard_name',trim(fileMeta%xStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'_CoordinateAxisType',trim(fileMeta%xAxisType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxisType attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'units',trim(fileMeta%xUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'resolution',fileMeta%xRes)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place resolution attribute into x coordinate variable')
      iret = nf90_def_var(ftn,'y',nf90_double,dimId(3),yVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'long_name',trim(fileMeta%yLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'standard_name',trim(fileMeta%yStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'_CoordinateAxisType',trim(fileMeta%yAxisType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxisType attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'units',trim(fileMeta%yUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'resolution',fileMeta%yRes)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place resolution attribute into y coordinate variable')
 
      ! Define compression for meta-variables only if nwmIo is set to 1.
      if(nlst_rt(1)%nwmIo .eq. 1) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
         iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')
      endif

      ! Create ProjectionCoordinateSystem variable
      iret = nf90_def_var(ftn,'ProjectionCoordinateSystem',nf90_char,varid=coordVarId)
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateTransformType',trim(fileMeta%projTransform))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateTransformType attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'transform_name',trim(fileMeta%projTransformName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place transform_name attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'grid_mapping_name',trim(fileMeta%projGridMappingName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping_name attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateAxes',trim(fileMeta%projCoordAxes))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxes attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'esri_pe_string',trim(fileMeta%projEsri))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'standard_parallel',fileMeta%stParallel)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_parallel attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'longitude_of_central_meridian',fileMeta%lonCtrlMer)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place longitude_of_central_meridian attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'latitude_of_projection_origin',fileMeta%latOrig)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place latitude_of_projection_origin attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'false_easting',fileMeta%falseEast)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to false_easting place attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'false_northing',fileMeta%falseNorth)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place false_northing attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'earth_radius',fileMeta%radius)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place earth_radius attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'proj4',trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into ProjectionCoordinateVariable')
 
      ! Loop through all possible variables and create them, along with their
      ! metadata attributes. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
            call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement.
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing output.
            ! Only compress if nwmIo is set to 1.
            if(nlst_rt(1)%nwmIo .eq. 1) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'proj4',trim(fileMeta%proj4))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping',trim(fileMeta%gridMapping))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'esri_pe_string',trim(fileMeta%projEsri))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do ! end looping through variable output list.
      
      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take RT_DOMAIN file out of definition mode')

      ! Read in coordinates from FullDom file. These will be placed into the
      ! output file coordinate variables. 
      allocate(xCoord(RT_DOMAIN(domainId)%g_ixrt))
      allocate(yCoord(RT_DOMAIN(domainId)%g_jxrt))
      allocate(yCoord2(RT_DOMAIN(domainId)%g_jxrt))
      iret = nf90_open(trim(nlst_rt(domainId)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnGeo)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to open FullDom file')
      iret = nf90_inq_varid(ftnGeo,'x',geoXVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find x coordinate in FullDom file')
      iret = nf90_get_var(ftnGeo,geoXVarId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract x coordinate from FullDom file')
      iret = nf90_inq_varid(ftnGeo,'y',geoYVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find y coordinate in FullDom file')
      iret = nf90_get_var(ftnGeo,geoYVarId,yCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract y coordinate from FullDom file')
      iret = nf90_close(ftnGeo)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close geoGrid file.')

      ! Reverse Y coordinates. They are read in reverse. 
      jTmp2 = 0
      do jTmp = RT_DOMAIN(domainId)%g_jxrt,1,-1
         jTmp2 = jTmp2 + 1
         yCoord2(jTmp2) = yCoord(jTmp) 
      end do
      ! Place coordinate values into output file
      iret = nf90_inq_varid(ftn,'x',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate x coordinate variable.')
      iret = nf90_put_var(ftn,varId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into x coordinate variable')
      iret = nf90_inq_varid(ftn,'y',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate y coordinate variable')
      iret = nf90_put_var(ftn,varId,yCoord2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into y coordinate variable')
      deallocate(xCoord)
      deallocate(yCoord)
      deallocate(yCoord2) 

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

   endif ! End if statement if on I/O ID

   ! Synce up processes.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND 
      call mpp_land_sync()
#endif
   endif

   ! Loop through each variable, collect local routing grid variables into a
   ! global routing grid and output through the master I/O process.
   do iTmp2=1,fileMeta%numVars
      if(fileMeta%outFlag(iTmp2) .eq. 1) then   
         !Allocate memory necessary 
         if(myId .eq. 0) then
            allocate(globalOutComp(RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt))
         else
            allocate(globalOutComp(1,1))
         endif
         allocate(localCompTmp(RT_DOMAIN(domainId)%ixrt,RT_DOMAIN(domainId)%jxrt))
         ! Initialize arrays to prescribed NDV value.
         globalOutComp = fileMeta%fillComp(iTmp2)
         localCompTmp = fileMeta%fillComp(iTmp2)

         ! Sync up processes
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif

         ! Loop through output array and convert floating point values to
         ! integers via scale_factor/add_offset. 
         do iTmp = 1,RT_DOMAIN(domainId)%ixrt
            do jTmp = 1,RT_DOMAIN(domainId)%jxrt
               if(iTmp2 .eq. 1) then
                  varRealTmp = RT_DOMAIN(domainId)%ZWATTABLRT(iTmp,jTmp)
               else if(iTmp2 .eq. 2) then
                  varRealTmp = RT_DOMAIN(domainId)%SFCHEADSUBRT(iTmp,jTmp)
               else if(iTmp2 .eq. 3) then
                  varRealTmp = RT_DOMAIN(domainId)%QSTRMVOLRT(iTmp,jTmp)
               else if(iTmp2 .eq. 4) then
                  varRealTmp = RT_DOMAIN(domainId)%QBDRYRT(iTmp,jTmp)
               endif
               ! If we are on time 0, make sure we don't need to fill in the
               ! grid with NDV values. 
               if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp2) .eq. 0) then
                  localCompTmp(iTmp,jTmp) = fileMeta%fillComp(iTmp2)
               else
                  if(varRealTmp .eq. fileMeta%modelNdv) then
                     localCompTmp(iTmp,jTmp) = INT(fileMeta%fillComp(iTmp2))
                  else
                     localCompTmp(iTmp,jTmp) = NINT((varRealTmp-fileMeta%addOffset(iTmp2))/fileMeta%scaleFactor(iTmp2))
                  endif
               endif
            end do
         end do
         ! Collect local integer arrays into the global integer grid to be
         ! written out. 
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call write_IO_rt_int(localCompTmp,globalOutComp)
#endif
         else
            globalOutComp = localCompTmp
         endif

         ! Sync up processes
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif

         ! Write output to NetCDF file. 
         if(myId .eq. 0) then
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp2)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp2)))
            iret = nf90_put_var(ftn,varId,globalOutComp,(/1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt,1/))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp2)))
         endif

         ! Deallocate memory for this variable. 
         deallocate(globalOutComp)
         deallocate(localCompTmp)
      endif
   end do

   if(myId .eq. 0) then
      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close RT_DOMAIN file.')
   endif


end subroutine output_rt_NWM

!==============================================================================
! Program Name: output_lakes_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for lake points for the National Water Model.
! History Log:
! 3/6/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_lakes_NWM(domainId,iGrid)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
#endif
   implicit none

   integer, intent(in) :: domainId
   integer, intent(in) :: iGrid

   ! Derived types.
   type(lakeMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(3) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: featureVarId ! feature_id NetCDF variable ID
   integer :: latVarId, lonVarId ! lat/lon NetCDF variable ID values
   integer :: elevVarId ! elevation NetCDF variable ID
   integer :: varRange(2) ! Local storage of valid min/max values
   integer :: gSize ! Global size of lake out arrays
   integer :: iTmp
   integer :: ftnRt,indVarId,indTmp ! For the feature_id sorting process.
   integer :: ierr, myId ! MPI return status, process ID
   integer :: ascFlag ! Flag for resorting timeseries output by feature_id.
   ! Allocatable arrays to hold output variables. 
   real, allocatable, dimension(:) :: g_lakeLat,g_lakeLon,g_lakeElev
   real, allocatable, dimension(:) :: g_lakeInflow,g_lakeOutflow
   integer, allocatable, dimension(:) :: g_lakeid
   real, allocatable, dimension(:) :: g_lakeLatOut,g_lakeLonOut,g_lakeElevOut 
   real, allocatable, dimension(:) :: g_lakeInflowOut,g_lakeOutflowOut
   integer, allocatable, dimension(:) :: g_lakeidOut
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset
                                                     ! have been applied.
   integer, allocatable, dimension(:) :: chIndArray ! Array of index values for
   !each channel point. feature_id will need to be sorted in ascending order once
   !data is collected into the global array. From there, the index values are
   !re-sorted, and used to re-sort output arrays. 

   ! Initialize the ascFlag.
   ascFlag = 1

   ! Establish macro variables to hlep guide this subroutine. 
#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initLakeDict(fileMeta)

   ! Depending on the NWM forecast config, we will be outputting different
   ! varibles. DO NOT MODIFY THESE ARRAYS WITHOUT CONSULTING NCAR OR
   ! OWP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if(nlst_rt(1)%iocFlag .eq. 0) then
      ! Diagnostics
      fileMeta%outFlag(:) = [1,1] 
   else if(nlst_rt(1)%iocFlag .eq. 1) then
      ! Analysis and Assimilation
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%iocFlag .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%iocFlag .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%iocFlag .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%iocFlag .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [1,1]
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! First step is to collect and assemble all data that will be written to the 
   ! NetCDF file. If we are not using MPI, we bypass the collection step through
   ! MPI. 
   if(mppFlag .eq. 1) then
      gSize = rt_domain(domainId)%NLAKES

      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      allocate(g_lakeLon(gsize))
      allocate(g_lakeLat(gsize))
      allocate(g_lakeElev(gsize))
      allocate(g_lakeInflow(gsize))
      allocate(g_lakeOutflow(gsize))
      allocate(g_lakeid(gsize))
      if(myId .eq. 0) then
         allocate(g_lakeLonOut(gsize))
         allocate(g_lakeLatOut(gsize))
         allocate(g_lakeElevOut(gsize))
         allocate(g_lakeInflowOut(gsize))
         allocate(g_lakeOutflowOut(gsize))
         allocate(g_lakeidOut(gsize))    
         allocate(chIndArray(gsize))
      endif

      g_lakeLat = RT_DOMAIN(domainID)%LATLAKE
      g_lakeLon = RT_DOMAIN(domainID)%LONLAKE
      g_lakeElev = RT_DOMAIN(domainID)%RESHT
      g_lakeInflow = RT_DOMAIN(domainID)%QLAKEI
      g_lakeOutflow = RT_DOMAIN(domainID)%QLAKEO
      g_lakeid = RT_DOMAIN(domainId)%LAKEIDM

      ! Sync everything up before the next step.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Collect arrays from various processors through MPI, and 
      ! assemble into global arrays previously allocated.
#ifdef MPP_LAND
      call write_lake_real(g_lakeLat,RT_DOMAIN(domainId)%lake_index,gsize)
      call write_lake_real(g_lakeLon,RT_DOMAIN(domainId)%lake_index,gsize)
      call write_lake_real(g_lakeElev,RT_DOMAIN(domainId)%lake_index,gsize)
      call write_lake_real(g_lakeInflow,RT_DOMAIN(domainId)%lake_index,gsize)
      call write_lake_real(g_lakeOutflow,RT_DOMAIN(domainId)%lake_index,gsize)
#endif
   else
      gSize = rt_domain(domainId)%NLAKES
      ! No MPI - single processor
      allocate(g_lakeLon(gsize))
      allocate(g_lakeLat(gsize))
      allocate(g_lakeElev(gsize))
      allocate(g_lakeInflow(gsize))
      allocate(g_lakeOutflow(gsize))
      allocate(g_lakeid(gsize))
      allocate(g_lakeLonOut(gsize))
      allocate(g_lakeLatOut(gsize))
      allocate(g_lakeElevOut(gsize))
      allocate(g_lakeInflowOut(gsize))
      allocate(g_lakeOutflowOut(gsize))
      allocate(g_lakeidOut(gsize))
      allocate(chIndArray(gsize))
      g_lakeLat = RT_DOMAIN(domainID)%LATLAKE
      g_lakeLon = RT_DOMAIN(domainID)%LONLAKE
      g_lakeElev = RT_DOMAIN(domainID)%RESHT
      g_lakeInflow = RT_DOMAIN(domainID)%QLAKEI
      g_lakeOutflow = RT_DOMAIN(domainID)%QLAKEO
      g_lakeid = RT_DOMAIN(domainId)%LAKEIDM
   endif

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif 

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! Compose output file name.
   write(output_flnm, '(A12,".LAKEOUT_DOMAIN",I1)')nlst_rt(domainId)%olddate(1:4)//&
         nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
         nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16),nlst_rt(domainId)%igrid

   ! Only run NetCDF library calls to output data if we are on the master
   ! processor.
   if(myId .eq. 0) then
      ! Read in index values from Routelink that will be used to sort output
      ! variables by ascending feature_id.
      iret = nf90_open(trim(nlst_rt(1)%route_lake_f),NF90_NOWRITE,ncid=ftnRt)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to open LAKEPARM file for index extraction')
      iret = nf90_inq_varid(ftnRt,'ascendingIndex',indVarId)
      if(iret .ne. 0) then
         call postDiagMsg(diagFlag,'WARNING: ascendingIndex not found in LAKEPARM file. No resorting will take place.')
         ascFlag = 0
      endif
      if(ascFlag .eq. 1) then
         iret = nf90_get_var(ftnRt,indVarId,chIndArray)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract ascendingIndex from LAKEPARM file.')
      endif
      iret = nf90_close(ftnRt)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAKEPARM file.')

      ! Place all output arrays into one real array that will be looped over
      ! during conversion to compressed integer format.
      allocate(varOutReal(fileMeta%numVars,gSize))
      allocate(varOutInt(gSize))
      if(ascFlag .eq. 1) then
         ! Sort feature_id values by ascending values using the index array
         ! extracted from the RouteLink file. 
         do iTmp=1,gSize
            indTmp = chIndArray(iTmp)
            indTmp = indTmp + 1 ! Python starts index values at 0, so we need to add one.
            g_lakeInflowOut(iTmp) = g_lakeInflow(indTmp)
            g_lakeOutflowOut(iTmp) = g_lakeOutflow(indTmp)
            g_lakeLonOut(iTmp) = g_lakeLon(indTmp)
            g_lakeLatOut(iTmp) = g_lakeLat(indTmp)
            g_lakeElevOut(iTmp) = g_lakeElev(indTmp)
            g_lakeidOut(iTmp) = g_lakeid(indTmp)
         end do
      else
         g_lakeInflowOut = g_lakeInflow
         g_lakeOutflowOut = g_lakeOutflow
         g_lakeLonOut = g_lakeLon
         g_lakeLatOut = g_lakeLat
         g_lakeElevOut = g_lakeElev
         g_lakeidOut = g_lakeid
      endif
      varOutReal(1,:) = g_lakeInflowOut(:)
      varOutReal(2,:) = g_lakeOutflowOut(:)

      ! Mask out missing values
      where ( varOutReal == fileMeta%modelNdv ) varOutReal = -9999.0

      iret = nf90_create(trim(output_flnm),cmode=nf90_hdf5,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create LAKEOUT NetCDF file.')

      ! Write global attributes.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"featureType",trim(fileMeta%fType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create featureType attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_initialization_time",trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model init attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"station_dimension",trim(fileMeta%lakeDim))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create st. dimension attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_valid_time",trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model valid attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"esri_pe_string",trim(fileMeta%esri))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create ESRI attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"Conventions",trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create conventions attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_version",trim(fileMeta%outVersion))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_version attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,"feature_id",gSize,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id dimension')
      iret = nf90_def_dim(ftn,"time",NF90_UNLIMITED,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time dimension')
      iret = nf90_def_dim(ftn,"reference_time",1,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(2),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(3),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create feature_id variable
      iret = nf90_def_var(ftn,"feature_id",nf90_int,dimId(1),featureVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id variable.')
      iret = nf90_put_att(ftn,featureVarId,'long_name',trim(fileMeta%featureIdLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'comment',trim(fileMeta%featureIdComment))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'cf_role',trim(fileMeta%cfRole))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place cf_role attribute into feature_id variable')

      ! Create lake lat/lon variables
      iret = nf90_def_var(ftn,"latitude",nf90_float,dimId(1),latVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create latitude variable.')
      iret = nf90_put_att(ftn,latVarId,'long_name',trim(fileMeta%latLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'standard_name',trim(fileMeta%latStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'units',trim(fileMeta%latUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into latitude variable')
      iret = nf90_def_var(ftn,"longitude",nf90_float,dimId(1),lonVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create longitude variable.')
      iret = nf90_put_att(ftn,lonVarId,'long_name',trim(fileMeta%lonLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'standard_name',trim(fileMeta%lonStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'units',trim(fileMeta%lonUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into longitude variable')

      ! Create channel elevation variable
      iret = nf90_def_var(ftn,"elevation",nf90_float,dimId(1),elevVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create elevation variable.')
      iret = nf90_put_att(ftn,elevVarId,'long_name',trim(fileMeta%elevLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into elevation variable')
      iret = nf90_put_att(ftn,elevVarId,'units',trim(fileMeta%elevUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into elevation variable')

      ! Define deflation levels for these meta-variables. For now, we are going to
      ! default to a compression level of 2. Only compress if nwmIo is set to 1.
      if(nlst_rt(1)%nwmIo .eq. 1) then 
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,latVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for latitude.')
         iret = nf90_def_var_deflate(ftn,lonVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for longitude.')
         iret = nf90_def_var_deflate(ftn,elevVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for elevation.')
      endif

      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout the
      ! output looping below for conversion to compressed integer values.
      ! Loop through and create each output variable, create variable attributes,
      ! and insert data.
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! First create variable
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,dimId(1),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing output.
            ! Only compress if nwmIo is set to 1.
            if(nlst_rt(1)%nwmIo .eq. 1) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'coordinates',trim(fileMeta%coordNames(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place coordinates attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
         endif
      end do

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take LAKETOUT file out of definition mode')
   
      ! Place lake ID, elevation, lat, and lon values into appropriate
      ! variables. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer. If we are on time 0, make sure we don't
            ! need to fill in with NDV values. 
            if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               varOutInt(:) = fileMeta%fillComp(iTmp)
            else
               varOutInt(:) = NINT((varOutReal(iTmp,:)-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
            endif
            ! Get NetCDF variable id.
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))

            ! Put data into NetCDF file
            iret = nf90_put_var(ftn,varId,varOutInt)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do

      ! Place link ID values into the NetCDF file
      iret = nf90_inq_varid(ftn,'feature_id',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_lakeidOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')

      ! Place lake metadata into NetCDF file
      iret = nf90_inq_varid(ftn,'elevation',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate elevation in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_lakeElevOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into elevation output variable.')

      iret = nf90_inq_varid(ftn,'latitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate latitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_lakeLatOut)   
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into latitude output variable.')

      iret = nf90_inq_varid(ftn,'longitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate longitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_lakeLonOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into longitude output variable.')

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAKE file.')

   endif ! End if we are on master processor. 

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Deallocate all memory
   if(myId .eq. 0) then
      deallocate(varOutReal)
      deallocate(varOutInt)
   endif
   deallocate(g_lakeLon)
   deallocate(g_lakeLat)
   deallocate(g_lakeElev)
   deallocate(g_lakeInflow)
   deallocate(g_lakeOutflow)
   deallocate(g_lakeid)
   if(myId .eq. 0) then
      deallocate(g_lakeLonOut)
      deallocate(g_lakeLatOut)
      deallocate(g_lakeElevOut)
      deallocate(g_lakeInflowOut)
      deallocate(g_lakeOutflowOut)
      deallocate(g_lakeidOut)
      deallocate(chIndArray)
   endif


end subroutine output_lakes_NWM

!==================================================================
! Program Name: output_chrtout_grd_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Ouptut routine for gridden streamflow variables
!           for non-reach based routing.
! History Log:
! 8/6/17 - Created, LRK.
! Usage:
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
! 
! User controllable options: None.

subroutine output_chrtout_grd_NWM(domainId,iGrid)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
   use module_mpp_land
   use module_mpp_reachls,  only: ReachLS_write_io
#endif
   implicit none

   ! subroutine arguments 
   integer, intent(in) :: domainId
   integer, intent(in) :: iGrid

   ! Derived types.
   type(chrtGrdMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_GRID filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(4) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: xVarId,yVarId,coordVarId ! Coordinate variable NC ID values
   integer :: varRange(2) ! Local storage for valid min/max ranges
   integer :: ierr, myId ! MPI return status, process ID
   integer :: ftnGeo,geoXVarId,geoYVarId
   integer :: iTmp,jTmp,jTmp2,iTmp2
   integer :: gNumLnks,lNumLnks
   integer :: indexVarId
   ! Allocatable array to hold temporary streamflow for checking
   real, allocatable, dimension(:) :: strFlowLocal
   ! Allocatable array to hold global qlink values
   real, allocatable, dimension(:,:) :: g_qlink
   ! Allocatable array to hold streamflow index values
   integer, allocatable, dimension(:,:) :: CH_NETLNK
   ! allocatable global array to hold grid of output streamflow values
   integer, allocatable, dimension(:,:) :: tmpFlow
   ! allocatable arrays to hold coordinate values
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2

! Establish macro variables to hlep guide this subroutine. 
#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Some sanity checking here. 
   if(nlst_rt(domainId)%CHRTOUT_GRID .eq. 0) then
      ! No output requested here. Return to the parent calling program.
      return
   endif

   if(nlst_rt(domainId)%CHRTOUT_GRID .ne. 1) then
      call nwmCheck(diagFlag,1,'ERROR: CHRTOUT_GRID value of 0 or 1 only allowed.')
   endif

   ! We must assume gridded routing is in place on a channel grid for this
   ! routine to be valid. 
   if(nlst_rt(domainId)%channel_option .ne. 3) then
      call nwmCheck(diagFlag,1,'ERROR: CHRTOUT_GRID only valid for channel_option of 3.')
   endif

   ! Initialize qlink arrays and collect data from processors for output.
   gNumLnks = rt_domain(domainId)%gnlinks
   lNumLnks = rt_domain(domainId)%NLINKS
   if(myId .eq. 0) then
      ! Channel index values
      allocate(CH_NETLNK(RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt))
      ! Global qlink values
      allocate(g_qlink(gNumLnks,2) )
      ! Grid of global streamflow values via scale_factor/add_offset
      allocate(tmpFlow(RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt))
   else
      allocate(CH_NETLNK(1,1))
      allocate(g_qlink(1,2) )
      allocate(tmpFlow(1,1))
   endif
   ! Allocate local streamflow array. We need to do a check to
   ! for lake_type 2. However, we cannot set the values in the global array 
   ! to missing as this causes the model to crash.
   allocate(strFlowLocal(RT_DOMAIN(domainId)%NLINKS))
   strFlowLocal = RT_DOMAIN(domainId)%QLINK(:,1)
   ! Loop through all the local links on this processor. For lake_type
   ! of 2, we need to manually set the streamflow values
   ! to the model NDV value.
   if (RT_DOMAIN(domainId)%NLAKES .gt. 0) then
      do iTmp=1,RT_DOMAIN(domainId)%NLINKS
         if (RT_DOMAIN(domainId)%TYPEL(iTmp) .eq. 2) then
            strFlowLocal(iTmp) = fileMeta%modelNdv
         endif
      end do
   endif
   if(nlst_rt(domainId)%channel_option .eq. 3) then
      call write_chanel_real(strFlowLocal,RT_DOMAIN(domainId)%map_l2g,gNumLnks,lNumLnks,g_qlink(:,1))
      call write_chanel_real(RT_DOMAIN(domainId)%qlink(:,2),RT_DOMAIN(domainId)%map_l2g,gNumLnks,lNumLnks,g_qlink(:,2))
   else
      call ReachLS_write_io(strFlowLocal,g_qlink(:,1))
      call ReachLS_write_io(RT_DOMAIN(domainId)%qlink(:,2),g_qlink(:,2))
   endif
   call write_IO_rt_int(RT_DOMAIN(domainId)%GCH_NETLNK, CH_NETLNK)

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initChrtGrdDict(fileMeta,myId,diagFlag)

   ! For now, we will default to outputting all variables until further notice. 
   fileMeta%outFlag(:) = [1]

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! Create output filename
   write(output_flnm, '(A12,".CHRTOUT_GRID",I1)') nlst_rt(domainId)%olddate(1:4)//&
                       nlst_rt(domainId)%olddate(6:7)//&
                       nlst_rt(domainId)%olddate(9:10)//&
                       nlst_rt(domainId)%olddate(12:13)//&
                       nlst_rt(domainId)%olddate(15:16), igrid

   if(myId .eq. 0) then
      ! Create output NetCDF file for writing. 
      iret = nf90_create(trim(output_flnm),cmode=nf90_hdf5,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create RT_DOMAIN NetCDF file.')
  
      ! Write global attributes
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_output_valid_time',trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'Conventions',trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_version",trim(fileMeta%outVersion))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_version attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,'time',NF90_UNLIMITED,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
      iret = nf90_def_dim(ftn,'x',RT_DOMAIN(domainId)%g_ixrt,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
      iret = nf90_def_dim(ftn,'y',RT_DOMAIN(domainId)%g_jxrt,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
      iret = nf90_def_dim(ftn,'reference_time',1,dimId(4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(1),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(4),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create x/y coordinate variables
      iret = nf90_def_var(ftn,'x',nf90_double,dimId(2),xVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'long_name',trim(fileMeta%xLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'standard_name',trim(fileMeta%xStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'_CoordinateAxisType',trim(fileMeta%xAxisType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxisType attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'units',trim(fileMeta%xUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'resolution',fileMeta%xRes)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place resolution attribute into x coordinate variable')
      iret = nf90_def_var(ftn,'y',nf90_double,dimId(3),yVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'long_name',trim(fileMeta%yLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'standard_name',trim(fileMeta%yStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'_CoordinateAxisType',trim(fileMeta%yAxisType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxisType attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'units',trim(fileMeta%yUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'resolution',fileMeta%yRes)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place resolution attribute into y coordinate variable')

      ! Define compression for meta-variables only if nwmIo is set to 1.
      if(nlst_rt(1)%nwmIo .eq. 1) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
         iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')
      endif

      ! Create ProjectionCoordinateSystem variable
      iret = nf90_def_var(ftn,'ProjectionCoordinateSystem',nf90_char,varid=coordVarId)
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateTransformType',trim(fileMeta%projTransform))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateTransformType attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'transform_name',trim(fileMeta%projTransformName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place transform_name attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'grid_mapping_name',trim(fileMeta%projGridMappingName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping_name attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateAxes',trim(fileMeta%projCoordAxes))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxes attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'esri_pe_string',trim(fileMeta%projEsri))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'standard_parallel',fileMeta%stParallel)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_parallel attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'longitude_of_central_meridian',fileMeta%lonCtrlMer)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place longitude_of_central_meridian attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'latitude_of_projection_origin',fileMeta%latOrig)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place latitude_of_projection_origin attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'false_easting',fileMeta%falseEast)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to false_easting place attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'false_northing',fileMeta%falseNorth)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place false_northing attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'earth_radius',fileMeta%radius)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place earth_radius attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'proj4',trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into ProjectionCoordinateVariable')

      ! Create channel index variable.
      iret = nf90_def_var(ftn,'index',nf90_int,(/dimId(2),dimId(3)/),varid=indexVarId)
      iret = nf90_put_att(ftn,indexVarId,'_FillValue',-9999)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable index')
      iret = nf90_put_att(ftn,indexVarId,'missing_value',-9999)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable index')
      iret = nf90_put_att(ftn,indexVarId,'long_name','Streamflow Index Value')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable index')
      iret = nf90_put_att(ftn,indexVarId,'units','-')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable index')
      iret = nf90_put_att(ftn,indexVarId,'proj4',trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into variable: index')
      iret = nf90_put_att(ftn,indexVarId,'grid_mapping',trim(fileMeta%gridMapping))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: index')
      iret = nf90_put_att(ftn,indexVarId,'esri_pe_string',trim(fileMeta%projEsri))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into variable: index')
      ! Define compression for meta-variables only if nwmIo is set to 1.
      if(nlst_rt(1)%nwmIo .eq. 1) then
         iret = nf90_def_var_deflate(ftn,indexVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for index.')
      endif

      ! Loop through all possible variables and create them, along with their
      ! metadata attributes. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
            call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement.
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)

            ! Establish a compression level for the variables. For now we are
            ! using a
            ! compression level of 2. In addition, we are choosing to turn the
            ! shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent
            ! writing output.
            ! Only compress if nwmIo is set to 1.
            if(nlst_rt(1)%nwmIo .eq. 1) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'proj4',trim(fileMeta%proj4))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping',trim(fileMeta%gridMapping))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'esri_pe_string',trim(fileMeta%projEsri))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do ! end looping through variable output list.

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take RT_DOMAIN file out of definition mode')

      ! Read in coordinates from FullDom file. These will be placed into the
      ! output file coordinate variables. 
      allocate(xCoord(RT_DOMAIN(domainId)%g_ixrt))
      allocate(yCoord(RT_DOMAIN(domainId)%g_jxrt))
      allocate(yCoord2(RT_DOMAIN(domainId)%g_jxrt))
      iret = nf90_open(trim(nlst_rt(domainId)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnGeo)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to open FullDom file')
      iret = nf90_inq_varid(ftnGeo,'x',geoXVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find x coordinate in FullDom file')
      iret = nf90_get_var(ftnGeo,geoXVarId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract x coordinate from FullDom file')
      iret = nf90_inq_varid(ftnGeo,'y',geoYVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find y coordinate in FullDom file')
      iret = nf90_get_var(ftnGeo,geoYVarId,yCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract y coordinate from FullDom file')
      iret = nf90_close(ftnGeo)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close geoGrid file.')

      ! Reverse Y coordinates. They are read in reverse. 
      jTmp2 = 0
      do jTmp = RT_DOMAIN(domainId)%g_jxrt,1,-1
         jTmp2 = jTmp2 + 1
         yCoord2(jTmp2) = yCoord(jTmp)
      end do
      ! Place coordinate values into output file
      iret = nf90_inq_varid(ftn,'x',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate x coordinate variable.')
      iret = nf90_put_var(ftn,varId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into x coordinate variable')
      iret = nf90_inq_varid(ftn,'y',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate y coordinate variable')
      iret = nf90_put_var(ftn,varId,yCoord2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into y coordinate variable')
      deallocate(xCoord)
      deallocate(yCoord)
      deallocate(yCoord2)

      ! Place streamflow index values into output file.
      iret = nf90_inq_varid(ftn,'index',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate index variable.')
      iret = nf90_put_var(ftn,varId,CH_NETLNK,(/1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt/))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CH_NETLNK values into index variable.')

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Since the only variable we are "looping" over for output is streamflow,
      ! handle below. If other variables are added later, we can modify this
      ! section.
      do jTmp=1,RT_DOMAIN(domainId)%g_jxrt
         do iTmp=1,RT_DOMAIN(domainId)%g_ixrt
            if(CH_NETLNK(iTmp,jTmp).GE.0) then
               tmpFlow(iTmp,jTmp) = NINT((g_qlink(CH_NETLNK(iTmp,jTmp),1)-fileMeta%addOffset(1))/fileMeta%scaleFactor(1))
            else
                tmpFlow(iTmp,jTmp) = fileMeta%fillComp(1)
            endif
         enddo
      enddo

      ! Place streamflow grid into output file.
      iret = nf90_inq_varid(ftn,'streamflow',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate streamflow variable.')
      iret = nf90_put_var(ftn,varId,tmpFlow,(/1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt,1/))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place streamflow values into CHRTOUT_GRID')

   endif ! End if statement if on I/O ID

! Synce up processes.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND 
      call mpp_land_sync()
#endif
   endif

   if(myId .eq. 0) then
      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close RT_DOMAIN file.')
   endif

   ! Deallocate memory as needed
   deallocate(g_qlink, CH_NETLNK, tmpFlow, strFlowLocal)

end subroutine output_chrtout_grd_NWM

!===============================================================================
! Program Name: output_lsmOut_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine fro diagnostic LSM grids.
! History Log:
! 8/9/17 -Created, LRK.
! Usage:
! Parameters: None.
! Input Files. None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_lsmOut_NWM(domainId)
  use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
#endif
   implicit none

   ! Subroutine arguments
   integer, intent(in) :: domainId

   ! Derived types.
   type(lsmMeta) :: fileMeta

   ! Local variables
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDateTmp ! Start of model simulation, represented as a string. 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: mppFlag, diagFlag
   character(len=1024) :: output_flnm ! Output file name
   integer :: iret ! NetCDF return status
   integer :: ftn  ! NetCDF file handle
   integer :: dimId(4) ! NetCDF dimension ID values
   integer :: varId ! NetCDF variable ID value
   integer :: timeId ! NetCDF time variable ID
   integer :: refTimeId ! NetCDF reference_time variable ID
   integer :: coordVarId ! NetCDF coordinate variable ID
   integer :: xVarId,yVarId ! NetCDF x/y variable ID
   integer :: ierr, myId ! MPI related values
   !integer :: varRange(2) ! Local storage of valid min/max values
   real :: varRange(2) ! Local storage of valid min/max values
   integer :: iTmp,jTmp,iTmp2,jTmp2
   integer :: ftnGeo,geoXVarId,geoYVarId
   integer :: waterVal ! Value in HRLDAS in WRFINPUT file used to define water bodies for masking
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2
   real :: varRealTmp
   real, allocatable, dimension(:,:) :: localRealTmp, globalOutReal
   !integer, allocatable, dimension(:,:) :: globalCompTmp, localCompTmp

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! Sync up processes. 
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Call routine to initialize metadata structure
   call initLsmOutDict(fileMeta,myId,diagFlag)

   ! Initialize water type to 16.
   ! NOTE THIS MAY CHANGE IN THE FUTURE!!!!!
   waterVal = 16

   ! Calculate necessary datetime information that will go into the output file.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDateTmp = trim(nlst_rt(1)%startdate(1:4)//"-"//&
                       nlst_rt(1)%startdate(6:7)//&
                       &"-"//nlst_rt(1)%startdate(9:10)//" "//&
                       nlst_rt(1)%startdate(12:13)//":"//&
                       nlst_rt(1)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDateTmp,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(1)%olddate(1:4)//'-'//&
                    nlst_rt(1)%olddate(6:7)//'-'//&
                    nlst_rt(1)%olddate(9:10)//'_'//&
                    nlst_rt(1)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(1)%startdate(1:4)//'-'//&
                  nlst_rt(1)%startdate(6:7)//'-'//&
                  nlst_rt(1)%startdate(9:10)//'_'//&
                  nlst_rt(1)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! For now, will always default to outputting all available
   ! variables since the nature of this output file is
   ! diagnostic in nature.
   fileMeta%outFlag(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1]

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   if(myId .eq. 0) then
      ! We are on the I/O node. Create output file.
      write(output_flnm,'(A12,".LSMOUT_DOMAIN",I1)') nlst_rt(domainId)%olddate(1:4)//&
            nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
            nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16)//  &
            nlst_rt(domainId)%hgrid
  
      iret = nf90_create(trim(output_flnm),cmode=nf90_hdf5,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create LSMOUT NetCDF file.')

      ! Write global attributes
      iret = nf90_put_att(ftn,NF90_GLOBAL,'TITLE',trim(fileMeta%title))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place TITLE attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_output_valid_time',trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'Conventions',trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_version",trim(fileMeta%outVersion))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_version attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,'time',NF90_UNLIMITED,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
      iret = nf90_def_dim(ftn,'x',global_nx,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
      iret = nf90_def_dim(ftn,'y',global_ny,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
      iret = nf90_def_dim(ftn,'reference_time',1,dimId(4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(1),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(4),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create x/y coordinate variables
      iret = nf90_def_var(ftn,'x',nf90_double,dimId(2),xVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'long_name',trim(fileMeta%xLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'standard_name',trim(fileMeta%xStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'_CoordinateAxisType',trim(fileMeta%xAxisType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxisType attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'units',trim(fileMeta%xUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into x coordinate variable')
      iret = nf90_put_att(ftn,xVarId,'resolution',fileMeta%xRes)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place resolution attribute into x coordinate variable')
      iret = nf90_def_var(ftn,'y',nf90_double,dimId(3),yVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'long_name',trim(fileMeta%yLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'standard_name',trim(fileMeta%yStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'_CoordinateAxisType',trim(fileMeta%yAxisType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxisType attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'units',trim(fileMeta%yUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into y coordinate variable')
      iret = nf90_put_att(ftn,yVarId,'resolution',fileMeta%yRes)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place resolution attribute into y coordinate variable')

      ! Define compression for meta-variables. Only compress if nwmIo is set
      ! to 1.
      if(nlst_rt(1)%nwmIo .eq. 1) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
         iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')
      endif

      ! Create ProjectionCoordinateSystem variable
      iret = nf90_def_var(ftn,'ProjectionCoordinateSystem',nf90_char,varid=coordVarId)
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateTransformType',trim(fileMeta%projTransform))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateTransformType attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'transform_name',trim(fileMeta%projTransformName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place transform_name attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'grid_mapping_name',trim(fileMeta%projGridMappingName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping_name attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateAxes',trim(fileMeta%projCoordAxes))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxes attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'esri_pe_string',trim(fileMeta%projEsri))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'standard_parallel',fileMeta%stParallel)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_parallel attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'longitude_of_central_meridian',fileMeta%lonCtrlMer)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place longitude_of_central_meridian attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'latitude_of_projection_origin',fileMeta%latOrig)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place latitude_of_projection_origin attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'false_easting',fileMeta%falseEast)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to false_easting place attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'false_northing',fileMeta%falseNorth)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place false_northing attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'earth_radius',fileMeta%radius)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place earth_radius attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'proj4',trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into ProjectionCoordinateVariable')

      ! Loop through all possible variables and create them, along with their
      ! metadata attributes. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            !iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(3),dimId(1)/),varId)
            call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement.
            !varRange(1) = fileMeta%validMinComp(iTmp)
            !varRange(2) = fileMeta%validMaxComp(iTmp)
            varRange(1) = fileMeta%validMinReal(iTmp)
            varRange(2) = fileMeta%validMaxReal(iTmp)

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing output.
            ! Only compress if nwmIo is set to 1.
            if(nlst_rt(1)%nwmIo .eq. 1) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            !iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillReal(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            !iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingReal(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            !iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
            !iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
            !iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'proj4',trim(fileMeta%proj4))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping',trim(fileMeta%gridMapping))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'esri_pe_string',trim(fileMeta%projEsri))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into variable: '//trim(fileMeta%varNames(iTmp)))
         endif ! End if output flag is on
      end do ! end looping through variable output list.

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take LSMOUT file out of definition mode')

      ! Read in coordinates from GeoGrid file. These will be placed into the
      ! output file coordinate variables. 
      allocate(xCoord(global_nx))
      allocate(yCoord(global_ny))
      allocate(yCoord2(global_ny))
      iret = nf90_open(trim(nlst_rt(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnGeo)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid. Warn the user no
         ! file was found, and set x/y coordinates to -9999.0
         call postDiagMsg(diagFlag,'WARNING: Unable to find LAND spatial metadata file')
         xCoord = -9999.0
         yCoord = -9999.0
         yCoord2 = -9999.0
      else
         iret = nf90_inq_varid(ftnGeo,'x',geoXVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x coordinate in geoGrid file')
         iret = nf90_get_var(ftnGeo,geoXVarId,xCoord)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract x coordinate from geoGrid file')
         iret = nf90_inq_varid(ftnGeo,'y',geoYVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find y coordinate in geoGrid file')
         iret = nf90_get_var(ftnGeo,geoYVarId,yCoord)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract y coordinate from geoGrid file')
         iret = nf90_close(ftnGeo)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close geoGrid file.')
         ! Reverse Y coordinates. They are read in reverse. 
         jTmp2 = 0
         do jTmp = global_ny,1,-1
            jTmp2 = jTmp2 + 1
            yCoord2(jTmp2) = yCoord(jTmp)
         end do
      endif

      ! Place coordinate values into output file
      iret = nf90_inq_varid(ftn,'x',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate x coordinate variable.')
      iret = nf90_put_var(ftn,varId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into x coordinate variable')
      iret = nf90_inq_varid(ftn,'y',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate y coordinate variable')
      iret = nf90_put_var(ftn,varId,yCoord2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into y coordinate variable')
      deallocate(xCoord)
      deallocate(yCoord)
      deallocate(yCoord2)

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

   end if ! End if we are on the I/O processor.

   ! Sync up all processes
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Allocate temporary local memory
   !allocate(localCompTmp(rt_domain(domainId)%ix,rt_domain(domainId)%jx))
   allocate(localRealTmp(rt_domain(domainId)%ix,rt_domain(domainId)%jx))

   ! Loop through all possible variables to output. Collect the data to the
   ! global grid and output to the necessary NetCDF variable.
   do iTmp=1,fileMeta%numVars
      if(fileMeta%outFlag(iTmp) .eq. 1) then
         ! Allocate memory necessary
         if(myId .eq. 0) then
            !allocate(globalOutComp(global_nx,global_ny))
            allocate(globalOutReal(global_nx,global_ny))
         else
            !allocate(globalOutComp(1,1))
            allocate(globalOutReal(1,1))
         endif

         !globalOutComp = fileMeta%fillComp(iTmp)
         !localCompTmp = fileMeta%fillComp(iTmp)

         ! Sync up processes
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif

         ! Loop through the local array and convert floating point values
         ! to integer via scale_factor/add_offset. If the pixel value 
         ! falls within a water class value, leave as ndv.
         do iTmp2 = 1,rt_domain(domainId)%ix
            do jTmp2 = 1,rt_domain(domainId)%jx
               if(iTmp .eq. 1) then
                  varRealTmp = rt_domain(domainId)%stc(iTmp2,jTmp2,1)
               else if(iTmp .eq. 2) then
                  varRealTmp = rt_domain(domainId)%smc(iTmp2,jTmp2,1)
               else if(iTmp .eq. 3) then
                  varRealTmp = rt_domain(domainId)%sh2ox(iTmp2,jTmp2,1)
               else if(iTmp .eq. 4) then
                  varRealTmp = rt_domain(domainId)%stc(iTmp2,jTmp2,2)
               else if(iTmp .eq. 5) then
                  varRealTmp = rt_domain(domainId)%smc(iTmp2,jTmp2,2)
               else if(iTmp .eq. 6) then
                  varRealTmp = rt_domain(domainId)%sh2ox(iTmp2,jTmp2,2)
               else if(iTmp .eq. 7) then
                  varRealTmp = rt_domain(domainId)%stc(iTmp2,jTmp2,3)
               else if(iTmp .eq. 8) then
                  varRealTmp = rt_domain(domainId)%smc(iTmp2,jTmp2,3)
               else if(iTmp .eq. 9) then
                  varRealTmp = rt_domain(domainId)%sh2ox(iTmp2,jTmp2,3)
               else if(iTmp .eq. 10) then
                  varRealTmp = rt_domain(domainId)%stc(iTmp2,jTmp2,4)
               else if(iTmp .eq. 11) then
                  varRealTmp = rt_domain(domainId)%smc(iTmp2,jTmp2,4)
               else if(iTmp .eq. 12) then
                  varRealTmp = rt_domain(domainId)%sh2ox(iTmp2,jTmp2,4)
               else if(iTmp .eq. 13) then
                  varRealTmp = rt_domain(domainId)%INFXSRT(iTmp2,jTmp2)
               else if(iTmp .eq. 14) then
                  varRealTmp = rt_domain(domainId)%SFCHEADRT(iTmp2,jTmp2)
               endif

               ! For now, we are foregoing converting these variables to integer
               ! via scale_factor/add_offset. This file is meant for diagnostic
               ! purposes, so we want to keep full precision.
               localRealTmp(iTmp2,jTmp2) = varRealTmp

               ! If we are on time 0, make sure we don't need to fill in the
               ! grid with NDV values. 
               !if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               !   localCompTmp(iTmp2,jTmp2) = fileMeta%fillComp(iTmp)
               !else
               !   if(varRealTmp .eq. fileMeta%modelNdv) then
               !      localCompTmp(iTmp2,jTmp2) = INT(fileMeta%fillComp(iTmp))
               !   else
               !      localCompTmp(iTmp2,jTmp2) = NINT((varRealTmp-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
               !   endif
               !   if(vegTyp(iTmp2,jTmp2) .eq. waterVal) then
               !      localCompTmp(iTmp2,jTmp2) = INT(fileMeta%fillComp(iTmp))
               !   endif
               !endif
            enddo
         enddo
         ! Collect local 2D arrays to global 2D array
         ! Sync all processes up.
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            !call write_IO_int(localCompTmp,globalCompTmp)
            call write_IO_real(localRealTmp,globalOutReal)
#endif
         else
            !globalCompTmp = localCompTmp
            globalOutReal = localRealTmp
         endif
         ! Sync all processes up.
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif

         ! Write array out to NetCDF file
         if(myId .eq. 0) then
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))
            !iret = nf90_put_var(ftn,varId,globalOutComp,(/1,1,1/),(/global_nx,global_ny,1/))
            iret = nf90_put_var(ftn,varId,globalOutReal,(/1,1,1/),(/global_nx,global_ny,1/))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif

         !deallocate(globalCompTmp)
         deallocate(globalOutReal)
      endif
   enddo

   if(myId .eq. 0) then
      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close LSMOUT_DOMAIN file.')
   endif

   !deallocate(localCompTmp)
   deallocate(localRealTmp)

end subroutine output_lsmOut_NWM

!==============================================================================
! Program Name: output_frxstPts
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output frxstPts ASCII file from streamflow at forecast points
! defined in the Fulldom file.
! History Log:
! 9/18/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.
subroutine output_frxstPts(domainId)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
#ifdef MPP_LAND
   use module_mpp_land
#endif
implicit none

   ! Pass in "did" value from hydro driving program. 
   integer, intent(in) :: domainId

   ! Derived types.
   !type(chrtMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag, ierr, myId
   integer :: seconds_since
   integer :: gSize, iTmp, numPtsOut
   integer, allocatable, dimension(:) :: g_STRMFRXSTPTS, g_outInd
   real, allocatable, dimension(:,:) :: g_qlink, g_qlinkOut
   real, allocatable, dimension(:) :: g_chlat, g_chlon, g_hlink, strFlowLocal
   integer, allocatable, dimension(:) :: frxstPtsLocal, g_STRMFRXSTPTSOut
   real, allocatable, dimension(:) :: g_chlatOut, g_chlonOut, g_hlinkOut

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   if(nlst_rt(domainId)%frxst_pts_out .eq. 0) then
      ! No output requested here, return to parent calling program/subroutine.
      return
   endif

   ! Also, we are only allowing a split_output_count of 1.
   if(nlst_rt(domainId)%split_output_count .ne. 1) then
      call nwmCheck(diagFlag,1,'ERROR: split_output_count of 1 only allowed.')
   endif

   ! For now, only allow frxstPts for gridded routing. 
   if(nlst_rt(domainId)%channel_option .ne. 3) then
      call nwmCheck(diagFlag,1,'ERROR: Only channel_option of 3 allowed.')
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file. A couple variables from this type are needed for
   ! frxstPts.
   !call initChrtDict(fileMeta,diagFlag,myId)

   ! Calculate datetime information
   seconds_since = int(nlst_rt(1)%out_dt*60*(rt_domain(1)%out_counts-1))
   
   ! First step is to allocate a global array of index values. This "index"
   ! array will be used to subset after collection has taken place. Also,
   ! the sum of this array will be used to determine the size of the output 
   ! arrays. 
   if(mppFlag .eq. 1) then
      gSize = rt_domain(domainId)%gnlinks
      if(rt_domain(domainId)%gnlinksl .gt. rt_domain(domainId)%gnlinks) then
         gSize = rt_domain(domainId)%gnlinksl
      endif

      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      if(myId .eq. 0) then
         allocate(g_STRMFRXSTPTS(gSize))
         allocate(g_outInd(gSize))
         allocate(g_qlink(gSize,2))
         allocate(g_chlat(gSize))
         allocate(g_chlon(gSize))
         allocate(g_hlink(gSize))
      else
         allocate(g_STRMFRXSTPTS(1))
         allocate(g_outInd(1))
         allocate(g_qlink(1,2))
         allocate(g_chlat(1))
         allocate(g_chlon(1))
         allocate(g_hlink(1))
      endif

      ! Initialize the index array to 0
      g_outInd = 0

      ! Allocate local streamflow arrays. We need to do a check to
      ! for lake_type 2. However, we cannot set the values in the global array 
      ! to missing as this causes the model to crash.
      allocate(strFlowLocal(RT_DOMAIN(domainId)%NLINKS))
      allocate(frxstPtsLocal(RT_DOMAIN(domainId)%NLINKS))
      strFlowLocal = RT_DOMAIN(domainId)%QLINK(:,1)
      frxstPtsLocal = rt_domain(domainId)%STRMFRXSTPTS

      ! Sync everything up before the next step.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Loop through all the local links on this processor. For lake_type
      ! of 2, we need to manually set the streamflow values
      ! to the model NDV value.
      if (RT_DOMAIN(domainId)%NLAKES .gt. 0) then
         do iTmp=1,RT_DOMAIN(domainId)%NLINKS
            if (RT_DOMAIN(domainId)%TYPEL(iTmp) .eq. 2) then
               !strFlowLocal(iTmp) = fileMeta%modelNdv
               strFlowLocal(iTmp) = -9.E15
               frxstPtsLocal(iTmp) = -9999
            endif
         end do
      endif

      ! Collect arrays from various processors
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         call write_chanel_int(frxstPtsLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_STRMFRXSTPTS)
         call write_chanel_real(strFlowLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,1))
         call write_chanel_real(RT_DOMAIN(domainId)%QLINK(:,2),rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,2))
         call write_chanel_real(RT_DOMAIN(domainId)%CHLAT,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlat)
         call write_chanel_real(RT_DOMAIN(domainId)%CHLON,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlon)
         call write_chanel_real(RT_DOMAIN(domainId)%HLINK,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_hlink)
      endif
      
      deallocate(strFlowLocal)
      deallocate(frxstPtsLocal)

   else
      ! Running sequentially on a single processor.
      gSize = rt_domain(domainId)%nlinks
      allocate(g_STRMFRXSTPTS(gSize))
      allocate(g_outInd(gSize))
      allocate(g_chlon(gSize))
      allocate(g_chlat(gSize))
      allocate(g_hlink(gSize))
      allocate(g_qlink(gSize,2))

      ! Initialize the index array to 0
      g_outInd = 0

      g_STRMFRXSTPTS = rt_domain(domainId)%STRMFRXSTPTS
      g_chlon = RT_DOMAIN(domainId)%CHLON
      g_chlat = RT_DOMAIN(domainId)%CHLAT
      g_hlink = RT_DOMAIN(domainId)%HLINK
      g_qlink = RT_DOMAIN(domainId)%QLINK
   endif
      
   if(myId .eq. 0) then
      ! Set index values to 1 where we have forecast points. 
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         where(g_STRMFRXSTPTS .ne. -9999) g_outInd = 1
      endif
      ! PLUG FOR CHECK FOR GAGES IN ROUTELINK WITH CHANNEL ROUTING 

      ! Allocate output arrays based on size of number of forecast points.
      numPtsOut = SUM(g_outInd)

      if(numPtsOut .eq. 0) then
         ! Write warning message to user showing there are NO forecast points to
         ! write. Simply return to the main calling function.
         call postDiagMsg(diagFlag,"WARNING: No forecast points found for frxstPtsOut.")
         return
      endif

      ! Allocate output arrays based on number of output forecast points. 
      allocate(g_STRMFRXSTPTSOut(numPtsOut))
      allocate(g_chlonOut(numPtsOut))
      allocate(g_chlatOut(numPtsOut))
      allocate(g_hlinkOut(numPtsOut))
      allocate(g_qlinkOut(numPtsOut,2))

      ! Subset global arrays for forecast points.
      g_STRMFRXSTPTSOut = PACK(g_STRMFRXSTPTS,g_outInd == 1)
      g_chlonOut = PACK(g_chlon,g_outInd == 1)
      g_chlatOut = PACK(g_chlat,g_outInd == 1)
      g_hlinkOut = PACK(g_hlink,g_outInd == 1)
      g_qlinkOut(:,1) = PACK(g_qlink(:,1),g_outInd == 1)
      g_qlinkOut(:,2) = PACK(g_qlink(:,2),g_outInd == 1)

      ! Open the output file.
      open (unit=55,file='frxst_pts_out.txt',status='unknown',position='append')
   
      ! Loop through forecast points and write output.
      do iTmp=1,numPtsOut
         if(nlst_rt(domainId)%channel_option .eq. 3) then 
            ! Instead of a gage ID, we are simply going to output the forecast
            ! point number assigned during the pre-processing.
117         FORMAT(I8,",",A10,1X,A8,",",I7,",",F10.5,",",F8.5,",",F9.3,",",F12.3,",",F6.3)
            write(55,117) seconds_since, nlst_rt(domainId)%olddate(1:18),&
                          nlst_rt(domainId)%olddate(12:19),&
                          g_STRMFRXSTPTSOut(iTmp),g_chlonOut(iTmp),&
                          g_chlatOut(iTmp),g_qlinkOut(iTmp,1),&
                          g_qlinkOut(iTmp,1)*35.314666711511576,&
                          g_hlinkOut(iTmp)
         else
            ! PLUG FOR CHANNEL ROUTING
            numPtsOut = numPtsOut
         endif
      end do  

      ! Close the output file
      close(55) 
   else
      allocate(g_STRMFRXSTPTSOut(1))
      allocate(g_chlonOut(1))
      allocate(g_chlatOut(1))
      allocate(g_hlinkOut(1))
      allocate(g_qlinkOut(1,2)) 
   endif

   ! Deallocate memory
   deallocate(g_STRMFRXSTPTS)
   deallocate(g_STRMFRXSTPTSOut)
   deallocate(g_chlonOut)
   deallocate(g_chlatOut)
   deallocate(g_hlinkOut)
   deallocate(g_qlinkOut)
   deallocate(g_chlat)
   deallocate(g_chlon)
   deallocate(g_hlink)
   deallocate(g_qlink)
   deallocate(g_outInd)

end subroutine output_frxstPts

!==============================================================================
! Program Name: output_chanObs_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for channel points at predefined forecast points.
! History Log:
! 9/19/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.
subroutine output_chanObs_NWM(domainId)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
   use module_mpp_land
   !use module_mpp_reachls,  only: ReachLS_write_io
#endif
   implicit none

   ! Pass in "did" value from hydro driving program. 
   integer, intent(in) :: domainId

   ! Derived types.
   type(chObsMeta) :: fileMeta

   ! Local variables
   integer :: nudgeFlag, mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(3) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: featureVarId, elevVarId, orderVarId ! Misc NetCDF variable id values
   integer :: latVarId, lonVarId ! Lat/lon NetCDF variable id values.
   integer :: varRange(2) ! Local storage of min/max valid range values.
   integer :: gSize ! Global size of channel point array. 
   integer :: numPtsOut ! Number of forecast/gage points
   integer :: iTmp, indTmp ! Misc integer values. 
   integer :: ierr, myId ! MPI return status, process ID
   ! Establish local, allocatable arrays
   ! These are used to hold global output arrays, and global output arrays after
   ! sorting has taken place by ascending feature_id value. 
   real, allocatable, dimension(:) :: strFlowLocal,velocityLocal
   real, allocatable, dimension(:,:) :: g_qlink
   integer, allocatable, dimension(:) :: g_linkid,g_order
   real, allocatable, dimension(:) :: g_chlat,g_chlon,g_hlink,g_zelev
   !real, allocatable, dimension(:) :: g_QLateral,g_velocity
   !real, allocatable, dimension(:) :: g_nudge,g_qSfcLatRunoff
   !real, allocatable, dimension(:) :: g_qBucket,g_qBtmVertRunoff,g_accBucket
   !real*8, allocatable, dimension(:) :: g_accSfcLatRunoff
   real, allocatable, dimension(:,:) :: g_qlinkOut
   integer, allocatable, dimension(:) :: g_orderOut,g_linkidOut
   real, allocatable, dimension(:) :: g_chlatOut,g_chlonOut,g_hlinkOut,g_zelevOut
   !real, allocatable, dimension(:) :: g_QLateralOut,g_velocityOut
   !real, allocatable, dimension(:) :: g_nudgeOut,g_qSfcLatRunoffOut
   !real, allocatable, dimension(:) :: g_qBucketOut,g_qBtmVertRunoffOut,g_accBucketOut
   !real*8, allocatable, dimension(:) :: g_accSfcLatRunoffOut
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset
                                                     ! have been applied.
   integer, allocatable, dimension(:) :: g_STRMFRXSTPTS, g_outInd
   integer, allocatable, dimension(:) :: frxstPtsLocal, g_STRMFRXSTPTSOut

   ! Establish macro variables to hlep guide this subroutine. 
#ifdef WRF_HYDRO_NUDGING
   nudgeFlag = 1
#else
   nudgeFlag = 0
#endif

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   if(nlst_rt(domainId)%CHANOBS_DOMAIN .eq. 0) then
      ! No output requested here, return to parent calling program/subroutine.
      return
   endif

   ! Also, we are only allowing a split_output_count of 1.
   if(nlst_rt(domainId)%split_output_count .ne. 1) then
      call nwmCheck(diagFlag,1,'ERROR: split_output_count of 1 only allowed.')
   endif

   ! Since this is NWM output, we are assuming only channel_option 2 is
   ! acceptable.
   if(nlst_rt(domainId)%channel_option .ne. 3) then
      call nwmCheck(diagFlag,1,'ERROR: Only channel_option of 3 allowed.')
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initChanObsDict(fileMeta,diagFlag,myId)

   ! For now, keep all output variables on, regardless of IOC flag
   fileMeta%outFlag(:) = [1]

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim  
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00') 
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! Compose output file name.
   write(output_flnm,'(A12,".CHANOBS_DOMAIN",I1)')nlst_rt(domainId)%olddate(1:4)//&
         nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
         nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16),&
         nlst_rt(domainId)%igrid


   ! First step is to allocate a global array of index values. This "index"
   ! array will be used to subset after collection has taken place. Also,
   ! the sum of this array will be used to determine the size of the output 
   ! arrays. 
   if(mppFlag .eq. 1) then
      gSize = rt_domain(domainId)%gnlinks
      if(rt_domain(domainId)%gnlinksl .gt. rt_domain(domainId)%gnlinks) then
         gSize = rt_domain(domainId)%gnlinksl
      endif

      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      if(myId .eq. 0) then
         allocate(g_STRMFRXSTPTS(gSize))
         allocate(g_outInd(gSize))
         allocate(g_qlink(gSize,2))
         allocate(g_chlat(gSize))
         allocate(g_chlon(gSize))
         allocate(g_hlink(gSize))
         allocate(g_zelev(gSize))
         allocate(g_order(gSize))
         allocate(g_linkid(gSize))
      else
         allocate(g_STRMFRXSTPTS(1))
         allocate(g_outInd(1))
         allocate(g_qlink(1,2))
         allocate(g_chlat(1))
         allocate(g_chlon(1))
         allocate(g_hlink(1))
         allocate(g_zelev(1))
         allocate(g_order(1))
         allocate(g_linkid(1))
      endif

      ! Initialize the index array to 0
      g_outInd = 0

      ! Allocate local streamflow arrays. We need to do a check to
      ! for lake_type 2. However, we cannot set the values in the global array 
      ! to missing as this causes the model to crash.
      allocate(strFlowLocal(RT_DOMAIN(domainId)%NLINKS))
      allocate(frxstPtsLocal(RT_DOMAIN(domainId)%NLINKS))
      strFlowLocal = RT_DOMAIN(domainId)%QLINK(:,1)
      frxstPtsLocal = rt_domain(domainId)%STRMFRXSTPTS

      ! Sync everything up before the next step.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Loop through all the local links on this processor. For lake_type
      ! of 2, we need to manually set the streamflow values
      ! to the model NDV value.
      if (RT_DOMAIN(domainId)%NLAKES .gt. 0) then
         do iTmp=1,RT_DOMAIN(domainId)%NLINKS
            if (RT_DOMAIN(domainId)%TYPEL(iTmp) .eq. 2) then
               !strFlowLocal(iTmp) = fileMeta%modelNdv
               strFlowLocal(iTmp) = -9.E15
               frxstPtsLocal(iTmp) = -9999
            endif
         end do
      endif

      ! Collect arrays from various processors
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         call write_chanel_int(frxstPtsLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_STRMFRXSTPTS)
         call write_chanel_real(strFlowLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,1))
         call write_chanel_real(RT_DOMAIN(domainId)%QLINK(:,2),rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,2))
         call write_chanel_real(RT_DOMAIN(domainId)%CHLAT,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlat)
         call write_chanel_real(RT_DOMAIN(domainId)%CHLON,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlon)
         call write_chanel_real(RT_DOMAIN(domainId)%HLINK,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_hlink)
         call write_chanel_int(RT_DOMAIN(domainId)%linkid,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_linkid)
         call write_chanel_int(RT_DOMAIN(domainId)%ORDER,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_order)
         call write_chanel_real(RT_DOMAIN(domainId)%ZELEV,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_zelev)
      endif
      
      deallocate(strFlowLocal)
      deallocate(frxstPtsLocal)

   else
      ! Running sequentially on a single processor.
      gSize = rt_domain(domainId)%nlinks
      allocate(g_STRMFRXSTPTS(gSize))
      allocate(g_outInd(gSize))
      allocate(g_chlon(gSize))
      allocate(g_chlat(gSize))
      allocate(g_hlink(gSize))
      allocate(g_qlink(gSize,2))
      allocate(g_linkid(gSize))
      allocate(g_order(gSize))
      allocate(g_zelev(gSize))

      ! Initialize the index array to 0
      g_outInd = 0

      g_STRMFRXSTPTS = rt_domain(domainId)%STRMFRXSTPTS
      g_chlon = RT_DOMAIN(domainId)%CHLON
      g_chlat = RT_DOMAIN(domainId)%CHLAT
      g_hlink = RT_DOMAIN(domainId)%HLINK
      g_qlink = RT_DOMAIN(domainId)%QLINK
      g_linkid = RT_DOMAIN(domainId)%linkid
      g_order = RT_DOMAIN(domainId)%ORDER
      g_zelev = RT_DOMAIN(domainId)%ZELEV
   endif

   if(myId .eq. 0) then
      ! Set index values to 1 where we have forecast points. 
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         where(g_STRMFRXSTPTS .ne. -9999) g_outInd = 1
      endif
      ! PLUG FOR CHECK FOR GAGES IN ROUTELINK WITH CHANNEL ROUTING 

      ! Allocate output arrays based on size of number of forecast points.
      numPtsOut = SUM(g_outInd)

      if(numPtsOut .eq. 0) then
         ! Write warning message to user showing there are NO forecast points to
         ! write. Simply return to the main calling function.
         call postDiagMsg(diagFlag,"WARNING: No forecast points found for CHANOBS.")
         return
      endif

      ! Allocate output arrays based on number of output forecast points. 
      allocate(g_STRMFRXSTPTSOut(numPtsOut))
      allocate(g_chlonOut(numPtsOut))
      allocate(g_chlatOut(numPtsOut))
      allocate(g_hlinkOut(numPtsOut))
      allocate(g_qlinkOut(numPtsOut,2))
      allocate(g_linkidOut(numPtsOut))
      allocate(g_orderOut(numPtsOut))
      allocate(g_zelevOut(numPtsOut))

      ! Subset global arrays for forecast points.
      g_STRMFRXSTPTSOut = PACK(g_STRMFRXSTPTS,g_outInd == 1)
      g_chlonOut = PACK(g_chlon,g_outInd == 1)
      g_chlatOut = PACK(g_chlat,g_outInd == 1)
      g_hlinkOut = PACK(g_hlink,g_outInd == 1)
      g_qlinkOut(:,1) = PACK(g_qlink(:,1),g_outInd == 1)
      g_qlinkOut(:,2) = PACK(g_qlink(:,2),g_outInd == 1)
      g_linkidOut = PACK(g_linkid,g_outInd == 1)
      g_orderOut = PACK(g_order,g_outInd == 1)
      g_zelevOut = PACK(g_zelev,g_outInd == 1)

      allocate(varOutReal(fileMeta%numVars,numPtsOut))
      allocate(varOutInt(numPtsOut))

      varOutReal(1,:) = g_qlinkOut(:,1)

      ! Mask out missing values
      where ( varOutReal == fileMeta%modelNdv ) varOutReal = -9999.0

      ! Create NetCDF for output.
      iret = nf90_create(trim(output_flnm),cmode=nf90_hdf5,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create CHANOBS NetCDF file.')
      
      ! Write global attributes.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"featureType",trim(fileMeta%fType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create featureType attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_initialization_time",trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model init attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"station_dimension",trim(fileMeta%stDim))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create st. dimension attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_valid_time",trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model valid attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"stream_order_output",fileMeta%stOrder)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create order attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"cdm_datatype",trim(fileMeta%cdm))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create CDM attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"esri_pe_string",trim(fileMeta%esri))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create ESRI attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"Conventions",trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create conventions attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_version",trim(fileMeta%outVersion))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_version attribute')

      ! Create global attributes specific to running output through the
      ! channel-only configuration of the model.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_OVRTSWCRT",nlst_rt(domainId)%OVRTSWCRT)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_OVRTSWCRT attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_NOAH_TIMESTEP",int(nlst_rt(domainId)%dt))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_NOAH_TIMESTEP attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_channel_only",nlst_rt(domainId)%channel_only)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_channel_only attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_channelBucket_only",nlst_rt(domainId)%channelBucket_only)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_channelBucket_only attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'dev','dev_ prefix indicates development/internal meta data')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,"feature_id",numPtsOut,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id dimension')
      iret = nf90_def_dim(ftn,"time",NF90_UNLIMITED,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time dimension')
      iret = nf90_def_dim(ftn,"reference_time",1,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(2),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(3),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create feature_id variable
      iret = nf90_def_var(ftn,"feature_id",nf90_int,dimId(1),featureVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id variable.')
      ! Specify these attributes based on channel routing methods specified by
      ! user.
      if(nlst_rt(domainId)%channel_option .eq. 3) then 
         iret = nf90_put_att(ftn,featureVarId,'long_name','User Specified Forecast Points')
         call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
         iret = nf90_put_att(ftn,featureVarId,'comment','Forecast Points Specified in Geogrid file')
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      else
         iret = nf90_put_att(ftn,featureVarId,'long_name',trim(fileMeta%featureIdLName))
         call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
         iret = nf90_put_att(ftn,featureVarId,'comment',trim(fileMeta%featureIdComment))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      endif
      iret = nf90_put_att(ftn,featureVarId,'cf_role',trim(fileMeta%cfRole))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place cf_role attribute into feature_id variable')

      ! Create channel lat/lon variables
      iret = nf90_def_var(ftn,"latitude",nf90_float,dimId(1),latVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create latitude variable.')
      iret = nf90_put_att(ftn,latVarId,'long_name',trim(fileMeta%latLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'standard_name',trim(fileMeta%latStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'units',trim(fileMeta%latUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into latitude variable')
      iret = nf90_def_var(ftn,"longitude",nf90_float,dimId(1),lonVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create longitude variable.')
      iret = nf90_put_att(ftn,lonVarId,'long_name',trim(fileMeta%lonLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'standard_name',trim(fileMeta%lonStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'units',trim(fileMeta%lonUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into longitude variable')

      ! Create channel order variable
      iret = nf90_def_var(ftn,"order",nf90_int,dimId(1),orderVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create order variable.')
      iret = nf90_put_att(ftn,orderVarId,'long_name',trim(fileMeta%orderLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into order variable')
      iret = nf90_put_att(ftn,orderVarId,'standard_name',trim(fileMeta%orderStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into order variable')

      ! Create channel elevation variable
      iret = nf90_def_var(ftn,"elevation",nf90_float,dimId(1),elevVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create elevation variable.')
      iret = nf90_put_att(ftn,elevVarId,'long_name',trim(fileMeta%elevLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into elevation variable')
      iret = nf90_put_att(ftn,elevVarId,'standard_name',trim(fileMeta%elevStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into elevation variable')

      ! Define deflation levels for these meta-variables. For now, we are going
      ! to
      ! default to a compression level of 2. Only compress if nwmIo is set to 1.
      if(nlst_rt(1)%nwmIo .eq. 1) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,latVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for latitude.')
         iret = nf90_def_var_deflate(ftn,lonVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for longitude.') 
         iret = nf90_def_var_deflate(ftn,orderVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for order.')  
         iret = nf90_def_var_deflate(ftn,elevVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for elevation.')  
      endif
      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout
      ! the
      ! output looping below for conversion to compressed integer values.
      ! Loop through and create each output variable, create variable
      ! attributes,
      ! and insert data.
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! First create variable
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,dimId(1),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)

            ! Establish a compression level for the variables. For now we are
            ! using a
            ! compression level of 2. In addition, we are choosing to turn the
            ! shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent
            ! writing
            ! output. Only compress if nwmIo is set to 1.
            if(nlst_rt(1)%nwmIo .eq. 1) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'coordinates',trim(fileMeta%coordNames(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place coordinates attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
         endif
      end do 

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take CHRTOUT file out of definition mode')

      ! Loop through all possible output variables, and convert floating
      ! points
      ! to integers via prescribed scale_factor/add_offset, then write to the
      ! NetCDF variable. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer. If this is time zero, check to see if we
            ! need to convert all data to NDV
            if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               varOutInt(:) = fileMeta%fillComp(iTmp)
            else
               varOutInt(:) = NINT((varOutReal(iTmp,:)-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
            endif
            ! Get NetCDF variable id.
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))
 
            ! Put data into NetCDF file
            iret = nf90_put_var(ftn,varId,varOutInt)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do

      ! Place link ID values into the NetCDF file
      iret = nf90_inq_varid(ftn,'feature_id',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      ! If we are running gridded routing, output the user-specified forecast
      ! point numbers. Otherwise, output the reach ID values. 
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         iret = nf90_put_var(ftn,varId,g_STRMFRXSTPTSOut)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')
      else
         iret = nf90_put_var(ftn,varId,g_linkidOut)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')
      endif  

      iret = nf90_inq_varid(ftn,'latitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate latitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_chlatOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into latitude output variable.')

      iret = nf90_inq_varid(ftn,'longitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate longitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_chlonOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into longitude output variable.')

      iret = nf90_inq_varid(ftn,'order',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate order in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_orderOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into order output variable.')

      iret = nf90_inq_varid(ftn,'elevation',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate elevation in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_zelevOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into elevation output variable.')

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Close the output file
      iret = nf90_close(ftn) 
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close CHANOBS file.')

      deallocate(varOutReal)
      deallocate(varOutInt)
   else
      allocate(g_STRMFRXSTPTSOut(1))
      allocate(g_chlonOut(1))
      allocate(g_chlatOut(1))
      allocate(g_hlinkOut(1))
      allocate(g_qlinkOut(1,2)) 
      allocate(g_linkidOut(1))
      allocate(g_zelevOut(1))
      allocate(g_orderOut(1))
   endif

   ! Deallocate memory
   deallocate(g_STRMFRXSTPTS)
   deallocate(g_STRMFRXSTPTSOut)
   deallocate(g_chlonOut)
   deallocate(g_chlatOut)
   deallocate(g_hlinkOut)
   deallocate(g_qlinkOut)
   deallocate(g_linkidOut)
   deallocate(g_zelevOut)
   deallocate(g_orderOut)
   deallocate(g_chlat)
   deallocate(g_chlon)
   deallocate(g_hlink)
   deallocate(g_qlink)
   deallocate(g_outInd)
   deallocate(g_linkid)
   deallocate(g_zelev)
   deallocate(g_order)


end subroutine output_chanObs_NWM

subroutine postDiagMsg(diagFlag,diagMsg)
   implicit none

   ! Subroutine arguments.
   integer, intent(in) :: diagFlag
   character(len=*), intent(in) :: diagMsg

   ! Only write out message if the diagnostic WRF_HYDRO_D flag was
   ! set to 1
   if (diagFlag .eq. 1) then
      print*, trim(diagMsg)
   end if

end subroutine postDiagMsg
   
subroutine nwmCheck(diagFlag,iret,msg)
   implicit none
 
   ! Subroutine arguments.
   integer, intent(in) :: diagFlag,iret
   character(len=*), intent(in) :: msg

   ! Check status. If status of command is not 0, then post the error message
   ! if WRF_HYDRO_D was set to be 1.
   if (iret .ne. 0) then
      call hydro_stop(trim(msg))
   end if

end subroutine nwmCheck

end module module_NWM_io
