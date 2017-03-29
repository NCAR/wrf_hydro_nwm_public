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
   integer :: featureVarId, elevVarId, orderVarId
   integer :: latVarId, lonVarId
   integer :: varRange(2)
   integer :: gSize
   integer :: indVarId,ftnRt ! values related to extraction of ascending order index values from the RouteLink file.
   integer :: iTmp, indTmp
   integer :: ierr, myId ! MPI return status, process ID
   ! Establish local, allocatable arrays
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
   if(nlst_rt(domainId)%CHRTOUT_DOMAIN .ne. 2) then
      call nwmCheck(diagFlag,1,'ERROR: CHRTOUT_DOMAIN value of 0 or 2 only allowed.')
   endif

   ! Also, we are only allowing a split_output_count of 1.
   if(nlst_rt(domainId)%split_output_count .ne. 1) then
      call nwmCheck(diagFlag,1,'ERROR: split_output_count of 1 only allowed.')
   endif

   ! Since this is NWM output, we are assuming only channel_option 2 is acceptable.
   if(nlst_rt(domainId)%channel_option .ne. 2) then
      call nwmCheck(diagFlag,1,'ERROR: Only channel_option of 2 allowed.')
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
   else
      myId = 0
   endif

   ! First step is to collect and assemble all data that will be written to the 
   ! NetCDF file. If we are not using MPI, we bypass the collection step through
   ! MPI. 
   if(mppFlag .eq. 1) then
      gSize = rt_domain(domainId)%gnlinksl

      ! Sync all processes up.
      call mpp_land_sync()

      if(myId .eq. 0) then
         ! Allocate memory for output.
         !allocate(g_chlon(gsize))
         !allocate(g_chlat(gsize))
         allocate(g_hlink(gsize))
         !allocate(g_zelev(gsize))
         allocate(g_qlink(gsize,2))
         !allocate(g_order(gsize))
         allocate(g_linkid(gsize))
         allocate(g_QLateral(gsize))
         allocate(g_velocity(gsize))
         allocate(g_nudge(gsize))
         allocate(g_qSfcLatRunoff(gsize))
         allocate(g_qBucket(gsize))
         allocate(g_qBtmVertRunoff(gsize))
         allocate(g_accSfcLatRunoff(gsize))
         allocate(g_accBucket(gsize))
         ! Allocate memory for output.
         !allocate(g_chlonOut(gsize))
         !allocate(g_chlatOut(gsize))
         allocate(g_hlinkOut(gsize))
         !allocate(g_zelevOut(gsize))
         allocate(g_qlinkOut(gsize,2))
         !allocate(g_orderOut(gsize))
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
         !allocate(g_chlon(1))
         !allocate(g_chlat(1))
         allocate(g_hlink(1))
         !allocate(g_zelev(1))
         allocate(g_qlink(1,2))
         !allocate(g_order(1))
         allocate(g_linkid(1))
         allocate(g_QLateral(1))
         allocate(g_velocity(1))
         allocate(g_nudge(1))
         allocate(g_qSfcLatRunoff(1))
         allocate(g_qBucket(1))
         allocate(g_qBtmVertRunoff(1))
         allocate(g_accSfcLatRunoff(1))
         allocate(g_accBucket(1))
      endif

      ! Sync everything up before the next step.
      call mpp_land_sync()

      ! Collect arrays from various processors through MPI, and 
      ! assemble into global arrays previously allocated.
      g_qlink = 0
#ifdef MPP_LAND
      call ReachLS_write_io(RT_DOMAIN(domainId)%QLINK(:,1),g_qlink(:,1))
      call ReachLS_write_io(RT_DOMAIN(domainId)%QLINK(:,2),g_qlink(:,2))
      !call ReachLS_write_io(RT_DOMAIN(domainId)%ORDER,g_order)
      call ReachLS_write_io(RT_DOMAIN(domainId)%linkid,g_linkid)
      !call ReachLS_write_io(RT_DOMAIN(domainId)%CHLAT,g_chlat)
      !call ReachLS_write_io(RT_DOMAIN(domainId)%CHLON,g_chlon)
      !call ReachLS_write_io(RT_DOMAIN(domainId)%ZELEV,g_zelev)
      call ReachLS_write_io(RT_DOMAIN(domainId)%QLateral,g_QLateral)
      call ReachLS_write_io(RT_DOMAIN(domainId)%velocity,g_velocity)
      call ReachLS_write_io(RT_DOMAIN(domainId)%HLINK,g_hlink)
      ! Optional outputs
      if(nudgeFlag .eq. 1)then
#ifdef WRF_HYDRO_NUDGING
         ! FUTURE GOAL TO PLACE NUDGE OUTSIDE OF RT_DOMAIN SO THIS
         ! IFDEF DOESN'T NEED TO BE CALLED!
         call ReachLS_write_io(RT_DOMAIN(domainID)%nudge,g_nudge)
#endif
      endif
      if(nlst_rt(domainId)%output_channelBucket_influx .eq. 1 .or. &
         nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
         call ReachLS_write_io(RT_DOMAIN(domainId)%qSfcLatRunoff,g_qSfcLatRunoff)
         call ReachLS_write_io(RT_DOMAIN(domainId)%qBucket,g_qBucket)
      endif
      if(nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
         call ReachLS_write_io(RT_DOMAIN(domainId)%qin_gwsubbas,g_qBtmVertRunoff)
      endif
      if(nlst_rt(domainId)%output_channelBucket_influx .eq. 3) then
         call ReachLS_write_io(RT_DOMAIN(domainId)%accSfcLatRunoff,g_accSfcLatRunoff)
         call ReachLS_write_io(RT_DOMAIN(domainId)%qBucket,g_accBucket)
      endif
#endif

   else
      gSize = rt_domain(domainId)%nlinksl
      ! Since this is on a single processor, we don't need to allocate/collect
      ! local arrays into a global array. We can pull straight from RT_DOMAIN
      ! in the steps below. 
   endif

   ! Sync everything up before the next step.
   call mpp_land_sync()

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initChrtDict(fileMeta) 

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
      ! Read in index values from Routelink that will be used to sort output
      ! variables by ascending feature_id.
      iret = nf90_open(trim(nlst_rt(1)%route_link_f),NF90_NOWRITE,ncid=ftnRt)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to open RouteLink file for index extraction')
      iret = nf90_inq_varid(ftnRt,'ascendingIndex',indVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find ascendingIndex from RouteLink file.')
      iret = nf90_get_var(ftnRt,indVarId,chIndArray)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract ascendingIndex from RouteLink file.')
      iret = nf90_close(ftnRt)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close RouteLink file.')
      ! Place all output arrays into one real array that will be looped over
      ! during conversion to compressed integer format.
      allocate(varOutReal(fileMeta%numVars,gSize))
      allocate(varOutInt(gSize))
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
         !g_chlatOut(iTmp) = g_chlat(indTmp)
         !g_chlonOut(iTmp) = g_chlon(indTmp)
         !g_orderOut(iTmp) = g_order(indTmp)
         !g_zelevOut(iTmp) = g_zelev(indTmp)
      end do
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

      iret = nf90_create(trim(output_flnm),cmode=nf90_clobber .or. nf90_hdf5, ncid = ftn)
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

      ! Create channel lat/lon variables
      !iret = nf90_def_var(ftn,"Latitude",nf90_float,dimId(1),latVarId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create Latitude variable.')
      !iret = nf90_put_att(ftn,latVarId,'long_name',trim(fileMeta%latLName))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into Latitude variable')
      !iret = nf90_put_att(ftn,latVarId,'standard_name',trim(fileMeta%latStName))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into Latitude variable')
      !iret = nf90_put_att(ftn,latVarId,'units',trim(fileMeta%latUnits))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into Latitude variable')
      !iret = nf90_def_var(ftn,"Longitude",nf90_float,dimId(1),lonVarId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create Longitude variable.')
      !iret = nf90_put_att(ftn,lonVarId,'long_name',trim(fileMeta%lonLName))
      !call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into Longitude variable')
      !iret = nf90_put_att(ftn,lonVarId,'standard_name',trim(fileMeta%lonStName))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into Longitude variable')
      !iret = nf90_put_att(ftn,lonVarId,'units',trim(fileMeta%lonUnits))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into Longitude variable')

      ! Create channel order variable
      !iret = nf90_def_var(ftn,"order",nf90_int,dimId(1),orderVarId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create order variable.')
      !iret = nf90_put_att(ftn,orderVarId,'long_name',trim(fileMeta%orderLName))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into order variable')
      !iret = nf90_put_att(ftn,orderVarId,'standard_name',trim(fileMeta%orderStName))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into order variable')

      ! Create channel elevation variable
      !iret = nf90_def_var(ftn,"elevation",nf90_int,dimId(1),elevVarId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create elevation variable.')
      !iret = nf90_put_att(ftn,elevVarId,'long_name',trim(fileMeta%elevLName))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into elevation variable')
      !iret = nf90_put_att(ftn,elevVarId,'standard_name',trim(fileMeta%elevStName))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into elevation variable')

      ! Define deflation levels for these meta-variables. For now, we are going to
      ! default to a compression level of 2.  
      iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
      iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
      iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
      !iret = nf90_def_var_deflate(ftn,latVarId,0,1,2)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for Latitude.')
      !iret = nf90_def_var_deflate(ftn,lonVarId,0,1,2)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for Longitude.') 
      !iret = nf90_def_var_deflate(ftn,orderVarId,0,1,2)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for order.')  
      !iret = nf90_def_var_deflate(ftn,elevVarId,0,1,2)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for elevation.')  

      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout the
      ! output looping below for conversion to compressed integer values.
      !allocate(
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
            ! output.
            iret = nf90_def_var_deflate(ftn,varId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))

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
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer
            varOutInt(:) = INT((varOutReal(iTmp,:)-fileMeta%addOffset(i))/fileMeta%scaleFactor(iTmp))
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
      
      !iret = nf90_inq_varid(ftn,'Latitude',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate Latitude in NetCDF file.')
      !iret = nf90_put_var(ftn,varId,g_chlatOut)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into Latitude output variable.')

      !iret = nf90_inq_varid(ftn,'Longitude',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate Longitude in NetCDF file.')
      !iret = nf90_put_var(ftn,varId,g_chlonOut)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into Longitude output variable.')

      !iret = nf90_inq_varid(ftn,'order',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate order in NetCDF file.')
      !iret = nf90_put_var(ftn,varId,g_orderOut)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into order output variable.')

      !iret = nf90_inq_varid(ftn,'elevation',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate elevation in NetCDF file.')
      !iret = nf90_put_var(ftn,varId,g_zelevOut)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into elevation output variable.')

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
   call mpp_land_sync()

   ! Deallocate all memory.
   if(myId .eq. 0) then
      deallocate(varOutReal)
      deallocate(varOutInt)
      !deallocate(g_chlonOut)
      !deallocate(g_chlatOut)
      deallocate(g_hlinkOut)
      !deallocate(g_zelevOut)
      deallocate(g_qlinkOut)
      !deallocate(g_orderOut)
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
   endif
   !deallocate(g_chlon)
   !deallocate(g_chlat)
   deallocate(g_hlink)
   !deallocate(g_zelev)
   deallocate(g_qlink)
   !deallocate(g_order)
   deallocate(g_linkid)
   deallocate(g_QLateral)
   deallocate(g_nudge)
   deallocate(g_qSfcLatRunoff)
   deallocate(g_qBucket)
   deallocate(g_qBtmVertRunoff)
   deallocate(g_accSfcLatRunoff)
   deallocate(g_accBucket)

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

subroutine output_NoahMP_NWM(outDir,iGrid,output_timestep,startdate,date,ixPar,jxPar,zNum,varReal,vegTyp,varInd)
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
   character(len=*), intent(in) :: outDir
   integer, intent(in) :: iGrid
   integer, intent(in) :: output_timestep
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDateTmp ! Start of model simulation, represented as a string. 
   character(len=19),intent(in) :: startdate
   character(len=19),intent(in) :: date
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer, intent(in) :: ixPar,jxPar ! I/J dimensions of local grid.
   integer, intent(in) :: zNum ! Number of vertical layers (most of the time 1)
   real, intent(in) :: varReal(ixPar,zNum,jxPar)
   integer, intent(inout) :: vegTyp(ixPar,jxPar) ! Vegetation type grid used to mask out variables.
   integer, intent(in) :: varInd ! Variable index used to extact meta-data from.

   ! Derived types.
   type(ldasMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag
   character(len=1024) :: output_flnm
   integer :: iret
   integer :: ftn
   integer :: dimId(6)
   integer :: varId
   integer :: timeId
   integer :: refTimeId
   integer :: coordVarId
   integer :: xVarId,yVarId
   integer :: ierr, myId
   integer :: varRange(2)
   integer :: iTmp,jTmp,zTmp,jTmp2
   integer :: ftnGeo,geoXVarId,geoYVarId
   integer :: waterVal ! Value in HRLDAS in WRFINPUT file used to define water bodies for masking
   integer, allocatable, dimension(:,:) :: localCompTmp, globalCompTmp
   integer, allocatable, dimension(:,:,:) :: globalOutComp
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2

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

   call mpp_land_sync()

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
   else
      myId = 0
   endif

   ! Initialize water type to 16.
   ! NOTE THIS MAY CHANGE IN THE FUTURE!!!!!
   waterVal = 16

   ! Initialize NWM dictionary derived type containing all the necessary
   ! metadata for the output file.
   call initLdasDict(fileMeta)

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
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,0,0,1,0,1]
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
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! Sync all processes up.
   call mpp_land_sync()

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

         iret = nf90_create(trim(output_flnm),cmode=nf90_clobber .or. nf90_hdf5,ncid = ftn)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create LDASOUT NetCDF file.')
         ftnNoahMP = ftn

         ! Write global attributes
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'TITLE',trim(fileMeta%title))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place TITLE attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'model_output_time',trim(fileMeta%validTime))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'CF-1.6',trim(fileMeta%conventions))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into LDASOUT file.')

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

         ! Define compression for meta-variables
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
         iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')

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
         do i=1,fileMeta%numVars
            if(fileMeta%outFlag(i) .eq. 1) then
               if(fileMeta%numLev(i) .eq. fileMeta%numSoilLayers) then
                  iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(i)),nf90_int,(/dimId(2),dimId(4),dimId(3),dimId(1)/),varId)
               else if(fileMeta%numLev(i) .eq. fileMeta%numSnowLayers) then
                  iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(i)),nf90_int,(/dimId(2),dimId(5),dimId(3),dimId(1)/),varId)
               else if(fileMeta%numLev(i) .eq. 1) then
                  iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(i)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
               endif
               call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(i)))            
            
               ! Extract valid range into a 1D array for placement.
               varRange(1) = fileMeta%validMinComp(i)
               varRange(2) = fileMeta%validMaxComp(i)

               ! Establish a compression level for the variables. For now we are
               ! using a
               ! compression level of 2. In addition, we are choosing to turn the
               ! shuffle
               ! filter off for now. Kelley Eicher did some testing with this and
               ! determined that the benefit wasn't worth the extra time spent
               ! writing
               ! output.
               iret = nf90_def_var_deflate(ftnNoahMP,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(i)))

               ! Create variable attributes
               iret = nf90_put_att(ftnNoahMP,varId,'_FillValue',fileMeta%fillComp(i))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'missing_value',fileMeta%missingComp(i))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'long_name',trim(fileMeta%longName(i)))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'units',trim(fileMeta%units(i)))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'scale_factor',fileMeta%scaleFactor(i))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'add_offset',fileMeta%addOffset(i))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'valid_range',varRange)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'proj4',trim(fileMeta%proj4))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into variable: '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'grid_mapping',trim(fileMeta%gridMapping))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(i)))
               iret = nf90_put_att(ftnNoahMP,varId,'esri_pe_string',trim(fileMeta%projEsri))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into variable: '//trim(fileMeta%varNames(i))) 
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
   call mpp_land_sync()

   ! Place data into NetCDF file. This involves a few steps:
   ! 1.) Allocate an integer array of local grid size.
   ! 2.) Allocate an integer array of global grid size.
   ! 3.) Loop through real local grid, convert floating point
   !     values to integer via scale_factor/add_offset. If
   !     missing value found, assign FillValue caluclated 
   !     in the dictionary. 
   ! 4.) Use MPP utilities to collect local integer arrays
   !     into global integer array. 
   ! 5.) Write global integer array into output file.
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
      globalOutComp = fileMeta%fillComp(varInd)
      

      ! Sync up processes
      call mpp_land_sync()

      do zTmp = 1,fileMeta%numLev(varInd)
         localCompTmp = fileMeta%fillComp(varInd)
         globalCompTmp = fileMeta%fillComp(varInd)
         do iTmp = 1,ixPar
            do jTmp = 1,jxPar
               if(vegTyp(iTmp,jTmp) .eq. waterVal) then
                  localCompTmp(iTmp,jTmp) = int(fileMeta%fillComp(varInd))
               else
                  localCompTmp(iTmp,jTmp) = int((varReal(iTmp,zTmp,jTmp)-fileMeta%addOffset(varInd))/fileMeta%scaleFactor(varInd))
               endif
            end do
         end do
         ! Sync all processes up.
         call mpp_land_sync()
         if(mppFlag .eq. 1) then
            call write_IO_int(localCompTmp,globalCompTmp)
         else
            globalCompTmp = localCompTmp
         endif
         ! Sync all processes up.
         call mpp_land_sync()
         if(myId .eq. 0) then
            globalOutComp(:,zTmp,:) = globalCompTmp
         endif
      end do
 
      ! Sync up processes
      call mpp_land_sync()

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

      deallocate(globalOutComp)
      deallocate(globalCompTmp)
      deallocate(localCompTmp)
   
   endif

   ! Sync all processes up.
   call mpp_land_sync()

   if(myId .eq. 0) then
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
   integer :: varRange(2)
   integer :: ierr, myId ! MPI return status, process ID
   integer :: iTmp,jTmp,jTmp2
   real :: varRealTmp
   integer :: ftnGeo,geoXVarId,geoYVarId
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
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
   else
      myId = 0
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initRtDomainDict(fileMeta)

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
      iret = nf90_create(trim(output_flnm),cmode=nf90_clobber .or. nf90_hdf5,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create RT_DOMAIN NetCDF file.')

      ! Write global attributes
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_output_time',trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'CF-1.6',trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into RT_DOMAIN file.')

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
 
      ! Define compression for meta-variables
      iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
      iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
      iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
      iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')

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
      do i=1,fileMeta%numVars
         if(fileMeta%outFlag(i) .eq. 1) then
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(i)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
            call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(i)))

            ! Extract valid range into a 1D array for placement.
            varRange(1) = fileMeta%validMinComp(i)
            varRange(2) = fileMeta%validMaxComp(i)

            ! Establish a compression level for the variables. For now we are
            ! using a
            ! compression level of 2. In addition, we are choosing to turn
            ! the
            ! shuffle
            ! filter off for now. Kelley Eicher did some testing with this
            ! and
            ! determined that the benefit wasn't worth the extra time spent
            ! writing
            ! output.
            iret = nf90_def_var_deflate(ftn,varId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(i)))

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(i)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(i)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'proj4',trim(fileMeta%proj4))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into variable: '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'grid_mapping',trim(fileMeta%gridMapping))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'esri_pe_string',trim(fileMeta%projEsri))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into variable: '//trim(fileMeta%varNames(i)))
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
      do jTmp = RT_DOMAIN(domainId)%jxrt,1,-1
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
      call mpp_land_sync()
   endif

   ! Loop through each variable, collect local routing grid variables into a
   ! global routing grid and output through the master I/O process.
   do i=1,fileMeta%numVars
      if(fileMeta%outFlag(i) .eq. 1) then   
         !Allocate memory necessary 
         if(myId .eq. 0) then
            allocate(globalOutComp(RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt))
         else
            allocate(globalOutComp(1,1))
         endif
         allocate(localCompTmp(RT_DOMAIN(domainId)%ixrt,RT_DOMAIN(domainId)%jxrt))
         globalOutComp = fileMeta%fillComp(i)
         localCompTmp = fileMeta%fillComp(i)

         ! Sync up processes
         call mpp_land_sync()

         do iTmp = 1,RT_DOMAIN(domainId)%ixrt
            do jTmp = 1,RT_DOMAIN(domainId)%jxrt
               if(i .eq. 1) then
                  varRealTmp = RT_DOMAIN(domainId)%ZWATTABLRT(iTmp,jTmp)
               else if(i .eq. 2) then
                  varRealTmp = RT_DOMAIN(domainId)%SFCHEADSUBRT(iTmp,jTmp)
               else if(i .eq. 3) then
                  varRealTmp = RT_DOMAIN(domainId)%QSTRMVOLRT(iTmp,jTmp)
               else if(i .eq. 4) then
                  varRealTmp = RT_DOMAIN(domainId)%QBDRYRT(iTmp,jTmp)
               endif
               if(varRealTmp .eq. fileMeta%modelNdv) then
                  localCompTmp(iTmp,jTmp) = int(fileMeta%fillComp(i))
               else
                  localCompTmp(iTmp,jTmp) = int((varRealTmp-fileMeta%addOffset(i))/fileMeta%scaleFactor(i))
               endif
            end do
         end do
         if(mppFlag .eq. 1) then
            call write_IO_rt_int(localCompTmp,globalOutComp)
         else
            globalOutComp = localCompTmp
         endif

         ! Sync up processes
         if(mppFlag .eq. 1) then
            call mpp_land_sync()
         endif

         if(myId .eq. 0) then
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(i)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(i)))
            iret = nf90_put_var(ftn,varId,globalOutComp,(/1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt,1/))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(i)))
         endif

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
   integer :: featureVarId
   integer :: latVarId, lonVarId
   integer :: elevVarId
   integer :: varRange(2)
   integer :: gSize
   integer :: iTmp
   integer :: ierr, myId ! MPI return status, process ID
   real, allocatable, dimension(:) :: g_lakeLat,g_lakeLon,g_lakeElev
   real, allocatable, dimension(:) :: g_lakeInflow,g_lakeOutflow
   integer, allocatable, dimension(:) :: g_lakeid
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset
                                                     ! have been applied.

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
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
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
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! First step is to collect and assemble all data that will be written to the 
   ! NetCDF file. If we are not using MPI, we bypass the collection step through
   ! MPI. 
   if(mppFlag .eq. 1) then
      gSize = rt_domain(domainId)%NLAKES

      ! Sync all processes up.
      call mpp_land_sync()

      allocate(g_lakeLon(gsize))
      allocate(g_lakeLat(gsize))
      allocate(g_lakeElev(gsize))
      allocate(g_lakeInflow(gsize))
      allocate(g_lakeOutflow(gsize))
      allocate(g_lakeid(gsize))
    
      g_lakeLat = RT_DOMAIN(domainID)%LATLAKE
      g_lakeLon = RT_DOMAIN(domainID)%LONLAKE
      g_lakeElev = RT_DOMAIN(domainID)%RESHT
      g_lakeInflow = RT_DOMAIN(domainID)%QLAKEI
      g_lakeOutflow = RT_DOMAIN(domainID)%QLAKEO
      g_lakeid = RT_DOMAIN(domainId)%LAKEIDM

      ! Sync everything up before the next step.
      call mpp_land_sync()

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
      ! Since this is on a single processor, we don't need to allocate/collect
      ! local arrays into a global array. We can pull straight from RT_DOMAIN
      ! in the steps below. 
   endif

   ! Sync all processes up.
   call mpp_land_sync()
 
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
      ! Place all output arrays into one real array that will be looped over
      ! during conversion to compressed integer format.
      allocate(varOutReal(fileMeta%numVars,gSize))
      allocate(varOutInt(gSize))
      varOutReal(1,:) = g_lakeInflow(:)
      varOutReal(2,:) = g_lakeOutflow(:)

      ! Sync all processes up.
      !call mpp_land_sync()

      iret = nf90_create(trim(output_flnm),cmode=nf90_clobber .or. nf90_hdf5, ncid = ftn)
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

      ! Create lake lat/lon variables
      iret = nf90_def_var(ftn,"Latitude",nf90_float,dimId(1),latVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create Latitude variable.')
      iret = nf90_put_att(ftn,latVarId,'long_name',trim(fileMeta%latLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into Latitude variable')
      iret = nf90_put_att(ftn,latVarId,'standard_name',trim(fileMeta%latStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into Latitude variable')
      iret = nf90_put_att(ftn,latVarId,'units',trim(fileMeta%latUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into Latitude variable')
      iret = nf90_def_var(ftn,"Longitude",nf90_float,dimId(1),lonVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create Longitude variable.')
      iret = nf90_put_att(ftn,lonVarId,'long_name',trim(fileMeta%lonLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into Longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'standard_name',trim(fileMeta%lonStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into Longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'units',trim(fileMeta%lonUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into Longitude variable')

      ! Create channel elevation variable
      iret = nf90_def_var(ftn,"elevation",nf90_int,dimId(1),elevVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create elevation variable.')
      iret = nf90_put_att(ftn,elevVarId,'long_name',trim(fileMeta%elevLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into elevation variable')
      iret = nf90_put_att(ftn,elevVarId,'units',trim(fileMeta%elevUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into elevation variable')

      ! Define deflation levels for these meta-variables. For now, we are going to
      ! default to a compression level of 2.  
      iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
      iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
      iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
      iret = nf90_def_var_deflate(ftn,latVarId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for Latitude.')
      iret = nf90_def_var_deflate(ftn,lonVarId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for Longitude.')
      iret = nf90_def_var_deflate(ftn,elevVarId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for elevation.')

      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout
      ! the
      ! output looping below for conversion to compressed integer values.
      !allocate(
      ! Loop through and create each output variable, create variable
      ! attributes,
      ! and insert data.
      do i=1,fileMeta%numVars
         if(fileMeta%outFlag(i) .eq. 1) then
            ! First create variable
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(i)),nf90_int,dimId(1),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(i)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(i)
            varRange(2) = fileMeta%validMaxComp(i)

            ! Establish a compression level for the variables. For now we are
            ! using a
            ! compression level of 2. In addition, we are choosing to turn the
            ! shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent
            ! writing
            ! output.
            iret = nf90_def_var_deflate(ftn,varId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(i)))

            ! Create variable attributes
            !iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(i))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(i)))
            !iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(i))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(i)))
            !iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(i)))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(i)))
            !iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(i)))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(i)))
            !iret = nf90_put_att(ftn,varId,'coordinates',trim(fileMeta%coordNames(i)))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place coordinates attribute into variable '//trim(fileMeta%varNames(i)))
            !iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(i))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(i)))
            !iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(i))
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(i)))
            !iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            !call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(i)))
         endif
      end do

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take CHRTOUT file out of definition mode')
   
      ! Sync all processes up.
      call mpp_land_sync()

      ! Place lake ID, elevation, lat, and lon values into appropriate
      ! variables. 
      !do iTmp=1,fileMeta%numVars
      !   if(fileMeta%outFlag(iTmp) .eq. 1) then
      !      print*, "LAKE VAR ID = ", iTmp
      !      ! We are outputing this variable.
      !      ! Convert reals to integer
      !      varOutInt(:) = INT((varOutReal(iTmp,:)-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
      !      ! Get NetCDF variable id.
      !      iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
      !      call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))

      !      print*, "PUTTING LAKE DATA IN FILE"
      !      ! Put data into NetCDF file
      !      iret = nf90_put_var(ftn,varId,varOutInt)
      !      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
      !   endif
      !end do

      ! Place link ID values into the NetCDF file
      !iret = nf90_inq_varid(ftn,'feature_id',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      !iret = nf90_put_var(ftn,varId,g_lakeid)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')

      ! Place lake metadata into NetCDF file
      !iret = nf90_inq_varid(ftn,'elevation',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate elevation in NetCDF file.')
      !iret = nf90_put_var(ftn,varId,g_lakeElev)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into elevation output variable.')

      !iret = nf90_inq_varid(ftn,'Latitude',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate Latitude in NetCDF file.')
      !iret = nf90_put_var(ftn,varId,g_lakeLat)   
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into Latitude output variable.')

      !iret = nf90_inq_varid(ftn,'Longitude',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate Longitude in NetCDF file.')
      !iret = nf90_put_var(ftn,varId,g_lakeLon)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into Longitude output variable.')

      ! Place time values into time variables.
      !iret = nf90_inq_varid(ftn,'time',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      !iret = nf90_put_var(ftn,varId,minSinceEpoch)
      !call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      !iret = nf90_inq_varid(ftn,'reference_time',varId)
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      !iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      !call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAKE file.')

   endif ! End if we are on master processor. 

   ! Sync all processes up.
   call mpp_land_sync()

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


end subroutine output_lakes_NWM

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
