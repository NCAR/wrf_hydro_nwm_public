! Module for handling National Water Model streamflow, land surface,
! gridded routing, lake, and groundwater output.

! Logan Karsten
! National Center for Atmospheric Research
! Research Applications Laboratory
! karsten@ucar.edu
! 303-497-2693

module module_NWM_io
!use netcdf
!use module_rt_data, only: rt_domain
!use module_namelist, only: nlst_rt
!use Module_Date_utilities_rt, only: geth_newdate, geth_idts
!use module_NWM_io_dict

implicit none
!include "mpif.h"

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

   ! subroutine arguments
   !type, intent(inout) :: rt_domain
   !type, intent(inout) :: nlst_rt
   !type(rt_domain), intent(inout) :: rt_domain
   !type(nlst_rt), intent(inout) :: nlst_rt
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
   integer :: featureVarId
   integer :: varRange(2)
   integer :: gSize
   integer :: ierr, myId ! MPI return status, process ID
   ! Establish local, allocatable arrays
   real, allocatable, dimension(:,:) :: g_qlink
   integer, allocatable, dimension(:) :: g_linkid,g_order
   real, allocatable, dimension(:) :: g_chlat,g_chlon,g_hlink,g_zelev
   real, allocatable, dimension(:) :: g_QLateral,g_velocity
   real, allocatable, dimension(:) :: g_nudge,g_qSfcLatRunoff
   real, allocatable, dimension(:) :: g_qBucket,g_qBtmVertRunoff,g_accBucket
   real*8, allocatable, dimension(:) :: g_accSfcLatRunoff
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset have been applied.

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

   print*, "PID = ",myId

   ! First step is to collect and assemble all data that will be written to the 
   ! NetCDF file. If we are not using MPI, we bypass the collection step through
   ! MPI. 
   if(mppFlag .eq. 1) then
      gSize = rt_domain(domainId)%gnlinksl

      print*, "GSIZE = ",gSize
      ! Sync all processes up.
      call mpp_land_sync()

      print*, "A"
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
         ! Handle optional output flags
         !if(nudgeFlag .eq. 1) then
         allocate(g_nudge(gsize))
         !endif
         !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 1 .or. &
         !   nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
         allocate(g_qSfcLatRunoff(gsize))
         allocate(g_qBucket(gsize))
         !endif
         !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
         allocate(g_qBtmVertRunoff(gsize))
         !endif
         !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 3) then
         allocate(g_accSfcLatRunoff(gsize))
         allocate(g_accBucket(gsize))
         !end if
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
         ! Handle optional output flags
         !if(nudgeFlag .eq. 1) then
         allocate(g_nudge(1))
         !endif
         !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 1 .or. &
         !   nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
         allocate(g_qSfcLatRunoff(1))
         allocate(g_qBucket(1))
         !endif
         !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
         allocate(g_qBtmVertRunoff(1))
         !endif
         !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 3) then
         allocate(g_accSfcLatRunoff(1))
         allocate(g_accBucket(1))
         !endif
      endif

      ! Sync everything up before the next step.
      call mpp_land_sync()

      print*, "B"
      ! Collect arrays from various processors through MPI, and 
      ! assemble into global arrays previously allocated.
      g_qlink = 0
#ifdef MPP_LAND
      print*, "COLLECTING ARRAYS"
      call ReachLS_write_io(RT_DOMAIN(domainId)%QLINK(:,1),g_qlink(:,1))
      print*, "STREAMFLOW = ",g_qlink(:,1)
      print*, "RT STREAMFLOW = ",RT_DOMAIN(domainId)%QLINK(:,1)
      call ReachLS_write_io(RT_DOMAIN(domainId)%QLINK(:,2),g_qlink(:,2))
      call ReachLS_write_io(RT_DOMAIN(domainId)%ORDER,g_order)
      call ReachLS_write_io(RT_DOMAIN(domainId)%linkid,g_linkid)
      call ReachLS_write_io(RT_DOMAIN(domainId)%CHLAT,g_chlat)
      call ReachLS_write_io(RT_DOMAIN(domainId)%CHLON,g_chlon)
      call ReachLS_write_io(RT_DOMAIN(domainId)%ZELEV,g_zelev)
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
      print*, "TEST"
      ! Since this is on a single processor, we don't need to allocate/collect
      ! local arrays into a global array. We can pull straight from RT_DOMAIN
      ! in the steps below. 
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initChrtDict(fileMeta) 

   print*, "DICTIONARY INITIATED"
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

   print*, "FILE OUT = ",output_flnm
   ! Only run NetCDF library calls to output data if we are on the master
   ! processor.
   if(myId .eq. 0) then
      ! Place all output arrays into one real array that will be looped over
      ! during conversion to compressed integer format.
      print*, "ALLOCATING OUTPUT ARRAYS"
      allocate(varOutReal(fileMeta%numVars,gSize))
      print*, "GSIZE 2 = ",gSize
      print*, "NUMVARS = ",fileMeta%numVars
      print*, "SHAPE = ",shape(varOutReal)
      allocate(varOutInt(gSize))
      print*, "PLACING DATA INTO OUTPUT REAL ARRAY"
      varOutReal(1,:) = g_qlink(:,1)
      print*, "C1"
      varOutReal(2,:) = g_nudge
      print*, "C2"
      varOutReal(3,:) = g_QLateral
      print*, "C3"
      varOutReal(4,:) = g_velocity
      print*, "C4"
      varOutReal(5,:) = g_chlat
      print*, "C5"
      varOutReal(6,:) = g_chlon
      print*, "C6"
      varOutReal(7,:) = g_zelev
      print*, "C7"
      varOutReal(8,:) = g_order
      print*, "C8"
      varOutReal(9,:) = g_hlink
      print*, "C9"
      varOutReal(10,:) = g_qSfcLatRunoff
      print*, "C10"
      varOutReal(11,:) = g_qBucket
      print*, "C11"
      varOutReal(12,:) = g_qBtmVertRunoff
      print*, "C12"
      varOutReal(13,:) = g_accSfcLatRunoff
      print*, "C13"
      varOutReal(14,:) = g_accBucket
      print*, "C14"

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
      ! PLACEHOLDER TO FIX NUMBER OF FEATURES
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

      ! Create and populate feature_id variable
      iret = nf90_def_var(ftn,"feature_id",nf90_int,dimId(1),featureVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id variable.')
      iret = nf90_put_att(ftn,featureVarId,'long_name',trim(fileMeta%featureIdLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'comment',trim(fileMeta%featureIdComment))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')

      ! Define deflation levels for these meta-variables. For now, we are going to
      ! default to a compression level of 2.  
      iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
      iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')

      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout the
      ! output looping below for conversion to compressed integer values.
      !allocate(
      ! Loop through and create each output variable, create variable attributes,
      ! and insert data.
      do i=1,fileMeta%numVars
         if(fileMeta%outFlag(i) .eq. 1) then
            print*, "VAR = ",i
            ! First create variable
            print*, 'VARNAME = ',trim(fileMeta%varNames(i))
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(i)),nf90_int,dimId(1),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(i)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(i)
            varRange(2) = fileMeta%validMaxComp(i)

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing
            ! output.
            iret = nf90_def_var_deflate(ftn,varId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(i)))

            ! Create variable attributes
            print*, "FILL = ",fileMeta%fillComp(i)
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "MISSING = ",fileMeta%missingComp(i)
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "LONG NAME = ",trim(fileMeta%longName(i))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(i)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "UNITS = ",trim(fileMeta%units(i))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(i)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "COORDINATES = ",trim(fileMeta%coordNames(i))
            iret = nf90_put_att(ftn,varId,'coordinates',trim(fileMeta%coordNames(i)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place coordinates attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "SCALE_FACTOR = ",fileMeta%scaleFactor(i)
            iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "ADD_OFFSET = ",fileMeta%addOffset(i)
            iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "VALID_RANGE = ",varRange
            iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(i)))
         endif
      end do 

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take CHRTOUT file out of definition mode')
      do i=1,fileMeta%numVars
         print*, "VARID = ",i
         print*, "OUT FLAT = ",fileMeta%outFlag(i)
         if(fileMeta%outFlag(i) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer
            if(i .eq. 1) then
               print*, "STREAMFLOW LOOP VALUES = ",varOutReal(i,:)
            endif
            varOutInt(:) = INT((varOutReal(i,:)-fileMeta%addOffset(i))/fileMeta%scaleFactor(i))
            if(i .eq. 1) then
               print*, "INTEGER VALS = ",varOutInt
            endif
            ! Get NetCDF variable id.
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(i)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(i)))
 
            ! Put data into NetCDF file
            iret = nf90_put_var(ftn,varId,varOutInt)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(i)))
         endif
      end do

      ! Place link ID values into the NetCDF file
      iret = nf90_inq_varid(ftn,'feature_id',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_linkid)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')

      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close CHRTOUT file.')

   endif ! End if we are on master processor. 

   ! Deallocate all memory.
   deallocate(varOutReal)
   deallocate(varOutInt)
   deallocate(g_chlon)
   deallocate(g_chlat)
   deallocate(g_hlink)
   deallocate(g_zelev)
   deallocate(g_qlink)
   deallocate(g_order)
   deallocate(g_linkid)
   deallocate(g_QLateral)
   !if(nudgeFlag .eq. 1) then
   deallocate(g_nudge)
   !endif
   !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 1 .or. &
   !   nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
   deallocate(g_qSfcLatRunoff)
   deallocate(g_qBucket)
   !endif
   !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
   deallocate(g_qBtmVertRunoff)
   !endif
   !if(nlst_rt(domainId)%output_channelBucket_influx .eq. 3) then
   deallocate(g_accSfcLatRunoff)
   deallocate(g_accBucket)
   !endif

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

subroutine output_NoahMP_NWM(outDir,iGrid,ixFull,jxFull,nSoil,nSnow,var,varInd)
   !use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
     !use module_mpp_reachls,  only: ReachLS_write_io
#endif
   implicit none

   ! Subroutine arguments
   character(len=*), intent(in) :: outDir
   integer, intent(in) :: iGrid
   integer, intent(in) :: output_timestep
   character(len=19),intent(in) :: startdate
   character(len=19),intent(in) :: date
   integer, intent(in) :: ixFull,jxFull,nSoil,nSnow
   integer, intent(in) :: varInd

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

   ! Initialize NWM dictionary derived type containing all the necessary
   ! metadata for the output file.
   call initLdasDict(fileMeta)

   if(varInd .eq. 0) then
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
      endif

      iret = nf90_create(trim(output_flnm),cmode=nf90_clobber .or. nf90_hdf5,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create LDASOUT NetCDF file.')

      ! Write global attributes
      iret = nf90_put_att(ftn,NF90_GLOBAL,'TITLE',trim(fileMeta%title))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place TITLE attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_initialization_time','PLACEHOLDER')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_output_time','PLACEHOLDER')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'CF-1.6',trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into LDASOUT file.')

      ! Create dimensions
      iret = nf90_def_dim(ftn,'time',NF90_UNLIMITED,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
      iret = nf90_def_dim(ftn,'x',ixFull,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
      iret = nf90_def_dim(ftn,'y',ijFull,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
      iret = nf90_def_dim(ftn,'soil_layers_stag',nSoil,dimId(4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define soil_layers_stag dimension')
      iret = nf90_def_dim(ftn,'snow_layers',nSnow,dimId(5))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define snow_layers dimension')
      iret = nf90_def_dim(ftn,'reference_time',1,dimId(6))
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
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(6),refTimeId)
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

      ! Create ProjectionCoordinateSystem variable
      iret = nf90_def_var(ftn,'ProjectionCoordinateSystem',nf90_char,varid=coordVarId)
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateTransformType',trim(fileMeta%projTransform))
      call nwmCheck(diagFlag,iret'ERROR: Unable to place _CoordinateTransformType attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'transform_name',trim(fileMeta%projTransformName))
      call nwmCheck(diagFlag,iret'ERROR: Unable to place transform_name attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'grid_mapping_name',trim(fileMeta%projGridMappingName))
      call nwmCheck(diagFlag,iret'ERROR: Unable to place grid_mapping_name attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateAxes',trim(fileMeta%projCoordAxes))
      call nwmCheck(diagFlag,iret'ERROR: Unable to place _CoordinateAxes attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'esri_pe_string',trim(fileMeta%projEsri))
      call nwmCheck(diagFlag,iret'ERROR: Unable to place esri_pe_string attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'standard_parallel',fileMeta%stParallel)
      call nwmCheck(diagFlag,iret'ERROR: Unable to place standard_parallel attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'longitude_of_central_meridian',fileMeta%lonCtrlMer)
      call nwmCheck(diagFlag,iret'ERROR: Unable to place longitude_of_central_meridian attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'latitude_of_projection_origin',fileMeta%latOrig)
      call nwmCheck(diagFlag,iret'ERROR: Unable to place latitude_of_projection_origin attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'false_easting',fileMeta%falseEast)
      call nwmCheck(diagFlag,iret'ERROR: Unable to false_easting place attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'false_northing',fileMeta%falseNorth)
      call nwmCheck(diagFlag,iret'ERROR: Unable to place false_northing attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'earth_radius',fileMeta%radius)
      call nwmCheck(diagFlag,iret'ERROR: Unable to place earth_radius attribute into ProjectionCoordinateVariable')
      iret = nf90_put_att(ftn,coordVarId,'proj4',trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret'ERROR: Unable to place proj4 attribute into ProjectionCoordinateVariable')

      ! Loop through all possible variables and create them, along with their
      ! metadata attributes. 
      do i=1,fileMeta%numVars
         if(fileMeta%outFlag(i) .eq. 1) then
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(i)),nf90_int,(/dimId(1),dimId(2),dimId(3)/,varId))
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
            iret = nf90_def_var_deflate(ftn,varId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(i)))

            ! Create variable attributes
            print*, "FILL = ",fileMeta%fillComp(i)
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "MISSING = ",fileMeta%missingComp(i)
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "LONG NAME = ",trim(fileMeta%longName(i))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(i)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "UNITS = ",trim(fileMeta%units(i))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(i)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "SCALE_FACTOR = ",fileMeta%scaleFactor(i)
            iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "ADD_OFFSET = ",fileMeta%addOffset(i)
            iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(i))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(i)))
            print*, "VALID_RANGE = ",varRange
            iret = nf90_put_att(ftn,varId,'valid_range',varRange)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'proj4',trim(fileMeta%proj4))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place proj4 attribute into variable: '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'grid_mapping',trim(fileMeta%gridMapping))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(i)))
            iret = nf90_put_att(ftn,varId,'esri_pe_string',trim(fileMeta%projEsri))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into variable: '//trim(fileMeta%varNames(i))) 
         endif
      end do

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take LDASOUT file out of definition mode')
      do i=1,fileMeta%numVars
         print*, "VARID = ",i
         print*, "OUT FLAT = ",fileMeta%outFlag(i)
         if(fileMeta%outFlag(i) .eq. 1) then
            ! We are outputing this variable.
            ! First assign local real array values passed in, depending on which
            ! variable we are on. This is different than with CHRTOUT as the
            ! data isn't stored in an object that can easily be accessed.
            if(i .eq. 1) then
               varReal = fsa
            else if(i .eq. 2) then
               varReal = fira
            else if(i .eq. 3) then
               varReal = grdflx
            else if(i .eq. 4) then
               varReal = hfx         
            else if(i .eq. 5) then
               varReal = lh  
            else if(i .eq. 6) then
               varReal = ugdrnoff  
            else if(i .eq. 7) then
               varReal = accecan
            else if(i .eq. 8) then
               varReal = accedir
            else if(i .eq. 9) then 
               varReal = accetran
            else if(i .eq. 10) then
               varReal = snowtAvg
            else if(i .eq. 11) then
               varReal = sfcrnoff
            else if(i .eq. 12) then
               varReal = trad
            else if(i .eq. 13) then
               varRealSnow = snliq
            else if(i .eq. 14) then
               varRealSoil = soilT
            else if(i .eq. 15) then
               varRealSoil = soilM
            else if(i .eq. 16) then
               varReal = snowh
            else if(i .eq. 17) then
               varReal = sneqv
            else if(i .eq. 18) then
               varReal = isnow
            else if(i .eq. 19) then
               varReal = fsno
            else if(i .eq. 20) then
               varReal = acsnom
            else if(i .eq. 21) then
               varReal = accet
            else if(i .eq. 22) then
               varReal = canwat
            else if(i .eq. 23) then
               varReal = soilice
            else if(i .eq. 24) then
               varReal = soilSatTop
            else if(i .eq. 25) then
               varReal = soilSat
            else if(i .eq. 26) then
               varReal = snowt
            else if(i .eq. 27) then
               varReal = snowtAvg
            endif

            ! Convert reals to integer
            if(i .eq. 13) then
               varInt = 
            else if(i .eq. 14) then

            else if(i .eq. 15) then

            else

            endif

         endif
      end do

   endif

end subroutine output_NoahMP_NWM

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
