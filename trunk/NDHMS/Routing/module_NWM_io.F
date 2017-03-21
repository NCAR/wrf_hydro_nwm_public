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
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:,:) :: varOutInt ! Array holding output variables after 
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
   iret = nf90_def_dim(ftn,"feature_id",10,dimId(1))
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

   end do 

   ! Remove NetCDF file from definition mode.
   iret = nf90_enddef(ftn)
   call nwmCheck(diagFlag,iret,'ERROR: Unable to take CHRTOUT file out of definition mode')

   ! PLACEHOLDER FOR PUTTIND DATA INTO FILE

   ! Close the output file
   iret = nf90_close(ftn)
   call nwmCheck(diagFlag,iret,'ERROR: Unable to close CHRTOUT file.')

end subroutine output_chrt_NWM

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
