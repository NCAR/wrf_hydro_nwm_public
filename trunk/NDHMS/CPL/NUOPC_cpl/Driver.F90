!>
!! @mainpage ADCIRC NUOPC Cap
!! @author Saeed Moghimi (moghimis@gmail.com)
!! @date 15/1/17 Original documentation
!------------------------------------------------------
!LOG-----------------
!
!
!
module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices

  use NUOPC_Connector, only: cplSS => SetServices
  
  use adc_cap,  only: adcSS  => SetServices
  use WAV,      only: wavSS  => SetServices
  use NWM_NUOPC_Cap,   only: nwmSS => SetServices

  !TODO: Use #ifdef to select only on atm cap at a time >>>>
  use hwrf_cap, only: hwrfSS => SetServices
  use atmesh  , only: atmeshSS => SetServices
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    integer                       :: petCount, i
    integer, allocatable          :: petList(:)

    ! vars to read config file
    character(ESMF_MAXPATHLEN)    :: fname ! config file name
    integer                       :: syear, smonth, sday, shour, sminute, ssecond
    integer                       :: eyear, emonth, eday, ehour, eminute, esecond
    integer                       :: adc_pet_num,ww3_pet_num,atm_pet_num,nwm_pet_num
    integer                       :: cpl_int,cpl_num,cpl_den
  

    type(ESMF_Config)             :: cf     ! the Config itself

    rc = ESMF_SUCCESS
    
   !Initiate reading resource file
    cf = ESMF_ConfigCreate(rc=rc)  ! Create the empty Config
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    fname = "config.rc" ! Name the Resource File
    call ESMF_ConfigLoadFile(cf, fname, rc=rc) ! Load the Resource File
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! read coupling info
    call ESMF_ConfigGetAttribute(cf, adc_pet_num, label="adc_pet_num:",default=4, rc=rc)
    call ESMF_ConfigGetAttribute(cf, ww3_pet_num, label="ww3_pet_num:",default=4, rc=rc)
    call ESMF_ConfigGetAttribute(cf, atm_pet_num, label="atm_pet_num:",default=0, rc=rc)
    call ESMF_ConfigGetAttribute(cf, nwm_pet_num, label="nwm_pet_num:",default=767, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    print *,  "adc_pet_num:", adc_pet_num
    print *,  "ww3_pet_num:", ww3_pet_num     
    print *,  "atm_pet_num:", atm_pet_num     
    print *,  "nwm_pet_num:", nwm_pet_num     


   ! read time hwrf data time interval info
    call ESMF_ConfigGetAttribute(cf, cpl_int, label="atm_int:",default=3600,rc=rc)
    call ESMF_ConfigGetAttribute(cf, cpl_num, label="atm_num:",default=0  , rc=rc)
    call ESMF_ConfigGetAttribute(cf, cpl_den, label="atm_den:",default=1  , rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! read start time info
    call ESMF_ConfigGetAttribute(cf, syear,  label="start_year:",  default=2016, rc=rc)
    call ESMF_ConfigGetAttribute(cf, smonth, label="start_month:", default=10, rc=rc)
    call ESMF_ConfigGetAttribute(cf, sday,   label="start_day:",   default=1, rc=rc)
    call ESMF_ConfigGetAttribute(cf, shour,  label="start_hour:",  default=0 , rc=rc)
    call ESMF_ConfigGetAttribute(cf, sminute,label="start_minute:",default=0 , rc=rc)
    call ESMF_ConfigGetAttribute(cf, ssecond,label="start_second:",default=0 , rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! read stop time info
    call ESMF_ConfigGetAttribute(cf, eyear,  label="stop_year:",  default=2016, rc=rc)
    call ESMF_ConfigGetAttribute(cf, emonth, label="stop_month:", default=10, rc=rc)
    call ESMF_ConfigGetAttribute(cf, eday,   label="stop_day:",   default=1, rc=rc)
    call ESMF_ConfigGetAttribute(cf, ehour,  label="stop_hour:",  default=3 , rc=rc)
    call ESMF_ConfigGetAttribute(cf, eminute,label="stop_minute:",default=0 , rc=rc)
    call ESMF_ConfigGetAttribute(cf, esecond,label="stop_second:",default=0 , rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ConfigDestroy(cf, rc=rc) ! Destroy the Config
   !--------------------------------------------------------------------------------


   !-------------------------------------------------------------------    
   ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!!#define WITHPETLISTS_on
#ifdef WITHPETLISTS_on
    if ((adc_pet_num+ww3_pet_num+atm_pet_num+nwm_pet_num).ne.petCount) then
      Stop "petCount does not match (adc_pet_num+ww3_pet_num+atm_pet_num+nwm_pet_num) from Config.rc file!"
    Endif
#endif

#ifdef WITHPETLISTS_on
    ! For WAV component
    allocate(petList(adc_pet_num))
    do i=1, adc_pet_num
      petList(i) = i-1 ! PET labeling goes from 0 to petCount-1 for ADC
    enddo
#endif

    ! SetServices for ADC
    call NUOPC_DriverAddComp(driver, "ADC", adcSS, &
#ifdef WITHPETLISTS_on
      petList=petList, &
#endif
     comp=child, rc=rc)

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="max", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifdef WITHPETLISTS_on
    deallocate(petList)
#endif

    !---------------------------------------------------
#ifdef WITHPETLISTS_on
    ! For WAV component
    allocate(petList(ww3_pet_num))
    do i=1, ww3_pet_num
      petList(i) = adc_pet_num + i-1 ! continue from ADC
    enddo
#endif
 
    ! SetServices for WAV
    call NUOPC_DriverAddComp(driver, "WAV", wavSS, &
#ifdef WITHPETLISTS_on
      petList=petList, &
#endif
      comp=child, rc=rc)

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="max", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
   
#ifdef WITHPETLISTS_on
    deallocate(petList)
#endif    
    
   !--------------------------------------------------
    !one processor for hwrf
    allocate(petList(1))
    petList(1) = petCount-1        ! PET labeling goes from 0 to petCount-1
    Print *, 'WRF>>>>', petCount, petList(1)

    ! SetServices for HWRF
    call NUOPC_DriverAddComp(driver, "HWRF", hwrfSS, &
#ifdef WITHPETLISTS_on
      petList=petList, &
#endif
      comp=child, rc=rc)

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompAttributeSet(child, name="Verbosity", value="max", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifdef WITHPETLISTS_on
    deallocate(petList)
#endif     


   !--------------------------------------------------
    !processor for nwm
    allocate(petList(nwm_pet_num))
    do i=1, nwm_pet_num
      petList(i) = adc_pet_num + i-1 ! continue from ADC
    enddo

    ! SetServices for HWRF
    call NUOPC_DriverAddComp(driver, "NWM", hwrfSS, &
#ifdef WITHPETLISTS_on
      petList=petList, &
#endif
      comp=child, rc=rc)

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompAttributeSet(child, name="Verbosity", value="max", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifdef WITHPETLISTS_on
    deallocate(petList)
#endif

    !---------------------------------------------------
    ! Disabling the following macro, e.g. renaming to WITHCONNECTORS_disable,
    ! will result in a driver that does not call connectors between the model
    ! components. This mode can be used if all model components are driven 
    ! as independent models. However, even for independent models the
    ! connectors can be set here, but will turn into no-ops.
#define WITHCONNECTORS
#ifdef WITHCONNECTORS
      
    ! SetServices for adc2wav
    call NUOPC_DriverAddComp(driver, srcCompLabel="ADC", dstCompLabel="WAV", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="max", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! SetServices for wav2adc
    call NUOPC_DriverAddComp(driver, srcCompLabel="WAV", dstCompLabel="ADC", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="max", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

   ! SetServices for hwrf2wav
    call NUOPC_DriverAddComp(driver, srcCompLabel="HWRF", dstCompLabel="WAV", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="max", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for hwrf2adc
    call NUOPC_DriverAddComp(driver, srcCompLabel="HWRF", dstCompLabel="ADC", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="max", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

   ! SetServices for adc2nwm
    call NUOPC_DriverAddComp(driver, srcCompLabel="ADC", dstCompLabel="NWM", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="max", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

   ! SetServices for nwm2adc
    call NUOPC_DriverAddComp(driver, srcCompLabel="NWM", dstCompLabel="ADC", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="max", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#endif


    ! set the model clock
    call ESMF_TimeIntervalSet(timeStep, s=cpl_int, sN=cpl_num, sD=cpl_den, rc=rc) ! 5 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !Set start time
    call ESMF_TimeSet(startTime, yy=syear, mm=smonth, dd=sday, h=shour, &
      m=sminute, s=ssecond,calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


    call ESMF_TimeSet(stopTime, yy=eyear, mm=emonth, dd=eday, h=ehour, &
      m=eminute, s=esecond,calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    internalClock = ESMF_ClockCreate(name="ADC_WW3 Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine

end module
