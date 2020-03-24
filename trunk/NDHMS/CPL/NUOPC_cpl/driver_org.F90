#define FILENAME "driver.F90"
#define MODNAME "DRIVER"
#include "NWM_NUOPC_Macros.h"


module driver

  !-----------------------------------------------------------------------------
  ! Code that specializes generic NUOPC_Driver
  !-----------------------------------------------------------------------------
  use mpi
  
  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices
  
  use NWM_NUOPC_Cap, only: nwmSS => SetServices
  
  implicit none

  private
  
  ! private module data --> ONLY PARAMETERS
  integer, parameter            :: stepCount = 5
  real(ESMF_KIND_R8), parameter :: stepTime  = 3600.0     ! step time [sec] - time interval by which the model is advanced
                                                        ! should be parent step - model timesteps must be divisor of this stepTime
  type(ESMF_VM)                 :: vm

  public SetServices
  
  
  contains

  !-----------------------------------------------------------------------------
  ! Set service to  
  !-----------------------------------------------------------------------------  
#undef METHOD
#define METHOD "SetServices" 

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif 

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

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif 

  end subroutine

  !-----------------------------------------------------------------------------
  ! Set model services
  !-----------------------------------------------------------------------------  
#undef METHOD
#define METHOD "SetModelServices"

  subroutine SetModelServices(driver, rc)
    use NWM_NUOPC_Cap, only: nwmSS => SetServices

    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc, esmf_comm
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_VM)                 :: vm

    ! for printing
    type(ESMF_Time)       :: currTime
    integer(ESMF_KIND_I8) :: advanceCount
    integer               :: YY, MM, DD, H, M, S

    rc = ESMF_SUCCESS
    
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif 

    ! Create and add a GridComp (i.e. Model, Mediator, or Driver) as a child
    ! component to a Driver. The component is created on the provided petList,
    ! or by default across all of the Driver PETs. Two interfaces: 
    ! (driver, UniqueCompLabel, compSetServicesRoutine, petList, comp, rc)
    ! (driver, UniqueCompLabel, sharedObj, petList, comp, rc) -- The
    ! SetServices() routine in the sharedObj is called back immediately after
    ! the new child component has been created internally.
    call NUOPC_DriverAddComp(driver, "NWM", nwmSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


    ! Set the Attribute name inside of comp on the highest level of the standard
    ! NUOPC AttPack hierarchy (convention="NUOPC", purpose="Instance").
    ! Return with error if the Attribute is not present or not set.
    ! (comp, name, value, rc)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! set the driver clock- s (integer(ESMF_KIND_I4))
    call ESMF_TimeSet(startTime, s = 0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

         ! i.e.       stopTime=18000   stepTime=3600.00, stepCount=5, 
    call ESMF_TimeSet(stopTime, s_r8 = stepTime * stepCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeStep, s_r8 = stepTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    internalClock = ESMF_ClockCreate(name="Driver Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out  

    ! Examin the clock
    ! get the clock's final current time
    call ESMF_ClockGet(internalClock, currTime=currTime, rc=rc)
    call ESMF_TimeGet(currTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc) 
    print *, "Beheeeeeeen 162 - The clock's final current time is ", YY, "/", MM, "/", DD, &
               " ", H, ":", M, ":", S
    ! get the number of times the clock was advanced
    call ESMF_ClockGet(internalClock, advanceCount=advanceCount, rc=rc)
    print *, "Beheeeeee 166 - The clock was advanced ", advanceCount, " times."
    

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif 
   
  end subroutine

end module
