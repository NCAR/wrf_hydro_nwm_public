program mainApp

  !-----------------------------------------------------------------------------
  ! Generic ESMF Main
  !-----------------------------------------------------------------------------

  use mpi
  
  use ESMF
  use NUOPC
  use Driver, only: driver_SS => SetServices
  use utility

  implicit none
  

  integer                       :: rc, userRc, petid, esmfComm, mpiComm, argCount
  type(ESMF_GridComp)           :: drvComp
  type(ESMF_VM)                 :: vm
  type(ESMF_Config)       :: config
  character(ESMF_MAXSTR)  :: configFile
  logical                       :: mpi_inited     


  
  ! Initialize ESMF
  call ESMF_Initialize(defaultCalkind=ESMF_CALKIND_GREGORIAN, vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  ! print information about ESMF default VM/PET
  ! call ESMF_VMDefault(vm)

  ! Add required fields to NUOPC field dictionary
  call InitFieldDictionary(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  
  call ESMF_LogWrite("mainApp STARTING", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  !-----------------------------------------------------------------------------
  
  ! CREATE THE DRIVER
  drvComp = ESMF_GridCompCreate(name="driver", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    

  ! SET DRIVER SERVICES
  call ESMF_GridCompSetServices(drvComp, driver_SS, userRc=userRc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)


  ! INITIALIZE THE DRIVER
  call ESMF_GridCompInitialize(drvComp, userRc=userRc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
  ! Call Run for earth system component
  call ESMF_GridCompRun(drvComp, userRc=userRc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Call Finalize for earth system component
  call ESMF_GridCompFinalize(drvComp, userRc=userRc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------

  call ESMF_LogWrite("mainApp Finished", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the driver Component
  call ESMF_GridCompDestroy(drvComp, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)


  ! Finalize ESMF
  call ESMF_Finalize()
  
  !------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine DummyRoutine(rc)
    integer, intent(out) :: rc

    ! local variables
    ! none

    rc = ESMF_SUCCESS

  end subroutine

end program  
