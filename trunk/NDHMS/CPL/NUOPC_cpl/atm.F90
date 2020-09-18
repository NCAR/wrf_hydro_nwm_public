!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ATM

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.
    
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
   
    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_VM)        :: vm
    integer              :: localPet
    integer              :: petCount
    integer,parameter    :: gblElementCnt = 185
    integer              :: gblElementDiv
    integer              :: gblElementExt
    integer              :: locElementBeg
    integer              :: locElementCnt
    integer,allocatable  :: arbSeqIndexList(:)
    integer              :: i
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Field)     :: field
    type(ESMF_LocStream) :: locStreamIn
    type(ESMF_LocStream) :: locStreamOut


    real(ESMF_KIND_R8), dimension(:), pointer :: rsnsPtr => null()
    real(ESMF_KIND_R8), dimension(:), pointer :: pmslPtr => null()
    real(ESMF_KIND_R8), dimension(:), pointer :: sstPtr => null()
    integer :: localcount    

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm=vm, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! calculate local element count and first local element
    gblElementDiv = gblElementCnt/petCount
    gblElementExt = MOD(gblElementCnt,petCount)
    if (localPet.eq.(petCount-1)) then
      locElementCnt = gblElementDiv + gblElementExt
    else
      locElementCnt = gblElementDiv
    endif
    locElementBeg = 1 + (gblElementDiv*localPet)

    ! create local element list
    allocate(arbSeqIndexList(locElementCnt))
    allocate(rsnsPtr(locElementCnt))
    allocate(pmslPtr(locElementCnt))
    allocate(sstPtr(locElementCnt))

    do i=1, locElementCnt
      arbSeqIndexList(i) = locElementBeg + (i - 1)
      rsnsPtr(i) = -8.0 * arbSeqIndexList(i)
      pmslPtr(i) = -9.0 * arbSeqIndexList(i)
    enddo

    !print *,"ATM: ",localPet,"arbSeqIndices=", locElementCnt, &
    !  arbSeqIndexList(1),arbSeqIndexList(locElementCnt)

    ! create DistGrid
    distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create a LocationObject object for Fields
    locStreamIn = ESMF_LocStreamCreate(distgrid=distgrid, &
                        coordSys=ESMF_COORDSYS_CART, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    locStreamOut = locStreamIn ! for now out same as in


    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(locstream=locStreamIn, &
                                  farrayPtr=sstPtr, &
              datacopyflag=ESMF_DATACOPY_REFERENCE, &
                                        name="sst", &
                                               rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(locstream=locStreamOut, &
                                  farrayPtr=pmslPtr, &
               datacopyflag=ESMF_DATACOPY_REFERENCE, &
                                        name="pmsl", &
                                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(locstream=locStreamOut, &
                                  farrayPtr=rsnsPtr, &
               datacopyflag=ESMF_DATACOPY_REFERENCE, &
                                        name="rsns", &
                                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    deallocate(arbSeqIndexList)
    !deallocate(rsnsPtr)
    !deallocate(pmslPtr)
    !deallocate(sstPtr)

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    character(len=160)          :: msgString
    
    ! for testing field values
    integer :: itemCnt, i
    character(len=ESMF_MAXSTR), allocatable :: itemNames(:)
    type(ESMF_Field) :: itemField
    real(ESMF_KIND_R8), dimension(:), pointer :: farrayPtr => null()


#define NUOPC_TRACE__OFF
#ifdef NUOPC_TRACE
    call ESMF_TraceRegionEnter("ATM:ModelAdvance")
#endif

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
                        exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  
    ! get field values from the exportState - testing
    call ESMF_StateGet(exportState, itemCount=itemCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    allocate(itemNames(itemCnt))
    call ESMF_StateGet(exportState, itemNameList=itemNames, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    do i=1, itemCnt
        print *, "ATM Field Item Name: ", trim(itemNames(i))
        call ESMF_StateGet(exportState, trim(itemNames(i)), itemField, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        call ESMF_FieldGet(itemField, localDe=0, farrayPtr=farrayPtr, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        print*, "ATM value of export field: ", farrayPtr
    end do
    deallocate(itemNames)
    ! end of test
    

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifdef NUOPC_TRACE
    call ESMF_TraceRegionExit("ATM:ModelAdvance")
#endif
    
  end subroutine

end module
