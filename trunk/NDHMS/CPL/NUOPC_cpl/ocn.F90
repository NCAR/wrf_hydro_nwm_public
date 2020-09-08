!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module OCN

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance
  
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
    call NUOPC_CompSpecialize(model, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
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
    
    ! Beheen
    logical                    :: isPresent


    rc = ESMF_SUCCESS

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(importState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: 
    call NUOPC_Advertise(importState, &
      StandardName="flow_rate", name="streamflow", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Beheen waterlevel mockup
    ! exportable field: waterlevel
    isPresent = NUOPC_FieldDictionaryHasEntry( "water_level", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry("water_level", "m", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    endif

    call NUOPC_Advertise(exportState, &
      StandardName="water_level", name="wl", rc=rc)
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
    type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_VM)        :: vm
    integer              :: localPet
    integer              :: petCount
    integer,parameter    :: gblElementCnt = 185
    integer              :: gblElementExt
    integer              :: gblElementDiv
    integer              :: locElementBeg
    integer              :: locElementCnt
    integer,allocatable  :: arbSeqIndexList(:)
    integer              :: i
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Field)     :: field
    type(ESMF_LocStream) :: locStreamIn
    type(ESMF_LocStream) :: locStreamOut
    
    ! Beheen
    type(ESMF_Field)    :: sstField
    type(ESMF_Field)    :: rsnsField
    type(ESMF_Field)    :: pmslField
    type(ESMF_Field)    :: streamflowField
    real, allocatable   :: rsnsarray(:)
    real, allocatable   :: pmslarray(:)
    real, allocatable   :: sstarray(:)
    real, allocatable   :: streamflowarray(:)

    ! Beheen added for water level mockup
    ! local variables
    integer              :: gblGridExt
    integer              :: gblGridDiv
    integer              :: locGridBeg
    integer              :: locGridCnt
    integer,parameter    :: gblGridCnt = 1000
    type(ESMF_Grid)      :: gridIn
    type(ESMF_Grid)      :: gridOut
    real, allocatable    :: wlarray(:,:)
    integer              :: j
    ! test for GridGet
    integer          :: dimCnt
    integer          :: tileCount
    integer          :: staggerlocCount
    integer          :: localDECount
    !integer, target  :: distgridToGridMap(:)
    !integer, target  :: coordDimCount(:)
    !integer, target  :: coordDimMap(:,:)
    integer          :: arbDim
    integer          :: rank
    integer          :: arbDimCount
    !integer, target  :: gridEdgeLWidth(:)
    !integer, target  :: gridEdgeUWidth(:)
    !integer, target  :: gridAlign(:)
    type(ESMF_Index_Flag)      :: indexflag
    type(ESMF_CoordSys_Flag)   :: coordSys
    type(ESMF_GridStatus_Flag) :: status
    character (len=15)         :: name

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


    ! Beheen for waterlevel mockup
    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/100, 10/), &
         minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
       maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! testing the grid
    !ESMF_GridGet(gridOut, dimCount=dimCnt, tileCount=tileCount, rank=rank,
    !rc=rc )
    !print*, "Beheen OCN: " ,dimCount,tileCount,rank

    ! fill with random values
    gblGridDiv = gblGridCnt/petCount
    gblGridExt = MOD(gblGridCnt,petCount)
    if (localPet.eq.(petCount-1)) then
      locGridCnt = gblGridDiv + gblGridExt
    else
      locGridCnt = gblGridDiv
    endif
    locGridBeg = 1 + (gblGridDiv*localPet)

    allocate(wlarray(10,100))
    do i = 1, 10
        do j = 1, 100
            wlarray(i,j) = i*j*0.2
        enddo
    end do

    ! exportable field: waterlevel
    !field = ESMF_FieldCreate(name="water_level", grid=gridOut, &
    !                  farray=wlarray, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out
    !call NUOPC_Realize(exportState, field=field, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out


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
    allocate(rsnsarray(locElementCnt))
    allocate(pmslarray(locElementCnt))
    allocate(sstarray(locElementCnt))
    allocate(streamflowarray(locElementCnt))
    do i=1, locElementCnt
      arbSeqIndexList(i) = locElementBeg + (i - 1)
      sstarray(i) = 3.0 * arbSeqIndexList(i)
    enddo

    !print *,"OCN: ",localPet,"arbSeqIndices=", &
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

    ! importable field: air_pressure_at_sea_level
    pmslField = ESMF_FieldCreate(locStreamIn, &
                                   pmslarray, &
                          ESMF_INDEX_DELOCAL, &
                                 name="pmsl", &
                                         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=pmslField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    !field = ESMF_FieldCreate(name="rsns", locStream=locStreamIn, &
    !  typekind=ESMF_TYPEKIND_R8, rc=rc)
    rsnsField = ESMF_FieldCreate(locStreamIn, &
                                   rsnsarray, &
                          ESMF_INDEX_DELOCAL, &
                                 name="rsns", &
                                         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=rsnsField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: streamflow
    streamflowField = ESMF_FieldCreate(locStreamIn, &
                                   streamflowarray, &
                                ESMF_INDEX_DELOCAL, &
                                 name="streamflow", &
                                               rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=streamflowField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    !field = ESMF_FieldCreate(name="sst", locStream=locStreamOut, &
    !  typekind=ESMF_TYPEKIND_R8, rc=rc)
    sstField = ESMF_FieldCreate(locStreamOut, &
                                    sstarray, &
                          ESMF_INDEX_DELOCAL, &
                                  name="sst", &
                                         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=sstField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    deallocate(arbSeqIndexList)

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime
    type(ESMF_TimeInterval)     :: timeStep
    character(len=160)          :: msgString

    integer :: itemCnt

#define NUOPC_TRACE__OFF
#ifdef NUOPC_TRACE
    call ESMF_TraceRegionEnter("OCN:ModelAdvance")
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
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing OCN from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
   
    call ESMF_StateGet(exportState, itemCount=itemCnt, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    !print *, "OCN Export State Item Count: ", itemCnt
    
    call ESMF_TimePrint(currTime + timeStep, &
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
    call ESMF_TraceRegionExit("OCN:ModelAdvance")
#endif
  end subroutine

end module
