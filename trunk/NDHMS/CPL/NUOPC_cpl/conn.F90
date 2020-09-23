!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module CON

  !-----------------------------------------------------------------------------
  ! Connector Component.
  !-----------------------------------------------------------------------------
  
  ! Enabling the followng macro, i.e. setting it to WITHSTATEUSE_on,
  ! will activate sections of code that demonstrate how
  ! the "state" member inside the NUOPC_Connector is used. The
  ! example creates an FieldBundle that's a duplicate of dstFields inside the
  ! connector, and precomputes two RouteHandles. The first is a Regrid, while 
  ! the second is simply an identity operation using FieldRedist() to show the
  ! principle.
#define WITHSTATEUSE_on

  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => SetServices, &
#ifdef WITHSTATEUSE_on
    con_label_ExecuteRH => label_ExecuteRouteHandle, &
    con_label_ReleaseRH => label_ReleaseRouteHandle, &
#endif
    con_label_ComputeRH => label_ComputeRouteHandle, &
    NUOPC_ConnectorGet, NUOPC_ConnectorSet
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC connector component will register the generic methods
    call NUOPC_CompDerive(connector, con_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method to compute the connection RouteHandle
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ComputeRH, &
      specRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef WITHSTATEUSE_on
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ExecuteRH, &
      specRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ReleaseRH, &
      specRoutine=ReleaseRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
      
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ComputeRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: dstFields, srcFields
#ifdef WITHSTATEUSE_on
    type(ESMF_FieldBundle)        :: interDstFields
    type(ESMF_Field), allocatable :: fields(:)
    integer                       :: fieldCount, i
    type(ESMF_Grid)               :: Grid
    type(ESMF_TypeKind_Flag)      :: typekind    
    type(ESMF_Field)              :: field
    type(ESMF_RouteHandle)        :: rh1, rh2
#else
    type(ESMF_RouteHandle)        :: rh
#endif

    rc = ESMF_SUCCESS
    
    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
                     dstFields=dstFields, state=state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifdef WITHSTATEUSE_on
    ! replicate dstFields FieldBundle in order to provide intermediate Fields
    ! - query number of fields in the FieldBundle
    call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! - pull out fields in list form
    ! - !!!! MUST specify itemorderflag=ESMF_ITEMORDER_ADDORDER in order to
    !   !!!! preserve the same order as the original FieldBundle, or else the
    !   !!!! FieldBundle communication methods will incorrectly map from 
    !   !!!! src -> dst!
    allocate(fields(fieldCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=fields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! - create the intermediate FieldBundle
    interDstFields = ESMF_FieldBundleCreate(name="interDstFields", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! - access fields one-by-one, create intermediaries, and add to new bundle
    do i=1, fieldCount
      call ESMF_FieldGet(fields(i), grid=grid, typekind=typekind, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      field = ESMF_FieldCreate(grid, typekind, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_FieldBundleAdd(interDstFields, (/field/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo
    deallocate(fields)
    ! add interDstFields to the state member
    call ESMF_StateAdd(state, (/interDstFields/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! compute the first RouteHandle for srcFields->interDstFields (Regrid)
    call ESMF_FieldBundleRegridStore(srcFields, interDstFields, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, routehandle=rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandleSet(rh1, name="src2interDstRH", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! compute the second RouteHandle for interDstFields->dstFields (Redist)
    call ESMF_FieldBundleRedistStore(interDstFields, dstFields, &
      routehandle=rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandleSet(rh2, name="interDst2dstRH", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! add rh1, rh2 to the state member
    call ESMF_StateAdd(state, (/rh1, rh2/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else      
    ! specialize with Redist, instead of the default Regrid
    call ESMF_FieldBundleRedistStore(srcFields, dstFields, &
      routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_ConnectorSet(connector, rh=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
 
  end subroutine
  
  !-----------------------------------------------------------------------------

#ifdef WITHSTATEUSE_on

  subroutine ExecuteRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_FieldBundle)        :: interDstFields
    type(ESMF_RouteHandle)        :: rh1, rh2
    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: dstFields, srcFields
    type(ESMF_Clock)              :: clock
    character(len=160)            :: msgString

    rc = ESMF_SUCCESS
    
    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, state=state, driverClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
     
    ! test the parent clock
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="Testing parentClock from conn.F90 ExecuteRH(): ", &
      unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! retrieve interDstFields FieldBundle from state member
    call ESMF_StateGet(state, "interDstFields", interDstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! retrieve rh1 from state member
    call ESMF_StateGet(state, "src2interDstRH", rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! retrieve rh2 from state member
    call ESMF_StateGet(state, "interDst2dstRH", rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! apply rh1
    call ESMF_FieldBundleRegrid(srcFields, interDstFields, &
      routehandle=rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! apply rh2
    call ESMF_FieldBundleRedist(interDstFields, dstFields, &
      routehandle=rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
 
  end subroutine
  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: interDstFields
    type(ESMF_RouteHandle)        :: rh1, rh2

    rc = ESMF_SUCCESS
    
    call NUOPC_ConnectorGet(connector, state=state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! retrieve interDstFields FieldBundle from state member
    call ESMF_StateGet(state, "interDstFields", interDstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! retrieve rh1 from state member
    call ESMF_StateGet(state, "src2interDstRH", rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! retrieve rh2 from state member
    call ESMF_StateGet(state, "interDst2dstRH", rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! release rh1
    call ESMF_FieldBundleRegridRelease(rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! release rh2
    call ESMF_FieldBundleRegridRelease(rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Could destroy intermediate Fields and interDstFields FieldBundle here,
    ! but it is more convenient to let ESMF automatic garbage collection take
    ! care of them.
      
  end subroutine

#endif 
  
end module

