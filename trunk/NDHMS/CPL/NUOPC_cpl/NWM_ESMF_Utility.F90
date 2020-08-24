#define FILENAME "NWM_ESMF_Utility.F90"
#define MODNAME "NWM_ESMF_Utility.F90"
#include "NWM_NUOPC_Macros.h"

#define DEBUG=on

!-------------------------------------------------------------------------------
! A test coupled application utility module
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   November 2014
!   Beheen M. Trimble, Lynker Tech, NOAA, 8/19/2020
!-------------------------------------------------------------------------------

module NWM_ESMF_Utility

  use ESMF
  use NUOPC

  implicit none
  save
  private

  public NWM_ReachStreamGet
  public NWM_FieldGetBounds
  public NWM_FieldGet
  public InitFieldDictionary
  public PrintTimers

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_Template"
  
  subroutine NWM_Template()
    ! Return object-wide information from a LocStream

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    !rc = ESMF_SUCCESS


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine


#undef METHOD
#define METHOD "NWM_FieldGet"
  
  subroutine NWM_FieldGet(field)
    ! Get a DE-local Fortran array pointer from a Field

    ! ARGUMENTS:
    type(ESMF_Field), intent(in) :: field 
    ! -- The following arguments require argument keyword syntax (e.g. rc=rc). --
    integer :: localDe=0 
    real(ESMF_KIND_R8), pointer :: farrayPtr(:) 
    integer, allocatable :: exclusiveLBound(:) 
    integer, allocatable :: exclusiveUBound(:) 
    integer, allocatable :: exclusiveCount(:) 
    integer, allocatable :: computationalLBound(:) 
    integer, allocatable :: computationalUBound(:) 
    integer, allocatable :: computationalCount(:) 
    integer, allocatable :: totalLBound(:) 
    integer, allocatable :: totalUBound(:) 
    integer, allocatable :: totalCount(:) 
    integer :: rc


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    allocate(exclusiveLBound(2))
    allocate(exclusiveUBound(2))
    allocate(exclusiveCount(2))
    allocate(computationalLBound(2))
    allocate(computationalUBound(2))
    allocate(computationalCount(2))
    allocate(totalLBound(2))
    allocate(totalUBound(2))
    allocate(totalCount(2))

    !call ESMF_FieldGet(field, localDe=localDe, & 
    !     farrayPtr, exclusiveLBound, exclusiveUBound, exclusiveCount, & 
    !     computationalLBound, computationalUBound, computationalCount, & 
    !     totalLBound, totalUBound, totalCount, rc)


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine


#undef METHOD
#define METHOD "NWM_FieldGetBounds"
  
#undef METHOD
#define METHOD "NWM_FieldiGetBounds"
  
  subroutine NWM_FieldGetBounds(field)
     ! Get DE-local Field data bounds

     ! ARGUMENTS:
     type(ESMF_Field), intent(in) :: field
     ! -- The following arguments require argument keyword syntax (e.g. rc=rc). --
     integer :: localDe
     integer, allocatable :: exclusiveLBound(:)
     integer, allocatable :: exclusiveUBound(:)
     integer, allocatable :: exclusiveCount(:)
     integer, allocatable :: computationalLBound(:)
     integer, allocatable :: computationalUBound(:)
     integer, allocatable :: computationalCount(:)
     integer, allocatable :: totalLBound(:)
     integer, allocatable :: totalUBound(:)
     integer, allocatable :: totalCount(:)
     integer :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

     rc = ESMF_SUCCESS

     !call ESMF_FieldGetBounds(field, localDe=0, &
     !      exclusiveLBound=exclusiveLBound, &
     !      exclusiveUBound=exclusiveUBound, &
     !      exclusiveCount=exclusiveCount, &
     !      computationalLBound=computationalLBound , &
     !      computationalUBound=computationalUBound, &
     !      computationalCount=computationalCount, &
     !      totalLBound=totalLBound, totalUBound=totalUBound, &
     !      totalCount=totalCount, rc=rc)
      

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine



#undef METHOD
#define METHOD "NWM_ReachStreamGet"
  
  subroutine NWM_ReachStreamGet(locstream, vm) 

    ! ARGUMENTS:
    type(ESMF_Locstream),  intent(in)  :: locstream
    type(ESMF_VM),         intent(in)  :: vm

    ! The following arguments require argument keyword syntax (e.g. rc=rc). --
    integer :: excLBnd
    integer :: excUBnd
    integer :: excCnt
    integer :: compLBnd
    integer :: compUBnd
    integer :: compCnt
    integer :: totLBnd
    integer :: totUBnd
    integer :: totCnt
    integer :: rc
    !type(ESMF_DataCopy_Flag), intent(in) :: datacopyflag

    ! The following arguments require argument keyword syntax (e.g. rc=rc). --
    character (len=10)          :: cname
    type(ESMF_DistGrid)         :: distgrid
    integer                     :: keyCnt
    character(len=ESMF_MAXSTR), allocatable  :: keyNames(:) 
    integer                     :: localDECnt
    type(ESMF_Index_Flag)       :: indexflg
    type(ESMF_CoordSys_Flag)    :: coordSys

    integer :: i, j, k, esmf_comm, localPet, petCnt

    integer(ESMF_KIND_I4), pointer  :: linkPtr(:) => null()
    real(ESMF_KIND_R8), pointer     :: latPtr(:) => null()
    real(ESMF_KIND_R8), pointer     :: lonPtr(:) => null()

    character(ESMF_MAXSTR)  :: logMsg

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    call ESMF_VMGet(vm=vm, localPet=localPet, petCount=petCnt, &
                               mpiCommunicator=esmf_comm, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! Return object-wide information from a LocStream
    call ESMF_LocStreamGet(locstream, distgrid=distgrid, keyCount=keyCnt, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    print*, "Beheen Printing LocStream NWM_ReachStreamGet"
    call ESMF_LocStreamPrint(locstream)
    print*, "Beheen Printing LocStream NWM_ReachStreamGet"

    allocate(keyNames(keyCnt))
    call ESMF_LocStreamGet(locstream, keyNames=keyNames, coordSys=coordSys, &
             name=cname, localDECount=localDECnt, indexflag=indexflg, rc=rc)

    ! Get a DE-local Fortran array pointer to key values
    call ESMF_LocStreamGetKey(locstream, "Lon", &
            localDE=0, exclusiveLBound=excLBnd, exclusiveUBound=excUBnd, &
            exclusiveCount=excCnt, computationalLBound=compLBnd,         &
            computationalUBound=compUBnd, computationalCount=compCnt,    &
            totalLBound=totLBnd, totalUBound=totUBnd, totalCount=totCnt, &
            datacopyflag=ESMF_DATACOPY_VALUE, farray=lonPtr, rc=rc)

    !allocate(lonPtr(compCnt))
    
    !call ESMF_LocStreamGetBounds(locstream, computationalCount=loccnt, rc=rc)

    do i=0, petCnt-1
      if(i==localPet) then
        print*, "Beheen .........."
        print*, "localPet:",localPet,"petCount:",petCnt
        print*, "LocStream:", cname, "keyCount:", keyCnt
        do j=1, keyCnt
          print*, "keyName:", trim(keyNames(j))
        enddo
        print*, "localDECount:",localDECnt,"coordSys:",coordSys,"indexflag:",indexflg
        print*, "exclusiveLBound:",excLBnd, "exclusiveUBound:",excUBnd
        print*, "exclusiveCount:",excCnt, "computationalLBound:", compLBnd
        print*, "computationalUBound:",compUBnd, "computationalCount:",compCnt
        print*, "totalLBound:",totLBnd, "totalUBound:",totUBnd, "totalCount:",totCnt 
      endif
      call MPI_Barrier(esmf_comm, rc)
      if(ESMF_STDERRORCHECK(rc)) return
    enddo

    deallocate(keyNames)
    !deallocate(lonPtr)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine


  subroutine InitFieldDictionary(rc)
    integer, intent(out) :: rc

    ! local variables
    integer, parameter :: maxFields = 50
    character(ESMF_MAXSTR) :: standardName(maxFields)
    character(ESMF_MAXSTR) :: canonicalUnits(maxFields)
    integer :: i, numFields
    logical :: isPresent

    rc = ESMF_SUCCESS

    i = 0
    ! ATM export fields
    i = i+1; standardName(i) = 'air_pressure_at_sea_level'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'eastward_wind_at_10m_height'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_wind_at_10m_height'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'magnitude_of_surface_downward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_downward_eastward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_downward_northward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'air_temperature_at_2m_height'
             canonicalUnits(i)='K'
    i = i+1; standardName(i) = 'relative_humidity_at_2m_height'
             canonicalUnits(i)='1'
    i = i+1; standardName(i) = 'surface_downward_latent_heat_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_downward_sensible_heat_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_net_downward_shortwave_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_net_downward_longwave_flux'
             canonicalUnits(i)='W m-2'
    ! OCN export fields
    i = i+1; standardName(i) = 'sea_surface_height_above_sea_level'
             canonicalUnits(i)='m'
    i = i+1; standardName(i) = 'sea_surface_temperature'
             canonicalUnits(i)='K'
    i = i+1; standardName(i) = 'sea_surface_salinity'
             canonicalUnits(i)='1e-3'
    i = i+1; standardName(i) = 'surface_eastward_sea_water_velocity'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'surface_northward_sea_water_velocity'
             canonicalUnits(i)='m s-1'
    ! WAV export fields
    i = i+1; standardName(i) = 'wave_z0_roughness_length'
             canonicalUnits(i)='m'
    i = i+1; standardName(i) = 'wave_induced_charnock_parameter'
             canonicalUnits(i)='1'
    i = i+1; standardName(i) = 'surface_total_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_eastward_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_northward_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'eastward_stokes_drift_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_stokes_drift_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'eastward_wave_bottom_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_wave_bottom_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'wave_bottom_current_radian_frequency'
             canonicalUnits(i)='rad s-1'
    i = i+1; standardName(i) = 'eastward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'eastward_northward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'northward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'eastward_wave_radiation_stress_gradient'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'northward_wave_radiation_stress_gradient'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'wave_orbital_turbulence_production'
             canonicalUnits(i)='m2 s-3'
    i = i+1; standardName(i) = 'sea_floor_depth_below_sea_surface'
             canonicalUnits(i)='m'
    i = i+1; standardName(i) = 'air_sea_temperature_difference'
             canonicalUnits(i)='K'
    i = i+1; standardName(i) = 'bottom_friction_coefficient'
             canonicalUnits(i)='1'
    ! ICE export fields
    i = i+1; standardName(i) = 'sea_ice_concentration'
             canonicalUnits(i)='m'
    numFields = i

    do i=1,numFields
      ! normal fields
      isPresent = NUOPC_FieldDictionaryHasEntry(trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry(trim(standardName(i)), &
          trim(canonicalUnits(i)), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      ! background fields (add mbg_ prefix)
      isPresent = NUOPC_FieldDictionaryHasEntry('mbg_'//trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry('mbg_'//trim(standardName(i)), &
          trim(canonicalUnits(i)), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      ! perturbation fields (add pert_ prefix)
      isPresent = NUOPC_FieldDictionaryHasEntry('pert_'//trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry('pert_'//trim(standardName(i)), &
          trim(canonicalUnits(i)), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PrintTimers(cname, wtnam, wtcnt, wtime)
    character(*)          :: cname
    character(*)          :: wtnam(:)
    integer(ESMF_KIND_I4) :: wtcnt(:)
    real(ESMF_KIND_R8)    :: wtime(:)

    ! local variables
    character(ESMF_MAXSTR) :: msg
    integer(ESMF_KIND_I4)  :: k

    write(msg,1) trim(cname),'timer','count','time'
    call ESMF_LogWrite(TRIM(msg), ESMF_LOGMSG_INFO)
    do k=lbound(wtcnt,1),ubound(wtcnt,1)
      write(msg,2) trim(cname),trim(wtnam(k)),wtcnt(k),wtime(k)
      call ESMF_LogWrite(TRIM(msg), ESMF_LOGMSG_INFO)
    enddo

1   format(a,': wtime: ',a20,a10,a14)
2   format(a,': wtime: ',a20,i10,e14.6)

  end subroutine

end module
