#define FILENAME "WRFHydro_NUOPC_Fields.F90"
#define MODNAME "wrfhydro_nuopc_fields"
#include "WRFHydro_NUOPC_Macros.h"

module wrfhydro_nuopc_fields
! !MODULE: wrfhydro_nuopc_fields
!
! !DESCRIPTION:
!   This module connects NUOPC field information for WRFHYDRO
!
! !REVISION HISTORY:
!  21Jul23    Dan Rosen  Initial Specification
!
! !USES:
  use ESMF
  use NUOPC
  use WRFHydro_ESMF_Extensions
  use WRFHydro_NUOPC_Flags
  use config_base,      only: nlst
  use module_rt_data,   only: rt_domain
  use overland_data,    only: overland_struct
  use overland_control, only: overland_control_struct

  implicit none

  private

  type cap_fld_type
    sequence
    character(len=64)           :: sd_name   = "dummy" ! standard name
    character(len=64)           :: st_name   = "dummy" ! state name
    character(len=64)           :: units     = "-"     ! units
    logical                     :: ad_import = .FALSE. ! advertise import
    logical                     :: ad_export = .FALSE. ! advertise export
    real(ESMF_KIND_R8)          :: vl_fillv  = ESMF_MISSING_VALUE ! default
    logical                     :: rl_import = .FALSE. ! realize import
    logical                     :: rl_export = .FALSE. ! realize export
  end type cap_fld_type

  type(cap_fld_type),target,dimension(20) :: cap_fld_list = (/          &
    cap_fld_type("inst_total_soil_moisture_content        ","smc     ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("inst_soil_moisture_content              ","slc     ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("inst_soil_temperature                   ","stc     ", &
                 "K     ",.TRUE. ,.FALSE.,288.d0),                      &
    cap_fld_type("liquid_fraction_of_soil_moisture_layer_1","sh2ox1  ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("liquid_fraction_of_soil_moisture_layer_2","sh2ox2  ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("liquid_fraction_of_soil_moisture_layer_3","sh2ox3  ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("liquid_fraction_of_soil_moisture_layer_4","sh2ox4  ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("soil_moisture_fraction_layer_1          ","smc1    ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("soil_moisture_fraction_layer_2          ","smc2    ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("soil_moisture_fraction_layer_3          ","smc3    ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("soil_moisture_fraction_layer_4          ","smc4    ", &
                 "m3 m-3",.TRUE. ,.TRUE. ,0.20d0),                      &
    cap_fld_type("soil_temperature_layer_1                ","stc1    ", &
                 "K     ",.TRUE. ,.FALSE.,288.d0),                      &
    cap_fld_type("soil_temperature_layer_2                ","stc2    ", &
                 "K     ",.TRUE. ,.FALSE.,288.d0),                      &
    cap_fld_type("soil_temperature_layer_3                ","stc3    ", &
                 "K     ",.TRUE. ,.FALSE.,288.d0),                      &
    cap_fld_type("soil_temperature_layer_4                ","stc4    ", &
                 "K     ",.TRUE. ,.FALSE.,288.d0),                      &
    cap_fld_type("soil_porosity                           ","smcmax1 ", &
                 "1     ",.FALSE.,.FALSE.,0.45d0),                      &
    cap_fld_type("vegetation_type                         ","vegtyp  ", &
                 "1     ",.FALSE.,.FALSE.,16.0d0),                      &
    cap_fld_type("surface_water_depth                     ","sfchead ", &
                 "mm    ",.FALSE.,.TRUE. ,0.00d0),                      &
    cap_fld_type("time_step_infiltration_excess           ","infxsrt ", &
                 "mm    ",.TRUE. ,.FALSE.,0.00d0),                      &
    cap_fld_type("soil_column_drainage                    ","soldrain", &
                 "mm    ",.TRUE. ,.FALSE.,0.00d0)                       &
    /)

  public cap_fld_list
  public field_dictionary_add
  public field_create
  public field_realize
  public field_advertise
  public check_lsm_forcings
  public field_advertise_log
  public field_realize_log
  public read_impexp_config_flnm
  public field_find_standardname
  public field_find_statename
  public state_fill_uniform
  public state_fill_prescribe
  public state_fill_file
  public state_copy_tohyd
  public state_copy_frhyd
  public state_check_missing
  public state_prescribe_missing
  public model_debug

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_dictionary_add"
  subroutine field_dictionary_add(fieldList, rc)
    type(cap_fld_type), intent(in) :: fieldList(:)
    integer, intent(out)           :: rc
    ! local variables
    integer :: n
    logical :: isPresent

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      isPresent = NUOPC_FieldDictionaryHasEntry( &
        fieldList(n)%sd_name, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry( &
          StandardName=trim(fieldList(n)%sd_name), &
          canonicalUnits=trim(fieldList(n)%units), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
    end do

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_realize"
  subroutine field_realize(fieldList, importState, exportState, grid, &
  did, realizeAllImport, realizeAllExport, memr_import, memr_export, rc)
    type(cap_fld_type), intent(inout) :: fieldList(:)
    type(ESMF_State), intent(inout)   :: importState
    type(ESMF_State), intent(inout)   :: exportState
    type(ESMF_Grid), intent(in)       :: grid
    integer, intent(in)               :: did
    logical, intent(in)               :: realizeAllImport
    logical, intent(in)               :: realizeAllExport
    type(memory_flag)                 :: memr_import
    type(memory_flag)                 :: memr_export
    integer, intent(out)              :: rc
    ! local variables
    integer :: n
    logical :: realizeImport
    logical :: realizeExport
    type(ESMF_Field) :: field_import
    type(ESMF_Field) :: field_export

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      ! check realize import
      if (fieldList(n)%ad_import) then
        if (realizeAllImport) then
          realizeImport = .true.
        else
          realizeImport = NUOPC_IsConnected(importState, &
            fieldName=trim(fieldList(n)%st_name),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        end if
      else
        realizeImport = .false.
      end if
      ! create import field
      if ( realizeImport ) then
        field_import=field_create(fld_name=fieldList(n)%st_name, &
          grid=grid, did=did, memflg=memr_import, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(importState, field=field_import, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_import = .true.
      else
        call ESMF_StateRemove(importState, (/fieldList(n)%st_name/), &
          relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_import = .false.
      end if

      ! check realize export
      if (fieldList(n)%ad_export) then
        if (realizeAllExport) then
          realizeExport = .true.
        else
          realizeExport = NUOPC_IsConnected(exportState, &
            fieldName=trim(fieldList(n)%st_name),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        end if
      else
        realizeExport = .false.
      end if
      ! create export field
      if( realizeExport ) then
        field_export=field_create(fld_name=fieldList(n)%st_name, &
          grid=grid, did=did, memflg=memr_export, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(exportState, field=field_export, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_export = .true.
      else
        call ESMF_StateRemove(exportState, (/fieldList(n)%st_name/), &
          relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_export = .false.
      end if
    end do
  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "lsm_forcings"

  function check_lsm_forcings(importState,rc)
    ! RETURN
    logical :: check_lsm_forcings
    ! ARGUMENTS
    type(ESMF_State), intent(in) :: importState
    integer, intent(out)         :: rc
    ! LOCAL VARIABLES
    integer                    :: fieldIndex
    type(ESMF_StateItem_Flag)  :: itemType
    integer                    :: s_smc, s_smc1, s_smc2, s_smc3, s_smc4
    integer                    :: s_slc, s_slc1, s_slc2, s_slc3, s_slc4
    integer                    :: s_stc, s_stc1, s_stc2, s_stc3, s_stc4
    integer                    :: s_infxsrt
    integer                    :: s_soldrain
    logical                    :: c_smc
    logical                    :: c_slc
    logical                    :: c_stc
    logical                    :: c_infxsrt
    logical                    :: c_soldrain

    ! total soil moisture content
    call ESMF_StateGet(importState,itemSearch="smc", itemCount=s_smc, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="smc1",itemCount=s_smc1,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="smc2",itemCount=s_smc2,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="smc3",itemCount=s_smc3,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="smc4",itemCount=s_smc4,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_smc.gt.0) then
      c_smc = NUOPC_IsConnected(importState, fieldName="smc")
    elseif ((s_smc1.gt.0) .and. (s_smc2.gt.0) .and. &
            (s_smc3.gt.0) .and. (s_smc4.gt.0)) then
      c_smc = (NUOPC_IsConnected(importState, fieldName="smc1") .and. &
               NUOPC_IsConnected(importState, fieldName="smc2") .and. &
               NUOPC_IsConnected(importState, fieldName="smc3") .and. &
               NUOPC_IsConnected(importState, fieldName="smc4"))
    else
      c_smc = .false.
    endif

    ! liquid soil moisture content
    call ESMF_StateGet(importState,itemSearch="slc", itemCount=s_slc, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="sh2ox1",itemCount=s_slc1,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="sh2ox2",itemCount=s_slc2,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="sh2ox3",itemCount=s_slc3,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="sh2ox4",itemCount=s_slc4,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_slc.gt.0) then
      c_slc = NUOPC_IsConnected(importState, fieldName="slc")
    elseif ((s_slc1.gt.0) .and. (s_slc2.gt.0) .and. &
            (s_slc3.gt.0) .and. (s_slc4.gt.0)) then
      c_slc = (NUOPC_IsConnected(importState, fieldName="sh2ox1") .and. &
               NUOPC_IsConnected(importState, fieldName="sh2ox2") .and. &
               NUOPC_IsConnected(importState, fieldName="sh2ox3") .and. &
               NUOPC_IsConnected(importState, fieldName="sh2ox4"))
    else
      c_slc = .false.
    endif

    ! soil temperature
    call ESMF_StateGet(importState,itemSearch="stc", itemCount=s_stc, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="stc1",itemCount=s_stc1,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="stc2",itemCount=s_stc2,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="stc3",itemCount=s_stc3,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState,itemSearch="stc4",itemCount=s_stc4,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_stc.gt.0) then
      c_stc = NUOPC_IsConnected(importState, fieldName="stc")
    elseif ((s_stc1.gt.0) .and. (s_stc2.gt.0) .and. &
            (s_stc3.gt.0) .and. (s_stc4.gt.0)) then
      c_stc = (NUOPC_IsConnected(importState, fieldName="stc1") .and. &
               NUOPC_IsConnected(importState, fieldName="stc2") .and. &
               NUOPC_IsConnected(importState, fieldName="stc3") .and. &
               NUOPC_IsConnected(importState, fieldName="stc4"))
    else
      c_stc = .false.
    endif

    ! infiltration excess
    call ESMF_StateGet(importState,itemSearch="infxsrt",itemCount=s_infxsrt,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_infxsrt.gt.0) then
      c_infxsrt = NUOPC_IsConnected(importState, fieldName="infxsrt")
    else
      c_infxsrt = .false.
    endif

    ! soil drainage
    call ESMF_StateGet(importState,itemSearch="soldrain",itemCount=s_soldrain,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_soldrain.gt.0) then
      c_soldrain = NUOPC_IsConnected(importState, fieldName="soldrain")
    else
      c_soldrain = .false.
    endif

    check_lsm_forcings = c_smc .and. c_slc .and. c_stc .and. &
                         c_infxsrt .and. c_soldrain

  end function

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_advertise"
  subroutine field_advertise(fieldList, importState, exportState, &
  transferOffer, rc)
    type(cap_fld_type), intent(in)    :: fieldList(:)
    type(ESMF_State), intent(inout)   :: importState
    type(ESMF_State), intent(inout)   :: exportState
    character(*), intent(in),optional :: transferOffer
    integer, intent(out)              :: rc
    ! local variables
    integer :: n

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%ad_import) then
        call NUOPC_Advertise(importState, &
          StandardName=fieldList(n)%sd_name, &
          Units=fieldList(n)%units, &
          TransferOfferGeomObject=transferOffer, &
          name=fieldList(n)%st_name, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
      if (fieldList(n)%ad_export) then
        call NUOPC_Advertise(exportState, &
          StandardName=fieldList(n)%sd_name, &
          Units=fieldList(n)%units, &
          TransferOfferGeomObject=transferOffer, &
          name=fieldList(n)%st_name, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
    end do

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_advertise_log"
  subroutine field_advertise_log(fieldList, cname, rc)
    type(cap_fld_type), intent(in) :: fieldList(:)
    character(*), intent(in)       :: cname
    integer, intent(out)           :: rc
    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: n
    character(32)              :: label
    character(ESMF_MAXSTR)     :: logMsg

    rc = ESMF_SUCCESS

    label = trim(cname)

    ! count advertised import and export fields
    cntImp = 0
    cntExp = 0
    do n = lbound(fieldList,1), ubound(fieldList,1)
      if (fieldList(n)%ad_import) cntImp = cntImp + 1
      if (fieldList(n)%ad_export) cntExp = cntExp + 1
    enddo

    ! log advertised import fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of advertised import fields(',cntImp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%ad_import) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntImp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

    ! log advertised export fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of advertised export fields(',cntExp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%ad_export) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntExp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_realize_log"
  subroutine field_realize_log(fieldList, cname, rc)
    type(cap_fld_type), intent(in) :: fieldList(:)
    character(*), intent(in)       :: cname
    integer, intent(out)           :: rc
    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: n
    character(32)              :: label
    character(ESMF_MAXSTR)     :: logMsg

    rc = ESMF_SUCCESS

    label = trim(cname)

    ! count realized import and export fields
    cntImp = 0
    cntExp = 0
    do n = lbound(fieldList,1), ubound(fieldList,1)
      if (fieldList(n)%rl_import) cntImp = cntImp + 1
      if (fieldList(n)%rl_export) cntExp = cntExp + 1
    enddo

    ! log realized import fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of realized import fields(',cntImp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%rl_import) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntImp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

    ! log realized export fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of realized export fields(',cntExp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%rl_export) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntExp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "read_impexp_config_flnm"
  subroutine read_impexp_config_flnm(fname, fieldList, rc)
    character(len=30),intent(in)     :: fname
    type(cap_fld_type),intent(inout) :: fieldList(:)
    integer,intent(out)              :: rc

    ! local variables
    type(ESMF_Config)                  :: fieldsConfig
    type(NUOPC_FreeFormat)             :: attrFF
    integer                            :: lineCount
    integer                            :: tokenCount
    character(len=NUOPC_FreeFormatLen),allocatable :: tokenList(:)
    integer                            :: i,j
    integer                            :: stat

    rc = ESMF_SUCCESS

!   load fname into fieldsConfig
    fieldsConfig = ESMF_ConfigCreate(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_ConfigLoadFile(fieldsConfig, fname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

!   read export fields from config
    attrFF = NUOPC_FreeFormatCreate(fieldsConfig, &
      label="hyd_fields", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_FreeFormatGet(attrFF, lineCount=lineCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    do i=1, lineCount
      call NUOPC_FreeFormatGetLine(attrFF, line=i, &
        tokenCount=tokenCount, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (.not.((tokenCount.eq.5).or.(tokenCount.eq.6))) then
        call ESMF_LogSetError(ESMF_FAILURE, &
          msg="Malformed ocn_export_fields item FORMAT="// &
            "'STATE_NAME' 'STANDARD_NAME' 'UNITS' 'IMPORT' 'EXPORT' "// &
!            "['FILLVAL'] "// &
            "in file: "//trim(fname), &
          CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(tokenList(tokenCount))
      call NUOPC_FreeFormatGetLine(attrFF, line=i, tokenList=tokenList, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call field_find_statename(fieldList, tokenList(1), location=j, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      fieldList(j)%st_name=tokenList(1)
      fieldList(j)%sd_name=tokenList(2)
      fieldList(j)%units=tokenList(3)
      tokenList(4) = ESMF_UtilStringUpperCase(tokenList(4), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      fieldList(j)%ad_import=((tokenList(4).eq.".TRUE.") .or. &
                         (tokenList(4).eq."TRUE"))
      tokenList(5) = ESMF_UtilStringUpperCase(tokenList(5), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      fieldList(j)%ad_export=((tokenList(5).eq.".TRUE.") .or. &
                         (tokenList(5).eq."TRUE"))
      if (tokenCount.eq.6) then
        fieldList(j)%vl_fillv = ESMF_UtilString2Real(tokenList(6), rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      deallocate(tokenList)
    enddo

!   cleanup
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_ConfigDestroy(fieldsConfig, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

  end subroutine read_impexp_config_flnm

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_find_standardname"
  subroutine field_find_standardname(fieldList, standardName, location, &
  fillValue, rc)
    type(cap_fld_type), intent(in)          :: fieldList(:)
    character(len=64), intent(in)           :: standardName
    integer, intent(out), optional          :: location
    real(ESMF_KIND_R8),intent(out),optional :: fillValue
    integer, intent(out)                    :: rc
    ! local variables
    integer :: n

    rc = ESMF_RC_NOT_FOUND

    if (present(location)) location = lbound(fieldList,1) - 1
    if (present(fillValue)) fillValue = ESMF_MISSING_VALUE

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%sd_name .eq. standardName) then
        if (present(location)) location = n
        if (present(fillValue)) fillValue = fieldList(n)%vl_fillv
        rc = ESMF_SUCCESS
        return
      end if
    end do

    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Field not found in fieldList "//trim(standardName), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_find_statename"
  subroutine field_find_statename(fieldList, stateName, location, &
  fillValue, rc)
    type(cap_fld_type), intent(in)          :: fieldList(:)
    character(len=64), intent(in)           :: stateName
    integer, intent(out), optional          :: location
    real(ESMF_KIND_R8),intent(out),optional :: fillValue
    integer, intent(out)                    :: rc
    ! local variables
    integer :: n

    rc = ESMF_RC_NOT_FOUND

    if (present(location)) location = lbound(fieldList,1) - 1
    if (present(fillValue)) fillValue = ESMF_MISSING_VALUE

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%st_name .eq. stateName) then
        if (present(location)) location = n
        if (present(fillValue)) fillValue = fieldList(n)%vl_fillv
        rc = ESMF_SUCCESS
        return
      end if
    end do

    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Field not found in fieldList "//trim(stateName), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "field_create"
  function field_create(fld_name,grid,did,memflg,rc)
    ! return value
    type(ESMF_Field) :: field_create
    ! arguments
    character(*), intent(in)      :: fld_name
    type(ESMF_Grid), intent(in)   :: grid
    integer, intent(in)           :: did
    type(memory_flag), intent(in) :: memflg
    integer,          intent(out) :: rc
    ! local variables
    character(len=16)       :: cmemflg


    rc = ESMF_SUCCESS

    if (memflg .eq. MEMORY_POINTER) then
      select case (trim(fld_name))
        case ('smc')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%smc(:,:,:), gridToFieldMap=(/1,2/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('slc')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%sh2ox(:,:,:), gridToFieldMap=(/1,2/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('stc')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%stc(:,:,:), gridToFieldMap=(/1,2/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('sh2ox1')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%sh2ox(:,:,1), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('sh2ox2')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%sh2ox(:,:,2), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('sh2ox3')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%sh2ox(:,:,3), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('sh2ox4')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%sh2ox(:,:,4), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('smc1')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%smc(:,:,1), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('smc2')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%smc(:,:,2), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('smc3')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%smc(:,:,3), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('smc4')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%smc(:,:,4), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('smcmax1')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%smcmax1, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('stc1')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%stc(:,:,1), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('stc2')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%stc(:,:,2), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('stc3')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%stc(:,:,3), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('stc4')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%stc(:,:,4), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('vegtyp')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%vegtyp, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('sfchead')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%overland%control%surface_water_head_lsm, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case ('infxsrt')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%infxsrt, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        case ('soldrain')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            farray=rt_domain(did)%soldrain, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        case default
          call ESMF_LogSetError(ESMF_FAILURE, &
            msg=METHOD//": Field hookup missing: "//trim(fld_name), &
            file=FILENAME,rcToReturn=rc)
          return  ! bail out
      end select
    elseif (memflg .eq. MEMORY_COPY) then
      select case (trim(fld_name))
        case ('smc','slc','stc')
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            typekind=ESMF_TYPEKIND_FIELD, gridToFieldMap=(/1,2/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/nlst(did)%nsoil/), &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
        case default
          field_create = ESMF_FieldCreate(name=fld_name, grid=grid, &
            typekind=ESMF_TYPEKIND_FIELD, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
      end select
      call ESMF_FieldFill(field_create, dataFillScheme="const", &
        const1=ESMF_MISSING_VALUE, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    else
      cmemflg = memflg
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Field memory flag unknown: "//trim(cmemflg), &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

  end function

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "state_fill_uniform"
  subroutine state_fill_uniform(state, fillValue, rc)
    type(ESMF_State), intent(inout)        :: state
    real(ESMF_KIND_R8), intent(in)         :: fillValue
    integer, intent(out)                   :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do n=1, itemCount
      if (itemTypeList(n) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state, field=field, &
          itemName=itemNameList(n),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_FieldFill(field, dataFillScheme="const", &
          const1=fillValue, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine state_fill_uniform

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "state_fill_prescribe"
  subroutine state_fill_prescribe(state, fieldList, rc)
    type(ESMF_State), intent(inout)        :: state
    type(cap_fld_type), intent(in)         :: fieldList(:)
    integer, intent(out)                   :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    real(ESMF_KIND_R8)                     :: filVal
    integer                                :: stat

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do n=1, itemCount
      if (itemTypeList(n) == ESMF_STATEITEM_FIELD) then
        call field_find_statename(fieldList, &
          stateName=itemNameList(n), fillValue=filVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_StateGet(state, field=field, &
          itemName=itemNameList(n),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_FieldFill(field, dataFillScheme="const", &
          const1=filVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine state_fill_prescribe

 !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "state_fill_file"
  subroutine state_fill_file(state, filePrefix, rc)
    type(ESMF_State), intent(inout) :: state
    character(len=*), intent(in)    :: filePrefix
    integer, intent(out)            :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    character(len=64)                      :: fldName
    integer                                :: stat

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do n=1, itemCount
      if ( itemTypeList(n) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state,field=field, &
          itemName=itemNameList(n),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_GetAttribute(field, name="StandardName", &
          value=fldName, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_FieldRead(field, variableName=trim(fldName), &
          fileName=trim(filePrefix)//"_"//trim(itemNameList(n))//".nc", &
          iofmt=ESMF_IOFMT_NETCDF, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine state_fill_file

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "state_copy_tohyd"
  subroutine state_copy_tohyd(state, did, rc)
    type(ESMF_State), intent(inout) :: state
    integer, intent(in)             :: did
    integer, intent(out)            :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: dimCount
    real(ESMF_KIND_FIELD), pointer         :: farrayPtr2d(:,:)
    real(ESMF_KIND_FIELD), pointer         :: farrayPtr3d(:,:,:)
    integer                                :: stat

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do n=1, itemCount
      if (itemTypeList(n) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state, field=field, &
          itemName=itemNameList(n),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_FieldGet(field, dimCount=dimCount, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        if (dimCount .eq. 2) then
          call ESMF_FieldGet(field, farrayPtr=farrayPtr2d, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        elseif (dimCount .eq. 3) then
          call ESMF_FieldGet(field, farrayPtr=farrayPtr3d, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        else
          call ESMF_LogSetError(ESMF_FAILURE, &
            msg=METHOD//": dimCount is not supported.", &
            file=FILENAME,rcToReturn=rc)
          return  ! bail out
        endif
        select case (ItemNameList(n))
          case ('smc')
            rt_domain(did)%smc = farrayPtr3d
          case ('slc')
            rt_domain(did)%sh2ox = farrayPtr3d
          case ('stc')
            rt_domain(did)%stc = farrayPtr3d
          case ('sh2ox1')
            rt_domain(did)%sh2ox(:,:,1) = farrayPtr2d
          case ('sh2ox2')
            rt_domain(did)%sh2ox(:,:,2) = farrayPtr2d
          case ('sh2ox3')
            rt_domain(did)%sh2ox(:,:,3) = farrayPtr2d
          case ('sh2ox4')
            rt_domain(did)%sh2ox(:,:,4) = farrayPtr2d
          case ('smc1')
            rt_domain(did)%smc(:,:,1) = farrayPtr2d
          case ('smc2')
            rt_domain(did)%smc(:,:,2) = farrayPtr2d
          case ('smc3')
            rt_domain(did)%smc(:,:,3) = farrayPtr2d
          case ('smc4')
            rt_domain(did)%smc(:,:,4) = farrayPtr2d
          case ('smcmax1')
            rt_domain(did)%smcmax1 = farrayPtr2d
          case ('stc1')
            rt_domain(did)%stc(:,:,1) = farrayPtr2d
          case ('stc2')
            rt_domain(did)%stc(:,:,2) = farrayPtr2d
          case ('stc3')
            rt_domain(did)%stc(:,:,3) = farrayPtr2d
          case ('stc4')
            rt_domain(did)%stc(:,:,4) = farrayPtr2d
          case ('vegtyp')
            rt_domain(did)%vegtyp = farrayPtr2d
          case ('sfchead')
            rt_domain(did)%overland%control%surface_water_head_lsm = &
              farrayPtr2d
          case ('infxsrt')
            rt_domain(did)%infxsrt = farrayPtr2d
          case ('soldrain')
            rt_domain(did)%soldrain = farrayPtr2d
          case default
            call ESMF_LogSetError(ESMF_FAILURE, &
              msg=METHOD//": Field hookup missing: "//trim(itemNameList(n)), &
              file=FILENAME,rcToReturn=rc)
            return  ! bail out
        endselect
      endif
    enddo

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine state_copy_tohyd

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "state_copy_frhyd"
  subroutine state_copy_frhyd(state, did, rc)
    type(ESMF_State), intent(inout)         :: state
    integer, intent(in)                     :: did
    integer, intent(out)                    :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: dimCount
    real(ESMF_KIND_FIELD), pointer         :: farrayPtr2d(:,:)
    real(ESMF_KIND_FIELD), pointer         :: farrayPtr3d(:,:,:)
    integer                                :: stat
    character(len=16)                      :: cmissingv_flag

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do n=1, itemCount
      if (itemTypeList(n) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state, field=field, &
          itemName=itemNameList(n),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_FieldGet(field, dimCount=dimCount, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        if (dimCount .eq. 2) then
          call ESMF_FieldGet(field, farrayPtr=farrayPtr2d, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        elseif (dimCount .eq. 3) then
          call ESMF_FieldGet(field, farrayPtr=farrayPtr3d, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        else
          call ESMF_LogSetError(ESMF_FAILURE, &
            msg=METHOD//": dimCount is not supported.", &
            file=FILENAME,rcToReturn=rc)
          return  ! bail out
        endif
        select case (ItemNameList(n))
          case ('smc')
            farrayPtr3d = rt_domain(did)%smc
          case ('slc')
            farrayPtr3d = rt_domain(did)%sh2ox
          case ('stc')
            farrayPtr3d = rt_domain(did)%stc
          case ('sh2ox1')
            farrayPtr2d = rt_domain(did)%sh2ox(:,:,1)
          case ('sh2ox2')
            farrayPtr2d = rt_domain(did)%sh2ox(:,:,2)
          case ('sh2ox3')
            farrayPtr2d = rt_domain(did)%sh2ox(:,:,3)
          case ('sh2ox4')
            farrayPtr2d = rt_domain(did)%sh2ox(:,:,4)
          case ('smc1')
            farrayPtr2d = rt_domain(did)%smc(:,:,1)
          case ('smc2')
            farrayPtr2d = rt_domain(did)%smc(:,:,2)
          case ('smc3')
            farrayPtr2d = rt_domain(did)%smc(:,:,3)
          case ('smc4')
            farrayPtr2d = rt_domain(did)%smc(:,:,4)
          case ('smcmax1')
            farrayPtr2d = rt_domain(did)%smcmax1
          case ('stc1')
            farrayPtr2d = rt_domain(did)%stc(:,:,1)
          case ('stc2')
            farrayPtr2d = rt_domain(did)%stc(:,:,2)
          case ('stc3')
            farrayPtr2d = rt_domain(did)%stc(:,:,3)
          case ('stc4')
            farrayPtr2d = rt_domain(did)%stc(:,:,4)
          case ('vegtyp')
            farrayPtr2d = rt_domain(did)%vegtyp
          case ('sfchead')
            farrayPtr2d = rt_domain(did)%overland%control%surface_water_head_lsm
          case ('infxsrt')
            farrayPtr2d = rt_domain(did)%infxsrt
          case ('soldrain')
            farrayPtr2d = rt_domain(did)%soldrain
          case default
            call ESMF_LogSetError(ESMF_FAILURE, &
              msg=METHOD//": Field hookup missing: "//trim(itemNameList(n)), &
              file=FILENAME,rcToReturn=rc)
            return  ! bail out
        end select
      endif
    enddo

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine state_copy_frhyd

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "state_check_missing"
  subroutine state_check_missing(state, did, rc)
    type(ESMF_State), intent(inout)   :: state
    integer, intent(in)               :: did
    integer, intent(out)              :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    real(ESMF_KIND_R8), parameter          :: chkVal = real(ESMF_MISSING_VALUE)
    logical                                :: missng
    integer                                :: stat

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do n=1, itemCount
      if (itemTypeList(n) == ESMF_STATEITEM_FIELD) then
        missng = .FALSE.
        select case (ItemNameList(n))
          case ('smc')
            missng = any(rt_domain(did)%smc.eq.chkVal)
          case ('slc')
            missng = any(rt_domain(did)%sh2ox.eq.chkVal)
          case ('stc')
            missng = any(rt_domain(did)%stc.eq.chkVal)
          case ('sh2ox1')
            missng = any(rt_domain(did)%sh2ox(:,:,1).eq.chkVal)
          case ('sh2ox2')
            missng = any(rt_domain(did)%sh2ox(:,:,2).eq.chkVal)
          case ('sh2ox3')
            missng = any(rt_domain(did)%sh2ox(:,:,3).eq.chkVal)
          case ('sh2ox4')
            missng = any(rt_domain(did)%sh2ox(:,:,4).eq.chkVal)
          case ('smc1')
            missng = any(rt_domain(did)%smc(:,:,1).eq.chkVal)
          case ('smc2')
            missng = any(rt_domain(did)%smc(:,:,2).eq.chkVal)
          case ('smc3')
            missng = any(rt_domain(did)%smc(:,:,3).eq.chkVal)
          case ('smc4')
            missng = any(rt_domain(did)%smc(:,:,4).eq.chkVal)
          case ('smcmax1')
            missng = any(rt_domain(did)%smcmax1.eq.chkVal)
          case ('stc1')
            missng = any(rt_domain(did)%stc(:,:,1).eq.chkVal)
          case ('stc2')
            missng = any(rt_domain(did)%stc(:,:,2).eq.chkVal)
          case ('stc3')
            missng = any(rt_domain(did)%stc(:,:,3).eq.chkVal)
          case ('stc4')
            missng = any(rt_domain(did)%stc(:,:,4).eq.chkVal)
          case ('vegtyp')
            missng = any(rt_domain(did)%vegtyp.eq.chkVal)
          case ('sfchead')
            missng = any(rt_domain(did)%overland%control%surface_water_head_lsm &
              .eq.chkVal)
          case ('infxsrt')
            missng = any(rt_domain(did)%infxsrt.eq.chkVal)
          case ('soldrain')
            missng = any(rt_domain(did)%soldrain.eq.chkVal)
          case default
            call ESMF_LogSetError(ESMF_FAILURE, &
              msg=METHOD//": Field hookup missing: "//trim(itemNameList(n)), &
              file=FILENAME,rcToReturn=rc)
            return  ! bail out
        endselect
        if (missng) then
          call ESMF_LogSetError(ESMF_FAILURE, &
            msg=METHOD//": Missing value: "//trim(itemNameList(n)), &
            file=FILENAME,rcToReturn=rc)
          return  ! bail out
        endif
      endif
    enddo
    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine state_check_missing

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "state_prescribe_missing"
  subroutine state_prescribe_missing(state, did, fieldList, rc)
    type(ESMF_State), intent(inout)   :: state
    integer, intent(in)               :: did
    type(cap_fld_type), intent(in)    :: fieldList(:)
    integer, intent(out)              :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    real(ESMF_KIND_R8), parameter          :: chkVal = real(ESMF_MISSING_VALUE)
    real(ESMF_KIND_R8)                     :: filVal
    integer                                :: stat

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do n=1, itemCount
      if (itemTypeList(n) == ESMF_STATEITEM_FIELD) then
        call field_find_statename(fieldList, &
          stateName=itemNameList(n), fillValue=filVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        select case (itemNameList(n))
          case ('smc')
            where (rt_domain(did)%smc.eq.chkVal) &
              rt_domain(did)%smc = filVal
          case ('slc')
            where (rt_domain(did)%sh2ox.eq.chkVal) &
              rt_domain(did)%sh2ox = filVal
          case ('stc')
            where (rt_domain(did)%stc.eq.chkVal) &
              rt_domain(did)%stc = filVal
          case ('sh2ox1')
            where (rt_domain(did)%sh2ox(:,:,1).eq.chkVal) &
              rt_domain(did)%sh2ox(:,:,1) = filVal
          case ('sh2ox2')
            where (rt_domain(did)%sh2ox(:,:,2).eq.chkVal) &
              rt_domain(did)%sh2ox(:,:,2) = filVal
          case ('sh2ox3')
            where (rt_domain(did)%sh2ox(:,:,3).eq.chkVal) &
              rt_domain(did)%sh2ox(:,:,3) = filVal
          case ('sh2ox4')
            where (rt_domain(did)%sh2ox(:,:,4).eq.chkVal) &
              rt_domain(did)%sh2ox(:,:,4) = filVal
          case ('smc1')
            where (rt_domain(did)%smc(:,:,1).eq.chkVal) &
              rt_domain(did)%smc(:,:,1) = filVal
          case ('smc2')
            where (rt_domain(did)%smc(:,:,2).eq.chkVal) &
              rt_domain(did)%smc(:,:,2) = filVal
          case ('smc3')
            where (rt_domain(did)%smc(:,:,3).eq.chkVal) &
              rt_domain(did)%smc(:,:,3) = filVal
          case ('smc4')
            where (rt_domain(did)%smc(:,:,4).eq.chkVal) &
              rt_domain(did)%smc(:,:,4) = filVal
          case ('smcmax1')
            where (rt_domain(did)%smcmax1.eq.chkVal) &
              rt_domain(did)%smcmax1 = filVal
          case ('stc1')
            where (rt_domain(did)%stc(:,:,1).eq.chkVal) &
              rt_domain(did)%stc(:,:,1) = filVal
          case ('stc2')
            where (rt_domain(did)%stc(:,:,2).eq.chkVal) &
              rt_domain(did)%stc(:,:,2) = filVal
          case ('stc3')
            where (rt_domain(did)%stc(:,:,3).eq.chkVal) &
              rt_domain(did)%stc(:,:,3) = filVal
          case ('stc4')
            where (rt_domain(did)%stc(:,:,4).eq.chkVal) &
              rt_domain(did)%stc(:,:,4) = filVal
          case ('vegtyp')
            where (rt_domain(did)%vegtyp.eq.chkVal) &
              rt_domain(did)%vegtyp = filVal
          case ('sfchead')
            where (rt_domain(did)%overland%control%surface_water_head_lsm &
              .eq.chkVal) &
              rt_domain(did)%overland%control%surface_water_head_lsm = filVal
          case ('infxsrt')
            where (rt_domain(did)%infxsrt.eq.chkVal) &
              rt_domain(did)%infxsrt = filVal
          case ('soldrain')
            where (rt_domain(did)%soldrain.eq.chkVal) &
              rt_domain(did)%soldrain = filVal
          case default
            call ESMF_LogSetError(ESMF_FAILURE, &
              msg=METHOD//": Field hookup missing: "//trim(itemNameList(n)), &
              file=FILENAME,rcToReturn=rc)
            return  ! bail out
        end select
      endif
    enddo

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine state_prescribe_missing

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "model_debug"
  subroutine model_debug(state, did, memflg, filePrefix, rc)
    type(ESMF_State), intent(inout)   :: state
    integer, intent(in)               :: did
    type(memory_flag)                 :: memflg
    character(len=*)                  :: filePrefix
    integer, intent(out)              :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: cpyfield, outfield
    type(ESMF_Grid)                        :: grid
    integer                                :: stat
    character(len=16)                      :: cmemflg

    rc = ESMF_SUCCESS

    if (memflg .eq. MEMORY_POINTER) then
      call NUOPC_Write(state, &
        fileNamePrefix=filePrefix, overwrite=.true., &
        status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    elseif(memflg .eq. MEMORY_COPY) then
      call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return ! bail out

      allocate(itemNameList(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of state item name memory failed.", &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(itemTypeList(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of state item type memory failed.", &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_StateGet(state,itemNameList=itemNameList, &
        itemTypeList=itemTypeList,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      do n=1, itemCount
        if (itemTypeList(n) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, field=cpyfield, &
            itemName=itemNameList(n),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          call ESMF_FieldGet(cpyfield, grid=grid, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          outfield = field_create(itemNameList(n), grid=grid, did=did, &
            memflg=MEMORY_POINTER, rc=rc)
          call ESMF_FieldWrite(outfield, variableName=itemNameList(n), &
            fileName=trim(filePrefix)//"_"//trim(itemNameList(n))//".nc", &
            iofmt=ESMF_IOFMT_NETCDF, rc=rc)
          call ESMF_FieldDestroy(outfield, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        endif
      enddo
      deallocate(itemNameList)
      deallocate(itemTypeList)
    else
      cmemflg = memflg
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Field memory flag unknown: "//trim(cmemflg), &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

  end subroutine model_debug

  !-----------------------------------------------------------------------------


end module wrfhydro_nuopc_fields
