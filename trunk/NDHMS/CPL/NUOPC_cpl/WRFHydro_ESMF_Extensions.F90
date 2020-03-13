!=========================================================================================
! WRFHYDRO ESMF Extensions Module
!=========================================================================================
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!=========================================================================================

! ESMF macros for logging
#define FILENAME "WRFHydro_ESMF_Extensions.F90"
#define CONTEXT  line=__LINE__,file=FILENAME,method=METHOD
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT

! Define ESMF real kind to match WRFHYDRO single/double precision
#if defined(SINGLE)
#define ESMF_KIND_RX ESMF_KIND_R4
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R4
#else
#define ESMF_KIND_RX ESMF_KIND_R8
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R8
#endif

! Macros for debugging
#define DEBUG_ESMF_IMPORT___disabled
#define DEBUG_ESMF_EXPORT___disabled

!=========================================================================================
! WRFHYDRO ESMF Extensions Module
!=========================================================================================
module WRFHydro_ESMF_Extensions

  use ESMF
  use NUOPC
  use NETCDF

  implicit none

  private

  public :: WRFHYDRO_ESMF_GridWrite
  public :: WRFHYDRO_ESMF_MAPPRESET_GLOBAL
  public :: WRFHYDRO_ESMF_MAPPRESET_CONUS
  public :: WRFHYDRO_ESMF_MAPPRESET_IRENE
  public :: WRFHYDRO_ESMF_MAPPRESET_FRONTRANGE
  public :: WRFHYDRO_ESMF_FieldFill
  public :: WRFHYDRO_ESMF_FillField
  public :: WRFHYDRO_ESMF_FillArray
  public :: WRFHYDRO_ESMF_FillFieldBundle
  public :: WRFHYDRO_ESMF_FillState
  public :: WRFHYDRO_ESMF_NetcdfReadIXJX
  public :: WRFHYDRO_ESMF_NetcdfIsPresent
  public :: WRFHYDRO_ESMF_LogStateList
  public :: WRFHYDRO_ESMF_LogState
  public :: WRFHYDRO_ESMF_LogFieldConnections
  public :: WRFHYDRO_ESMF_LogGrid
  public :: WRFHYDRO_ESMF_LogFieldList
  public :: WRFHYDRO_ESMF_LogField
  public :: WRFHYDRO_ESMF_LogFieldLclVal
  public :: WRFHYDRO_ESMF_LogArrayLclVal
  public :: WRFHYDRO_ESMF_LogFarrayLclVal
  public :: WRFHYDRO_ESMF_LogCplList
  public :: WRFHYDRO_ESMF_ChDir


!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

  interface WRFHYDRO_ESMF_GridWrite
    module procedure WRFHYDRO_ESMF_GridWrite_coords
    module procedure WRFHYDRO_ESMF_GridWrite_preset
    module procedure WRFHYDRO_ESMF_GridWrite_default
  end interface

  interface WRFHYDRO_ESMF_NclScriptWrite
    module procedure WRFHYDRO_ESMF_NclScriptWrite_coords
    module procedure WRFHYDRO_ESMF_NclScriptWrite_preset
    module procedure WRFHYDRO_ESMF_NclScriptWrite_default
  end interface

  interface WRFHYDRO_ESMF_FerretScriptWrite
    module procedure WRFHYDRO_ESMF_FerretScriptWrite_coords
    module procedure WRFHYDRO_ESMF_FerretScriptWrite_preset
    module procedure WRFHYDRO_ESMF_FerretScriptWrite_default
  end interface

  interface WRFHYDRO_ESMF_FillState
    module procedure WRFHYDRO_ESMF_FillState_I4
    module procedure WRFHYDRO_ESMF_FillState_I8
    module procedure WRFHYDRO_ESMF_FillState_R4
    module procedure WRFHYDRO_ESMF_FillState_R8
    module procedure WRFHYDRO_ESMF_FillState_SCHEME
  end interface

  interface WRFHYDRO_ESMF_FillFieldBundle
    module procedure WRFHYDRO_ESMF_FillFieldBundle_I4
    module procedure WRFHYDRO_ESMF_FillFieldBundle_I8
    module procedure WRFHYDRO_ESMF_FillFieldBundle_R4
    module procedure WRFHYDRO_ESMF_FillFieldBundle_R8
    module procedure WRFHYDRO_ESMF_FillFieldBundle_SCHEME
  end interface

  interface WRFHYDRO_ESMF_FillField
    module procedure WRFHYDRO_ESMF_FillField_I4
    module procedure WRFHYDRO_ESMF_FillField_I8
    module procedure WRFHYDRO_ESMF_FillField_R4
    module procedure WRFHYDRO_ESMF_FillField_R8
  end interface

  interface WRFHYDRO_ESMF_FillArray
    module procedure WRFHYDRO_ESMF_FillArray_I4
    module procedure WRFHYDRO_ESMF_FillArray_I8
    module procedure WRFHYDRO_ESMF_FillArray_R4
    module procedure WRFHYDRO_ESMF_FillArray_R8
  end interface

  interface WRFHYDRO_ESMF_NetcdfReadIXJX
    module procedure WRFHYDRO_ESMF_NetcdfReadIXJX_Field
    module procedure WRFHYDRO_ESMF_NetcdfReadIXJX_Array
    module procedure WRFHYDRO_ESMF_NetcdfReadIXJX_I4
    module procedure WRFHYDRO_ESMF_NetcdfReadIXJX_I8
    module procedure WRFHYDRO_ESMF_NetcdfReadIXJX_R4
    module procedure WRFHYDRO_ESMF_NetcdfReadIXJX_R8
  end interface

  interface WRFHYDRO_ESMF_LogFarrayLclVal
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_I41D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_I42D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_I43D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_I81D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_I82D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_I83D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_R41D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_R42D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_R43D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_R81D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_R82D
    module procedure WRFHYDRO_ESMF_LogFarrayLclVal_R83D
  end interface

  interface
    integer function local_chdir(path) bind(C, name="chdir")
      use iso_c_binding
      character(c_char) :: path(*)
    end function
  end interface

!==============================================================================
!
! DERIVED TYPES
!
!==============================================================================

  type MapDesc
    character(len=16) :: name
    logical           :: global
    real              :: minLat
    real              :: maxLat
    real              :: minLon
    real              :: maxLon
  endtype MapDesc


!==============================================================================
!
! PRESET VALUES
!
!==============================================================================

  type(MapDesc),parameter :: &
    WRFHYDRO_ESMF_MAPPRESET_GLOBAL = MapDesc("GLOBAL",.TRUE.,-90.0,90.0,-180.0,-180.0), &
    WRFHYDRO_ESMF_MAPPRESET_CONUS = MapDesc("CONUS",.FALSE.,18.0,49.0,235.0,298.0), &
    WRFHYDRO_ESMF_MAPPRESET_IRENE = MapDesc("IRENE",.FALSE.,10.0,60.0,240.0,320.0), &
    WRFHYDRO_ESMF_MAPPRESET_FRONTRANGE = MapDesc("FRONTRANGE",.FALSE.,38.5,41.0,-107.0,-103.5)
  character(len=*),parameter :: NUOPC_COPY_FWD = 'from ESMF_Array to FORTRAN array'
  character(len=*),parameter :: NUOPC_COPY_BWD = 'from FORTRAN array to ESMF_Array'


contains

  !-----------------------------------------------------------------------------
#define METHOD "WRFHYDRO_ESMF_GridWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_GridWrite - Write Grid data to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_GridWrite
  subroutine WRFHYDRO_ESMF_GridWrite_coords(grid, nclScript, mapName, minCoords, &
    maxCoords, fileName, overwrite, status, timeslice, iofmt, relaxedflag, rc)
! ! ARGUMENTS
    type(ESMF_Grid),            intent(in)            :: grid
    logical,                    intent(in)            :: nclScript
    character(len=*),           intent(in)            :: mapName
    real,                       intent(in)            :: minCoords(2)
    real,                       intent(in)            :: maxCoords(2)
    character(len=*),           intent(in),  optional :: fileName
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional :: iofmt
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write the data in {\tt grid} to {\tt file} if supported by the
!   {\tt iofmt}.
!
!   The arguments are:
!   \begin{description}
!   \item[grid]
!     The {\tt ESMF\_Grid} object whose data is to be written.
!   \item[{[nclScript]}]
!     If {\tt .true.} then NUOPC will write an NCL script that can be used to
!     generate grid graphics. Default is {\tt .false.}.
!   \item[{[mapName]}]
!     Map name to be written to NCL script.
!   \item[{[minCoords]}]
!     Minimum map coordinates to be written to NCL script.
!   \item[{[maxCoords]}]
!     Maximum map coordinates to be written to NCL script.
!   \item[fileName]
!     The name of the file to write to. If not present then the file will
!     be written to the grid's name.
!   \item[{[overwrite]}]
!      A logical flag, the default is .false., i.e., existing Field data may
!      {\em not} be overwritten. If .true., the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!   \item[{[status]}]
!      The file status. Valid options are {\tt ESMF\_FILESTATUS\_NEW},
!      {\tt ESMF\_FILESTATUS\_OLD}, {\tt ESMF\_FILESTATUS\_REPLACE}, and
!      {\tt ESMF\_FILESTATUS\_UNKNOWN} (default).
!   \item[{[timeslice]}]
!     Time slice counter. Must be positive. The behavior of this
!     option may depend on the setting of the {\tt overwrite} flag:
!     \begin{description}
!     \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!     less than the maximum time already in the file, the write will fail.
!     \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!     \end{description}
!     By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file,
!     however, if the file already contains a time axis for the variable,
!     a timeslice one greater than the maximum will be written.
!   \item[{[iofmt]}]
!    The IO format.  Valid options are  {\tt ESMF\_IOFMT\_BIN} and
!    {\tt ESMF\_IOFMT\_NETCDF}. If not present, file names with a {\tt .bin}
!    extension will use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc}
!    extension will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!   \item[{[relaxedflag]}]
!     If {\tt .true.}, then no error is returned even if the call cannot write
!     the file due to library limitations. Default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(MapDesc)  :: map

    if (present(rc)) rc = ESMF_SUCCESS

    map = MapDesc(trim(mapName),.FALSE., &
      minCoords(2),maxCoords(2),minCoords(1),maxCoords(1))

    call WRFHYDRO_ESMF_GridWrite(grid, fileName=fileName, overwrite=overwrite, &
      status=status, timeslice=timeslice, iofmt=iofmt, &
      relaxedflag=relaxedflag, nclScript=nclScript, map=map, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_GridWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_GridWrite - Write Grid data to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_GridWrite
  subroutine WRFHYDRO_ESMF_GridWrite_preset(grid, nclScript, mapPreset, &
    fileName, overwrite, status, timeslice, iofmt, relaxedflag, rc)
! ! ARGUMENTS
    type(ESMF_Grid),            intent(in)            :: grid
    logical,                    intent(in)            :: nclScript
    character(len=*),           intent(in)            :: mapPreset
    character(len=*),           intent(in),  optional :: fileName
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional :: iofmt
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write the data in {\tt grid} to {\tt file} if supported by the
!   {\tt iofmt}.
!
!   The arguments are:
!   \begin{description}
!   \item[grid]
!     The {\tt ESMF\_Grid} object whose data is to be written.
!   \item[{[nclScript]}]
!     If {\tt .true.} then NUOPC will write an NCL script that can be used to
!     generate grid graphics. Default is {\tt .false.}.
!   \item[{[mapPreset]}]
!     Map preset to use when writting NCL script.
!   \item[fileName]
!     The name of the file to write to. If not present then the file will
!     be written to the grid's name.
!   \item[{[overwrite]}]
!      A logical flag, the default is .false., i.e., existing Field data may
!      {\em not} be overwritten. If .true., the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!   \item[{[status]}]
!      The file status. Valid options are {\tt ESMF\_FILESTATUS\_NEW},
!      {\tt ESMF\_FILESTATUS\_OLD}, {\tt ESMF\_FILESTATUS\_REPLACE}, and
!      {\tt ESMF\_FILESTATUS\_UNKNOWN} (default).
!   \item[{[timeslice]}]
!     Time slice counter. Must be positive. The behavior of this
!     option may depend on the setting of the {\tt overwrite} flag:
!     \begin{description}
!     \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!     less than the maximum time already in the file, the write will fail.
!     \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!     \end{description}
!     By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file,
!     however, if the file already contains a time axis for the variable,
!     a timeslice one greater than the maximum will be written.
!   \item[{[iofmt]}]
!    The IO format.  Valid options are  {\tt ESMF\_IOFMT\_BIN} and
!    {\tt ESMF\_IOFMT\_NETCDF}. If not present, file names with a {\tt .bin}
!    extension will use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc}
!    extension will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!   \item[{[relaxedflag]}]
!     If {\tt .true.}, then no error is returned even if the call cannot write
!     the file due to library limitations. Default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! no local variables

    if (present(rc)) rc = ESMF_SUCCESS

    select case (trim(mapPreset))
      case ('global','GLOBAL','Global')
        call WRFHYDRO_ESMF_GridWrite(grid, fileName=fileName, overwrite=overwrite, &
          timeslice=timeslice, iofmt=iofmt, relaxedflag=relaxedflag, &
          nclScript=nclScript, map=WRFHYDRO_ESMF_MAPPRESET_GLOBAL, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('conus','CONUS','Conus')
        call WRFHYDRO_ESMF_GridWrite(grid, fileName=fileName, overwrite=overwrite, &
          timeslice=timeslice, iofmt=iofmt, relaxedflag=relaxedflag, &
          nclScript=nclScript, map=WRFHYDRO_ESMF_MAPPRESET_CONUS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('irene','IRENE','Irene')
        call WRFHYDRO_ESMF_GridWrite(grid, fileName=fileName, overwrite=overwrite, &
          timeslice=timeslice, iofmt=iofmt, relaxedflag=relaxedflag, &
          nclScript=nclScript, map=WRFHYDRO_ESMF_MAPPRESET_IRENE, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('frontrange','FRONTRANGE','FrontRange')
        call WRFHYDRO_ESMF_GridWrite(grid, fileName=fileName, overwrite=overwrite, &
          timeslice=timeslice, iofmt=iofmt, relaxedflag=relaxedflag, &
          nclScript=nclScript, map=WRFHYDRO_ESMF_MAPPRESET_FRONTRANGE, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case default
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE,   &
          msg="Unknown map preset value "//trim(mapPreset)//".", &
          CONTEXT, rcToReturn=rc)
        return
    endselect

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_GridWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_GridWrite - Write Grid data to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_GridWrite
  subroutine WRFHYDRO_ESMF_GridWrite_default(grid, fileName, overwrite, status, &
    timeslice, iofmt, relaxedflag, nclScript, map, rc)
! ! ARGUMENTS
    type(ESMF_Grid),            intent(in)            :: grid
    character(len=*),           intent(in),  optional :: fileName
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional :: iofmt
    logical,                    intent(in),  optional :: relaxedflag 
    logical,                    intent(in),  optional :: nclScript
    type(MapDesc),              intent(in),  optional :: map
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write the data in {\tt grid} to {\tt file} if supported by the
!   {\tt iofmt}.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object whose data is to be written.
!   \item[fileName]
!     The name of the file to write to. If not present then the file will
!     be written to the grid's name.
!   \item[{[overwrite]}]
!      A logical flag, the default is .false., i.e., existing Field data may
!      {\em not} be overwritten. If .true., the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!   \item[{[status]}]
!      The file status. Valid options are {\tt ESMF\_FILESTATUS\_NEW},
!      {\tt ESMF\_FILESTATUS\_OLD}, {\tt ESMF\_FILESTATUS\_REPLACE}, and
!      {\tt ESMF\_FILESTATUS\_UNKNOWN} (default).
!   \item[{[timeslice]}]
!     Time slice counter. Must be positive. The behavior of this
!     option may depend on the setting of the {\tt overwrite} flag:
!     \begin{description}
!     \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!     less than the maximum time already in the file, the write will fail.
!     \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!     \end{description}
!     By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file,
!     however, if the file already contains a time axis for the variable,
!     a timeslice one greater than the maximum will be written.
!   \item[{[iofmt]}]
!    The IO format.  Valid options are  {\tt ESMF\_IOFMT\_BIN} and
!    {\tt ESMF\_IOFMT\_NETCDF}. If not present, file names with a {\tt .bin}
!    extension will use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc}
!    extension will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!   \item[{[relaxedflag]}]
!     If {\tt .true.}, then no error is returned even if the call cannot write
!     the file due to library limitations. Default is {\tt .false.}.
!   \item[{[nclScript]}]
!     If {\tt .true.} then NUOPC will write an NCL script that can be used to 
!     generate grid graphics. Default is {\tt .false.}.
!   \item[{[map]}]
!     Derived type including the map name and boundary coordinates.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    logical                 :: ioCapable
    logical                 :: doItFlag
    character(len=64)       :: lfileName
    character(len=64)       :: gridName
    type(ESMF_Array)        :: array
    type(ESMF_ArrayBundle)  :: arraybundle
    logical                 :: isPresent
    integer                 :: dimCount
    integer                 :: dimIndex
    integer,allocatable     :: coordDimCount(:)
    integer                 :: coordDimMax
    integer                 :: stat
    logical                 :: lnclScript
    logical                 :: hasCorners

    if (present(rc)) rc = ESMF_SUCCESS

    ioCapable = (ESMF_IO_PIO_PRESENT .and. &
      (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT))

    doItFlag = .true. ! default
    if (present(relaxedFlag)) then
      doItFlag = .not.relaxedflag .or. (relaxedflag.and.ioCapable)
    endif

    if (doItFlag) then

      if (present(fileName)) then
        lfileName = trim(fileName)
      else
        call ESMF_GridGet(grid, name=gridName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        lfileName = trim(gridName)//".nc"
      endif

      arraybundle = ESMF_ArrayBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

      ! -- centers --

      call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      if (isPresent) then
        call ESMF_GridGetCoord(grid, coordDim=1, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_ArraySet(array, name="lon_center", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=2, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_ArraySet(array, name="lat_center", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      endif

      ! -- corners --

      call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        isPresent=hasCorners, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      if (hasCorners) then
        call ESMF_GridGetCoord(grid, coordDim=1, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        if (.not. ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) then
          call ESMF_ArraySet(array, name="lon_corner", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        endif
        call ESMF_GridGetCoord(grid, coordDim=2, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        if (.not. ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) then
          call ESMF_ArraySet(array, name="lat_corner", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        endif
      endif

      ! -- mask --

      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_ArraySet(array, name="mask", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      endif

      ! -- area --

      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_ArraySet(array, name="area", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      endif

      call ESMF_ArrayBundleWrite(arraybundle, &
        fileName=trim(lfileName),rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

      call ESMF_ArrayBundleDestroy(arraybundle,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

      if (present(nclScript)) then
        lnclScript = nclScript
      else
        lnclScript = .FALSE.
      endif

      if (lnclScript) then
        call ESMF_GridGet(grid,dimCount=dimCount,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

        ! allocate coordDim info accord. to dimCount and tileCount
        allocate(coordDimCount(dimCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of coordinate dimensions memory failed.", &
          CONTEXT, rcToReturn=rc)) return  ! bail out

        ! get coordDim info
        call ESMF_GridGet(grid, coordDimCount=coordDimCount, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

        coordDimMax = 0
        do dimIndex=1,dimCount
          coordDimMax = MAX(coordDimMax,coordDimCount(dimIndex))
        enddo

        ! deallocate coordDim info accord. to dimCount and tileCount
        deallocate(coordDimCount, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of coordinate dimensions memory failed.", &
          CONTEXT, rcToReturn=rc)) return  ! bail out

        if (coordDimMax == 1) then
          call WRFHYDRO_ESMF_NclScriptWrite(gridFile=lfileName, map=map, &
            uniformRect=.TRUE., writeCorners=hasCorners, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        else
          call WRFHYDRO_ESMF_NclScriptWrite(gridFile=lfileName, map=map, &
            writeCorners=hasCorners, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        endif
      endif
    endif
  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NclScriptWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NclScriptWrite_coords - Write NCL Script to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NclScriptWrite
  subroutine WRFHYDRO_ESMF_NclScriptWrite_coords(gridFile,mapName,minCoords,maxCoords, &
  title,nclFile,uniformRect,writeCorners,rc)
! ! ARGUMENTS
    character(len=*),intent(in)          :: gridFile
    character(len=*),intent(in)          :: mapName         
    real,intent(in)                      :: minCoords(2)
    real,intent(in)                      :: maxCoords(2)
    character(len=*),intent(in),optional :: title
    character(len=*),intent(in),optional :: nclFile
    logical,intent(in),optional          :: uniformRect
    logical,intent(in),optional          :: writeCorners
    integer,intent(out),optional         :: rc
! !DESCRIPTION:
!   Write NCL script that can be used to generate NCL grid visual.
!
!   The arguments are:
!   \begin{description}
!   \item[gridFile]
!     NetCDF grid file name used plot data on coordinates.
!   \item[mapName]
!     Map name.
!   \item[minCoords]
!     Minimum coordinate limits for plot.
!   \item[maxCoords]
!     Maximum coordinate limits for plot.
!   \item[title]
!     Grid plot title.
!   \item[nclFile]
!     NCL script file name
!   \item[uniformRect]
!     Repeat coordinates for uniform rectangular grids.
!   \item[writeCorners]
!     Plot corners.  Default plot center coordinates.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(MapDesc)  :: map

    if (present(rc)) rc = ESMF_SUCCESS

    map = MapDesc(trim(mapName),.FALSE., &
      minCoords(2),maxCoords(2),minCoords(1),maxCoords(1))

    call WRFHYDRO_ESMF_NclScriptWrite(gridFile, map=map, title=title, &
      nclFile=nclFile, uniformRect=uniformRect, writeCorners=writeCorners, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NclScriptWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NclScriptWrite_preset - Write NCL Script to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NclScriptWrite
  subroutine WRFHYDRO_ESMF_NclScriptWrite_preset(gridFile,mapPreset, &
  title,nclFile,uniformRect,writeCorners,rc)
! ! ARGUMENTS
    character(len=*),intent(in)          :: gridFile
    character(len=*),intent(in)          :: mapPreset
    character(len=*),intent(in),optional :: title
    character(len=*),intent(in),optional :: nclFile
    logical,intent(in),optional          :: uniformRect
    logical,intent(in),optional          :: writeCorners
    integer,intent(out),optional         :: rc
! !DESCRIPTION:
!   Write NCL script that can be used to generate NCL grid visual.
!
!   The arguments are:
!   \begin{description}
!   \item[gridFile]
!     NetCDF grid file name used plot data on coordinates.
!   \item[mapPreset]
!     Preset coordinate limits.
!   \item[title]
!     Grid plot title.
!   \item[nclFile]
!     NCL script file name
!   \item[uniformRect]
!     Repeat coordinates for uniform rectangular grids.
!   \item[writeCorners]
!     Plot corners.  Default plot center coordinates.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables

    if (present(rc)) rc = ESMF_SUCCESS

    select case (trim(mapPreset))
      case ('global','GLOBAL','Global')
        call WRFHYDRO_ESMF_NclScriptWrite(gridFile, &
          map=WRFHYDRO_ESMF_MAPPRESET_GLOBAL, title=title, nclFile=nclFile, &
          uniformRect=uniformRect, writeCorners=writeCorners, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('conus','CONUS','Conus')
        call WRFHYDRO_ESMF_NclScriptWrite(gridFile, &
          map=WRFHYDRO_ESMF_MAPPRESET_CONUS, title=title, nclFile=nclFile, &
          uniformRect=uniformRect, writeCorners=writeCorners, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('irene','IRENE','Irene')
        call WRFHYDRO_ESMF_NclScriptWrite(gridFile, &
          map=WRFHYDRO_ESMF_MAPPRESET_IRENE, title=title, nclFile=nclFile, &
          uniformRect=uniformRect, writeCorners=writeCorners, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('frontrange','FRONTRANGE','FrontRange')
        call WRFHYDRO_ESMF_NclScriptWrite(gridFile, &
          map=WRFHYDRO_ESMF_MAPPRESET_FRONTRANGE, title=title, nclFile=nclFile, &
          uniformRect=uniformRect, writeCorners=writeCorners, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case default
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE,   &
          msg="Unknown map preset value "//trim(mapPreset)//".", &
          CONTEXT, rcToReturn=rc)
        return
    endselect

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NclScriptWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NclScriptWrite_default - Write NCL Script to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NclScriptWrite
  subroutine WRFHYDRO_ESMF_NclScriptWrite_default(gridFile, map, title, &
    nclFile, uniformRect, writeCorners, rc)
! ! ARGUMENTS
    character(len=*),intent(in)          :: gridFile
    type(MapDesc),intent(in)             :: map
    character(len=*),intent(in),optional :: title
    character(len=*),intent(in),optional :: nclFile
    logical,intent(in),optional          :: uniformRect
    logical,intent(in),optional          :: writeCorners
    integer,intent(out),optional         :: rc
! !DESCRIPTION:
!   Write NCL script that can be used to generate NCL grid visual.
!
!   The arguments are:
!   \begin{description}
!   \item[gridFile]
!     NetCDF grid file name used plot data on coordinates.
!   \item[map]
!     Coordinate limits.
!   \item[title]
!     Grid plot title.
!   \item[nclFile]
!     NCL script file name
!   \item[uniformRect]
!     Repeat coordinates for uniform rectangular grids.
!   \item[writeCorners]
!     Plot corners.  Default plot center coordinates.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_VM)                   :: vm
    integer                         :: lpe
    character(len=64)               :: ltitle
    character(len=64)               :: lnclFile
    character(len=64)               :: fileBase
    logical                         :: luniformRect
    logical                         :: lcorners
    integer                         :: markExt
    integer                         :: fUnit
    integer                         :: stat
    character(len=10)               :: varlat
    character(len=10)               :: varlon

    if (present(rc)) rc = ESMF_SUCCESS

    ! Get current VM and pet number
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call ESMF_VMGet(vm, localPet=lpe, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (lpe /= 0) return

    markExt = index(gridFile,".",back=.TRUE.)
    if (markExt > 1) then
      fileBase = gridFile(1:markExt-1)
    elseif (markExt == 1) then
      fileBase = "grid"
    elseif (len(trim(gridFile)) > 0) then
      fileBase = trim(gridFile)
    else
      fileBase = "grid"
    endif

    if (present(nclFile)) then
      lnclFile = trim(nclFile)
    else
      lnclFile = trim(fileBase)//".ncl"
    endif

    if (present(title)) then
      ltitle = trim(title)
    else
      ltitle = trim(fileBase)//" "//trim(map%name)
    endif

    if (present(uniformRect)) then
      luniformRect = uniformRect
    else
      luniformRect = .FALSE.
    endif

    if (present(writeCorners)) then
      lcorners = writeCorners
    else
      lcorners = .FALSE.
    endif

    if (lcorners) then
      varlat = 'lat_corner'
      varlon = 'lon_corner'
    else
      varlat = 'lat_center'
      varlon = 'lon_center'
    endif

    call ESMF_UtilIOUnitGet(fUnit, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
    open (fUnit,file=trim(lnclFile),action="write", &
      status="new",iostat=stat)
    if (stat /= 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
        msg="Cound not open "//trim(lnclFile)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    write (fUnit,"(A)") '; NUOPC_FileWriteMapNCL used to generate this file'
    write (fUnit,"(A)") '; execute ncl <this file> to generate grid png file'
    write (fUnit,"(A)") 'begin'
    write (fUnit,"(A)") '  in = addfile("'//trim(gridFile)//'","r")'
    write (fUnit,"(A)") '  wks = gsn_open_wks("png","'//trim(fileBase)//'")'
    write (fUnit,"(A)") '  res              = True'
    write (fUnit,"(A)") '  res@gsnMaximize  = True'
    write (fUnit,"(A)") '  res@gsnDraw      = False'
    write (fUnit,"(A)") '  res@gsnFrame     = False'
    write (fUnit,"(A)") '  res@tiMainString = "'//trim(ltitle)//'"'
    write (fUnit,"(A)") '  res@pmTickMarkDisplayMode = "Always"'
    write (fUnit,"(A)") '; '//trim(map%name)//' Map Grid'
    if (.NOT. map%global) then
      write (fUnit,"(A,F0.3)") &
        '  res@mpMinLatF    = ',map%minLat
      write (fUnit,"(A,F0.3)") &
        '  res@mpMaxLatF    = ',map%maxLat
      write (fUnit,"(A,F0.3)") &
        '  res@mpMinLonF    = ',map%minLon
      write (fUnit,"(A,F0.3)") &
        '  res@mpMaxLonF    = ',map%maxLon
    endif
    write (fUnit,"(A)") '  map = gsn_csm_map_ce(wks,res)'
    write (fUnit,"(A)") '  hgt = in->lon_center(:,:)'
    write (fUnit,"(A)") '  dimlon = getfilevardimsizes(in,"'//trim(varlon)//'")'
    write (fUnit,"(A)") '  dimlat = getfilevardimsizes(in,"'//trim(varlat)//'")'
    if (luniformRect) then
      write (fUnit,"(A)") '  hgt@lat2d = conform_dims((/dimlon(0),dimlat(1)/),'// &
        'in->'//trim(varlat)//'(:,0),1)'
      write (fUnit,"(A)") '  hgt@lon2d = conform_dims((/dimlon(0),dimlat(1)/),'// &
        'in->'//trim(varlon)//'(0,:),0)'
    else
      write (fUnit,"(A)") '  hgt@lat2d = in->'//trim(varlat)//'(:,:)'
      write (fUnit,"(A)") '  hgt@lon2d = in->'//trim(varlon)//'(:,:)'
    endif
    write (fUnit,"(A)") '  pres                   = True'
    if (lcorners) then
      write (fUnit,"(A)") '  pres@gsnCoordsAsLines  = True'
    else
      write (fUnit,"(A)") '  pres@gsnCoordsAsLines  = False'
      write (fUnit,"(A)") '  if (dimlon(0)*dimlat(1) .gt. 99) then'
      write (fUnit,"(A)") '    pres@gsMarkerIndex = 1'
      write (fUnit,"(A)") '  end if'
    endif
    write (fUnit,"(A)") '  gsn_coordinates(wks,map,hgt,pres)'
    write (fUnit,"(A)") 'end'

    close (fUnit,iostat=stat)
    if (stat /= 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
        msg="Cound not close "//trim(lnclFile)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FerretScriptWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FerretScriptWrite_coords - Write Ferret Script to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FerretScriptWrite
  subroutine WRFHYDRO_ESMF_FerretScriptWrite_coords(varName, dataFile, &
    gridFile, slices, mapName, minCoords, maxCoords, scale, jnlFile, &
    uniformRect,rc)
! ! ARGUMENTS
    character(len=*),intent(in)          :: varName
    character(len=*),intent(in)          :: dataFile
    character(len=*),intent(in)          :: gridFile
    integer,intent(in)                   :: slices(:)
    character(len=*),intent(in)          :: mapName
    real,intent(in)                      :: minCoords(2)
    real,intent(in)                      :: maxCoords(2)
    real,intent(in),optional             :: scale(3)
    character(len=*),intent(in),optional :: jnlFile
    logical,intent(in),optional          :: uniformRect
    integer,intent(out),optional         :: rc
! !DESCRIPTION:
!   Write Ferret script that can be used to generate Ferret field visual.
!
!   The arguments are:
!   \begin{description}
!   \item[varName]
!     NetCDF variable name in dataFile.
!   \item[dataFile]
!     NetCDF field data file name.
!   \item[gridFile]
!     NetCDF grid file name used plot data on coordinates.
!   \item[slices]
!     Array of slice numbers to for which to create plots.
!   \item[mapName]
!     Map name used to create map description.
!   \item[minCoords]
!     Minimum coordinates used to plot data.
!   \item[maxCoords]
!     Maximum coordinates used to plot data.
!   \item[scale]
!     Field data scale (/ minimum value, maximum value, step size /)
!   \item[jnlFile]
!     Output script filename.
!   \item[uniformRect]
!     Repeat coordinates for uniform rectangular grids.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(MapDesc)  :: map

    if (present(rc)) rc = ESMF_SUCCESS

    map = MapDesc(trim(mapName),.FALSE., &
      minCoords(2),maxCoords(2),minCoords(1),maxCoords(1))

    call WRFHYDRO_ESMF_FerretScriptWrite(varName,dataFile,gridFile,slices, &
      map=map,scale=scale,jnlFile=jnlFile, uniformRect=uniformRect,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FerretScriptWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FerretScriptWrite_preset - Write Ferret Script to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FerretScriptWrite
  subroutine WRFHYDRO_ESMF_FerretScriptWrite_preset(varName, dataFile, &
    gridFile, slices, mapPreset, scale, jnlFile, &
    uniformRect,rc)
! ! ARGUMENTS
    character(len=*),intent(in)          :: varName
    character(len=*),intent(in)          :: dataFile
    character(len=*),intent(in)          :: gridFile
    integer,intent(in)                   :: slices(:)
    character(len=*),intent(in)          :: mapPreset
    real,intent(in),optional             :: scale(3)
    character(len=*),intent(in),optional :: jnlFile
    logical,intent(in),optional          :: uniformRect
    integer,intent(out),optional         :: rc
! !DESCRIPTION:
!   Write Ferret script that can be used to generate Ferret field visual.
!
!   The arguments are:
!   \begin{description}
!   \item[varName]
!     NetCDF variable name in dataFile.
!   \item[dataFile]
!     NetCDF field data file name.
!   \item[gridFile]
!     NetCDF grid file name used plot data on coordinates.
!   \item[slices]
!     Array of slice numbers to for which to create plots.
!   \item[mapPreset]
!     Map preset to use to define coordinate limits.
!   \item[scale]
!     Field data scale (/ minimum value, maximum value, step size /)
!   \item[jnlFile]
!     Output script filename.
!   \item[uniformRect]
!     Repeat coordinates for uniform rectangular grids.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables

    if (present(rc)) rc = ESMF_SUCCESS

    select case (trim(mapPreset))
      case ('global','GLOBAL','Global')
        call WRFHYDRO_ESMF_FerretScriptWrite(varName, dataFile, gridFile, slices, &
          map=WRFHYDRO_ESMF_MAPPRESET_GLOBAL, scale=scale, jnlFile=jnlFile, &
          uniformRect=uniformRect, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('conus','CONUS','Conus')
        call WRFHYDRO_ESMF_FerretScriptWrite(varName, dataFile, gridFile, slices, &
          map=WRFHYDRO_ESMF_MAPPRESET_CONUS, scale=scale, jnlFile=jnlFile, &
          uniformRect=uniformRect, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('irene','IRENE','Irene')
        call WRFHYDRO_ESMF_FerretScriptWrite(varName, dataFile, gridFile, slices, &
          map=WRFHYDRO_ESMF_MAPPRESET_IRENE, scale=scale, jnlFile=jnlFile, &
          uniformRect=uniformRect, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case ('frontrange','FRONTRANGE','FrontRange')
        call WRFHYDRO_ESMF_FerretScriptWrite(varName, dataFile, gridFile, slices, &
          map=WRFHYDRO_ESMF_MAPPRESET_FRONTRANGE, scale=scale, jnlFile=jnlFile, &
          uniformRect=uniformRect, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      case default
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE,   &
          msg="Unknown map preset value "//trim(mapPreset)//".", &
          CONTEXT, rcToReturn=rc)
        return
    endselect

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FerretScriptWrite"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FerretScriptWrite_default - Write Ferret Script to file
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FerretScriptWrite
  subroutine WRFHYDRO_ESMF_FerretScriptWrite_default(varName, dataFile, gridFile, &
    slices, map, scale, jnlFile, uniformRect, rc)
! ! ARGUMENTS
    character(len=*),intent(in)          :: varName
    character(len=*),intent(in)          :: dataFile
    character(len=*),intent(in)          :: gridFile
    integer,intent(in)                   :: slices(:)
    type(MapDesc),intent(in)             :: map
    real,intent(in),optional             :: scale(3)
    character(len=*),intent(in),optional :: jnlFile
    logical,intent(in),optional          :: uniformRect
    integer,intent(out),optional         :: rc
! !DESCRIPTION:
!   Write Ferret script that can be used to generate Ferret field visual.
!
!   The arguments are:
!   \begin{description}
!   \item[varName]
!     NetCDF variable name in dataFile.
!   \item[dataFile]
!     NetCDF field data file name.
!   \item[gridFile]
!     NetCDF grid file name used plot data on coordinates.
!   \item[slices]
!     Array of slice numbers to for which to create plots.
!   \item[map]
!     Coordinate limits.
!   \item[scale]
!     Field data scale (/ minimum value, maximum value, step size /)
!   \item[jnlFile]
!     Output script filename.
!   \item[uniformRect]
!     Repeat coordinates for uniform rectangular grids.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_VM)                   :: vm
    integer                         :: lpe
    character(len=64)               :: ljnlFile
    logical                         :: luniformRect
    character(len=64)               :: fileBase
    character(len=64)               :: limits
    character(len=64)               :: levels
    character(len=64)               :: latlon
    integer                         :: markExt
    integer                         :: sIndex
    integer                         :: fUnit
    integer                         :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    ! Get current VM and pet number
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call ESMF_VMGet(vm, localPet=lpe, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (lpe /= 0) return

    markExt = index(dataFile,".",back=.TRUE.)
    if (markExt > 1) then
      fileBase = dataFile(1:markExt-1)
    elseif (markExt == 1) then
      fileBase = "data"
    elseif (len(trim(dataFile)) > 0) then
      fileBase = trim(dataFile)
    else
      fileBase = "data"
    endif

    if (present(jnlFile)) then
      ljnlFile = trim(jnlFile)
    else
      ljnlFile = trim(fileBase)//".jnl"
    endif

    if (present(uniformRect)) then
      luniformRect = uniformRect
    else
      luniformRect = .FALSE.
    endif

    if (map%global) then
      limits = ''
    else
      write (limits,"(2(A,F0.3,A,F0.3))") &
        '/vlimits=',map%minLat,':',map%maxLat, &
        '/hlimits=',map%minLon,':',map%maxLon
    endif

    if (luniformRect) then
      latlon = ',lon_center[d=2,j=1:1],lat_center[d=2,i=1:1]'
    else
      latlon = ',lon_center[d=2],lat_center[d=2]'
    endif

    if (present(scale)) then
      write (levels,"(A,F0.3,A,F0.3,A,F0.3,A)") &
        '/levels=(',scale(1),',',scale(2),',',scale(3),')'
    else
      levels = ''
    endif

    call ESMF_UtilIOUnitGet(fUnit, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
    open (fUnit,file=trim(ljnlFile),action="write", &
      status="new",iostat=stat)
    if (stat /= 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
        msg="Cound not open "//trim(ljnlFile)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    write (fUnit,"(A)") ' ! NUOPC_FileWriteJNL used to generate this file'
    write (fUnit,"(A)") ' ! execute ferret -script <this file> to '
    write (fUnit,"(A)") ' ! generate data gif file for each slice listed'
    write (fUnit,"(A)") ''
    write (fUnit,"(A,A)") 'use ',trim(dataFile)
    write (fUnit,"(A,A)") 'use ',trim(gridFile)
    write (fUnit,"(A,A)") ''

    do sIndex=1,size(slices)
      write (fUnit,"((A,I0),A,A,(A,A,A),A)") &
        'shade/k=',slices(sIndex), &
        trim(limits), &
        trim(levels), &
        ' ',trim(varName),'[d=1]', &
        trim(latlon)
      write (fUnit,"(A,(A,A,I0,A))") 'FRAME/FILE=', &
        trim(fileBase),'_',slices(sIndex),'.gif'
      write (fUnit,"(A)") ''
    enddo

    write (fUnit,"(A)") 'exit'

    close (fUnit,iostat=stat)
    if (stat /= 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
        msg="Cound not close "//trim(ljnlFile)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FieldFill"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FieldFill - Fill data into a Field
! !INTERFACE:
  subroutine WRFHYDRO_ESMF_FieldFill(field, keywordEnforcer, &
    dataFillScheme, member, step, amplitude, meanValue, rc)
! !ARGUMENTS:
    type(ESMF_Field), intent(inout)           :: field
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*), intent(in), optional    :: dataFillScheme
    integer, intent(in), optional             :: member
    integer, intent(in), optional             :: step
    real, intent(in), optional                :: amplitude
    real, intent(in), optional                :: meanValue
    integer, intent(out), optional            :: rc
! !DESCRIPTION:
!   Fill {\tt field} with data according to {\tt dataFillScheme}. Depending
!   on the chosen fill scheme, the {\tt member} and {\tt step} arguments are
!   used to provide differing fill data patterns.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to fill with data.
!   \item[{[dataFillScheme]}]
!     The fill scheme. The available options are "sincos", and "one".
!     Defaults to "sincos".
!   \item[{[member]}]
!     Member incrementor. Defaults to 1.
!   \item[{[step]}]
!     Step incrementor. Defaults to 1.
!   \item[{[amplitude]}]
!     Magnitude of change. Defaults to 1.
!   \item[{[meanValue]}]
!     Mean value. Defaults to 0.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Grid)                 :: grid
    type(ESMF_TypeKind_Flag)        :: typekind
    type(ESMF_TypeKind_Flag)        :: coordTypeKind
    integer                         :: rank
    integer, allocatable            :: coordDimCount(:)
    real(ESMF_KIND_R8), pointer     :: dataPtrR8D1(:)
    real(ESMF_KIND_R8), pointer     :: dataPtrR8D2(:,:)
    real(ESMF_KIND_R8), pointer     :: dataPtrR8D3(:,:,:)
    real(ESMF_KIND_R4), pointer     :: dataPtrR4D1(:)
    real(ESMF_KIND_R4), pointer     :: dataPtrR4D2(:,:)
    real(ESMF_KIND_R4), pointer     :: dataPtrR4D3(:,:,:)
    real(ESMF_KIND_R8), pointer     :: coord1PtrR8D1(:)
    real(ESMF_KIND_R8), pointer     :: coord2PtrR8D1(:)
    real(ESMF_KIND_R8), pointer     :: coord1PtrR8D2(:,:)
    real(ESMF_KIND_R8), pointer     :: coord2PtrR8D2(:,:)
    real(ESMF_KIND_R8), pointer     :: coord1PtrR8D3(:,:,:)
    real(ESMF_KIND_R8), pointer     :: coord2PtrR8D3(:,:,:)
    real(ESMF_KIND_R8), pointer     :: coord3PtrR8D3(:,:,:)
    real(ESMF_KIND_R4), pointer     :: coord1PtrR4D1(:)
    real(ESMF_KIND_R4), pointer     :: coord2PtrR4D1(:)
    real(ESMF_KIND_R4), pointer     :: coord1PtrR4D2(:,:)
    real(ESMF_KIND_R4), pointer     :: coord2PtrR4D2(:,:)
    real(ESMF_KIND_R4), pointer     :: coord1PtrR4D3(:,:,:)
    real(ESMF_KIND_R4), pointer     :: coord2PtrR4D3(:,:,:)
    real(ESMF_KIND_R4), pointer     :: coord3PtrR4D3(:,:,:)
    integer                         :: i, j, k

    integer                         :: l_member, l_step
    real                            :: l_amplitude, l_meanValue
    character(len=16)               :: l_dataFillScheme
    integer                         :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, typekind=typekind, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    l_member = 1
    if(present(member)) l_member = member
    l_step = 1
    if(present(step)) l_step = step
    l_dataFillScheme = "sincos"
    if(present(dataFillScheme)) l_dataFillScheme = dataFillScheme
    l_amplitude = 1.0
    if(present(amplitude)) l_amplitude = amplitude
    l_meanValue = 0.0
    if(present(meanValue)) l_meanValue = meanValue

    allocate(coordDimCount(rank))
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of coordinate dimensions memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    if (trim(l_dataFillScheme)=="sincos") then
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      call ESMF_GridGet(grid,coordTypeKind=coordTypeKind, &
        coordDimCount=coordDimCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      if (rank==1) then
        ! 1D sin pattern
        ! TODO: support Meshes
        call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR8D1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        if (typekind==ESMF_TYPEKIND_R4) then
          call ESMF_FieldGet(field, farrayPtr=dataPtrR4D1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          do i=lbound(dataPtrR4D1,1),ubound(dataPtrR4D1,1)
            dataPtrR4D1(i) = &
              (sin(real(l_member)*3.1416*(coord1PtrR8D1(i)+real(l_step))/180.)) * &
              l_amplitude+l_meanValue
          enddo
        elseif (typekind==ESMF_TYPEKIND_R8) then
          call ESMF_FieldGet(field, farrayPtr=dataPtrR8D1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          do i=lbound(dataPtrR8D1,1),ubound(dataPtrR8D1,1)
            dataPtrR8D1(i) = &
              (sin(real(l_member)*3.1416*(coord1PtrR8D1(i)+real(l_step))/180.)) * &
              l_amplitude+l_meanValue
          enddo
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Unsupported typekind-rank and scheme combination requested.", &
            CONTEXT, rcToReturn=rc)
          return ! bail out
        endif
      elseif (rank==2) then
        ! 2D sin*cos pattern
        ! TODO: support Meshes
        if (coordTypeKind==ESMF_TYPEKIND_R4) then
          if (coordDimCount(1)==1) then
            call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR4D1, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          else
            ! assume the only other choice here is 2D, if not will trigger error
            call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR4D2, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          endif
          if (coordDimCount(2)==1) then
            call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=coord2PtrR4D1, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          else
            ! assume the only other choice here is 2D, if not will trigger error
            call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=coord2PtrR4D2, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          endif
        elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
          if (coordDimCount(1)==1) then
            call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR8D1, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          else
            ! assume the only other choice here is 2D, if not will trigger error
            call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR8D2, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          endif
          if (coordDimCount(2)==1) then
            call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=coord2PtrR8D1, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          else
            ! assume the only other choice here is 2D, if not will trigger error
            call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=coord2PtrR8D2, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          endif
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Unsupported coordinate typekind.", &
            CONTEXT, rcToReturn=rc)
          return ! bail out
        endif

        if (typekind==ESMF_TYPEKIND_R4) then
          call ESMF_FieldGet(field, farrayPtr=dataPtrR4D2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          if (coordDimCount(1)==1 .and. coordDimCount(2)==1) then
            if (coordTypeKind==ESMF_TYPEKIND_R4) then
            do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
            do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
              dataPtrR4D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR4D1(i)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR4D1(j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
            do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
            do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
              dataPtrR4D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR8D1(i)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR8D1(j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            endif
          else if (coordDimCount(1)==2 .and. coordDimCount(2)==1) then
            if (coordTypeKind==ESMF_TYPEKIND_R4) then
            do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
            do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
              dataPtrR4D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR4D2(i,j)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR4D1(j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
            do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
            do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
              dataPtrR4D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR8D2(i,j)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR8D1(j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            endif
          else if (coordDimCount(1)==1 .and. coordDimCount(2)==2) then
            if (coordTypeKind==ESMF_TYPEKIND_R4) then
            do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
            do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
              dataPtrR4D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR4D1(i)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR4D2(i,j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
            do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
            do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
              dataPtrR4D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR8D1(i)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR8D2(i,j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            endif
           else
            ! only choice left is both 2d coordinate arrays
            if (coordTypeKind==ESMF_TYPEKIND_R4) then
            do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
            do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
              dataPtrR4D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR4D2(i,j)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR4D2(i,j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
            do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
            do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
              dataPtrR4D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR8D2(i,j)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR8D2(i,j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            endif
          endif
        elseif (typekind==ESMF_TYPEKIND_R8) then
          call ESMF_FieldGet(field, farrayPtr=dataPtrR8D2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          if (coordDimCount(1)==1 .and. coordDimCount(2)==1) then
            if (coordTypeKind==ESMF_TYPEKIND_R4) then
            do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
            do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
              dataPtrR8D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR4D1(i)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR4D1(j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
            do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
            do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
              dataPtrR8D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR8D1(i)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR8D1(j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            endif
          else if (coordDimCount(1)==2 .and. coordDimCount(2)==1) then
            if (coordTypeKind==ESMF_TYPEKIND_R4) then
            do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
            do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
              dataPtrR8D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR4D2(i,j)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR4D1(j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
            do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
            do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
              dataPtrR8D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR8D2(i,j)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR8D1(j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            endif
          else if (coordDimCount(1)==1 .and. coordDimCount(2)==2) then
            if (coordTypeKind==ESMF_TYPEKIND_R4) then
            do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
            do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
              dataPtrR8D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR4D1(i)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR4D2(i,j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
            do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
            do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
              dataPtrR8D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR8D1(i)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR8D2(i,j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            endif
          else
            ! only choice left is both 2d coordinate arrays
            if (coordTypeKind==ESMF_TYPEKIND_R4) then
            do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
            do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
              dataPtrR8D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR4D2(i,j)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR4D2(i,j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            elseif (coordTypeKind==ESMF_TYPEKIND_R8) then
            do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
            do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
              dataPtrR8D2(i,j) = &
                (sin(real(l_member)*3.1416*(coord1PtrR8D2(i,j)+real(l_step))/180.) * &
                cos(real(l_member)*3.1416*(coord2PtrR8D2(i,j)+real(l_step))/180.)) * &
                l_amplitude+l_meanValue
            enddo
            enddo
            endif
          endif
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Unsupported typekind-rank and scheme combination requested.", &
            CONTEXT, rcToReturn=rc)
          return ! bail out
        endif
      elseif (rank==3) then
        ! 3D sin*cos*sin pattern
        ! TODO: support Meshes
        call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=coord2PtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) &
          return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=3, farrayPtr=coord3PtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      if (typekind==ESMF_TYPEKIND_R4) then
        call ESMF_FieldGet(field, farrayPtr=dataPtrR4D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        do k=lbound(dataPtrR4D3,3),ubound(dataPtrR4D3,3)
        do j=lbound(dataPtrR4D3,2),ubound(dataPtrR4D3,2)
        do i=lbound(dataPtrR4D3,1),ubound(dataPtrR4D3,1)
          dataPtrR4D3(i,j,k) = &
            (sin(real(l_member)*3.1416*(coord1PtrR8D3(i,j,k)+real(l_step))/180.) * &
            cos(real(l_member)*3.1416*(coord2PtrR8D3(i,j,k)+real(l_step))/180.) * &
            sin(real(l_member)*3.1416*(coord3PtrR8D3(i,j,k)+real(l_step))/180.)) * &
            l_amplitude+l_meanValue
        enddo
        enddo
        enddo
        elseif (typekind==ESMF_TYPEKIND_R8) then
          call ESMF_FieldGet(field, farrayPtr=dataPtrR8D3, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          do k=lbound(dataPtrR8D3,3),ubound(dataPtrR8D3,3)
          do j=lbound(dataPtrR8D3,2),ubound(dataPtrR8D3,2)
          do i=lbound(dataPtrR8D3,1),ubound(dataPtrR8D3,1)
            dataPtrR8D3(i,j,k) = &
              (sin(real(l_member)*3.1416*(coord1PtrR8D3(i,j,k)+real(l_step))/180.) * &
              cos(real(l_member)*3.1416*(coord2PtrR8D3(i,j,k)+real(l_step))/180.) * &
              sin(real(l_member)*3.1416*(coord3PtrR8D3(i,j,k)+real(l_step))/180.)) * &
              l_amplitude+l_meanValue
          enddo
          enddo
          enddo
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Unsupported typekind-rank and scheme combination requested.", &
            CONTEXT, rcToReturn=rc)
          return ! bail out
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Unsupported typekind-rank and scheme combination requested.", &
          CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
    else if (trim(dataFillScheme)=="one") then
      if (typekind==ESMF_TYPEKIND_R8 .and. rank==1) then
        ! 1D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        ! initialize the entire array
        dataPtrR8D1 = 1._ESMF_KIND_R8
      elseif (typekind==ESMF_TYPEKIND_R4 .and. rank==1) then
        ! 1D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR4D1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        ! initialize the entire array
        dataPtrR4D1 = 1._ESMF_KIND_R4
      elseif (typekind==ESMF_TYPEKIND_R8 .and. rank==2) then
        ! 2D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        ! initialize the entire array
        dataPtrR8D2 = 1._ESMF_KIND_R8
      elseif (typekind==ESMF_TYPEKIND_R4 .and. rank==2) then
        ! 2D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR4D2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        ! initialize the entire array
        dataPtrR4D2 = 1._ESMF_KIND_R4
      elseif (typekind==ESMF_TYPEKIND_R8 .and. rank==3) then
        ! 3D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        ! initialize the entire array
        dataPtrR8D3 = 1._ESMF_KIND_R8
      elseif (typekind==ESMF_TYPEKIND_R4 .and. rank==3) then
        ! 3D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR4D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        ! initialize the entire array
        dataPtrR4D3 = 1._ESMF_KIND_R4
      endif
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Unknown dataFillScheme requested.", &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    deallocate(coordDimCount,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of coordinate dimensions memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

!------------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillState"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillState_I4 - Fill data into State
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillState
  subroutine WRFHYDRO_ESMF_FillState_I4(state,value,rc)
! ! ARGUMENTS
    type(ESMF_State), intent(in)                :: state
    integer(ESMF_KIND_I4),intent(in)            :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_State.
!
!   The arguments are:
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do iIndex = 1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state,field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_FillField(field,value=value,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillState"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillState_I8 - Fill data into State
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillState
  subroutine WRFHYDRO_ESMF_FillState_I8(state,value,rc)
! ! ARGUMENTS
    type(ESMF_State), intent(in)                :: state
    integer(ESMF_KIND_I8),intent(in)            :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_State.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do iIndex = 1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state,field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_FillField(field,value=value,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillState"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillState_R4 - Fill data into State
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillState
  subroutine WRFHYDRO_ESMF_FillState_R4(state,value,rc)
! ! ARGUMENTS
    type(ESMF_State), intent(in)                :: state
    real(ESMF_KIND_R4),intent(in)               :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_State.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do iIndex = 1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state,field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_FillField(field,value=value,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillState"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillState_R8 - Fill data into State
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillState
  subroutine WRFHYDRO_ESMF_FillState_R8(state,value,rc)
! ! ARGUMENTS
    type(ESMF_State), intent(in)                :: state
    real(ESMF_KIND_R8),intent(in)               :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_State.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do iIndex = 1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state,field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_FillField(field,value=value,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillState"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillState_SCHEME - Fill data into State
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillState
  subroutine WRFHYDRO_ESMF_FillState_SCHEME(state,dataFillScheme,step,rc)
! ! ARGUMENTS
    type(ESMF_State), intent(in)                :: state
    character(len=*), intent(in)                :: dataFillScheme
    integer, intent(in), optional               :: step
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_State.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                :: k
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    k=1 ! initialize
    do iIndex = 1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state,field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call ESMF_FieldFill(field, dataFillScheme=dataFillScheme, &
          member=k, step=step, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        k=k+1 ! increment the member counter
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillFieldBundle"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillFieldBundle_I4 - Fill data into FieldBundle
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillFieldBundle
  subroutine WRFHYDRO_ESMF_FillFieldBundle_I4(fieldbundle,value,rc)
! ! ARGUMENTS
    type(ESMF_FieldBundle), intent(in)          :: fieldbundle
    integer(ESMF_KIND_I4),intent(in)            :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_FieldBundle.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: fIndex
    integer                         :: fieldCount
    type(ESMF_Field),pointer        :: fieldList(:)
    integer                         :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(fieldList(fieldCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do fIndex=1,fieldCount
      call WRFHYDRO_ESMF_FillField(fieldList(fIndex),value=value,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
    enddo

    deallocate(fieldList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillFieldBundle"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillFieldBundle_I8 - Fill data into FieldBundle
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillFieldBundle
  subroutine WRFHYDRO_ESMF_FillFieldBundle_I8(fieldbundle,value,rc)
! ! ARGUMENTS
    type(ESMF_FieldBundle), intent(in)          :: fieldbundle
    integer(ESMF_KIND_I8),intent(in)            :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_FieldBundle.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: fIndex
    integer                         :: fieldCount
    type(ESMF_Field),pointer        :: fieldList(:)
    integer                         :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(fieldList(fieldCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do fIndex=1,fieldCount
      call WRFHYDRO_ESMF_FillField(fieldList(fIndex),value=value,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
    enddo

    deallocate(fieldList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillFieldBundle"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillFieldBundle_R4 - Fill data into FieldBundle
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillFieldBundle
  subroutine WRFHYDRO_ESMF_FillFieldBundle_R4(fieldbundle,value,rc)
! ! ARGUMENTS
    type(ESMF_FieldBundle), intent(in)          :: fieldbundle
    real(ESMF_KIND_R4),intent(in)               :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_FieldBundle.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: fIndex
    integer                         :: fieldCount
    type(ESMF_Field),pointer        :: fieldList(:)
    integer                         :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(fieldList(fieldCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do fIndex=1,fieldCount
      call WRFHYDRO_ESMF_FillField(fieldList(fIndex),value=value,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
    enddo

    deallocate(fieldList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillFieldBundle"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillFieldBundle_R8 - Fill data into FieldBundle
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillFieldBundle
  subroutine WRFHYDRO_ESMF_FillFieldBundle_R8(fieldbundle,value,rc)
! ! ARGUMENTS
    type(ESMF_FieldBundle), intent(in)          :: fieldbundle
    real(ESMF_KIND_R8),intent(in)               :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_FieldBundle.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: fIndex
    integer                         :: fieldCount
    type(ESMF_Field),pointer        :: fieldList(:)
    integer                         :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(fieldList(fieldCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do fIndex=1,fieldCount
      call WRFHYDRO_ESMF_FillField(fieldList(fIndex),value=value,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
    enddo

    deallocate(fieldList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillFieldBundle"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillFieldBundle_SCHEME - Fill data into FieldBundle
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillFieldBundle
  subroutine WRFHYDRO_ESMF_FillFieldBundle_SCHEME(fieldbundle,dataFillScheme,step,rc)
! ! ARGUMENTS
    type(ESMF_FieldBundle), intent(in)          :: fieldbundle
    character(len=*), intent(in)                :: dataFillScheme
    integer, intent(in), optional               :: step
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_FieldBundle.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: fIndex
    integer                         :: fieldCount
    type(ESMF_Field),pointer        :: fieldList(:)
    integer                         :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(fieldList(fieldCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_FieldBundleGet(fieldbundle, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do fIndex=1,fieldCount
      call ESMF_FieldFill(fieldList(fIndex), dataFillScheme=dataFillScheme, &
        member=fIndex, step=step, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out 
    enddo

    deallocate(fieldList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of field lists failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillField"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillField_I4 - Fill data into Field
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillField
  subroutine WRFHYDRO_ESMF_FillField_I4(field,value,rc)
! ! ARGUMENTS
    type(ESMF_Field), intent(in)                :: field
    integer(ESMF_KIND_I4),intent(in)            :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_Field.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Array)  :: array    

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field,array=array,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call WRFHYDRO_ESMF_FillArray(array,value=value,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillField"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillField_I8 - Fill data into Field
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillField
  subroutine WRFHYDRO_ESMF_FillField_I8(field,value,rc)
! ! ARGUMENTS
    type(ESMF_Field), intent(in)                :: field
    integer(ESMF_KIND_I8),intent(in)            :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_Field.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Array)  :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field,array=array,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call WRFHYDRO_ESMF_FillArray(array,value=value,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillField"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillField_R4 - Fill data into Field
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillField
  subroutine WRFHYDRO_ESMF_FillField_R4(field,value,rc)
! ! ARGUMENTS
    type(ESMF_Field), intent(in)                :: field
    real(ESMF_KIND_R4),intent(in)               :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_Field.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Array)  :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field,array=array,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call WRFHYDRO_ESMF_FillArray(array,value=value,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillField"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillField_R8 - Fill data into Field
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillField
  subroutine WRFHYDRO_ESMF_FillField_R8(field,value,rc)
! ! ARGUMENTS
    type(ESMF_Field), intent(in)                :: field
    real(ESMF_KIND_R8),intent(in)               :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_Field.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Array)  :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field,array=array,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call WRFHYDRO_ESMF_FillArray(array,value=value,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillArray"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillArray_I4 - Fill data into Array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillArray
  subroutine WRFHYDRO_ESMF_FillArray_I4(array,value,rc)
! ! ARGUMENTS
    type(ESMF_Array), intent(in)                :: array
    integer(ESMF_KIND_I4),intent(in)            :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_Array.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer(ESMF_KIND_I4),pointer :: farray_I41D(:)
    integer(ESMF_KIND_I4),pointer :: farray_I42D(:,:)
    integer(ESMF_KIND_I4),pointer :: farray_I43D(:,:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I81D(:)
    integer(ESMF_KIND_I8),pointer :: farray_I82D(:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I83D(:,:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R41D(:)
    real(ESMF_KIND_R4),pointer    :: farray_R42D(:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R43D(:,:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R81D(:)
    real(ESMF_KIND_R8),pointer    :: farray_R82D(:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R83D(:,:,:)
    type(ESMF_TypeKind_Flag)      :: typekind
    integer                       :: rank
    integer                       :: localDeCount
    integer                       :: deIndex

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array,typekind=typekind,rank=rank,localDeCount=localDeCount,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (rank == 1) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I41D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I41D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I81D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I81D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R41D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R41D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R81D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R81D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    elseif (rank == 2) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I42D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I82D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R42D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R82D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    elseif (rank == 3) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I43D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I43D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I83D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I83D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R43D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R43D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R83D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R83D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
        msg="Cannot fill ESMF Array because rank is not supported", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillArray"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillArray_I8 - Fill data into Array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillArray
  subroutine WRFHYDRO_ESMF_FillArray_I8(array,value,rc)
! ! ARGUMENTS
    type(ESMF_Array), intent(in)                :: array
    integer(ESMF_KIND_I8),intent(in)            :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_Array.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer(ESMF_KIND_I4),pointer :: farray_I41D(:)
    integer(ESMF_KIND_I4),pointer :: farray_I42D(:,:)
    integer(ESMF_KIND_I4),pointer :: farray_I43D(:,:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I81D(:)
    integer(ESMF_KIND_I8),pointer :: farray_I82D(:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I83D(:,:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R41D(:)
    real(ESMF_KIND_R4),pointer    :: farray_R42D(:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R43D(:,:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R81D(:)
    real(ESMF_KIND_R8),pointer    :: farray_R82D(:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R83D(:,:,:)
    type(ESMF_TypeKind_Flag)      :: typekind
    integer                       :: rank
    integer                       :: localDeCount
    integer                       :: deIndex

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array,typekind=typekind,rank=rank,localDeCount=localDeCount,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (rank == 1) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I41D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I41D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I81D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I81D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R41D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R41D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R81D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R81D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    elseif (rank == 2) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I42D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I82D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R42D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R82D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    elseif (rank == 3) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I43D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I43D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I83D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I83D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R43D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R43D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R83D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R83D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
        msg="Cannot fill ESMF Array because rank is not supported", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillArray"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillArray_R4 - Fill data into Array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillArray
  subroutine WRFHYDRO_ESMF_FillArray_R4(array,value,rc)
! ! ARGUMENTS
    type(ESMF_Array), intent(in)                :: array
    real(ESMF_KIND_R4),intent(in)               :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_Array.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer(ESMF_KIND_I4),pointer :: farray_I41D(:)
    integer(ESMF_KIND_I4),pointer :: farray_I42D(:,:)
    integer(ESMF_KIND_I4),pointer :: farray_I43D(:,:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I81D(:)
    integer(ESMF_KIND_I8),pointer :: farray_I82D(:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I83D(:,:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R41D(:)
    real(ESMF_KIND_R4),pointer    :: farray_R42D(:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R43D(:,:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R81D(:)
    real(ESMF_KIND_R8),pointer    :: farray_R82D(:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R83D(:,:,:)
    type(ESMF_TypeKind_Flag)      :: typekind
    integer                       :: rank
    integer                       :: localDeCount
    integer                       :: deIndex

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array,typekind=typekind,rank=rank,localDeCount=localDeCount,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (rank == 1) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I41D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I41D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I81D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I81D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R41D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R41D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R81D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R81D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    elseif (rank == 2) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I42D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I82D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R42D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R82D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    elseif (rank == 3) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I43D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I43D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I83D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I83D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R43D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R43D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R83D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R83D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
        msg="Cannot fill ESMF Array because rank is not supported", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_FillArray"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_FillArray_R8 - Fill data into Array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_FillArray
  subroutine WRFHYDRO_ESMF_FillArray_R8(array,value,rc)
! ! ARGUMENTS
    type(ESMF_Array), intent(in)                :: array
    real(ESMF_KIND_R8),intent(in)               :: value
    integer, intent(out),optional               :: rc
! !DESCRIPTION:
!   Fill data into ESMF_Array.
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer(ESMF_KIND_I4),pointer :: farray_I41D(:)
    integer(ESMF_KIND_I4),pointer :: farray_I42D(:,:)
    integer(ESMF_KIND_I4),pointer :: farray_I43D(:,:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I81D(:)
    integer(ESMF_KIND_I8),pointer :: farray_I82D(:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I83D(:,:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R41D(:)
    real(ESMF_KIND_R4),pointer    :: farray_R42D(:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R43D(:,:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R81D(:)
    real(ESMF_KIND_R8),pointer    :: farray_R82D(:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R83D(:,:,:)
    type(ESMF_TypeKind_Flag)      :: typekind
    integer                       :: rank
    integer                       :: localDeCount
    integer                       :: deIndex

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array,typekind=typekind,rank=rank,localDeCount=localDeCount,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (rank == 1) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I41D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I41D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I81D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I81D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R41D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R41D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R81D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R81D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    elseif (rank == 2) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I42D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I82D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R42D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R82D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    elseif (rank == 3) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I43D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I43D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I83D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_I83D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R43D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R43D = value
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R83D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          farray_R83D = value
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot fill ESMF Array because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
        msg="Cannot fill ESMF Array because rank is not supported", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NetcdfIsPresent"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NetcdfIsPresent - Check NetCDF file for varname
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NetcdfIsPresent
  function WRFHYDRO_ESMF_NetcdfIsPresent(varname,filename,rc)
! ! RETURN VALUE
    logical :: WRFHYDRO_ESMF_NetcdfIsPresent
! ! ARGUMENTS
    character(len=*), intent(in)          :: varname
    character(len=*), intent(in)          :: filename
    integer, intent(out),optional         :: rc
! !DESCRIPTION:
!   Check NetCDF file for varname
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                               :: stat
    integer                               :: varid
    integer                               :: ncid

    if (present(rc)) rc = ESMF_SUCCESS

    stat = nf90_open(filename,nf90_NoWrite,ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
        msg="Error opening NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inq_varid(ncid,varname,varid)
    if(stat == nf90_NoErr) then
      WRFHYDRO_ESMF_NetcdfIsPresent = .TRUE.
    elseif(stat == nf90_eNotVar) then
      WRFHYDRO_ESMF_NetcdfIsPresent = .FALSE.
    else
      WRFHYDRO_ESMF_NetcdfIsPresent = .FALSE.
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_close(ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
        msg="Error closing NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end function
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NetcdfReadIXJX_Field"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NetcdfReadIXJX_Field - Read NetCDF var into ESMF field
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NetcdfReadIXJX
  subroutine WRFHYDRO_ESMF_NetcdfReadIXJX_Field(varname,filename,start,field,rc)
! ! ARGUMENTS
    character(len=*), intent(in)          :: varname
    character(len=*), intent(in)          :: filename
    integer,intent(in)                    :: start(2)
    type(ESMF_Field),intent(inout)        :: field
    integer, intent(out),optional         :: rc
! !DESCRIPTION:
!   Read NetCDF variable into ESMF field
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Array)                      :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field,array=array,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call WRFHYDRO_ESMF_NetcdfReadIXJX(varname,filename,start,array=array,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NetcdfReadIXJX_Array"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NetcdfReadIXJX_Array - Read NetCDF var into ESMF array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NetcdfReadIXJX
  subroutine WRFHYDRO_ESMF_NetcdfReadIXJX_Array(varname,filename,start,array,rc)
! ! ARGUMENTS
    character(len=*), intent(in)          :: varname
    character(len=*), intent(in)          :: filename
    integer,intent(in)                    :: start(2)
    type(ESMF_Array),intent(inout)        :: array
    integer, intent(out),optional         :: rc
! !DESCRIPTION:
!   Read NetCDF var into ESMF array
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer(ESMF_KIND_I4),pointer :: farray_I42D(:,:)
    integer(ESMF_KIND_I8),pointer :: farray_I82D(:,:)
    real(ESMF_KIND_R4),pointer    :: farray_R42D(:,:)
    real(ESMF_KIND_R8),pointer    :: farray_R82D(:,:)
    type(ESMF_TypeKind_Flag)      :: typekind
    integer                       :: rank
    integer                       :: localDeCount
    integer                       :: deIndex

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array,typekind=typekind,rank=rank,localDeCount=localDeCount,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (rank == 2) then
      if (typekind == ESMF_TYPEKIND_I4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          call WRFHYDRO_ESMF_NetcdfReadIXJX(varname,filename,start,farray=farray_I42D, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        enddo
      elseif (typekind == ESMF_TYPEKIND_I8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_I82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          call WRFHYDRO_ESMF_NetcdfReadIXJX(varname,filename,start,farray=farray_I82D, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        enddo
      elseif (typekind == ESMF_TYPEKIND_R4) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R42D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          call WRFHYDRO_ESMF_NetcdfReadIXJX(varname,filename,start,farray=farray_R42D, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        enddo
      elseif (typekind == ESMF_TYPEKIND_R8) then
        do deIndex=0,localDeCount-1
          call ESMF_ArrayGet(array,farrayPtr=farray_R82D,localDe=deIndex,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          call WRFHYDRO_ESMF_NetcdfReadIXJX(varname,filename,start,farray=farray_R82D, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
          msg="Cannot read NetCDF because typekind is not supported", &
          CONTEXT, rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK,   &
        msg="Cannot read NetCDF because rank is not supported", &
        CONTEXT, rcToReturn=rc)
      return
    endif 

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NetcdfReadIXJX_I4"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NetcdfReadIXJX_I4 - Read NetCDF variable into I4 array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NetcdfReadIXJX
  subroutine WRFHYDRO_ESMF_NetcdfReadIXJX_I4(varname,filename,start,farray,rc)
! ! ARGUMENTS
    character(len=*), intent(in)          :: varname
    character(len=*), intent(in)          :: filename
    integer,intent(in)                    :: start(2)
    integer(ESMF_KIND_I4),intent(inout)   :: farray(:,:)
    integer, intent(out),optional         :: rc
! !DESCRIPTION:
!   Read NetCDF variable into I4 array
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                               :: stat
    integer                               :: varid
    integer, dimension(nf90_max_var_dims) :: dimIDs
    integer                               :: dimCnt(2)
    integer                               :: ncid

    if (present(rc)) rc = ESMF_SUCCESS

    stat = nf90_open(filename,nf90_NoWrite,ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
        msg="Error opening NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inq_varid(ncid,varname,varid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    ! How big is the netCDF variable, that is, what are the lengths of
    !   its constituent dimensions?
    stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error locating variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inquire_dimension(ncid, dimIDs(1), len = dimCnt(1))
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable dim1 "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inquire_dimension(ncid, dimIDs(2), len = dimCnt(2))
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable dim2 "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    if ((start(1)+size(farray,1)-1 > dimCnt(1)) .OR. &
    (start(2)+size(farray,2)-1 > dimCnt(2))) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE,   &
        msg="Error FORTRAN array size mismatch "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_get_var(ncid, varid, values=farray, start=(/start(1),start(2)/), &
      count=(/size(farray,1),size(farray,2)/))
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable values "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_close(ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
        msg="Error closing NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NetcdfReadIXJX_I8"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NetcdfReadIXJX_I8 - Read NetCDF variable into I8 array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NetcdfReadIXJX
  subroutine WRFHYDRO_ESMF_NetcdfReadIXJX_I8(varname,filename,start,farray,rc)
! ! ARGUMENTS
    character(len=*), intent(in)          :: varname
    character(len=*), intent(in)          :: filename
    integer,intent(in)                    :: start(2)
    integer(ESMF_KIND_I8),intent(inout)   :: farray(:,:)
    integer, intent(out),optional         :: rc
! !DESCRIPTION:
!   Read NetCDF variable into I8 array
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                               :: stat
    integer                               :: varid
    integer, dimension(nf90_max_var_dims) :: dimIDs
    integer                               :: dimCnt(2)
    integer                               :: ncid

    if (present(rc)) rc = ESMF_SUCCESS

    stat = nf90_open(filename,nf90_NoWrite,ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
        msg="Error opening NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inq_varid(ncid,varname,varid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    ! How big is the netCDF variable, that is, what are the lengths of
    !   its constituent dimensions?
    stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error loacating variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inquire_dimension(ncid, dimIDs(1), len = dimCnt(1))
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable dim1 "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inquire_dimension(ncid, dimIDs(2), len = dimCnt(2))
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable dim2 "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    if ((start(1)+size(farray,1)-1 > dimCnt(1)) .OR. &
    (start(2)+size(farray,2)-1 > dimCnt(2))) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE,   &
        msg="Error FORTRAN array size mismatch "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_get_var(ncid, varid, values=farray, start=(/start(1),start(2)/), &
      count=(/size(farray,1),size(farray,2)/))
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable values "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_close(ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
        msg="Error closing NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NetcdfReadIXJX_R4"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NetcdfReadIXJX_R4 - Read NetCDF variable into R4 array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NetcdfReadIXJX
  subroutine WRFHYDRO_ESMF_NetcdfReadIXJX_R4(varname,filename,start,farray,rc)
! ! ARGUMENTS
    character(len=*), intent(in)          :: varname
    character(len=*), intent(in)          :: filename
    integer,intent(in)                    :: start(2)
    real(ESMF_KIND_R4),intent(inout)      :: farray(:,:)
    integer, intent(out),optional         :: rc
! !DESCRIPTION:
!   Read NetCDF variable into R4 array
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                               :: stat
    integer                               :: varid
    integer, dimension(nf90_max_var_dims) :: dimIDs
    integer                               :: dimCnt(2)
    integer                               :: ncid

    if (present(rc)) rc = ESMF_SUCCESS

    stat = nf90_open(filename,nf90_NoWrite,ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
        msg="Error opening NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inq_varid(ncid,varname,varid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    ! How big is the netCDF variable, that is, what are the lengths of
    !   its constituent dimensions?
    stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error loacating variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inquire_dimension(ncid, dimIDs(1), len = dimCnt(1))
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable dim1 "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inquire_dimension(ncid, dimIDs(2), len = dimCnt(2))
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable dim2 "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    if ((start(1)+size(farray,1)-1 > dimCnt(1)) .OR. &
    (start(2)+size(farray,2)-1 > dimCnt(2))) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE,   &
        msg="Error FORTRAN array size mismatch "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_get_var(ncid, varid, values=farray, start=(/start(1),start(2)/), &
      count=(/size(farray,1),size(farray,2)/))
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable values "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_close(ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
        msg="Error closing NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_NetcdfReadIXJX_R8"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_NetcdfReadIXJX_R8 - Read NetCDF variable into R8 array
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_NetcdfReadIXJX
  subroutine WRFHYDRO_ESMF_NetcdfReadIXJX_R8(varname,filename,start,farray,rc)
! ! ARGUMENTS
    character(len=*), intent(in)          :: varname
    character(len=*), intent(in)          :: filename
    integer,intent(in)                    :: start(2)
    real(ESMF_KIND_R8),intent(inout)      :: farray(:,:)
    integer, intent(out),optional         :: rc
! !DESCRIPTION:
!   Read NetCDF variable into R8 array
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                               :: stat
    integer                               :: varid
    integer, dimension(nf90_max_var_dims) :: dimIDs
    integer                               :: dimCnt(2)
    integer                               :: ncid

    if (present(rc)) rc = ESMF_SUCCESS

    stat = nf90_open(filename,nf90_NoWrite,ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
        msg="Error opening NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inq_varid(ncid,varname,varid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    ! How big is the netCDF variable, that is, what are the lengths of
    !   its constituent dimensions?
    stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error loacating variable "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inquire_dimension(ncid, dimIDs(1), len = dimCnt(1))
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable dim1 "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_inquire_dimension(ncid, dimIDs(2), len = dimCnt(2))
    if(stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable dim2 "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    if ((start(1)+size(farray,1)-1 > dimCnt(1)) .OR. &
    (start(2)+size(farray,2)-1 > dimCnt(2))) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE,   &
        msg="Error FORTRAN array size mismatch "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_get_var(ncid, varid, values=farray, start=(/start(1),start(2)/), &
      count=(/size(farray,1),size(farray,2)/))
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_READ,   &
        msg="Error reading variable values "//trim(varname)// &
          " in "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

    stat = nf90_close(ncid)
    if (stat /= nf90_NoErr) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
        msg="Error closing NetCDF file "//trim(filename)//".", &
        CONTEXT, rcToReturn=rc)
      return
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogStateList"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogStateList - Write ESMF state information to PET Logs
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogStateList
  subroutine WRFHYDRO_ESMF_LogStateList(stateList,nestedFlag,label,rc)
! ! ARGUMENTS
    type(ESMF_State),intent(in)          :: stateList(:)
    logical,intent(in),optional          :: nestedFlag
    character(len=*),intent(in),optional :: label
    integer, intent(out),optional        :: rc
! !DESCRIPTION:
!   Write ESMF state information to PET Logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64) :: llabel
    integer           :: sIndex

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'WRFHYDRO_ESMF_LogStateList'
    endif

    do sIndex=1, size(stateList)
      call WRFHYDRO_ESMF_LogState(stateList(sIndex),nestedFlag,label,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
    enddo

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogState"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogState - Write ESMF state information to PET Logs
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogState
  subroutine WRFHYDRO_ESMF_LogState(state,nestedFlag,label,fvalues,rc)
! ! ARGUMENTS
    type(ESMF_State),intent(in)  :: state
    logical,intent(in),optional  :: nestedFlag
    character(len=*),optional    :: label
    logical,intent(in),optional  :: fvalues
    integer,intent(out),optional :: rc
! !DESCRIPTION:
!   Write ESMF state information to PET Logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)                     :: llabel
    logical                               :: lfvalues
    integer                               :: stat
    integer                               :: itemCount
    character(len=64),allocatable         :: itemNameList(:)
    type(ESMF_StateItem_Flag),allocatable :: itemTypeList(:)
    type(ESMF_Field)                      :: field
    character(len=64)                     :: stateName
    character(len=12)                     :: itemTypeStr
    integer                               :: iIndex
    character(len=ESMF_MAXSTR)            :: logMsg

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'WRFHYDRO_ESMF_LogState'
    endif

    if (present(fvalues)) then
      lfvalues = fvalues
    else
      lfvalues = .FALSE.
    endif

    call ESMF_StateGet(state, nestedFlag=nestedFlag, &
      itemCount=itemCount, name=stateName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out  
   
    if (itemCount > 0 ) then

      allocate(itemNameList(itemCount),itemTypeList(itemCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of item list memory failed.", &
        CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_StateGet(state, nestedFlag=nestedFlag, &
        itemNameList=itemNameList,itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

      do iIndex=1, itemCount

        if (itemTypeList(iIndex) == ESMF_STATEITEM_ARRAY) then
          itemTypeStr = 'ARRAY'
        elseif (itemTypeList(iIndex) == ESMF_STATEITEM_ARRAYBUNDLE) then
          itemTypeStr = 'ARRAYBUNDLE'
        elseif (itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
          itemTypeStr = 'FIELD'
        elseif (itemTypeList(iIndex) == ESMF_STATEITEM_FIELDBUNDLE) then
          itemTypeStr = 'FIELDBUNDLE'
        elseif (itemTypeList(iIndex) == ESMF_STATEITEM_ROUTEHANDLE) then
          itemTypeStr = 'ROUTEHANDLE'
        elseif (itemTypeList(iIndex) == ESMF_STATEITEM_STATE) then
          itemTypeStr = 'STATE'
        else
          itemTypeStr = 'UNKNOWN'
        endif

        write (logMsg,"(A,A,(A,I0,A,I0,A),A)") trim(llabel)//": ", &
          trim(stateName), &
          " StateItem(",iIndex," of ",itemCount,") ", &
          trim(itemNameList(iIndex))//"["//trim(itemTypeStr)//"]"
        call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

      enddo

      deallocate(itemNameList,itemTypeList,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of item list memory failed.", &
        CONTEXT, rcToReturn=rc)) return  ! bail out

    else
      write (logMsg,"(A,A)") trim(llabel)//": ", &
        trim(stateName)//" is empty."
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFieldConnections"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFieldConnections - Write ESMF state connection to logs
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFieldConnections
  subroutine WRFHYDRO_ESMF_LogFieldConnections(state,nestedFlag,label,rc)
! ! ARGUMENTS
    type(ESMF_State), intent(in)            :: state
    logical, intent(in), optional           :: nestedFlag
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write ESMF state connection to PET logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)                       :: llabel
    character(len=64), allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(len=32)                       :: stateName
    integer                                 :: iIndex, itemCount
    integer                                 :: stat
    character(len=ESMF_MAXSTR)              :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(state, itemCount=itemCount, &
      nestedFlag=nestedFlag,name=stateName,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if(present(label)) then
      llabel = trim(label)
    else
      llabel = 'WRFHYDRO_ESMF_LogFieldConnections '//trim(stateName)
    endif

    call ESMF_StateGet(state, itemCount=itemCount, &
      nestedFlag=nestedFlag,name=stateName,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    allocate(itemNameList(itemCount),itemTypeList(itemCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of item name and type list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet(state, itemNameList=itemNameList, &
      itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do iIndex=1, itemCount
      if (itemTypeList(iIndex) /= ESMF_STATEITEM_FIELD) then
        write (logMsg,"(A,A)") trim(llabel)//": ", &
          trim(itemNameList(iIndex))//" is not a field."
      elseif (NUOPC_IsConnected(state, fieldName=itemNameList(iIndex))) then
        write (logMsg,"(A,A)") trim(llabel)//": ", &
          trim(itemNameList(iIndex))//" is connected."
      else
        write (logMsg,"(A,A)") trim(llabel)//": ", &
          trim(itemNameList(iIndex))//" is not connected."
      endif
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    enddo

    deallocate(itemNameList,itemTypeList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of item name and type list memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogGrid"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogGrid - Write ESMF grid information to PET logs
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogGrid
  subroutine WRFHYDRO_ESMF_LogGrid(grid,label,rc)
! ! ARGUMENTS
    type(ESMF_Grid), intent(in)            :: grid
    character(len=*), intent(in), optional :: label
    integer, intent(out), optional         :: rc
! !DESCRIPTION:
!   Write ESMF grid information to PET logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)           :: llabel
    character(len=64)           :: gridName
    type(ESMF_DistGrid)         :: distgrid
    character(len=64)           :: transferAction
    integer                     :: localDeCount
    integer                     :: dimCount, tileCount
    integer                     :: dimIndex, tileIndex
    integer,allocatable         :: coordDimCount(:)
    integer                     :: coordDimMax
    integer,allocatable         :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer                     :: stat
    character(len=ESMF_MAXSTR)  :: logMsg

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'WRFHYDRO_ESMF_LogGrid'
    endif

    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, name=gridName, &
      localDeCount=localDeCount, distgrid=distgrid, &
      dimCount=dimCount,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    ! allocate coordDim info accord. to dimCount and tileCount
    allocate(coordDimCount(dimCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of coordinate dimensions memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    ! get coordDim info
    call ESMF_GridGet(grid, coordDimCount=coordDimCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    coordDimMax = 0
    do dimIndex=1,dimCount
      coordDimMax = MAX(coordDimMax,coordDimCount(dimIndex))
    enddo

    if (coordDimMax == 1) then
      write (logMsg,"(A,A,A)") trim(llabel)//": ", &
        trim(gridName), &
        " is a rectilinear grid with 1D coordinates in each dimension."
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    endif

    deallocate(coordDimCount, &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Dellocation of coordinate dimensions memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " local decomposition count=",localDeCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " dimension count=",dimCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " tile count=",tileCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of index array memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
       maxIndexPTile=maxIndexPTile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    do tileIndex=1,tileCount
    do dimIndex=1,dimCount
      write (logMsg,"(A,A,A,4(I0,A))") trim(llabel)//": ", &
        trim(gridName), &       
        " (tile,dim,minIndexPTile,maxIndexPTile)=(", &
        tileIndex,",",dimIndex,",", &
        minIndexPTile(dimIndex,tileIndex),",", &
        maxIndexPTile(dimIndex,tileIndex),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo
    enddo

    deallocate(minIndexPTile, maxIndexPTile,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of index array memory failed.", &
      CONTEXT, rcToReturn=rc)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFieldList"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFieldList - Write ESMF field information to PET logs
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFieldList
  subroutine WRFHYDRO_ESMF_LogFieldList(fieldList,label,rc)
! ! ARGUMENTS
    type(ESMF_Field),intent(in)          :: fieldList(:)
    character(len=*),intent(in),optional :: label
    integer,intent(out),optional         :: rc
! !DESCRIPTION:
!   Write ESMF field information to PET logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64) :: llabel
    integer           :: fIndex

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'WRFHYDRO_ESMF_LogFieldList'
    endif

    do fIndex=1,size(fieldList)
      call WRFHYDRO_ESMF_LogField(fieldList(fIndex),llabel,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
    enddo

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogField"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogField - Write ESMF field information to PET logs
! !INTERFACE:
  ! call using generic interface: ??
  subroutine WRFHYDRO_ESMF_LogField(field,label,rc)
! ! ARGUMENTS
    type(ESMF_Field)                     :: field
    character(len=*),intent(in),optional :: label
    integer,intent(out),optional         :: rc
! !DESCRIPTION:
!   Write ESMF field information to PET logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)            :: llabel
    character(ESMF_MAXSTR)       :: logMsg
    integer                      :: stat
    type(ESMF_FieldStatus_Flag)  :: fieldStatus
    character(len=10)            :: fieldStatusStr
    character(ESMF_MAXSTR)       :: fieldName
    type(ESMF_GeomType_Flag)     :: fieldGeomtype
    character(len=10)            :: fieldGeomtypeStr
    character(len=64)            :: fieldConsumerConn
    character(len=64)            :: fieldTransferOffer
    character(len=64)            :: fieldTransferAction

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'WRFHYDRO_ESMF_LogField'
    endif

    call ESMF_FieldGet(field,status=fieldStatus,name=fieldName,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
      fieldStatusStr = 'EMPTY'
    elseif (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
      fieldStatusStr = 'GRIDSET'
    elseif (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
      fieldStatusStr = 'COMPLETE'
    else
      fieldStatusStr = 'UNKNOWN'
    endif

    if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE .OR. &
    fieldStatus == ESMF_FIELDSTATUS_GRIDSET ) then
      call ESMF_FieldGet(field, geomtype=fieldGeomtype,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      if (fieldGeomtype == ESMF_GEOMTYPE_GRID) then
        fieldGeomtypeStr = 'GRID'
      elseif (fieldGeomtype == ESMF_GEOMTYPE_MESH) then
        fieldGeomtypeStr = 'MESH'
      elseif (fieldGeomtype == ESMF_GEOMTYPE_XGRID) then
        fieldGeomtypeStr = 'XGRID'
      elseif (fieldGeomtype == ESMF_GEOMTYPE_LOCSTREAM) then
        fieldGeomtypeStr = 'LOCSTREAM'
      else
        fieldGeomtypeStr = 'UNKNOWN'
      endif
    else
      fieldGeomtypeStr = 'NOTSET'
    endif

    call NUOPC_GetAttribute(field, name="ConsumerConnection", &
      value=fieldConsumerConn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call NUOPC_GetAttribute(field, name="TransferOfferGeomObject", &
      value=fieldTransferOffer, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
      value=fieldTransferAction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    write (logMsg,"(A,A,(2A))") trim(llabel)//": ", &
      trim(fieldName), &
      " status=",trim(fieldStatusStr)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A,(2A))") trim(llabel)//": ", &
      trim(fieldName), &
      " geomtype=",trim(fieldGeomtypeStr)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A,(2A))") trim(llabel)//": ", &
      trim(fieldName), &
      " consumerconn=",trim(fieldConsumerConn)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A,2(2A))") trim(llabel)//": ", &
      trim(fieldName), &
      " transferOffer=",trim(fieldTransferOffer), &
      " transferAction=",trim(fieldTransferAction)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFieldLclVal"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFieldLclVal - Write ESMF field local vals to PET logs
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFieldLclVal
  subroutine WRFHYDRO_ESMF_LogFieldLclVal(field, label, rc)
! ! ARGUMENTS
    type(ESMF_Field), intent(in)            :: field
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write ESMF field local vals to PET logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Array)  :: array
    character(len=64) :: llabel
    character(len=64) :: fieldName

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = "WRFHYDRO_ESMF_LogFieldLclVal"
    endif

    call ESMF_FieldGet(field,array=array,name=fieldName,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out    

    call WRFHYDRO_ESMF_LogArrayLclVal(array,fieldName=fieldName,label=llabel,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogArrayLclVal"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogArrayLclVal - Write ESMF array local vals to PET logs
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogArrayLclVal
  subroutine WRFHYDRO_ESMF_LogArrayLclVal(array, fieldName, label, rc)
! ! ARGUMENTS
    type(ESMF_Array), intent(in)            :: array
    character(len=*), intent(in), optional  :: fieldName
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write ESMF array local vals to PET logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)             :: llabel
    type(ESMF_TypeKind_Flag)      :: typekind
    integer                       :: rank
    integer(ESMF_KIND_I4),pointer :: dataPtr_I4_1D(:)
    integer(ESMF_KIND_I4),pointer :: dataPtr_I4_2D(:,:)
    integer(ESMF_KIND_I4),pointer :: dataPtr_I4_3D(:,:,:)
    integer(ESMF_KIND_I8),pointer :: dataPtr_I8_1D(:)
    integer(ESMF_KIND_I8),pointer :: dataPtr_I8_2D(:,:)
    integer(ESMF_KIND_I8),pointer :: dataPtr_I8_3D(:,:,:)
    real(ESMF_KIND_R4),pointer    :: dataPtr_R4_1D(:)
    real(ESMF_KIND_R4),pointer    :: dataPtr_R4_2D(:,:)
    real(ESMF_KIND_R4),pointer    :: dataPtr_R4_3D(:,:,:)
    real(ESMF_KIND_R8),pointer    :: dataPtr_R8_1D(:,:)
    real(ESMF_KIND_R8),pointer    :: dataPtr_R8_2D(:,:)
    real(ESMF_KIND_R8),pointer    :: dataPtr_R8_3D(:,:,:)

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = "WRFHYDRO_ESMF_LogArrayLclVal"
    endif

    call ESMF_ArrayGet(array, typekind=typekind,rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (typekind == ESMF_TYPEKIND_I4) then
      if (rank == 1) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_I4_1D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_I4_1D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      elseif (rank == 2) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_I4_2D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_I4_2D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      elseif (rank == 3) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_I4_3D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_I4_3D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      else
        call ESMF_LogWrite(trim(llabel)//" rank out of log utility range.", &
          ESMF_LOGMSG_INFO)
      endif
    elseif (typekind == ESMF_TYPEKIND_I8) then
      if (rank == 1) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_I8_1D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_I8_1D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      elseif (rank == 2) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_I8_2D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_I8_2D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      elseif (rank == 3) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_I8_3D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_I8_3D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      else
        call ESMF_LogWrite(trim(llabel)//" rank out of log uttility range.", &
          ESMF_LOGMSG_INFO)
      endif
    elseif (typekind == ESMF_TYPEKIND_R4) then
      if (rank == 1) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_1D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_R4_1D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      elseif (rank == 2) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_2D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_R4_2D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      elseif (rank == 3) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_3D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_R4_3D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      else
        call ESMF_LogWrite(trim(llabel)//" rank out of log utility range.", &
          ESMF_LOGMSG_INFO)
      endif
    elseif (typekind == ESMF_TYPEKIND_R8) then
      if (rank == 1) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_1D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_R8_1D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      elseif (rank == 2) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_2D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_R8_2D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      elseif (rank == 3) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_3D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
        call WRFHYDRO_ESMF_LogFarrayLclVal(dataPtr_R8_3D, fieldName=fieldName, &
          label=trim(llabel), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
      else
        call ESMF_LogWrite(trim(llabel)//" rank out of log utility range.", &
          ESMF_LOGMSG_INFO)
      endif
    else
      call ESMF_LogWrite(trim(llabel)//" typekind out of log utility range.", &
        ESMF_LOGMSG_INFO)
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_I41D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_I41D - Write I41D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_I41D(farray, fieldName, label, rc)
! ! ARGUMENTS
    integer(ESMF_KIND_I4),intent(in)           :: farray(:)
    character(len=*), intent(in), optional     :: fieldName
    character(len=*), intent(in), optional     :: label
    integer, intent(out), optional             :: rc
! !DESCRIPTION:
!   Write I41D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_I42D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_I42D - Write I42D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_I42D(farray, fieldName, label, rc)
! ! ARGUMENTS
    integer(ESMF_KIND_I4),intent(in)           :: farray(:,:)
    character(len=*), intent(in), optional     :: fieldName
    character(len=*), intent(in), optional     :: label
    integer, intent(out), optional             :: rc
! !DESCRIPTION:
!   Write I42D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_I43D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_I43D - Write I43D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_I43D(farray, fieldName, label, rc)
! ! ARGUMENTS
    integer(ESMF_KIND_I4),intent(in)           :: farray(:,:,:)
    character(len=*), intent(in), optional     :: fieldName
    character(len=*), intent(in), optional     :: label
    integer, intent(out), optional             :: rc
! !DESCRIPTION:
!   Write I43D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_I81D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_I81D - Write I81D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_I81D(farray, fieldName, label, rc)
! ! ARGUMENTS
    integer(ESMF_KIND_I8),intent(in)           :: farray(:)
    character(len=*), intent(in), optional     :: fieldName
    character(len=*), intent(in), optional     :: label
    integer, intent(out), optional             :: rc
! !DESCRIPTION:
!   Write I81D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_I82D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_I82D - Write I82D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_I82D(farray, fieldName, label, rc)
! ! ARGUMENTS
    integer(ESMF_KIND_I8),intent(in)           :: farray(:,:)
    character(len=*), intent(in), optional     :: fieldName
    character(len=*), intent(in), optional     :: label
    integer, intent(out), optional             :: rc
! !DESCRIPTION:
!   Write I82D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_I83D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_I83D - Write I83D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_I83D(farray, fieldName, label, rc)
! ! ARGUMENTS
    integer(ESMF_KIND_I8),intent(in)           :: farray(:,:,:)
    character(len=*), intent(in), optional     :: fieldName
    character(len=*), intent(in), optional     :: label
    integer, intent(out), optional             :: rc
! !DESCRIPTION:
!   Write I83D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_R41D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_R41D - Write R41D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_R41D(farray, fieldName, label, rc)
! ! ARGUMENTS
    real(ESMF_KIND_R4),intent(in)           :: farray(:)
    character(len=*), intent(in), optional  :: fieldName
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write R41D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_R42D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_R42D - Write R42D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_R42D(farray, fieldName, label, rc)
! ! ARGUMENTS
    real(ESMF_KIND_R4),intent(in)           :: farray(:,:)
    character(len=*), intent(in), optional  :: fieldName
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write R41D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_R43D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_R43D - Write R43D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_R43D(farray, fieldName, label, rc)
! ! ARGUMENTS
    real(ESMF_KIND_R4),intent(in)           :: farray(:,:,:)
    character(len=*), intent(in), optional  :: fieldName
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write R43D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_R81D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_R81D - Write R81D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_R81D(farray, fieldName, label, rc)
! ! ARGUMENTS
    real(ESMF_KIND_R8),intent(in)           :: farray(:)
    character(len=*), intent(in), optional  :: fieldName
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write R81D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_R82D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_R82D - Write R82D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_R82D(farray, fieldName, label, rc)
! ! ARGUMENTS
    real(ESMF_KIND_R8),intent(in)           :: farray(:,:)
    character(len=*), intent(in), optional  :: fieldName
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write R81D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogFarrayLclVal_R83D"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogFarrayLclVal_R83D - Write R83D array local vals to log
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogFarrayLclVal
  subroutine WRFHYDRO_ESMF_LogFarrayLclVal_R83D(farray, fieldName, label, rc)
! ! ARGUMENTS
    real(ESMF_KIND_R8),intent(in)           :: farray(:,:,:)
    character(len=*), intent(in), optional  :: fieldName
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
! !DESCRIPTION:
!   Write R83D array local vals to log
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)          :: llabel
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      if (present(fieldName)) then
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"//" "//trim(fieldName)
      else
        llabel = "WRFHYDRO_ESMF_LogFarrayLclVal"
      endif
    endif

    write(logMsg,'(A,A,3(F0.3,A))') trim(llabel), &
      " MinVal=",minval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " MaxVal=",maxval(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(A,(A,F0.3))') trim(llabel), &
      " Sum=",sum(farray)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_LogCplList"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_LogCplList - Write ESMF CplList to PET logs
! !INTERFACE:
  ! call using generic interface: WRFHYDRO_ESMF_LogCplList
  subroutine WRFHYDRO_ESMF_LogCplList(cplcomp,label,rc)
! ! ARGUMENTS
    type(ESMF_CplComp),intent(in)        :: cplcomp
    character(len=*),intent(in),optional :: label
    integer, intent(out),optional        :: rc
! !DESCRIPTION:
!   Write ESMF CplList to PET logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)                   :: llabel
    character(ESMF_MAXSTR), allocatable :: cplList(:)
    integer                             :: cplListSize
    integer                             :: cIndex
    integer                             :: stat
    character(len=64)                   :: name
    character(ESMF_MAXSTR)              :: logMsg

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'WRFHYDRO_ESMF_LogCplList'
    endif

    ! query the CplComp for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    ! get the CplList Attribute
    call NUOPC_CompAttributeGet(cplcomp, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out

    if (cplListSize>0) then
      allocate(cplList(cplListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal CplList memory failed.", &
        CONTEXT, rcToReturn=rc)) return  ! bail out
      call NUOPC_CompAttributeGet(cplcomp, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
   else
     write (logMsg,"(A,A,A)") trim(llabel)//": ", &
       trim(name), &
       " CplList is empty."
     call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
   endif

    ! main loop over all entries in the cplList
    do cIndex=1, cplListSize
      write (logMsg,"(A,A,(A,I0,A,I0,A),A)") trim(llabel)//": ", &
        trim(name), &
        " CplListItem(",cIndex, " of ", cplListSize, ")=", &
        trim(cplList(cIndex))
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

    ! clean-up
    if (cplListSize>0) then
      deallocate(cplList,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of internal CplList memory failed.", &
        CONTEXT, rcToReturn=rc)) return  ! bail out
    endif

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

#define METHOD "WRFHYDRO_ESMF_ChDir"
!BOP
! !IROUTINE: WRFHYDRO_ESMF_ChDir - Change working directory
! !INTERFACE:
  ! call using: WRFHYDRO_ESMF_ChDir
  subroutine WRFHYDRO_ESMF_ChDir(path,rc)
! ! USES
    use iso_c_binding
! ! ARGUMENTS
    character(*), intent(in)       :: path
    integer, intent(out), optional :: rc
! !DESCRIPTION:
!   Change working directory
!
!   The arguments are:
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer :: stat

    stat = local_chdir(path//c_null_char)
    if (stat /= 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD,   &
        msg="WRFHYDRO_ESMF_ChDir failed changing to "//trim(path), &
        CONTEXT, rcToReturn=stat)
    else
      stat = ESMF_SUCCESS
    endif
    if (present(rc)) rc = stat

  end subroutine
#undef METHOD

  !-----------------------------------------------------------------------------

end module WRFHydro_ESMF_Extensions
