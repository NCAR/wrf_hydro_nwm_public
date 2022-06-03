#define FILENAME "WRFHydro_NUOPC_Flags.F90"
#define MODNAME "wrfhydro_nuopc_flags"
#include "WRFHydro_NUOPC_Macros.h"

module wrfhydro_nuopc_flags
! !MODULE: wrfhydro_nuopc_flags
!
! !DESCRIPTION:
!   This module controls WRFHYDRO configuration flags for NUOPC cap
!
! !REVISION HISTORY:
!  21Sep23    Dan Rosen  Initial Specification
!
! !USES:
  use ESMF, only: ESMF_UtilStringUpperCase, ESMF_SUCCESS

  implicit none

  private

  type memory_flag
    sequence
    private
      integer :: mem
  end type memory_flag

  type(memory_flag), parameter ::     &
    MEMORY_ERROR   = memory_flag(-1), &
    MEMORY_POINTER = memory_flag(0),  &
    MEMORY_COPY    = memory_flag(1)

  type fillv_flag
    sequence
    private
      integer :: fillv
  end type fillv_flag

  type(fillv_flag), parameter ::       &
    FILLV_ERROR      = fillv_flag(-1), &
    FILLV_MISSING    = fillv_flag(0),  &
    FILLV_ZERO       = fillv_flag(1),  &
    FILLV_PRESCRIBE  = fillv_flag(2),  &
    FILLV_MODEL      = fillv_flag(3),  &
    FILLV_DEPENDENCY = fillv_flag(4),  &
    FILLV_FILE       = fillv_flag(5)

  type checkclock_flag
    sequence
    private
      integer :: checkclock
  end type checkclock_flag

  type(checkclock_flag), parameter ::       &
    CHECKCLOCK_ERROR = checkclock_flag(-1), &
    CHECKCLOCK_CURRT = checkclock_flag(0),  &
    CHECKCLOCK_NEXTT = checkclock_flag(1),  &
    CHECKCLOCK_NONE  = checkclock_flag(2)

  type missingval_flag
    sequence
    private
      integer :: missingval
  end type missingval_flag

  type(missingval_flag), parameter ::           &
    MISSINGVAL_ERROR     = missingval_flag(-1), &
    MISSINGVAL_IGNORE    = missingval_flag(0),  &
    MISSINGVAL_FAIL      = missingval_flag(1),  &
    MISSINGVAL_PRESCRIBE = missingval_flag(2)

  public memory_flag
  public fillv_flag
  public checkclock_flag
  public missingval_flag
  public MEMORY_ERROR
  public MEMORY_COPY
  public MEMORY_POINTER
  public FILLV_ERROR
  public FILLV_ZERO
  public FILLV_MISSING
  public FILLV_PRESCRIBE
  public FILLV_MODEL
  public FILLV_DEPENDENCY
  public FILLV_FILE
  public CHECKCLOCK_ERROR
  public CHECKCLOCK_CURRT
  public CHECKCLOCK_NEXTT
  public CHECKCLOCK_NONE
  public MISSINGVAL_ERROR
  public MISSINGVAL_IGNORE
  public MISSINGVAL_FAIL
  public MISSINGVAL_PRESCRIBE

  public operator(==), assignment(=)

  interface operator (==)
    module procedure memory_eq
    module procedure fillv_eq
    module procedure checkclock_eq
    module procedure missingval_eq
  end interface

  interface assignment (=)
    module procedure memory_toString
    module procedure memory_frString
    module procedure fillv_toString
    module procedure fillv_frString
    module procedure checkclock_toString
    module procedure checkclock_frString
    module procedure missingval_toString
    module procedure missingval_frString
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function memory_eq(val1, val2)
    logical memory_eq
    type(memory_flag), intent(in) :: val1, val2
    memory_eq = (val1%mem == val2%mem)
  end function memory_eq

  !-----------------------------------------------------------------------------
  subroutine memory_toString(string, val)
    character(len=*), intent(out) :: string
    type(memory_flag), intent(in) :: val
    if (val == MEMORY_COPY) then
      write(string,'(a)') 'MEMORY_COPY'
    elseif (val == MEMORY_POINTER) then
      write(string,'(a)') 'MEMORY_POINTER'
    else
      write(string,'(a)') 'MEMORY_ERROR'
    endif
  end subroutine memory_toString

  !-----------------------------------------------------------------------------

  subroutine memory_frString(val, string)
    type(memory_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=16) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = MEMORY_ERROR
    elseif (ustring .eq. 'MEMORY_COPY') then
      val = MEMORY_COPY
    elseif (ustring .eq. 'MEMORY_POINTER') then
      val = MEMORY_POINTER
    else
      val = MEMORY_ERROR
    endif
  end subroutine memory_frString

  !-----------------------------------------------------------------------------

  function fillv_eq(val1, val2)
    logical fillv_eq
    type(fillv_flag), intent(in) :: val1, val2
    fillv_eq = (val1%fillv == val2%fillv)
  end function fillv_eq

  !-----------------------------------------------------------------------------

  subroutine fillv_toString(string, val)
    character(len=*), intent(out) :: string
    type(fillv_flag), intent(in) :: val
    if (val == FILLV_ZERO) then
      write(string,'(a)') 'FILLV_ZERO'
    elseif (val == FILLV_MISSING) then
      write(string,'(a)') 'FILLV_MISSING'
    elseif (val == FILLV_PRESCRIBE) then
      write(string,'(a)') 'FILLV_PRESCRIBE'
    elseif (val == FILLV_MODEL) then
      write(string,'(a)') 'FILLV_MODEL'
    elseif (val == FILLV_DEPENDENCY) then
      write(string,'(a)') 'FILLV_DEPENDENCY'
    elseif (val == FILLV_FILE) then
      write(string,'(a)') 'FILLV_FILE'
    else
      write(string,'(a)') 'FILLV_ERROR'
    endif
  end subroutine fillv_toString

  !-----------------------------------------------------------------------------

  subroutine fillv_frString(val, string)
    type(fillv_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=16) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = FILLV_ERROR
    elseif (ustring .eq. 'FILLV_ZERO') then
      val = FILLV_ZERO
    elseif (ustring .eq. 'FILLV_MISSING') then
      val = FILLV_MISSING
    elseif (ustring .eq. 'FILLV_PRESCRIBE') then
      val = FILLV_PRESCRIBE
    elseif (ustring .eq. 'FILLV_MODEL') then
      val = FILLV_MODEL
    elseif (ustring .eq. 'FILLV_DEPENDENCY') then
      val = FILLV_DEPENDENCY
    elseif (ustring .eq. 'FILLV_FILE') then
      val = FILLV_FILE
    else
      val = FILLV_ERROR
    endif
  end subroutine fillv_frString

  !-----------------------------------------------------------------------------

  function checkclock_eq(val1, val2)
    logical checkclock_eq
    type(checkclock_flag), intent(in) :: val1, val2
    checkclock_eq = (val1%checkclock == val2%checkclock)
  end function checkclock_eq

  !-----------------------------------------------------------------------------

  subroutine checkclock_toString(string, val)
    character(len=*), intent(out) :: string
    type(checkclock_flag), intent(in) :: val
    if (val == CHECKCLOCK_CURRT) then
      write(string,'(a)') 'CHECKCLOCK_CURRT'
    elseif (val == CHECKCLOCK_NEXTT) then
      write(string,'(a)') 'CHECKCLOCK_NEXTT'
    elseif (val == CHECKCLOCK_NONE) then
      write(string,'(a)') 'CHECKCLOCK_NONE'
    else
      write(string,'(a)') 'CHECKCLOCK_ERROR'
    endif
  end subroutine checkclock_toString

  !-----------------------------------------------------------------------------

  subroutine checkclock_frString(val, string)
    type(checkclock_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=16) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = CHECKCLOCK_ERROR
    elseif (ustring .eq. 'CHECKCLOCK_CURRT') then
      val = CHECKCLOCK_CURRT
    elseif (ustring .eq. 'CHECKCLOCK_NEXTT') then
      val = CHECKCLOCK_NEXTT
    elseif (ustring .eq. 'CHECKCLOCK_NONE') then
      val = CHECKCLOCK_NONE
    else
      val = CHECKCLOCK_ERROR
    endif
  end subroutine checkclock_frString

  !-----------------------------------------------------------------------------

  function missingval_eq(val1, val2)
    logical missingval_eq
    type(missingval_flag), intent(in) :: val1, val2
    missingval_eq = (val1%missingval == val2%missingval)
  end function missingval_eq

  !-----------------------------------------------------------------------------

  subroutine missingval_toString(string, val)
    character(len=*), intent(out) :: string
    type(missingval_flag), intent(in) :: val
    if (val == MISSINGVAL_IGNORE) then
      write(string,'(a)') 'MISSINGVAL_IGNORE'
    elseif (val == MISSINGVAL_FAIL) then
      write(string,'(a)') 'MISSINGVAL_FAIL'
    elseif (val == MISSINGVAL_PRESCRIBE) then
      write(string,'(a)') 'MISSINGVAL_PRESCRIBE'
    else
      write(string,'(a)') 'MISSINGVAL_ERROR'
    endif
  end subroutine missingval_toString

  !-----------------------------------------------------------------------------

  subroutine missingval_frString(val, string)
    type(missingval_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=20) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = MISSINGVAL_ERROR
    elseif (ustring .eq. 'MISSINGVAL_IGNORE') then
      val = MISSINGVAL_IGNORE
    elseif (ustring .eq. 'MISSINGVAL_FAIL') then
      val = MISSINGVAL_FAIL
    elseif (ustring .eq. 'MISSINGVAL_PRESCRIBE') then
      val = MISSINGVAL_PRESCRIBE
    else
      val = MISSINGVAL_ERROR
    endif
  end subroutine missingval_frString

  !-----------------------------------------------------------------------------

end module
