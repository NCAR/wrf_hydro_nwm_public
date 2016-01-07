!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

!subroutine hrldas_RAPID_drv
!  use rapid_main , only : rapid_ini, rapid_main_exe
!  implicit none

!  integer, parameter :: ii = 224
!  integer, parameter :: jj = 242
!  real,dimension(ii,jj) :: runoff
!  integer ITIME, NTIME

! initialize the RAPID model: including reading namelist, allocate memory...
!  call rapid_ini(NTIME)

! for each time step, 
! call RAPID_coupler
! call RAPID_main (modify the time loop to 1)
!  do ITIME=1,NTIME
!       call rapid_main_exe(ITIME,runoff,ii,jj)
!  end do  
! end loop for calling RAPID programs

!end subroutine hrldas_RAPID_drv

   subroutine hrldas_RAPID_ini(ntime)
     use rapid_main , only : rapid_ini
     implicit none
     integer :: ntime
print *,'***********************************************************'
print *,'*******Initialize RAPID model******************************'
print *,'***********************************************************'
!print *,'ntime = ',ntime
     call rapid_ini(ntime)
   end subroutine hrldas_RAPID_ini

   subroutine hrldas_RAPID_exe(runoff,ii,jj)
     use rapid_main , only : rapid_main_exe
     implicit none
     real,dimension(ii,jj) :: runoff
     integer :: ii,jj
     call rapid_main_exe(1,runoff,ii,jj)
   end subroutine hrldas_RAPID_exe
