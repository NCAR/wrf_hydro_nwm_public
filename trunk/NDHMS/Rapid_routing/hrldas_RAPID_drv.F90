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

  program main_program
   use rapid_main , only : rapid_ini, rapid_main_exe
  implicit none

  integer, parameter :: ii = 224
  integer, parameter :: jj = 242
  real,dimension(ii,jj) :: runoff
  integer ITIME, NTIME

! initialize the RAPID model: including reading namelist, allocate memory...
  call rapid_ini(NTIME)

! for each time step, 
! call RAPID_coupler
! call RAPID_main (modify the time loop to 1)
   do ITIME=1,NTIME
        call rapid_main_exe(ITIME,runoff,ii,jj)
   end do  
! end loop for calling RAPID programs
   
  end 
