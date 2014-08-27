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
