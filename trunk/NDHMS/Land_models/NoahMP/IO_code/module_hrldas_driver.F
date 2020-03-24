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


module module_hrldas_driver

  ! this is used to drive NoahMP
  use module_noahmp_hrldas_driver, only: land_driver_ini, land_driver_exe

  ! NEW MODULE WITH DT INCLUDING STATE, PARAMETER,FORCING AND GEOMETRY. Different modules
  use state_module, only: state_type
  use parameters
  use forcing
  use geometry
  use orchestrator_base
  
  IMPLICIT NONE

  contains

  subroutine noahMP_init(NTIME,state,nuopc_comm)
    ! arguments
    integer :: NTIME, nuopc_comm
    type(state_type) :: state

print *, "entered noahMP_init 11111111111......... "

    call orchestrator%init()
print *, "entered noahMP_init 2222222222222222......... "
    call land_driver_ini(NTIME, state, nuopc_comm)
print *, "entered noahMP_init 3333333333333333......... "
  
  end subroutine noahMP_init

  subroutine noahMP_exe(ITIME, state)
    ! arguments
    integer :: ITIME
    type(state_type) :: state

     ! DTs must be passed to the exe.
     call land_driver_exe(ITIME, state)
  end subroutine

  
  subroutine noahMP_finish()
    ! arguments
    type(state_type) :: state

    !Pass state for destruction
    call hydro_finish(state)
  end subroutine

end module module_hrldas_driver
