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

program noah_hrldas_driver
! this is the main program to drive HRLDAS-Noah, HRLDAS-NoahMP, and other Land models.

#ifdef Noah1d
! this is used to drive Noah1d
  use module_noah1d_hrldas_driver, only: land_driver_ini, land_driver_exe
#else
  ! this is used to drive NoahMP
  use module_noahmp_hrldas_driver, only: land_driver_ini, land_driver_exe
#endif

  ! NEW MODULE WITH DT INCLUDING STATE, PARAMETER,FORCING AND GEOMETRY. Different modules
  use state_module, only: state_type
  use parameters
  use forcing
  use geometry
  use orchestrator_base
  
  implicit none
  integer :: ITIME, NTIME
  type(state_type) :: state
  
  call orchestrator%init()
  
  call land_driver_ini(NTIME, state)
  
#ifdef WRF_HYDRO   
  do ITIME = 1, NTIME
#else
  do ITIME = 0, NTIME
#endif
     ! DTs must be passed to the exe.
     call land_driver_exe(ITIME, state)
  end do
#ifdef WRF_HYDRO
  !Pass state for destruction
  call hydro_finish()
#endif
     
end program noah_hrldas_driver
