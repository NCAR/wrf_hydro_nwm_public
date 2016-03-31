!2345678
!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!  <brief list of changes to this source file>
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they
!        include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

       subroutine clm_drv_HYDRO()
          use module_clm_HYDRO, only: clm_cpl_HYDRO
          implicit none
          call clm_cpl_HYDRO()
       end subroutine clm_drv_HYDRO
