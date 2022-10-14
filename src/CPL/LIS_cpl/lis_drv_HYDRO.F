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
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

!2345678
       subroutine lis_drv_HYDRO(n)
          use module_lis_HYDRO, only: lis_cpl_HYDRO
          implicit none
          integer n
#ifdef HYDRO_D
          write(6,*) "calling lis_cpl_HYDRO "
#endif
!         stop 888
          call lis_cpl_HYDRO(n)
       end subroutine lis_drv_HYDRO
