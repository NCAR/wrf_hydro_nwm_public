program Noah_hrldas_driver
! this is the main program to drive HRLDAS-Noah, HRLDAS-NoahMP, and other Land models.

#ifdef Noah1d
! this is used to drive Noah1d
   use module_noah1d_hrldas_driver, only: land_driver_ini, land_driver_exe
#else
! this is used to drive NoahMP
   use module_noahmp_hrldas_driver, only: land_driver_ini, land_driver_exe
#endif

   implicit none
   integer :: ITIME, NTIME

   call land_driver_ini(NTIME)
   
   do ITIME = 1, NTIME
       call land_driver_exe(ITIME)
   end do
#ifdef WRF_HYDRO
   call hydro_finish() 
#endif

END 

