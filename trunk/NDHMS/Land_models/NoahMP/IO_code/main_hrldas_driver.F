program Noah_hrldas_driver
! this is the main program to drive HRLDAS-Noah, HRLDAS-NoahMP, and other Land models.

#ifdef Noah1d
! this is used to drive Noah1d
   use module_noah1d_hrldas_driver, only: land_driver_ini, land_driver_exe
#else
! this is used to drive NoahMP
   use module_noahmp_hrldas_driver, only: land_driver_ini, land_driver_exe
#endif

#ifdef WRF_HYDRO_NUDGING
#ifdef HYDRO_D
   use module_stream_nudging, only: init_stream_nudging_clock
   use module_nudging_utils, only: accum_nudging_time, nudging_timer
   use module_mpp_land, only: my_id, io_id
#endif
#endif

   implicit none
   integer :: ITIME, NTIME

#ifdef WRF_HYDRO_NUDGING
#ifdef HYDRO_D
   !! no mpp defined here... so this happens on all processors... 
   real :: startcodeTime, endCodeTime
   call init_stream_nudging_clock()
   call nudging_timer(startCodeTime)
#endif
#endif


   call land_driver_ini(NTIME)
#ifdef WRF_HYDRO   
   do ITIME = 1, NTIME
#else
   do ITIME = 0, NTIME
#endif
       call land_driver_exe(ITIME)
   end do

   call hydro_finish() 

#ifdef WRF_HYDRO_NUDGING
#ifdef HYDRO_D
#ifdef MPP_LAND
   if(my_id .eq. io_id) then 
#endif 
      call nudging_timer(endCodeTime)
      call accum_nudging_time(startCodeTime, endCodeTime,  &
                              'Entire model time', .false.)
#ifdef MPP_LAND
   endif
#endif 
#endif
#endif

END 

