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
#ifdef HYDRO_D /* Temp un-ifdef to get timing results */
   use module_stream_nudging, only: init_stream_nudging_clock
   use module_nudging_utils, only: accum_nudging_time, nudging_timer
#ifdef MPP_LAND
   use module_mpp_land, only: my_id, io_id
#endif /* MPP_LAND */
#endif /* HYDRO_D Temp un-ifdef to get timing results */
#endif

   implicit none
   integer :: ITIME, NTIME

#ifdef WRF_HYDRO_NUDGING
#ifdef HYDRO_D /* JLM Temp un-ifdef to get timing results */
   !! no mpp defined here... so this happens on all processors... 
   real :: startcodeTime, endCodeTime
   call init_stream_nudging_clock()
   call nudging_timer(startCodeTime)
#endif /* HYDRO_D Temp un-ifdef to get timing results */
#endif

   call land_driver_ini(NTIME)
#ifdef WRF_HYDRO   
   do ITIME = 1, NTIME
#else
   do ITIME = 0, NTIME
#endif
       call land_driver_exe(ITIME)
   end do
#ifdef WRF_HYDRO
   call hydro_finish() 
#endif

#ifdef WRF_HYDRO_NUDGING
#ifdef HYDRO_D /* un-ifdef to get timing results */
#ifdef MPP_LAND
   if(my_id .eq. io_id) then 
#endif 
      call nudging_timer(endCodeTime)
      call accum_nudging_time(startCodeTime, endCodeTime,  &
                              'Entire model time', .false.)
#ifdef MPP_LAND
   endif
#endif 
#endif /* HYDRO_D :: un-ifdef to get timing results */
#endif


END 

