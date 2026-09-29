program wrf_hydro_nwm_bmi_init
  use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_COMPONENT_NAME
  implicit none
  type(bmi_wrf_hydro_nwm) :: wrf_hydro
  integer :: stat
  character(len=BMI_MAX_COMPONENT_NAME), pointer :: model_name
  double precision :: end_time, current_time, time_step
  stat = BMI_SUCCESS

  wrf_hydro = wrf_hydro_nwm()
  call stat_check(wrf_hydro%get_component_name(model_name), stat)
  call stat_check(wrf_hydro%initialize("no config file"), stat)
  call stat_check(wrf_hydro%get_start_time(current_time), stat)
  call stat_check(wrf_hydro%get_end_time(end_time), stat)

  ! only checking the first 13, full string would be ~= WRF-Hydro v5.Y.Z
  if (trim(model_name(1:13)) /= "WRF-Hydro v5.") then
     print * , "model_name(1:14) = ", trim(model_name(1:13))
     error stop "ERROR: bmi_init :: model_name /= WRF-Hydro v5."
  end if
  if (current_time /= 1.0) then
     print *, "current time =", current_time
     error stop "ERROR: bmi_init :: current_time /= 1.0"
  end if
  if (end_time /= 24.0) then
     print *, "end time =", end_time
     error stop "ERROR: bmi_init :: current_time /= 1.0"
  end if

end program wrf_hydro_nwm_bmi_init
