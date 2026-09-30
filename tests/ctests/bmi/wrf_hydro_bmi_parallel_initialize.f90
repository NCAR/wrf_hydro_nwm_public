program wrf_hydro_nwm_bmi_init
  use mpi
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
  call stat_check(wrf_hydro%parallel_initialize(MPI_COMM_WORLD), stat)
  call stat_check(wrf_hydro%initialize("no config file"), stat)

end program wrf_hydro_nwm_bmi_init
