program wrf_hydro_nwm_bmi_get_var
  use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_VAR_NAME
  implicit none
  type(bmi_wrf_hydro_nwm) :: wrf_hydro
  integer :: stat
  character(len=BMI_MAX_VAR_NAME), allocatable :: var_name
  character(len=64) :: type_name, var_unit
  integer :: var_grid, grid_size, item_size, nbytes

  stat = BMI_SUCCESS

  wrf_hydro = wrf_hydro_nwm()
  call stat_check(wrf_hydro%initialize("no config file"))

  var_name = "qlink1"
  call stat_check(wrf_hydro%get_var_grid(var_name, var_grid))
  call stat_check(wrf_hydro%get_var_type(var_name, type_name))
  call stat_check(wrf_hydro%get_var_units(var_name, var_unit))
  call stat_check(wrf_hydro%get_grid_size(var_grid, grid_size))
  call stat_check(wrf_hydro%get_var_itemsize(var_name, item_size))
  call stat_check(wrf_hydro%get_var_nbytes(var_name, nbytes))

  if (trim(var_name) /= "qlink1") then
     print *, "var_name = ", trim(var_name)
     error stop "ERROR: bmi :: var_name /= qlink1"
  end if
  if (var_grid /= 201) then
     print *, "var_grid = ", var_grid
     error stop "ERROR: get_var_grid :: var_grid /= 201"
  end if
  if (type_name /= "real") then
     print *, "type_name = ", type_name
     error stop "ERROR: get_var_type :: type_name /= real"
  end if
  if (var_unit /= "m3/s") then
     print *, "var_unit = ", var_unit
     error stop "ERROR: get_var_units :: var_unit /= m3/s"
  end if
  if (grid_size /= 445) then
     print *, "grid_size = ", grid_size
     error stop "ERROR: get_grid_size :: grid_size /= 445"
  end if
  if (item_size /= 4) then
     print *, "item_size = ", item_size
     error stop "ERROR: get_var_itemsize :: item_size /= 4"
  end if
  if (nbytes /= 1780) then
     print *, "nbytes = ", nbytes
     error stop "ERROR: get_var_nbytes :: nbytes /= 1780"
  end if
end program wrf_hydro_nwm_bmi_get_var
