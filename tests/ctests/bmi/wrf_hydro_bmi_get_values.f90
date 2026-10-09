program wrf_hydro_nwm_bmi_get_values
  use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_VAR_NAME
  implicit none
  type(bmi_wrf_hydro_nwm) :: wrf_hydro
  integer :: stat
  character(len=BMI_MAX_VAR_NAME), allocatable :: var_name
  character(len=64) :: type_name, var_unit
  integer :: input_item_count, output_item_count, i
  character(len=BMI_MAX_VAR_NAME), pointer :: output_var_names_ptr(:)
  character(len=BMI_MAX_VAR_NAME), pointer :: input_var_names(:), &
       output_var_names(:)
  integer, allocatable :: var_data(:)
  integer, allocatable :: grid_shape(:)

  stat = BMI_SUCCESS

  wrf_hydro = wrf_hydro_nwm()
  call stat_check(wrf_hydro%initialize("no config file"))

  var_name = "qlink1"
  ! var_name = "IVGTYP"
  print *, "var_name is ", trim(var_name)
  call stat_check(wrf_hydro%get_var_grid(var_name, var_grid))
  call stat_check(wrf_hydro%get_var_type(var_name, type_name))
  call stat_check(wrf_hydro%get_var_units(var_name, var_unit))
  call stat_check(wrf_hydro%get_grid_size(var_grid, grid_size))
  call stat_check(wrf_hydro%get_var_itemsize(var_name, item_size))
  call stat_check(wrf_hydro%get_var_nbytes(var_name, nbytes))

  allocate(var_data(grid_size))
  call stat_check(wrf_hydro%get_value(trim(output_var_names(var_grid)), var_data))
  print *, "getting data", var_data(228:230), "should match 11, 5, 11"
  print *, "-----", stat
  call stat_check(wrf_hydro%get_grid_shape(var_grid, grid_shape))
  print *, "Shape is ", grid_shape, " and stat" ,stat

  ! print *, "====== PTR TEST ======"
  ! call stat_check(wrf_hydro%get_value_ptr(trim(output_var_names(var_grid)), ptr_data))
  ! stat = BMI_SUCCESS

  ! print *, "ptr_data associated value is ", associated(ptr_data)

  print *, "====== GET AT INDICES ======"

  ! ind_data: need to allocate?
  indices = [228,230,229]
  allocate(ind_data(size(indices)))
  call stat_check(wrf_hydro%get_value_at_indices(trim(output_var_names(var_grid)), ind_data, &
       [228,230,229]))

  print *, "got the data from indices:", ind_data




  ! check first two input var names
  if (trim(input_var_names(1)) /= "FOO") then
     print *, "input_var_name(1) = ", trim(input_var_names(1))
     error stop "ERROR: get_input_var_names :: input_var_names /= FOO"
  end if
  if (trim(input_var_names(2)) /= "BAR") then
     print *, "input_var_name(2) = ", trim(input_var_names(2))
     error stop "ERROR: get_input_var_names :: input_var_names /= BAR"
  end if
end program wrf_hydro_nwm_bmi_get_values
