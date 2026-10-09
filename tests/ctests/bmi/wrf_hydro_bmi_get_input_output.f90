program wrf_hydro_nwm_bmi_get_input_output
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

  stat = BMI_SUCCESS

  wrf_hydro = wrf_hydro_nwm()
  call stat_check(wrf_hydro%initialize("no config file"))

  call stat_check(wrf_hydro%get_input_item_count(input_item_count))
  call stat_check(wrf_hydro%get_input_var_names(input_var_names))
  call stat_check(wrf_hydro%get_output_item_count(output_item_count))
  call stat_check(wrf_hydro%get_output_var_names(output_var_names))

  print *, "input item count =", input_item_count
  print *, "output item count =", output_item_count
  do i=1,input_item_count
     print *, "input item", i, ":", trim(input_var_names(i))
  end do
  do i=1,output_item_count
     print *, "output item", i, ":", trim(output_var_names(i))
  end do

  ! check first two input var names
  if (trim(input_var_names(1)) /= "FOO") then
     print *, "input_var_name(1) = ", trim(input_var_names(1))
     error stop "ERROR: get_input_var_names :: input_var_names /= FOO"
  end if
  if (trim(input_var_names(2)) /= "BAR") then
     print *, "input_var_name(2) = ", trim(input_var_names(2))
     error stop "ERROR: get_input_var_names :: input_var_names /= BAR"
  end if

  ! check first two output var names
  if (trim(output_var_names(1)) /= "IVGTYP") then
     print *, "output_var_name = ", trim(output_var_names(1))
     error stop "ERROR: get_output_var_names :: output_var_names /= IVGTYP"
  end if
  if (trim(output_var_names(2)) /= "ISLTYP") then
     print *, "output_var_name = ", trim(output_var_names(2))
     error stop "ERROR: get_output_var_names :: output_var_names /= ISLTYP"
  end if
end program wrf_hydro_nwm_bmi_get_input_output
