program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  implicit none
  class(bmi_wrf_hydro_nwm), allocatable :: wrf_hydro
  integer :: stat
  character(len=16), pointer :: model_name
  double precision :: end_time, current_time, time_step

  stat = BMI_SUCCESS
  allocate(wrf_hydro)
  call stat_check(wrf_hydro%get_component_name(model_name), stat)
  print * , "--- Starting ", trim(model_name), " ---"

  call stat_check(wrf_hydro%initialize("no config file"), stat)
  call stat_check(wrf_hydro%get_start_time(current_time), stat)
  call stat_check(wrf_hydro%get_end_time(end_time), stat)

  do while (current_time < end_time)
     call stat_check(wrf_hydro%update(), stat)
     call stat_check(wrf_hydro%get_current_time(current_time), stat)
  end do

  call stat_check(wrf_hydro%finalize(), stat)

  print *, "--- Finished running test driver ---"
  if (stat .ne. BMI_SUCCESS) then
     print *, "WRF-Hydro driver returned failure"
     error stop stat
  end if
end program wrf_hydro_nwm_bmi_driver

! =======
!   use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_COMPONENT_NAME, BMI_MAX_VAR_NAME
!   use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_TYPE_NAME, BMI_MAX_UNITS_NAME
!   use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
!   implicit none
!   type(bmi_wrf_hydro_nwm) :: wrf_hydro
!   integer :: stat, i
!   character(len=BMI_MAX_COMPONENT_NAME), pointer :: model_name
!   character(len=BMI_MAX_VAR_NAME) :: time_unit
!   ! character(len=BMI_MAX_TYPE_NAME) :: type_name, var_unit
!   character(len=64) :: type_name, var_unit
!   ! character(len=:), allocatable :: type_name, var_unit
!   double precision :: end_time, current_time, time_step
!   integer :: input_item_count, output_item_count
!   character(len=BMI_MAX_VAR_NAME), pointer :: output_var_names_ptr(:)
!   character(len=BMI_MAX_VAR_NAME), pointer :: input_var_names(:), &
!        output_var_names(:)
!   integer :: var_grid, grid_size, item_size, nbytes
!   integer :: res

!   character(len=BMI_MAX_VAR_NAME), allocatable :: var_name
!   integer, allocatable ::  var_data(:)
!   integer, allocatable ::  ind_data(:), indices_data(:)
!   integer, allocatable ::  indices(:)
!   integer, pointer ::  ptr_data(:)
!   integer, allocatable :: grid_shape(:)
!   stat = BMI_SUCCESS

!   wrf_hydro = wrf_hydro_nwm()
!   call stat_check(wrf_hydro%get_component_name(model_name))
!   print * , "--- Starting ", trim(model_name), " ---"
!   call stat_check(wrf_hydro%initialize("no config file"))
!   call stat_check(wrf_hydro%get_start_time(current_time))
!   call stat_check(wrf_hydro%get_end_time(end_time))
!   ! call stat_check(wrf_hydro%get_time_units(time_unit))

!   print *, "----------------------------------------"
!   print *, "Starting Unstructured Grid Investigation"
!   print *, "----------------------------------------"
!   var_name = "qlink1"
!   ! var_name = "IVGTYP"
!   print *, "var_name is ", trim(var_name)
!   call stat_check(wrf_hydro%get_var_grid(var_name, var_grid))
!   call stat_check(wrf_hydro%get_var_type(var_name, type_name))
!   call stat_check(wrf_hydro%get_var_units(var_name, var_unit))
!   call stat_check(wrf_hydro%get_grid_size(var_grid, grid_size))
!   call stat_check(wrf_hydro%get_var_itemsize(var_name, item_size))
!   call stat_check(wrf_hydro%get_var_nbytes(var_name, nbytes))

!   print *, "var ", trim(var_name), " is of type ", trim(type_name), &
!        " and has units ", trim(var_unit)
!   print *, "var ", trim(var_name), " is grid", var_grid, &
!        " also has grid size", grid_size, "has item size", item_size, "so naturally nbytes of", nbytes








!   ! print *, "time unit is: ", trim(time_unit)
!   ! call stat_check(wrf_hydro%get_input_item_count(input_item_count))
!   ! call stat_check(wrf_hydro%get_input_var_names(input_var_names))
!   ! call stat_check(wrf_hydro%get_output_item_count(output_item_count))
!   ! call stat_check(wrf_hydro%get_output_var_names(output_var_names))
!   ! do i=1,input_item_count
!   !    print *, "input item", i, ":", trim(input_var_names(i))
!   ! end do
!   ! do i=1,output_item_count
!   !    print *, "output item", i, ":", trim(output_var_names(i))
!   ! end do


!   ! allocate(var_data(grid_size))
!   ! call stat_check(wrf_hydro%get_value(trim(output_var_names(var_grid)), var_data))
!   ! print *, "getting data", var_data(228:230), "should match 11, 5, 11"
!   ! print *, "-----", stat
!   ! call stat_check(wrf_hydro%get_grid_shape(var_grid, grid_shape))
!   ! print *, "Shape is ", grid_shape, " and stat" ,stat

!   ! ! print *, "====== PTR TEST ======"
!   ! ! call stat_check(wrf_hydro%get_value_ptr(trim(output_var_names(var_grid)), ptr_data))
!   ! ! stat = BMI_SUCCESS

!   ! ! print *, "ptr_data associated value is ", associated(ptr_data)

!   ! print *, "====== GET AT INDICES ======"

!   ! ! ind_data: need to allocate?
!   ! indices = [228,230,229]
!   ! allocate(ind_data(size(indices)))
!   ! call stat_check(wrf_hydro%get_value_at_indices(trim(output_var_names(var_grid)), ind_data, &
!   !      [228,230,229]))

!   ! print *, "got the data from indices:", ind_data

!   ! print *, "====== SETTING DATA ======"
!   ! var_data = -77
!   ! call stat_check(wrf_hydro%set_value(trim(output_var_names(var_grid)), var_data))
!   ! print *, "-- now by indices"
!   ! indices = [2,4,20]
!   ! indices_data = [11., 12., 13.]
!   ! call stat_check(wrf_hydro%set_value_at_indices(trim(output_var_names(var_grid)), &
!   !      indices, indices_data))
!   print *, "--- FIN ---"

!   ! do while (current_time < end_time)
!   !    call stat_check(wrf_hydro%update())
!   !    call stat_check(wrf_hydro%get_current_time(current_time))
!   ! end do

!   ! call stat_check(wrf_hydro%finalize())

!   ! print *, "--- Finished running test driver ---"
!   ! if (stat .ne. BMI_SUCCESS) then
!   !    print *, "WRF-Hydro driver returned failure"
!   !    error stop stat
!   ! end if
