module bmi_wrf_hydro_nwm_bind_c_mod
  ! use bmif_2_0
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, input_item_count, &
       output_item_count
  use orchestrator_base, only : orchestrator_
  use state_module, only: wrf_hydro_model => state_type
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_int, &
       c_double, c_char, c_null_char
  implicit none

  integer(c_int), parameter :: BMI_MAX_COMPONENT_NAME = 2048
  integer(c_int), parameter :: BMI_MAX_VAR_NAME = 2048
  integer(c_int), parameter :: BMI_MAX_TYPE_NAME = 2048
  integer(c_int), parameter :: BMI_MAX_UNITS_NAME = 2048

  integer(c_int), parameter :: BMI_FAILURE = 1
  integer(c_int), parameter :: BMI_SUCCESS = 0
  character(256) :: name = ""

  type (bmi_wrf_hydro_nwm) :: wrf_hydro

  interface

    ! Perform startup tasks for the model.
    module function wrf_hydro_initialize_c(config_file) &
         result(bmi_status) &
         bind(c, name="initialize")
      character(c_char), intent(in) :: config_file(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_initialize_c

    ! Advance the model one time step.
    module function wrf_hydro_update_c() &
         result(bmi_status) &
         bind(c, name="update")
      integer(c_int) :: bmi_status
    end function wrf_hydro_update_c

    ! Advance the model until the given time.
    module function wrf_hydro_update_until_c(time) &
         result(bmi_status) &
         bind(c, name="update_until")
      real(c_double), intent(in) :: time
      integer(c_int) :: bmi_status
    end function wrf_hydro_update_until_c

    ! Perform teardown tasks for the model.
    module function wrf_hydro_finalize_c() &
         result(bmi_status) &
         bind(c, name="finalize")
      integer(c_int) :: bmi_status
    end function wrf_hydro_finalize_c

    ! Get the name of the model.
    module function wrf_hydro_component_name_c(name) &
         result(bmi_status) &
         bind(c, name="get_component_name")
      character(c_char), intent(out) :: name(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_component_name_c

    ! Count a model's input variables.
    module function wrf_hydro_input_item_count_c(count) &
         result(bmi_status) &
         bind(c, name="get_input_item_count")
      integer(c_int), intent(out) :: count
      integer(c_int) :: bmi_status
    end function wrf_hydro_input_item_count_c

    ! Count a model's output variables.
    module function wrf_hydro_output_item_count_c(count) &
         result(bmi_status) &
         bind(c, name="get_output_item_count")
      integer(c_int), intent(out) :: count
      integer(c_int) :: bmi_status
    end function wrf_hydro_output_item_count_c

    ! List a model's input variables.
    module function wrf_hydro_input_var_names_c(names) &
         result(bmi_status) &
         bind(c, name="get_input_var_names")
      character(c_char), intent(out) :: names(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_input_var_names_c

    ! List a model's output variables.
    module function wrf_hydro_output_var_names_c(names) &
         result(bmi_status) &
         bind(c, name="get_output_var_names")
      character(c_char), intent(out) :: names(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_output_var_names_c

    ! Get the grid identifier for the given variable.
    module function wrf_hydro_var_grid_c(name, grid) &
         result(bmi_status) &
         bind(c, name="get_var_grid")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: grid
      integer(c_int) :: bmi_status
    end function wrf_hydro_var_grid_c

    ! Get the data type of the given variable as a string.
    module function wrf_hydro_var_type_c(name, type) &
         result(bmi_status) &
         bind(c, name="get_var_type")
      character(c_char), intent(in) :: name(*)
      character(c_char), intent(out) :: type(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_var_type_c

    ! Get the units of the given variable.
    module function wrf_hydro_var_units_c(name, units) &
         result(bmi_status) &
         bind(c, name="get_var_units")
      character(c_char), intent(in) :: name(*)
      character(c_char), intent(out) :: units(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_var_units_c

    ! Get memory use per array element, in bytes.
    module function wrf_hydro_var_itemsize_c(name, size) &
         result(bmi_status) &
         bind(c, name="get_var_itemsize")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: size
      integer(c_int) :: bmi_status
    end function wrf_hydro_var_itemsize_c

    ! Get size of the given variable, in bytes.
    module function wrf_hydro_var_nbytes_c(name, nbytes) &
         result(bmi_status) &
         bind(c, name="get_var_nbytes")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: nbytes
      integer(c_int) :: bmi_status
    end function wrf_hydro_var_nbytes_c

    ! Describe where a variable is located: node, edge, or face.
    module function wrf_hydro_var_location_c(name, location) &
         result(bmi_status) &
         bind(c, name="get_var_location")
      character(c_char), intent(in) :: name(*)
      character(c_char), intent(out) :: location(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_var_location_c

    ! Current time of the model.
    module function wrf_hydro_current_time_c(time) &
         result(bmi_status) &
         bind(c, name="get_current_time")
      real(c_double), intent(out) :: time
      integer(c_int) :: bmi_status
    end function wrf_hydro_current_time_c

    ! Start time of the model.
    module function wrf_hydro_start_time_c(time) &
         result(bmi_status) &
         bind(c, name="get_start_time")
      real(c_double), intent(out) :: time
      integer(c_int) :: bmi_status
    end function wrf_hydro_start_time_c

    ! End time of the model.
    module function wrf_hydro_end_time_c(time) &
         result(bmi_status) &
         bind(c, name="get_end_time")
      real(c_double), intent(out) :: time
      integer(c_int) :: bmi_status
    end function wrf_hydro_end_time_c

    ! Time units of the model.
    module function wrf_hydro_time_units_c(units) &
         result(bmi_status) &
         bind(c, name="get_time_units")
      character(c_char), intent(out) :: units(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_time_units_c

    ! Time step of the model.
    module function wrf_hydro_time_step_c(time_step) &
         result(bmi_status) &
         bind(c, name="get_time_step")
      real(c_double), intent(out) :: time_step
      integer(c_int) :: bmi_status
    end function wrf_hydro_time_step_c

    ! Get a copy of values (flattened!) of the given integer variable.
    module function wrf_hydro_get_int_c(name, dest) &
         result(bmi_status) &
         bind(c, name="get_value_int")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(inout) :: dest(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_int_c

    ! Get a copy of values (flattened!) of the given real variable.
    module function wrf_hydro_get_float_c(name, dest) &
         result(bmi_status) &
         bind(c, name="get_value_float")
      character(c_char), intent(in) :: name(*)
      real, intent(inout) :: dest(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_float_c

    ! Get a copy of values (flattened!) of the given double variable.
    module function wrf_hydro_get_double_c(name, dest) &
         result(bmi_status) &
         bind(c, name="get_value_double")
      character(c_char), intent(in) :: name(*)
      real(c_double), intent(inout) :: dest(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_double_c

    ! Get a reference to the given integer variable.
    module function wrf_hydro_get_ptr_int_c(name, dest_ptr) &
         result(bmi_status) &
         bind(c, name="get_value_ptr_int")
      character(c_char), intent(in) :: name(*)
      integer(c_int), pointer, intent(inout) :: dest_ptr(:)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_ptr_int_c

    ! Get a reference to the given real variable.
    module function wrf_hydro_get_ptr_float_c(name, dest_ptr) &
         result(bmi_status) &
         bind(c, name="get_value_ptr_float")
      character(c_char), intent(in) :: name(*)
      real, pointer, intent(inout) :: dest_ptr(:)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_ptr_float_c

    ! Get a reference to the given double variable.
    module function wrf_hydro_get_ptr_double_c(name, dest_ptr) &
         result(bmi_status) &
         bind(c, name="get_value_ptr_double")
      character(c_char), intent(in) :: name(*)
      real(c_double), pointer, intent(inout) :: dest_ptr(:)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_ptr_double_c

    ! Get integer values at particular (one-dimensional) indices.
    module function wrf_hydro_get_at_indices_int_c(name, dest, inds) &
         result(bmi_status) &
         bind(c, name="get_value_at_indices_int")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(inout) :: dest(*)
      integer(c_int), intent(in) :: inds(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_at_indices_int_c

    ! Get real values at particular (one-dimensional) indices.
    module function wrf_hydro_get_at_indices_float_c(name, dest, inds) &
         result(bmi_status) &
         bind(c, name="get_value_at_indices_float")
      character(c_char), intent(in) :: name(*)
      real, intent(inout) :: dest(*)
      integer(c_int), intent(in) :: inds(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_at_indices_float_c

    ! Get double values at particular (one-dimensional) indices.
    module function wrf_hydro_get_at_indices_double_c(name, dest, inds) &
         result(bmi_status) &
         bind(c, name="get_value_at_indices_double")
      character(c_char), intent(in) :: name(*)
      real(c_double), intent(inout) :: dest(*)
      integer(c_int), intent(in) :: inds(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_get_at_indices_double_c

    ! Set new values for an integer model variable.
    module function wrf_hydro_set_int_c(name, src) result(bmi_status) &
      bind(c, name="set_value_int")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(in) :: src(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_set_int_c

    ! Set new values for a real model variable.
    module function wrf_hydro_set_float_c(name, src) result(bmi_status) &
      bind(c, name="set_value_float")
      character(c_char), intent(in) :: name(*)
      real, intent(in) :: src(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_set_float_c

    ! Set new values for a double model variable.
    module function wrf_hydro_set_double_c(name, src) result(bmi_status) &
      bind(c, name="set_value_double")
      character(c_char), intent(in) :: name(*)
      real(c_double), intent(in) :: src(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_set_double_c

    ! Set integer values at particular (one-dimensional) indices.
    module function wrf_hydro_set_at_indices_int_c(name, inds, src) &
         result(bmi_status) &
         bind(c, name="set_value_at_indices_int")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(in) :: inds(*)
      integer(c_int), intent(in) :: src(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_set_at_indices_int_c

    ! Set real values at particular (one-dimensional) indices.
    module function wrf_hydro_set_at_indices_float_c(name, inds, src) &
         result(bmi_status) &
         bind(c, name="set_value_at_indices_float")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(in) :: inds(*)
      real, intent(in) :: src(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_set_at_indices_float_c

    ! Set double values at particular (one-dimensional) indices.
    module function wrf_hydro_set_at_indices_double_c(name, inds, src) &
         result(bmi_status)  &
         bind(c, name="set_value_at_indices_double")
      character(c_char), intent(in) :: name(*)
      integer(c_int), intent(in) :: inds(*)
      real(c_double), intent(in) :: src(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_set_at_indices_double_c

    ! Get number of dimensions of the computational grid.
    module function wrf_hydro_grid_rank_c(grid, rank) result(bmi_status) &
      bind(c, name="get_grid_rank")
      integer(c_int), intent(in) :: grid
      integer(c_int), intent(out) :: rank
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_rank_c

    ! Get the total number of elements in the computational grid.
    module function wrf_hydro_grid_size_c(grid, size) result(bmi_status) &
      bind(c, name="get_grid_size")
      integer(c_int), intent(in) :: grid
      integer(c_int), intent(out) :: size
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_size_c

    ! Get the grid type as a string.
    module function wrf_hydro_grid_type_c(grid, type) result(bmi_status) &
      bind(c, name="get_grid_type")
      integer(c_int), intent(in) :: grid
      character(c_char), intent(out) :: type(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_type_c

    ! Get the dimensions of the computational grid.
    module function wrf_hydro_grid_shape_c(grid, shape) result(bmi_status) &
      bind(c, name="get_grid_shape")
      integer(c_int), intent(in) :: grid
      integer(c_int), intent(out) :: shape(*)
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_shape_c

    ! Get distance between nodes of the computational grid.
    module function wrf_hydro_grid_spacing_c(grid, spacing) &
         result(bmi_status) &
         bind(c, name="get_grid_spacing")
      integer(c_int), intent(in) :: grid
      real(c_double), dimension(:), intent(out) :: spacing
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_spacing_c

    ! Get coordinates of the origin of the computational grid.
    module function wrf_hydro_grid_origin_c(grid, origin) result(bmi_status) &
      bind(c, name="get_grid_origin")
      integer(c_int), intent(in) :: grid
      real(c_double), dimension(:), intent(out) :: origin
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_origin_c

    ! Get the x-coordinates of the nodes of a computational grid.
    module function wrf_hydro_grid_x_c(grid, x) result(bmi_status) &
      bind(c, name="get_grix_x")
      integer(c_int), intent(in) :: grid
      real(c_double), dimension(:), intent(out) :: x
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_x_c

    ! Get the y-coordinates of the nodes of a computational grid.
    module function wrf_hydro_grid_y_c(grid, y) result(bmi_status) &
      bind(c, name="get_grid_y")
      integer(c_int), intent(in) :: grid
      real(c_double), dimension(:), intent(out) :: y
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_y_c

    ! Get the z-coordinates of the nodes of a computational grid.
    module function wrf_hydro_grid_z_c(grid, z) result(bmi_status) &
      bind(c, name="get_grid_z")
      integer(c_int), intent(in) :: grid
      real(c_double), dimension(:), intent(out) :: z
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_z_c

    ! Get the number of nodes in an unstructured grid.
    module function wrf_hydro_grid_node_count_c(grid, count) &
         result(bmi_status) &
         bind(c, name="get_grid_node_count")
      integer(c_int), intent(in) :: grid
      integer(c_int), intent(out) :: count
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_node_count_c

    ! Get the number of edges in an unstructured grid.
    module function wrf_hydro_grid_edge_count_c(grid, count) &
         result(bmi_status) &
         bind(c, name="get_grid_edge_count")
      integer(c_int), intent(in) :: grid
      integer(c_int), intent(out) :: count
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_edge_count_c

    ! Get the number of faces in an unstructured grid.
    module function wrf_hydro_grid_face_count_c(grid, count) result(bmi_status) &
      bind(c, name="get_grid_face_count")
      integer(c_int), intent(in) :: grid
      integer(c_int), intent(out) :: count
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_face_count_c

    ! Get the edge-node connectivity.
    module function wrf_hydro_grid_edge_nodes_c(grid, edge_nodes) &
         result(bmi_status) &
         bind(c, name="get_grid_edge_nodes")
      integer(c_int), intent(in) :: grid
      integer(c_int), dimension(:), intent(out) :: edge_nodes
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_edge_nodes_c

    ! Get the face-edge connectivity.
    module function wrf_hydro_grid_face_edges_c(grid, face_edges) &
         result(bmi_status) &
         bind(c, name="get_grid_face_edges")
      integer(c_int), intent(in) :: grid
      integer(c_int), dimension(:), intent(out) :: face_edges
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_face_edges_c

    ! Get the face-node connectivity.
    module function wrf_hydro_grid_face_nodes_c(grid, face_nodes) &
         result(bmi_status) &
         bind(c, name="get_grid_face_nodes")
      integer(c_int), intent(in) :: grid
      integer(c_int), dimension(:), intent(out) :: face_nodes
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_face_nodes_c

    ! Get the number of nodes for each face.
    module function wrf_hydro_grid_nodes_per_face_c(grid, nodes_per_face) &
         result(bmi_status) &
         bind(c, name="get_grid_nodes_per_face")
      integer(c_int), intent(in) :: grid
      integer(c_int), dimension(:), intent(out) :: nodes_per_face
      integer(c_int) :: bmi_status
    end function wrf_hydro_grid_nodes_per_face_c

    ! ------------------------------------
    ! Non-BMI procedures
    ! ------------------------------------

    ! Model introspection.
    module subroutine print_model_info_c()
    end subroutine print_model_info_c

    ! Check the status and update bmi_status if necessary
    module subroutine stat_check_c(status, bmi_status)
      integer(c_int), intent(in) :: status
      integer(c_int), intent(inout) :: bmi_status
    end subroutine stat_check_c
  end interface
end module bmi_wrf_hydro_nwm_bind_c_mod
