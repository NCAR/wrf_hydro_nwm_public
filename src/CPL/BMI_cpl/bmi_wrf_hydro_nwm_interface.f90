module bmi_wrf_hydro_nwm_mod
  use bmif_2_0
  use orchestrator_base, only : orchestrator_
  use state_module, only: wrf_hydro_model => state_type
  use module_NWM_io_dict, only: numLdasVars, ldasMeta
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  logical, parameter :: PROCEED = .true.
  integer, parameter :: input_item_count = 2
  integer, parameter :: output_item_count = numLdasVars
  ! Two way of handeling input/output variable names
  character(len=BMI_MAX_VAR_NAME), parameter :: input_item_list(input_item_count) = &
    ["FOO", "BAR"]
  character(len=BMI_MAX_VAR_NAME), target :: output_item_list(output_item_count)
  type(ldasMeta) :: ldasOutDict

  type, extends(bmi) :: bmi_wrf_hydro_nwm
     private
     type (wrf_hydro_model) :: model
     type (orchestrator_) :: orchestrator
   contains
     procedure :: get_component_name => wrf_hydro_component_name
     procedure :: get_input_item_count => wrf_hydro_input_item_count
     procedure :: get_output_item_count => wrf_hydro_output_item_count
     procedure :: get_input_var_names => wrf_hydro_input_var_names
     procedure :: get_output_var_names => wrf_hydro_output_var_names
     procedure :: initialize => wrf_hydro_initialize
     procedure :: finalize => wrf_hydro_finalize
     procedure :: get_start_time => wrf_hydro_start_time
     procedure :: get_end_time => wrf_hydro_end_time
     procedure :: get_current_time => wrf_hydro_current_time
     procedure :: get_time_step => wrf_hydro_time_step
     procedure :: get_time_units => wrf_hydro_time_units
     procedure :: update => wrf_hydro_update
     procedure :: update_until => wrf_hydro_update_until
     procedure :: get_var_grid => wrf_hydro_var_grid
     procedure :: get_grid_type => wrf_hydro_grid_type
     procedure :: get_grid_rank => wrf_hydro_grid_rank
     procedure :: get_grid_shape => wrf_hydro_grid_shape
     procedure :: get_grid_size => wrf_hydro_grid_size
     procedure :: get_grid_spacing => wrf_hydro_grid_spacing
     procedure :: get_grid_origin => wrf_hydro_grid_origin
     procedure :: get_grid_x => wrf_hydro_grid_x
     procedure :: get_grid_y => wrf_hydro_grid_y
     procedure :: get_grid_z => wrf_hydro_grid_z
     procedure :: get_grid_node_count => wrf_hydro_grid_node_count
     procedure :: get_grid_edge_count => wrf_hydro_grid_edge_count
     procedure :: get_grid_face_count => wrf_hydro_grid_face_count
     procedure :: get_grid_edge_nodes => wrf_hydro_grid_edge_nodes
     procedure :: get_grid_face_edges => wrf_hydro_grid_face_edges
     procedure :: get_grid_face_nodes => wrf_hydro_grid_face_nodes
     procedure :: get_grid_nodes_per_face => wrf_hydro_grid_nodes_per_face
     procedure :: get_var_type => wrf_hydro_var_type
     procedure :: get_var_units => wrf_hydro_var_units
     procedure :: get_var_itemsize => wrf_hydro_var_itemsize
     procedure :: get_var_nbytes => wrf_hydro_var_nbytes
     procedure :: get_var_location => wrf_hydro_var_location
     procedure :: get_value_int => wrf_hydro_get_int
     procedure :: get_value_float => wrf_hydro_get_float
     procedure :: get_value_double => wrf_hydro_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
     procedure :: get_value_ptr_int => wrf_hydro_get_ptr_int
     procedure :: get_value_ptr_float => wrf_hydro_get_ptr_float
     procedure :: get_value_ptr_double => wrf_hydro_get_ptr_double
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_float, &
          get_value_ptr_double
     procedure :: get_value_at_indices_int => wrf_hydro_get_at_indices_int
     procedure :: get_value_at_indices_float => wrf_hydro_get_at_indices_float
     procedure :: get_value_at_indices_double => wrf_hydro_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => wrf_hydro_set_int
     procedure :: set_value_float => wrf_hydro_set_float
     procedure :: set_value_double => wrf_hydro_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
     procedure :: set_value_at_indices_int => wrf_hydro_set_at_indices_int
     procedure :: set_value_at_indices_float => wrf_hydro_set_at_indices_float
     procedure :: set_value_at_indices_double => wrf_hydro_set_at_indices_double
     generic :: set_value_at_indices => &
          set_value_at_indices_int, &
          set_value_at_indices_float, &
          set_value_at_indices_double
     ! non-BMI procedures
     procedure :: print_model_info

     ! parallel BMI procedures
     generic :: parallel_initialize => &
          parallel_initialize_mpi!, &
          ! parallel_initialize_mpif08
     procedure :: parallel_initialize_mpi
     ! procedure :: parallel_initialize_mpif08
     procedure :: get_grid_partition_size
     procedure :: get_grid_partition_range
     procedure :: get_grid_global_node_nr
     procedure :: get_grid_global_edge_nr
     procedure :: get_grid_global_face_nr
     procedure :: get_grid_node_partition
     procedure :: get_grid_edge_partition
     procedure :: get_grid_face_partition
     procedure :: get_grid_partition_node_count
     procedure :: get_grid_partition_edge_count
     procedure :: get_grid_partition_face_count

  end type bmi_wrf_hydro_nwm

  interface

    ! Perform startup tasks for the model.
    module function wrf_hydro_initialize(this, config_file) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(out) :: this
      character(len=*), intent(in) :: config_file
      integer :: bmi_status
    end function wrf_hydro_initialize

    ! Advance the model one time step.
    module function wrf_hydro_update(this) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      integer :: bmi_status
    end function wrf_hydro_update

    ! Advance the model until the given time.
    module function wrf_hydro_update_until(this, time) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      double precision, intent(in) :: time
      integer :: bmi_status
    end function wrf_hydro_update_until

    ! Perform teardown tasks for the model.
    module function wrf_hydro_finalize(this) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      integer :: bmi_status
    end function wrf_hydro_finalize

     ! Get the name of the model.
     module function wrf_hydro_component_name(this, name) result(bmi_status)
       class(bmi_wrf_hydro_nwm), intent(in) :: this
       character(len=*), pointer, intent(out) :: name
       integer :: bmi_status
     end function wrf_hydro_component_name

    ! Count a model's input variables.
    module function wrf_hydro_input_item_count(this, count) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(out) :: count
      integer :: bmi_status
    end function wrf_hydro_input_item_count

    ! Count a model's output variables.
    module function wrf_hydro_output_item_count(this, count) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(out) :: count
      integer :: bmi_status
    end function wrf_hydro_output_item_count

    ! List a model's input variables.
    module function wrf_hydro_input_var_names(this, names) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function wrf_hydro_input_var_names

    ! List a model's output variables.
    module function wrf_hydro_output_var_names(this, names) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function wrf_hydro_output_var_names

    ! Get the grid identifier for the given variable.
    module function wrf_hydro_var_grid(this, name, grid) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: grid
      integer :: bmi_status
    end function wrf_hydro_var_grid

    ! Get the data type of the given variable as a string.
    module function wrf_hydro_var_type(this, name, type) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: type
      integer :: bmi_status
    end function wrf_hydro_var_type

    ! Get the units of the given variable.
    module function wrf_hydro_var_units(this, name, units) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: units
      integer :: bmi_status
    end function wrf_hydro_var_units

    ! Get memory use per array element, in bytes.
    module function wrf_hydro_var_itemsize(this, name, size) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: size
      integer :: bmi_status
    end function wrf_hydro_var_itemsize

    ! Get size of the given variable, in bytes.
    module function wrf_hydro_var_nbytes(this, name, nbytes) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: nbytes
      integer :: bmi_status
    end function wrf_hydro_var_nbytes

    ! Describe where a variable is located: node, edge, or face.
    module function wrf_hydro_var_location(this, name, location) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: location
      integer :: bmi_status
    end function wrf_hydro_var_location

    ! Current time of the model.
    module function wrf_hydro_current_time(this, time) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function wrf_hydro_current_time

    ! Start time of the model.
    module function wrf_hydro_start_time(this, time) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function wrf_hydro_start_time

    ! End time of the model.
    module function wrf_hydro_end_time(this, time) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function wrf_hydro_end_time

    ! Time units of the model.
    module function wrf_hydro_time_units(this, units) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(out) :: units
      integer :: bmi_status
    end function wrf_hydro_time_units

    ! Time step of the model.
    module function wrf_hydro_time_step(this, time_step) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      double precision, intent(out) :: time_step
      integer :: bmi_status
    end function wrf_hydro_time_step

    ! Get a copy of values (flattened!) of the given integer variable.
    module function wrf_hydro_get_int(this, name, dest) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(inout) :: dest(:)
      integer :: bmi_status
    end function wrf_hydro_get_int

    ! Get a copy of values (flattened!) of the given real variable.
    module function wrf_hydro_get_float(this, name, dest) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      real, intent(inout) :: dest(:)
      integer :: bmi_status
    end function wrf_hydro_get_float

    ! Get a copy of values (flattened!) of the given double variable.
    module function wrf_hydro_get_double(this, name, dest) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      double precision, intent(inout) :: dest(:)
      integer :: bmi_status
    end function wrf_hydro_get_double

    ! Get a reference to the given integer variable.
    module function wrf_hydro_get_ptr_int(this, name, dest_ptr) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function wrf_hydro_get_ptr_int

    ! Get a reference to the given real variable.
    module function wrf_hydro_get_ptr_float(this, name, dest_ptr) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      real, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function wrf_hydro_get_ptr_float

    ! Get a reference to the given double variable.
    module function wrf_hydro_get_ptr_double(this, name, dest_ptr) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      double precision, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function wrf_hydro_get_ptr_double

    ! Get integer values at particular (one-dimensional) indices.
    module function wrf_hydro_get_at_indices_int(this, name, dest, inds) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function wrf_hydro_get_at_indices_int

    ! Get real values at particular (one-dimensional) indices.
    module function wrf_hydro_get_at_indices_float(this, name, dest, inds) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      real, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function wrf_hydro_get_at_indices_float

    ! Get double values at particular (one-dimensional) indices.
    module function wrf_hydro_get_at_indices_double(this, name, dest, inds) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      character(len=*), intent(in) :: name
      double precision, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function wrf_hydro_get_at_indices_double

    ! Set new values for an integer model variable.
    module function wrf_hydro_set_int(this, name, src) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: src(:)
      integer :: bmi_status
    end function wrf_hydro_set_int

    ! Set new values for a real model variable.
    module function wrf_hydro_set_float(this, name, src) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      character(len=*), intent(in) :: name
      real, intent(in) :: src(:)
      integer :: bmi_status
    end function wrf_hydro_set_float

    ! Set new values for a double model variable.
    module function wrf_hydro_set_double(this, name, src) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      character(len=*), intent(in) :: name
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    end function wrf_hydro_set_double

    ! Set integer values at particular (one-dimensional) indices.
    module function wrf_hydro_set_at_indices_int(this, name, inds, src) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      integer, intent(in) :: src(:)
      integer :: bmi_status
    end function wrf_hydro_set_at_indices_int

    ! Set real values at particular (one-dimensional) indices.
    module function wrf_hydro_set_at_indices_float(this, name, inds, src) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      real, intent(in) :: src(:)
      integer :: bmi_status
    end function wrf_hydro_set_at_indices_float

    ! Set double values at particular (one-dimensional) indices.
    module function wrf_hydro_set_at_indices_double(this, name, inds, src) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    end function wrf_hydro_set_at_indices_double

    ! Get number of dimensions of the computational grid.
    module function wrf_hydro_grid_rank(this, grid, rank) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: rank
      integer :: bmi_status
    end function wrf_hydro_grid_rank

    ! Get the total number of elements in the computational grid.
    module function wrf_hydro_grid_size(this, grid, size) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: size
      integer :: bmi_status
    end function wrf_hydro_grid_size

    ! Get the grid type as a string.
    module function wrf_hydro_grid_type(this, grid, type) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      character(len=*), intent(out) :: type
      integer :: bmi_status
    end function wrf_hydro_grid_type

    ! Get the dimensions of the computational grid.
    module function wrf_hydro_grid_shape(this, grid, shape) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out), allocatable :: shape
      integer :: bmi_status
    end function wrf_hydro_grid_shape

    ! Get distance between nodes of the computational grid.
    module function wrf_hydro_grid_spacing(this, grid, spacing) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: spacing
      integer :: bmi_status
    end function wrf_hydro_grid_spacing

    ! Get coordinates of the origin of the computational grid.
    module function wrf_hydro_grid_origin(this, grid, origin) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: origin
      integer :: bmi_status
    end function wrf_hydro_grid_origin

    ! Get the x-coordinates of the nodes of a computational grid.
    module function wrf_hydro_grid_x(this, grid, x) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out), allocatable :: x
      integer :: bmi_status
    end function wrf_hydro_grid_x

    ! Get the y-coordinates of the nodes of a computational grid.
    module function wrf_hydro_grid_y(this, grid, y) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out), allocatable :: y
      integer :: bmi_status
    end function wrf_hydro_grid_y

    ! Get the z-coordinates of the nodes of a computational grid.
    module function wrf_hydro_grid_z(this, grid, z) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: z
      integer :: bmi_status
    end function wrf_hydro_grid_z

    ! Get the number of nodes in an unstructured grid.
    module function wrf_hydro_grid_node_count(this, grid, count) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function wrf_hydro_grid_node_count

    ! Get the number of edges in an unstructured grid.
    module function wrf_hydro_grid_edge_count(this, grid, count) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function wrf_hydro_grid_edge_count

    ! Get the number of faces in an unstructured grid.
    module function wrf_hydro_grid_face_count(this, grid, count) result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function wrf_hydro_grid_face_count

    ! Get the edge-node connectivity.
    module function wrf_hydro_grid_edge_nodes(this, grid, edge_nodes) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: edge_nodes
      integer :: bmi_status
    end function wrf_hydro_grid_edge_nodes

    ! Get the face-edge connectivity.
    module function wrf_hydro_grid_face_edges(this, grid, face_edges) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_edges
      integer :: bmi_status
    end function wrf_hydro_grid_face_edges

    ! Get the face-node connectivity.
    module function wrf_hydro_grid_face_nodes(this, grid, face_nodes) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_nodes
      integer :: bmi_status
    end function wrf_hydro_grid_face_nodes

    ! Get the number of nodes for each face.
    module function wrf_hydro_grid_nodes_per_face(this, grid, nodes_per_face) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: nodes_per_face
      integer :: bmi_status
    end function wrf_hydro_grid_nodes_per_face

    ! ------------------------------------
    ! Parallel BMI procedures
    ! ------------------------------------
    module function parallel_initialize_mpi(this, comm) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: comm
      integer :: bmi_status
    end function parallel_initialize_mpi

    ! module function parallel_initialize_mpif08(this, comm) &
    !    result(bmi_status)
    !   use mpi_f08
    !   class(bmi_wrf_hydro_nwm), intent(in) :: this
    !   type(MPI_COMM), intent(in) :: comm
    !   integer :: bmi_status
    ! end function parallel_initialize_mpif08

    module function get_grid_partition_size(this, grid, size) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: size
      integer :: bmi_status
    end function get_grid_partition_size

    module function get_grid_partition_range(this, grid, start, end) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: start(:)
      integer, intent(out) :: end(:)
      integer :: bmi_status
    end function get_grid_partition_range

    module function get_grid_global_node_nr(this, grid, global_index) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: global_index(:)
      integer :: bmi_status
    end function get_grid_global_node_nr

    module function get_grid_global_edge_nr(this, grid, global_index) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: global_index(:)
      integer :: bmi_status
    end function get_grid_global_edge_nr

    module function get_grid_global_face_nr(this, grid, global_index) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: global_index(:)
      integer :: bmi_status
    end function get_grid_global_face_nr

    module function get_grid_node_partition(this, grid, partition) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: partition(:)
      integer :: bmi_status
    end function get_grid_node_partition

    module function get_grid_edge_partition(this, grid, partition) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: partition(:)
      integer :: bmi_status
    end function get_grid_edge_partition

    module function get_grid_face_partition(this, grid, partition) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: partition(:)
      integer :: bmi_status
    end function get_grid_face_partition

    module function get_grid_partition_node_count(this, grid, count) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count(:)
      integer :: bmi_status
    end function get_grid_partition_node_count

    module function get_grid_partition_edge_count(this, grid, count) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count(:)
      integer :: bmi_status
    end function get_grid_partition_edge_count

    module function get_grid_partition_face_count(this, grid, count) &
      result(bmi_status)
      class(bmi_wrf_hydro_nwm), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count(:)
      integer :: bmi_status
    end function get_grid_partition_face_count

    ! ------------------------------------
    ! Non-BMI procedures
    ! ------------------------------------

    ! Model introspection.
    module subroutine print_model_info(this)
      class (bmi_wrf_hydro_nwm), intent(in) :: this
    end subroutine print_model_info

    ! Check the status and update bmi_status if necessary
    module subroutine stat_check(bmi_status_in, bmi_status_out, advance)
      integer, intent(in) :: bmi_status_in
      integer, intent(out) :: bmi_status_out
      logical, intent(in), optional :: advance
    end subroutine stat_check
  end interface

  interface wrf_hydro_nwm
     module procedure constructor
  end interface wrf_hydro_nwm
contains
  function constructor() result(this)
    type(bmi_wrf_hydro_nwm) :: this
  end function constructor
end module bmi_wrf_hydro_nwm_mod
