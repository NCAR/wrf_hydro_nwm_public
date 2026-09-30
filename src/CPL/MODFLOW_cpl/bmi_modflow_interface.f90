module bmi_modflow_mod
  use bmif_2_0
  use mf6bmiUtil, only: BMI_LENCOMPONENTNAME
  use mf6bmiUtil, only: BMI_LENVARADDRESS
  use ConstantsModule, only: LENBOUNDNAME
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_int, c_char, &
       c_null_char, c_double
  implicit none

  integer, parameter :: input_item_count = 2
  integer, parameter :: output_item_count = 1
  ! Two way of handeling input/output variable names
  character(len=BMI_MAX_VAR_NAME), parameter :: input_item_list(input_item_count) = &
    ["FOO", "BAR"]
  character(len=BMI_MAX_VAR_NAME), target :: output_item_list(output_item_count)
  ! type(ldasMeta) :: ldasOutDict

  ! component names for get_modflow_var_address()
  character(len=:), allocatable :: f_component_name
  character(len=:), allocatable :: f_subcomponent_name
  character(c_char), allocatable :: c_component_name(:)
  character(c_char), allocatable :: c_subcomponent_name(:)

  type, extends(bmi) :: bmi_modflow
     private
     ! type (modflow_model) :: model
     ! type (orchestrator_) :: orchestrator
   contains
     procedure :: get_component_name => modflow_component_name
     procedure :: get_input_item_count => modflow_input_item_count
     procedure :: get_output_item_count => modflow_output_item_count
     procedure :: get_input_var_names => modflow_input_var_names
     procedure :: get_output_var_names => modflow_output_var_names
     procedure :: initialize => modflow_initialize
     procedure :: finalize => modflow_finalize
     procedure :: get_start_time => modflow_start_time
     procedure :: get_end_time => modflow_end_time
     procedure :: get_current_time => modflow_current_time
     procedure :: get_time_step => modflow_time_step
     procedure :: get_time_units => modflow_time_units
     procedure :: update => modflow_update
     procedure :: update_until => modflow_update_until
     procedure :: get_var_grid => modflow_var_grid
     procedure :: get_grid_type => modflow_grid_type
     procedure :: get_grid_rank => modflow_grid_rank
     procedure :: get_grid_shape => modflow_grid_shape
     procedure :: get_grid_size => modflow_grid_size
     procedure :: get_grid_spacing => modflow_grid_spacing
     procedure :: get_grid_origin => modflow_grid_origin
     procedure :: get_grid_x => modflow_grid_x
     procedure :: get_grid_y => modflow_grid_y
     procedure :: get_grid_z => modflow_grid_z
     procedure :: get_grid_flipped => modflow_grid_flipped
     procedure :: get_grid_node_count => modflow_grid_node_count
     procedure :: get_grid_edge_count => modflow_grid_edge_count
     procedure :: get_grid_face_count => modflow_grid_face_count
     procedure :: get_grid_edge_nodes => modflow_grid_edge_nodes
     procedure :: get_grid_face_edges => modflow_grid_face_edges
     procedure :: get_grid_face_nodes => modflow_grid_face_nodes
     procedure :: get_grid_nodes_per_face => modflow_grid_nodes_per_face
     procedure :: get_var_type => modflow_var_type
     procedure :: get_var_units => modflow_var_units
     procedure :: get_var_itemsize => modflow_var_itemsize
     procedure :: get_var_nbytes => modflow_var_nbytes
     procedure :: get_var_location => modflow_var_location
     procedure :: get_value_int => modflow_get_int
     procedure :: get_value_float => modflow_get_float
     procedure :: get_value_double => modflow_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
     procedure :: get_value_ptr_int => modflow_get_ptr_int
     procedure :: get_value_ptr_float => modflow_get_ptr_float
     procedure :: get_value_ptr_double => modflow_get_ptr_double
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_float, &
          get_value_ptr_double
     procedure :: get_value_at_indices_int => modflow_get_at_indices_int
     procedure :: get_value_at_indices_float => modflow_get_at_indices_float
     procedure :: get_value_at_indices_double => modflow_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => modflow_set_int
     procedure :: set_value_float => modflow_set_float
     procedure :: set_value_double => modflow_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
     procedure :: set_value_at_indices_int => modflow_set_at_indices_int
     procedure :: set_value_at_indices_float => modflow_set_at_indices_float
     procedure :: set_value_at_indices_double => modflow_set_at_indices_double
     generic :: set_value_at_indices => &
          set_value_at_indices_int, &
          set_value_at_indices_float, &
          set_value_at_indices_double
     ! non-BMI procedures
     ! procedure :: print_model_info
     procedure :: parallel_initialize
  end type bmi_modflow

  interface

    module function parallel_initialize(this, comm) &
      result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: comm
      integer :: bmi_status
    end function parallel_initialize

    ! Perform startup tasks for the model.
    module function modflow_initialize(this, config_file) result(bmi_status)
      class(bmi_modflow), intent(out) :: this
      character(len=*), intent(in) :: config_file
      integer :: bmi_status
    end function modflow_initialize

    ! Advance the model one time step.
    module function modflow_update(this) result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      integer :: bmi_status
    end function modflow_update

    ! Advance the model until the given time.
    module function modflow_update_until(this, time) result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      double precision, intent(in) :: time
      integer :: bmi_status
    end function modflow_update_until

    ! Perform teardown tasks for the model.
    module function modflow_finalize(this) result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      integer :: bmi_status
    end function modflow_finalize

     ! Get the name of the model.
     module function modflow_component_name(this, name) result(bmi_status)
       class(bmi_modflow), intent(in) :: this
       character(len=*), pointer, intent(out) :: name
       integer :: bmi_status
     end function modflow_component_name

    ! Count a model's input variables.
    module function modflow_input_item_count(this, count) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(out) :: count
      integer :: bmi_status
    end function modflow_input_item_count

    ! Count a model's output variables.
    module function modflow_output_item_count(this, count) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(out) :: count
      integer :: bmi_status
    end function modflow_output_item_count

    ! List a model's input variables.
    module function modflow_input_var_names(this, names) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function modflow_input_var_names

    ! List a model's output variables.
    module function modflow_output_var_names(this, names) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function modflow_output_var_names

    ! Get the grid identifier for the given variable.
    module function modflow_var_grid(this, name, grid) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: grid
      integer :: bmi_status
    end function modflow_var_grid

    ! Get the data type of the given variable as a string.
    module function modflow_var_type(this, name, type) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: type
      integer :: bmi_status
    end function modflow_var_type

    ! Get the units of the given variable.
    module function modflow_var_units(this, name, units) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: units
      integer :: bmi_status
    end function modflow_var_units

    ! Get memory use per array element, in bytes.
    module function modflow_var_itemsize(this, name, size) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: size
      integer :: bmi_status
    end function modflow_var_itemsize

    ! Get size of the given variable, in bytes.
    module function modflow_var_nbytes(this, name, nbytes) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: nbytes
      integer :: bmi_status
    end function modflow_var_nbytes

    ! Describe where a variable is located: node, edge, or face.
    module function modflow_var_location(this, name, location) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: location
      integer :: bmi_status
    end function modflow_var_location

    ! Current time of the model.
    module function modflow_current_time(this, time) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function modflow_current_time

    ! Start time of the model.
    module function modflow_start_time(this, time) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function modflow_start_time

    ! End time of the model.
    module function modflow_end_time(this, time) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function modflow_end_time

    ! Time units of the model.
    module function modflow_time_units(this, units) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(out) :: units
      integer :: bmi_status
    end function modflow_time_units

    ! Time step of the model.
    module function modflow_time_step(this, time_step) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      double precision, intent(out) :: time_step
      integer :: bmi_status
    end function modflow_time_step

    ! Get a copy of values (flattened!) of the given integer variable.
    module function modflow_get_int(this, name, dest) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(inout) :: dest(:)
      integer :: bmi_status
    end function modflow_get_int

    ! Get a copy of values (flattened!) of the given real variable.
    module function modflow_get_float(this, name, dest) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      real, intent(inout) :: dest(:)
      integer :: bmi_status
    end function modflow_get_float

    ! Get a copy of values (flattened!) of the given double variable.
    module function modflow_get_double(this, name, dest) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      double precision, intent(inout) :: dest(:)
      integer :: bmi_status
    end function modflow_get_double

    ! Get a reference to the given integer variable.
    module function modflow_get_ptr_int(this, name, dest_ptr) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function modflow_get_ptr_int

    ! Get a reference to the given real variable.
    module function modflow_get_ptr_float(this, name, dest_ptr) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      real, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function modflow_get_ptr_float

    ! Get a reference to the given double variable.
    module function modflow_get_ptr_double(this, name, dest_ptr) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      double precision, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function modflow_get_ptr_double

    ! Get integer values at particular (one-dimensional) indices.
    module function modflow_get_at_indices_int(this, name, dest, inds) &
      result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function modflow_get_at_indices_int

    ! Get real values at particular (one-dimensional) indices.
    module function modflow_get_at_indices_float(this, name, dest, inds) &
      result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      real, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function modflow_get_at_indices_float

    ! Get double values at particular (one-dimensional) indices.
    module function modflow_get_at_indices_double(this, name, dest, inds) &
      result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      double precision, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function modflow_get_at_indices_double

    ! Set new values for an integer model variable.
    module function modflow_set_int(this, name, src) result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: src(:)
      integer :: bmi_status
    end function modflow_set_int

    ! Set new values for a real model variable.
    module function modflow_set_float(this, name, src) result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      character(len=*), intent(in) :: name
      real, intent(in) :: src(:)
      integer :: bmi_status
    end function modflow_set_float

    ! Set new values for a double model variable.
    module function modflow_set_double(this, name, src) result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      character(len=*), intent(in) :: name
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    end function modflow_set_double

    ! Set integer values at particular (one-dimensional) indices.
    module function modflow_set_at_indices_int(this, name, inds, src) &
      result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      integer, intent(in) :: src(:)
      integer :: bmi_status
    end function modflow_set_at_indices_int

    ! Set real values at particular (one-dimensional) indices.
    module function modflow_set_at_indices_float(this, name, inds, src) &
      result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      real, intent(in) :: src(:)
      integer :: bmi_status
    end function modflow_set_at_indices_float

    ! Set double values at particular (one-dimensional) indices.
    module function modflow_set_at_indices_double(this, name, inds, src) &
      result(bmi_status)
      class(bmi_modflow), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    end function modflow_set_at_indices_double

    ! Get number of dimensions of the computational grid.
    module function modflow_grid_rank(this, grid, rank) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: rank
      integer :: bmi_status
    end function modflow_grid_rank

    ! Get the total number of elements in the computational grid.
    module function modflow_grid_size(this, grid, size) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: size
      integer :: bmi_status
    end function modflow_grid_size

    ! Get the grid type as a string.
    module function modflow_grid_type(this, grid, type) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      character(len=*), intent(out) :: type
      integer :: bmi_status
    end function modflow_grid_type

    ! Get the dimensions of the computational grid.
    module function modflow_grid_shape(this, grid, shape) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out), allocatable :: shape
      integer :: bmi_status
    end function modflow_grid_shape

    ! Get distance between nodes of the computational grid.
    module function modflow_grid_spacing(this, grid, spacing) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: spacing
      integer :: bmi_status
    end function modflow_grid_spacing

    ! Get coordinates of the origin of the computational grid.
    module function modflow_grid_origin(this, grid, origin) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: origin
      integer :: bmi_status
    end function modflow_grid_origin

    ! Get the x-coordinates of the nodes of a computational grid.
    module function modflow_grid_x(this, grid, x) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out), allocatable :: x
      integer :: bmi_status
    end function modflow_grid_x

    ! Get the y-coordinates of the nodes of a computational grid.
    module function modflow_grid_y(this, grid, y) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out), allocatable :: y
      integer :: bmi_status
    end function modflow_grid_y

    ! Get the z-coordinates of the nodes of a computational grid.
    module function modflow_grid_z(this, grid, z) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: z
      integer :: bmi_status
    end function modflow_grid_z

    ! Get the grid flipped when array orientation is opposite between two components.
    ! here betweeh MODFLOW and WRF-Hydro
    ! module function modflow_grid_flipped(this, name, src, dst) result(bmi_status)
    module function modflow_grid_flipped(this, name, dst) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      character(len=*), intent(in) :: name
      ! double precision, intent(in) :: src(:)
      double precision, dimension(:), intent(out), allocatable :: dst
      integer :: bmi_status
    end function modflow_grid_flipped

    ! Get the number of nodes in an unstructured grid.
    module function modflow_grid_node_count(this, grid, count) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function modflow_grid_node_count

    ! Get the number of edges in an unstructured grid.
    module function modflow_grid_edge_count(this, grid, count) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function modflow_grid_edge_count

    ! Get the number of faces in an unstructured grid.
    module function modflow_grid_face_count(this, grid, count) result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function modflow_grid_face_count

    ! Get the edge-node connectivity.
    module function modflow_grid_edge_nodes(this, grid, edge_nodes) &
      result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: edge_nodes
      integer :: bmi_status
    end function modflow_grid_edge_nodes

    ! Get the face-edge connectivity.
    module function modflow_grid_face_edges(this, grid, face_edges) &
      result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_edges
      integer :: bmi_status
    end function modflow_grid_face_edges

    ! Get the face-node connectivity.
    module function modflow_grid_face_nodes(this, grid, face_nodes) &
      result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_nodes
      integer :: bmi_status
    end function modflow_grid_face_nodes

    ! Get the number of nodes for each face.
    module function modflow_grid_nodes_per_face(this, grid, nodes_per_face) &
      result(bmi_status)
      class(bmi_modflow), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: nodes_per_face
      integer :: bmi_status
    end function modflow_grid_nodes_per_face

!     ! ------------------------------------
!     ! Non-BMI procedures
!     ! ------------------------------------

!     ! Model introspection.
!     module subroutine print_model_info(this)
!       class (bmi_modflow), intent(in) :: this
!     end subroutine print_model_info

!     ! Check the status and update bmi_status if necessary
    module subroutine stat_check(status, bmi_status)
      integer, intent(in) :: status
      integer, intent(inout) :: bmi_status
    end subroutine stat_check
  end interface

  interface modflow6
     module procedure constructor
  end interface modflow6
contains
  function constructor() result(this)
    type(bmi_modflow) :: this
  end function constructor
end module bmi_modflow_mod
