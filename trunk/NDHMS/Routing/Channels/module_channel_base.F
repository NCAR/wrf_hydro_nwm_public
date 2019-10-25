!> @brief this module holds the data structures used to define the base class for the hierarchy of channel objects

module module_channel_base

    !> @brief class that holds data about the geometry of channel objects
    type :: channel_base_geometry

        real :: channel_length   !< the length of the channel or waterbody along the primary direction of flow
        real :: slope            !< the slope of channel
        real :: side_slope       !< the side slope of the channel
        real :: top_width        !< the width of channel at top bank
        real :: bottom_width     !< the width at the bottom of the channel

        integer :: node_area     !< area of a node
    contains
        procedure :: init => channel_base_geometry_init
        procedure :: destory => channel_base_geometry_destroy

    end type channel_base_geometry

    !> @breif class that holds state variables for a channel base object
    type channel_base_state
        real :: current_flow    !< the flow on the channel at the current timestep
        real :: prev_flow       !< the flow on the channel at the previous timestep
        real :: depth           !< current depth of the channel
        real :: retention_depth !< minimal depth of water for this channel
        real :: nudge          !< observed value to nudge flow closer to
    contains
        procedure :: init => channel_base_state_init
        procedure :: destory => channel_base_state_destroy
    end type


    !> @breif class that holds properties for a channel base object
    type channel_base_properties
        real :: mannings_num    !< the Mannings Number that approximates friction ect along the channel

        integer :: typel        !< the type of this channel (depreciated!)
        integer :: stream_order !< the steam order of this channel

        integer :: to_node      !< the destination node for this channel link
        integer :: from_node    !< the source of this channel link
    contains
        procedure :: init => channel_base_properties_init
        procedure :: destroy => channel_base_properties_init
    end type

    !> @breif class that holds parameter information for a base channels object
    type, abstract :: channel_base_params

    end type channel_base_params

    !> @breif class that holds input data needed by the physics code that processes channel base objects
    type channel_base_input
        double precision :: accumulated_surface_lateral_runnoff  !< accumulated surface runnoff for this link
        double precision :: accumulated_bucket_flux              !< accumulated groundwater flux for this link

        real :: subsurface_inflow !< subsurface inflow flow this link

    end type

    !> @breif class that holds output data produced by the physics code that processes channel base objects
    type channel_base_output
        real :: surface_lateral_flux  !< lateral surface flux from this link
        real :: bucket_flux           !< flux to groundwater from this link

        real :: stream_flow  !< flux along the link
    end type

    !> &brief class that is root of the hierarchy of channel objects. It holds data that is useful to most channel types
    !  and waterbody types
    type, abstract :: channel_base_struct

        type (channel_base_geometry), pointer :: geometry          !< structure that holds variables that describe channel geometry
        type (channel_base_state), pointer :: state           !< structure that holds the state of channel objects
        type (channel_base_properties), pointer :: properties !< structure that holds properties of the channel object
        type (channel_base_input), pointer :: input           !< structure that holds the input data for a channel object
        type (channel_base_output), pointer :: output         !< structure that holds the output data for a channel object

    contains

        procedure (run_channel_time_step_interface), deferred ::run_timestep !< abstract procedure establishing the run_timestep method.

    end type channel_base_struct

    !> Definition of run_groundwater_time_step_interface
    abstract interface
        subroutine run_channel_time_step_interface(this)
            import :: channel_base_struct
            class(channel_base_struct), intent(inout) :: this
        end subroutine
    end interface

contains

    !> @brief constructor for a channel base geometry object
    subroutine channel_base_geometry_init(this, channel_length, slope, side_slope, top_width, bottom_width)
        implicit none
        class(channel_base_geometry), intent(inout) :: this
        real, intent(in) :: channel_length
        real, intent(in) :: slope
        real, intent(in) :: side_slope
        real, intent(in) :: top_width
        real, intent(in) :: bottom_width

        this%channel_length = channel_length
        this%channel_length = channel_length
        this%slope = slope
        this%side_slope = side_slope
        this%top_width = top_width
        this%bottom_width = bottom_width

    end subroutine channel_base_geometry_init

    !> @brief destructor for a channel base geometry object
    subroutine channel_base_geometry_destroy(this)
        implicit none
        class(channel_base_geometry), intent(inout) :: this

        ! no dynamic allocations nothing to do
    end subroutine channel_base_geometry_destroy

    !> @brief constructor for a channel base state object
    subroutine channel_base_state_init(this, current_flow, prev_flow, depth, retention_depth, nudge)
        implicit none
        class(channel_base_state), intent(inout) :: this
        real, intent(in) :: current_flow
        real, intent(in) :: prev_flow
        real, intent(in) :: depth
        real, intent(in) :: retention_depth
        real, intent(in) :: nudge

        this%current_flow = current_flow
        this%prev_flow = prev_flow
        this%depth = depth
        this%retention_depth = retention_depth
        this%nudge = nudge

    end subroutine channel_base_state_init

    !> @brief destructor for a channel base state object
    subroutine channel_base_state_destroy(this)
        implicit none
        class(channel_base_state), intent(inout) :: this

        ! no dynamic allocations nothing to do
    end subroutine channel_base_state_destroy

    !> @brief constructor for a channel base properties object
    subroutine channel_base_properties_init(this, mannings_num, typel, stream_order, to_node, from_node)
        implicit none
        class(channel_base_properties), intent(inout) :: this
        real, intent(in) :: mannings_num
        integer, intent(in) :: typel
        integer, intent(in) :: stream_order
        integer, intent(in) :: to_node
        integer, intent(in) :: from_node

        this%mannings_num = mannings_num
        this%typel = typel
        this%stream_order = stream_order
        this%to_node = to_node
        this%from_node = from_node

    end subroutine channel_base_properties_init

    !> @brief destructor for a channel base properties object
    subroutine channel_base_properties_destroy(this)
        implicit none
        class(channel_base_properties), intent(inout) :: this

        ! no dynamic allocations nothing to do
    end subroutine channel_base_properties_destroy

end module module_channel_base
