!> @brief base classes for groundwater implementations
!!
!! This model contains virtual base classes for groundwater data structures
!! for properties, basin details, and state. In addition concrete base classes
!! are provided for input, output, and aggregation. The information in the concrete
!! classes is the information provided by the current model implementation and changing
!! this information would require modification of other parts of the model.

module module_groundwater_base

    !> Virtual base for groundwater state classes
    type, abstract ::  groundwater_base_state_struct

    end type

    !> Virtual base for groundwater properties classes
    type, abstract ::  groundwater_base_properties_struct

    end type

    !> Virtual base for groundwater basin classes
    type, abstract :: groundwater_base_basin_struct

    end type

    !>  Base for groundwater input classes.
    type :: groundwater_base_input_struct
        !> Infiltration excess on LSM grid as updated by the LSM (passed from LSM to hydro)
        real, allocatable, dimension(:,:) :: infxsrt

        !> Drainage from bottom of soil column (from LSM)
        real, allocatable, dimension(:,:) :: soldrain

        !> Flow to GW bucket (used for channel buckket only configuration)
        real, allocatable, dimension(:) :: qin_gwsubbas

    contains

        procedure :: init => groundwater_base_input_struct_init
        procedure :: destroy => groundwater_base_input_struct_destroy
    end type

    !> Base class for groundwater output classes
    type :: groundwater_base_output_struct
        real, allocatable, dimension(:) :: qout_gwsubbas  !< flow from the GW bucket
        real, allocatable, dimension(:,:) :: qinflowbase     !< inflow/baseflow from QW to stream network

    contains

        procedure :: init => groundwater_base_output_struct_init
        procedure :: destroy => groundwater_base_output_struct_destroy
    end type

    !> Base class for ground water aggregation classes
    type :: groundwater_base_aggregation_struct
        real, allocatable, dimension(:,:) :: infxswgt   !< Aggregation weights

        integer :: agg_factor !< The aggregation factor used to move from the LSM to routing domains

    contains

        procedure :: init => groundwater_base_aggregation_struct_init
        procedure :: destroy => groundwater_base_aggregation_struct_destroy
    end type

    !> Base class for ground water static classes
    type, abstract :: groundwater_base_static_struct

    end type

    !> Virutal base class for ground water data
    !!
    !! All ground water implementations must extend this class to create a data structure.

    type, abstract :: groundwater_base_data_struct

        type(groundwater_base_input_struct), pointer :: input  !< structure to hold inputs to module
        type(groundwater_base_output_struct), pointer :: output !< structure to hold outputs to module
        type(groundwater_base_aggregation_struct), pointer :: aggregation !< structure to hold aggregation data for module

    contains

        procedure (run_groundwater_time_step_interface), deferred ::run_timestep !< abstract procedure establishing the run_timestep method.

    end type

    !> Definition of run_groundwater_time_step_interface
    abstract interface
        subroutine run_groundwater_time_step_interface(ground_water_data)
            import :: groundwater_base_data_struct
            class(groundwater_base_data_struct), intent(inout) :: ground_water_data
        end subroutine
    end interface

contains

    !> Constructor for the groundwater base input structs
    !! @param this The groundwater base input struct being constructed
    !! @param ix The x size of the LSM data grids
    !! @param jx the y size of the LSM data grids
    !! @param numbasins The number of local basins
    subroutine groundwater_base_input_struct_init(this, ix, jx, numbasins)
        implicit none
        class (groundwater_base_input_struct), intent(inout) :: this ! object being initialized
        integer, intent(in) :: ix         ! number of rows in the lsm grid
        integer, intent(in) :: jx         ! number of cols in the lsm grid
        integer, intent(in) :: numbasins

        logical :: allocation_error = .false.

        ! allocate infxsrt
        if ( .not. allocated(this%infxsrt) ) then
            allocate(this%infxsrt(ix,jx))
            this%infxsrt = 0.0
        else
            allocation_error = .true.
        end if

        ! allocate soldrain
        if ( .not. allocated(this%soldrain) ) then
            allocate(this%soldrain(ix,jx))
            this%soldrain = 0.0
        else
            allocation_error = .true.
        end if

        ! allocate qin_gwsubbas
        if ( .not. allocated(this%qin_gwsubbas) ) then
            allocate(this%qin_gwsubbas(numbasins))
            this%qin_gwsubbas = 0.0
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of groundwater base input structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine groundwater_base_input_struct_init

    !> Destructor for the groundwater base input structs
    !! @param this The groundwater base input struct being destroyed

    subroutine groundwater_base_input_struct_destroy(this)
        implicit none
        class (groundwater_base_input_struct), intent(inout) :: this ! object being destroyed

        logical :: allocation_error = .false.

        ! deallocate infxsrt
        if (  allocated(this%infxsrt) ) then
            deallocate(this%infxsrt)
        else
            allocation_error = .true.
        end if

        ! deallocate soldrain
        if (  allocated(this%soldrain) ) then
            deallocate(this%soldrain)
        else
            allocation_error = .true.
        end if

        ! deallocate qin_gwsubbas
        if (  allocated(this%qin_gwsubbas) ) then
            deallocate(this%qin_gwsubbas)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater base input structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine groundwater_base_input_struct_destroy

    !> Constructor for the groundwater base output structs
    !! @param this The groundwater base input struct being constructed
    !! @param ixrt The x size of the routing data grids
    !! @param jxrt the y size of the routing data grids
    !! @param numbasins The number of local basins
    subroutine groundwater_base_output_struct_init(this,ixrt,jxrt, numbasins)
        implicit none
        class (groundwater_base_output_struct), intent(inout) :: this ! object being initialized
        integer, intent(in) :: ixrt         ! number of rows in the routing grid
        integer, intent(in) :: jxrt         ! number of cols in the routing grid
        integer, intent(in) :: numbasins

        logical :: allocation_error = .false.

        if ( .not. allocated(this%qout_gwsubbas) ) then
            allocate(this%qout_gwsubbas(numbasins))
            this%qout_gwsubbas = 0.0
        else
            allocation_error = .true.
        end if

        if ( .not. allocated(this%qinflowbase) ) then
            allocate(this%qinflowbase(ixrt,jxrt))
            this%qinflowbase = 0.0
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of groundwater base output structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine groundwater_base_output_struct_init

    !> Destructor for the groundwater base output structs
    !! @param this The groundwater base input struct being destroyed

    subroutine groundwater_base_output_struct_destroy(this)
        implicit none
        class (groundwater_base_output_struct), intent(inout) :: this ! object being destroyed

        logical :: allocation_error = .false.

        if ( allocated(this%qout_gwsubbas) ) then
            deallocate(this%qout_gwsubbas)
        else
            allocation_error = .true.
        end if

        if ( allocated(this%qinflowbase) ) then
            deallocate(this%qinflowbase)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater base output structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine groundwater_base_output_struct_destroy

    !> Constructor for the groundwater base aggregation structs
    !! @param this The groundwater base input struct being constructed
    !! @param ixrt The x size of the routing data grids
    !! @param jxrt the y size of the routing data grids
    !! @param agg_factor The aggregation factor used to move from the LSM to routing domains
    subroutine groundwater_base_aggregation_struct_init(this,ixrt,jxrt,agg_factor)
        implicit none
        class (groundwater_base_aggregation_struct), intent(inout) :: this ! object being initalized
        integer, intent(in) :: ixrt ! x size of routing grid
        integer, intent(in) :: jxrt ! y size of the routing grid
        integer, intent(in) :: agg_factor

        logical :: allocation_error = .false.

        this%agg_factor = agg_factor

        !allocate infxswgt
        if ( .not.  allocated(this%infxswgt)) then
            allocate( this%infxswgt(ixrt,jxrt) )
            this%infxswgt = 1./(this%agg_factor * this%agg_factor)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of groundwater base aggregation structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine groundwater_base_aggregation_struct_init

    !> Destructor for the groundwater base aggregation structs
    !! @param this The groundwater base input struct being destroyed
    subroutine groundwater_base_aggregation_struct_destroy(this)
        implicit none
        class (groundwater_base_aggregation_struct), intent(inout) :: this ! object being destroyed

        logical :: allocation_error = .false.

        if ( allocated(this%infxswgt) ) then
            deallocate(this%infxswgt)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater base aggregation structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine groundwater_base_aggregation_struct_destroy

end module module_groundwater_base
