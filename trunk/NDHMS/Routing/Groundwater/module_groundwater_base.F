module module_groundwater_base

    type, abstract ::  groundwater_base_state_struct

    end type

    type, abstract ::  groundwater_base_properties_struct

    end type

    type, abstract :: groundwater_base_basin_struct

    end type

    type :: groundwater_base_input_struct
        ! infiltration excess on LSM grid as updated by the LSM (passed from LSM to hydro)
        real, allocatable, dimension(:,:) :: infxsrt

        ! drainage from bottom of soil column (from LSM)
        real, allocatable, dimension(:,:) :: soldrain

        ! flow to GW bucket
        real, allocatable, dimension(:) :: qin_gwsubbas

    contains

        procedure :: init => groundwater_base_input_struct_init
        procedure :: destroy => groundwater_base_input_struct_destroy
    end type

    type :: groundwater_base_output_struct
        real, allocatable, dimension(:) :: qout_gwsubbas  ! flow from the GW bucket
        integer, allocatable, dimension(:,:) :: qinflowbase     ! inflow/baseflow from QW to stream network

    contains

        procedure :: init => groundwater_base_output_struct_init
        procedure :: destroy => groundwater_base_output_struct_destroy
    end type

    type :: groundwater_base_aggregation_struct
        real, allocatable, dimension(:,:) :: infxswgt

        integer :: agg_factor

    contains

        procedure :: init => groundwater_base_aggregation_struct_init
        procedure :: destroy => groundwater_base_aggregation_struct_destroy
    end type

    type, abstract :: groundwater_base_static_struct

    end type

    type, abstract :: groundwater_base_data_struct

        type(groundwater_base_input_struct), pointer :: input
        type(groundwater_base_output_struct), pointer :: output
        type(groundwater_base_aggregation_struct), pointer :: aggregation

    contains

        procedure (run_groundwater_time_step_interface), deferred ::run_timestep

    end type

    abstract interface
        subroutine run_groundwater_time_step_interface(ground_water_data)
            import :: groundwater_base_data_struct
            class(groundwater_base_data_struct), intent(inout) :: ground_water_data
        end subroutine
    end interface

contains

    !constructor for the ground water struct
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

    ! destructor for the ground water struct
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

    !constructor for the ground water struct
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

    ! destructor for the ground water struct
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

    ! create a groundwater aggregation object
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

    ! destroy a groundwater aggregation object
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
