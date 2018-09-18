!> @brief simple data module for groundwater
!!
!! Contains the data structures and code used to implement and exponential bucket model for simulation of groundwater
!! in simulations that use the simple scheme

module module_simple_groundwater_data
    use module_simple_state
    use module_simple_basin
    use module_simple_properties
    use module_groundwater_static_data
    use module_groundwater_base
    implicit none

    !> data type that hold necessary data for exponential bucket groundwater model

    type, extends(groundwater_base_data_struct) :: simple_groundwater_struct

        type (simple_state_interface), pointer :: state => null()       !< structure to hold state variables
        type (simple_properties_interface), pointer :: properties => null() !< structure to hold properties
        type (simple_basin_interface), pointer :: basin => null()           !< structure to hold basin interface variables
        type (groundwater_static_struct), pointer :: static_data => null()  !< structure to hold static variables

        logical, private :: pointer_allocation_guard = .false.
        !integer :: gnumlinks

    contains

        procedure :: init => simple_groundwater_struct_init
        procedure :: destroy => simple_groundwater_struct_destroy
        procedure :: run_timestep => run_simple_groundwater_timestep
    end type simple_groundwater_struct

contains

    !> Constructor for the simple groundwater struct.
    !! @param this  The simple ground water object being constructed
    !! @param ix  The x dimension of the lsm grid
    !! @param jx  The y dimension of the lsm grid
    !! @param ixrt  The x dimension of the routing grid
    !! @param jxrt  The y dimensions of the routing grid
    !! @param dt  The size of the time step
    !! @param gwbaseswcrt  Groundwater configuration option: 0 = disabled, 1 = exp bucket, 2 = passthrough.
    !! @param ovrtswcrt  Overland routing contributes to groundwater: 0 = off, 1 = on
    !! @param numbasins  The number of basins being modeled in this process
    !! @param gnumbasins  The global number of basins
    !! @param agg_factor  The aggregation factor between the lsm and routing domains
    !! @param cell_area  2D array containing the area of each routing cell
    !! @param cell_area_lsm  2D array contain the area of lsm cell

    subroutine simple_groundwater_struct_init(this, ix, jx, ixrt, jxrt, dt, gwbaseswcrt, ovrtswcrt, numbasins, gnumbasins, agg_factor, cell_area, cell_area_lsm)
        implicit none
        class (simple_groundwater_struct), intent(inout) :: this ! object being initialized
        integer, intent(in) :: ix
        integer, intent(in) :: jx
        integer, intent(in) :: ixrt
        integer, intent(in) :: jxrt
        real, intent(in) :: dt
        integer, intent(in) :: gwbaseswcrt
        integer, intent(in) :: ovrtswcrt
        integer, intent(in) :: numbasins
        integer, intent(in) :: gnumbasins
        integer, intent(in) :: agg_factor
        real, dimension(:,:), intent(in) :: cell_area
        real, dimension(:,:), intent(in) :: cell_area_lsm

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write and error
                write(0,*) "Failure to allocate groundwater input structure"
            else
                ! initialize the input structure
                call this%input%init(ix,jx,numbasins)
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write and error
                write(0,*) "Failure to allocate groundwater output structure"
            else
                ! initialize the output structure
                call this%output%init(ixrt,jxrt,numbasins)
            end if

            !try to allocate aggregation
            allocate ( this%aggregation )
            if ( .not. associated(this%aggregation) ) then
                ! if the output structure could not be created write and error
                write(0,*) "Failure to allocate groundwater aggregation structure"
            else
                ! initialize the output structure
                call this%aggregation%init(ixrt,jxrt,agg_factor)
            end if

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the input structure could not be created write and error
                write(0,*) "Failure to allocate groundwater state structure"
            else
                ! initialize the state structure
                call this%state%init(numbasins)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate properties
            allocate ( this%properties )
            if ( .not. associated(this%properties) ) then
                ! if the properties structure could not be created write and error
                write(0,*) "Failure to allocate groundwater properties structure"
            else
                ! initialize the properties structure
                call this%properties%init( ix, jx, ixrt, jxrt, numbasins, cell_area, cell_area_lsm)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate basin
            allocate ( this%basin )
            if ( .not. associated(this%basin) ) then
                ! if the basin structure could not be created write and error
                write(0,*) "Failure to allocate groundwater properties structure"
            else
                ! initialize the basin structure
                call this%basin%init(ix, jx, ixrt, jxrt, numbasins, gnumbasins)
            end if

            ! try to allocate static data
            allocate ( this%static_data )
            if ( .not. associated(this%static_data) ) then
                ! if the static data structure could not be created write and error
                write(0,*) "Failure to allocate groundwater properties structure"
            else
                ! initialize the static data structure
                call this%static_data%init(ix, jx, ixrt, jxrt, dt, gwbaseswcrt, ovrtswcrt)
            end if

            this%pointer_allocation_guard = .true.
        end if

    end subroutine simple_groundwater_struct_init

    !> Destructor for the simple groundwater struct. Calling this subroutine will deallocate all memory allocated to a
    !! nhd_ground_water structure. If one or more pointer where disassociated or deallocated from code outside this
    !! module they will not be deallocated and an error message will be displayed.
    !! @param this  The nhd groundwater struct being destroyed

    subroutine simple_groundwater_struct_destroy(this)
        implicit none
        class (simple_groundwater_struct), intent(inout) :: this ! object being destroyed
    end subroutine simple_groundwater_struct_destroy

    subroutine run_simple_groundwater_timestep(ground_water_data)
        implicit none
        class(simple_groundwater_struct), intent(inout) :: ground_water_data
        logical :: rv

    end subroutine

end module module_simple_groundwater_data
