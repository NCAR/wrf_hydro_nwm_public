!> @brief NHD data module for groundwater
!!
!! Contains the data structures and code used to implement and exponential bucket model for simulation of groundwater
!! in simulations that use the NHD stream network

module module_bucket_impl
    use module_bucket_state
    use module_bucket_properties
    use module_groundwater_static_data
    use module_groundwater_base
    implicit none

    !> base data type that hold necessary data for exponential bucket groundwater model

    type, abstract, extends(groundwater_base_data_struct) :: bucket_groundwater_struct

        !> sub structure to hold state data
        type (bucket_state_interface), pointer :: state => null()

        !> sub structure to hold property data
        type (bucket_properties_interface), pointer :: properties => null()

        !> sub structure to hold static data
        type (groundwater_static_struct), pointer :: static_data => null()

        logical, private :: bucket_pointer_allocation_guard = .false.
        !integer :: gnumlinks

    contains

        procedure :: loss => groundwater_bucket_loss_func

    end type bucket_groundwater_struct

contains

    !> Constructor for the nhd groundwater struct.
    !! @param this  The nhd ground water object being constructed
    !! @param ix  The x dimension of the lsm grid
    !! @param jx  The y dimension of the lsm grid
    !! @param ixrt  The x dimension of the routing grid
    !! @param jxrt  The y dimensions of the routing grid
    !! @param dt  The size of the time step
    !! @param gwbaseswcrt  Groundwater configuration option: 0 = disabled, 1 = exp bucket, 2 = passthrough.
    !! @param ovrtswcrt  Overland routing contributes to groundwater: 0 = off, 1 = on
    !! @param numbasins  The number of basins being modeled in this process
    !! @param agg_factor  The aggregation factor between the lsm and routing domains
    !! @param cell_area  2D array containing the area of each routing cell
    !! @param cell_area_lsm  2D array contain the area of lsm cell

    subroutine bucket_groundwater_struct_init(this, ix, jx, ixrt, jxrt, dt, gwbaseswcrt, ovrtswcrt, numbasins, agg_factor, cell_area, cell_area_lsm)
        implicit none
        class (bucket_groundwater_struct), intent(inout) :: this ! object being initialized
        integer, intent(in) :: ix
        integer, intent(in) :: jx
        integer, intent(in) :: ixrt
        integer, intent(in) :: jxrt
        real, intent(in) :: dt
        integer, intent(in) :: gwbaseswcrt
        integer, intent(in) :: ovrtswcrt
        integer, intent(in) :: numbasins
        integer, intent(in) :: agg_factor
        real, dimension(:,:), intent(in) :: cell_area
        real, dimension(:,:), intent(in) :: cell_area_lsm

        if (this%bucket_pointer_allocation_guard .eqv. .false. ) then
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

            ! try to allocate properties
            allocate ( this%properties )
            if ( .not. associated(this%properties) ) then
                ! if the properties structure could not be created write and error
                write(0,*) "Failure to allocate groundwater properties structure"
            else
                ! initialize the properties structure
                call this%properties%init( ix, jx, ixrt, jxrt, numbasins, cell_area, cell_area_lsm)
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

            this%bucket_pointer_allocation_guard = .true.
        end if

    end subroutine bucket_groundwater_struct_init

    !> Destructor for the nhd groundwater struct. Calling this subroutine will deallocate all memory allocated to a
    !! nhd_ground_water structure. If one or more pointer where disassociated or deallocated from code outside this
    !! module they will not be deallocated and an error message will be displayed.
    !! @param this  The nhd groundwater struct being destroyed

    subroutine bucket_groundwater_struct_destroy(this)
        implicit none
        class (bucket_groundwater_struct), intent(inout) :: this ! object being destroyed

        logical :: io_err = .false.

        ! pointer allocation guard should be set if data structures where properly initalized
        if (this%bucket_pointer_allocation_guard  ) then
            !check each pointer then deallocate

            ! check the input structure
            if ( associated(this%input)) then
                deallocate(this%input)
            else
                write(0,*) "Error the input structure the nhd data object was not associated"
                io_err = .true.
            end if

            ! check the output structure
            if ( associated(this%output)) then
                deallocate(this%output)
            else
                write(0,*) "Error the output structure the nhd data object was not associated"
                io_err = .true.
            end if

            ! check the aggregation structure
            if ( associated(this%aggregation)) then
                deallocate(this%aggregation)
            else
                write(0,*) "Error the aggregation structure the nhd data object was not associated"
                io_err = .true.
            end if

            ! check the state structure
            if ( associated(this%state)) then
                deallocate(this%state)
            else
                write(0,*) "Error the state structure the nhd data object was not associated"
                io_err = .true.
            end if

            ! check the properties structure
            if ( associated(this%properties)) then
                deallocate(this%properties)
            else
                write(0,*) "Error the properties structure the nhd data object was not associated"
                io_err = .true.
            end if

            ! check the static_data structure
            if ( associated(this%static_data)) then
                deallocate(this%static_data)
            else
                write(0,*) "Error the static_data structure the nhd data object was not associated"
                io_err = .true.
            end if

            this%bucket_pointer_allocation_guard = .false.
        end if

    end subroutine bucket_groundwater_struct_destroy

    subroutine groundwater_bucket_loss_func(this)
        class(bucket_groundwater_struct) :: this

    end subroutine

end module module_bucket_impl
