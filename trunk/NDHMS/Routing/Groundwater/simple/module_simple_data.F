module module_simple_data
    use module_simple_state
    use module_simple_basin
    use module_simple_properties
    use module_groundwater_static_data
    use module_groundwater_base
    implicit none

    type, extends(groundwater_base_data_struct) :: simple_groundwater_struct

        type (simple_state_interface), pointer :: state => null()
        type (simple_properties_interface), pointer :: properties => null()
        type (simple_basin_interface), pointer :: basin => null()
        type (groundwater_static_struct), pointer :: static_data => null()

        logical :: pointer_allocation_guard = .false.
        !integer :: gnumlinks

    contains

        procedure :: init => simple_groundwater_struct_init
        procedure :: destroy => simple_groundwater_struct_destroy
        procedure :: run_timestep => run_simple_groundwater_timestep
    end type simple_groundwater_struct
! ------------------DEBUG---------------------!
!    integer :: g_num_links
! ------------------DEBUG---------------------!
contains

    !constructor for the ground water struct
    subroutine simple_groundwater_struct_init(this, ix, jx, ixrt, jxrt, dt, gwbaseswcrt, ovrtswcrt, numbasins, num_local_links, cb_only, agg_factor, cell_area, cell_area_lsm)
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
        integer, intent(in) :: num_local_links
        integer, intent(in) :: cb_only
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
                call this%basin%init(ix, jx, numbasins, num_local_links, cb_only, agg_factor)
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

    ! destructor for the ground water struct
    subroutine simple_groundwater_struct_destroy(this)
        implicit none
        class (simple_groundwater_struct), intent(inout) :: this ! object being destroyed
    end subroutine simple_groundwater_struct_destroy

    subroutine run_simple_groundwater_timestep(ground_water_data)
        implicit none
        class(nhd_groundwater_struct), intent(inout) :: ground_water_data
        logical :: rv

    end subroutine

end module module_nhd_data
