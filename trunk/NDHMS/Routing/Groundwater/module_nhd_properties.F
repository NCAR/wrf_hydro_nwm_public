module module_nhd_properties
    use overland_data
    use module_groundwater_base
    implicit none

    type, extends(groundwater_base_properties_struct) :: nhd_properties_interface

        ! centerpoint distance to each neighbor on the routing grid (m)
        real, pointer, dimension(:,:,:) :: distance_to_neighbor => null()

        ! centerpoint distance to each neighbor on the land surface grid (m)
        real, allocatable, dimension(:,:,:) :: distance_to_neighbor_lsm

        ! Coefficient parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: gw_buck_coeff

        ! Exponent parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: gw_buck_exp

        ! Max water table parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: z_max

        logical :: pointers_need_set = .true.

    contains

        procedure :: init => nhd_properties_init
        procedure :: destroy => nhd_properties_destroy
    end type nhd_properties_interface

contains

    subroutine nhd_properties_init(this,ix_lsm,jx_lsm,numbasins,overland_data)
        implicit none
        class(nhd_properties_interface), intent(inout) :: this ! the type object being initalized
        integer, intent(in) :: ix_lsm                     ! x grid size
        integer, intent(in) :: jx_lsm                     ! y grid size
        integer, intent(in) :: numbasins                  ! number of basins
        class(overland_struct), intent(inout) :: overland_data

        logical :: allocation_error = .false.

        ! allocate local properties

        ! allocate distance_to_neighbor lsm, NOT on the routing grid
        if ( .not. allocated(this%distance_to_neighbor_lsm) ) then
            allocate(this%distance_to_neighbor_lsm(ix_lsm,jx_lsm,9))
            this%distance_to_neighbor_lsm = 0.0
        else
            allocation_error = .true.
        end if

        ! allocate the groundwater bucket coefficents
        if ( .not. allocated(this%gw_buck_coeff) ) then
            allocate(this%gw_buck_coeff(numbasins))
            this%gw_buck_coeff = 0.04
        else
            allocation_error = .true.
        end if

        ! allocate the groundwater bucket exponents
        if ( .not. allocated(this%gw_buck_exp) ) then
            allocate(this%gw_buck_exp(numbasins))
            this%gw_buck_exp = 0.2
        else
            allocation_error = .true.
        end if

        ! allocate the groundwater max water table parameter
        if ( .not. allocated(this%z_max) ) then
            allocate(this%z_max(numbasins))
            this%z_max = 0.1
        else
            allocation_error = .true.
        end if

        ! now set shared properties
        if ( .not. associated(overland_data%properties%distance_to_neighbor) .or. this%pointers_need_set) then
            this%distance_to_neighbor => overland_data%properties%distance_to_neighbor
        else
            allocation_error = .true.
        end if

        this%pointers_need_set = .false.

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of groundwater properties structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine nhd_properties_init

    subroutine nhd_properties_destroy(this)
        implicit none
        class(nhd_properties_interface), intent(inout) :: this ! the type object being destroyed

        logical :: allocation_error = .false.

        ! deallocate local properties

        ! deallocate distance_to_neighbor lsm, NOT on the routing grid
        if (  allocated(this%distance_to_neighbor_lsm) ) then
            deallocate(this%distance_to_neighbor_lsm)
        else
            allocation_error = .true.
        end if

        ! deallocate the groundwater bucket coefficents
        if (  allocated(this%gw_buck_coeff) ) then
            deallocate(this%gw_buck_coeff)
        else
            allocation_error = .true.
        end if

        ! deallocate the groundwater bucket exponents
        if ( allocated(this%gw_buck_exp) ) then
            deallocate(this%gw_buck_exp)
        else
            allocation_error = .true.
        end if

        ! deallocate the groundwater max water table parameter
        if ( allocated(this%z_max) ) then
            deallocate(this%z_max)
        else
            allocation_error = .true.
        end if

        ! now set shared properties
        if ( associated(this%distance_to_neighbor) ) then
            this%distance_to_neighbor => null()
        else
            allocation_error = .true.
        end if

        this%pointers_need_set = .true.

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater properties structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine nhd_properties_destroy

end module module_nhd_properties
