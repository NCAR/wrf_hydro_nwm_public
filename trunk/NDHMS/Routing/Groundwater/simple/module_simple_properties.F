module module_simple_properties
    use module_groundwater_base
    implicit none

    type, extends(groundwater_base_properties_struct) :: simple_properties_interface

        ! centerpoint distance to each neighbor on the routing grid (m)
        real, allocatable, dimension(:,:) :: cell_area

        ! centerpoint distance to each neighbor on the land surface grid (m)
        real, allocatable, dimension(:,:) :: cell_area_lsm

        ! Coefficient parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: gw_buck_coeff

        ! Exponent parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: gw_buck_exp

        ! Max water table parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: z_max

    contains

        procedure :: init => simple_properties_init
        procedure :: destroy => simple_properties_destroy
    end type simple_properties_interface

contains

    subroutine simple_properties_init(this,ix_lsm,jx_lsm,ix_rt,jx_rt,numbasins,cell_area,cell_area_lsm)
        implicit none
        class(simple_properties_interface), intent(inout) :: this ! the type object being initalized
        integer, intent(in) :: ix_rt
        integer, intent(in) :: jx_rt
        integer, intent(in) :: ix_lsm                     ! x grid size
        integer, intent(in) :: jx_lsm                     ! y grid size
        integer, intent(in) :: numbasins                  ! number of basins
        real, dimension(:,:), intent(in) :: cell_area
        real, dimension(:,:), intent(in) :: cell_area_lsm

        logical :: allocation_error = .false.

        ! allocate local properties

        if ( .not. allocated(this%cell_area) ) then
            allocate( this%cell_area(ix_rt,jx_rt) )
            this%cell_area = cell_area
        else
            allocation_error = .true.
        end if

        ! allocate distance_to_neighbor lsm, NOT on the routing grid
        if ( .not. allocated(this%cell_area_lsm) ) then
            allocate(this%cell_area_lsm(ix_lsm,jx_lsm))
            this%cell_area_lsm = cell_area_lsm
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

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of groundwater properties structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine simple_properties_init

    subroutine simple_properties_destroy(this)
        implicit none
        class(simple_properties_interface), intent(inout) :: this ! the type object being destroyed

        logical :: allocation_error = .false.

        ! deallocate local properties

        ! deallocate distance_to_neighbor lsm, NOT on the routing grid
        if (  allocated(this%cell_area_lsm) ) then
            deallocate(this%cell_area_lsm)
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

        if ( allocated(this%cell_area) ) then
            deallocate(this%cell_area)
        else
            allocation_error = .true.
        end if

        if ( allocated(this%cell_area_lsm) ) then
            deallocate(this%cell_area_lsm)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater properties structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine simple_properties_destroy

end module module_simple_properties
