!> @brief bucket properties module for groundwater
!!
!! This module implements the data structures used to interact with the properties
!! in the nhd implementation of an exponential bucket groundwater model

module module_bucket_properties
    use module_groundwater_base
    implicit none

    !> type used to contain information used to interact with property data about the modeled basins in a
    !! implementation of an exponential bucket groundwater bucket model

    type, extends(groundwater_base_properties_struct) :: bucket_properties_interface

        !> the area of each element of the routing grid, units: m
        real, allocatable, dimension(:,:) :: cell_area

        !> the area of each element of the LSM grid, units: m
        real, allocatable, dimension(:,:) :: cell_area_lsm

        !> Coefficient parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: gw_buck_coeff

        !> Exponent parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: gw_buck_exp

        !> Max water table parameter in the GW exponential decay equation
        real, allocatable, dimension(:) :: z_max

    contains

        procedure :: init => bucket_properties_init
        procedure :: destroy => bucket_properties_destroy
    end type bucket_properties_interface

contains

    !> Constructor for the bucket properties structure
    !! @param this The bucket properties structure being allocated
    !! @param ix_lsm The size of the x dimension of the lsm grids
    !! @param jx_lsm The size of the y dimension of the lsm grids
    !! @param ix_rt The size of the x dimension of the routing grids
    !! @param jx_rt The size of the y dimension of the routing grids
    !! @param numbasins The number of basins being modeled in this process
    !! @param cell_area 2D array containing the area of each element of the local routing grid
    !! @param cell_area_lsm 2D array contain the area of each element of the local LSM grid

    subroutine bucket_properties_init(this,ix_lsm,jx_lsm,ix_rt,jx_rt,numbasins,cell_area,cell_area_lsm)
        implicit none
        class(bucket_properties_interface), intent(inout) :: this ! the type object being initalized
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

    end subroutine bucket_properties_init

    !> Destructor for the nhd properties structure
    !! @param this The nhd properties structure being destroyed

    subroutine bucket_properties_destroy(this)
        implicit none
        class(bucket_properties_interface), intent(inout) :: this ! the type object being destroyed

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

    end subroutine bucket_properties_destroy

end module module_bucket_properties
