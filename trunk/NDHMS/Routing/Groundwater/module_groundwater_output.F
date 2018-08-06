module module_groundwater_output
    use module_groundwater_base
    implicit none

    type, extends(groundwater_base_output_struct) :: groundwater_output_struct

        real, allocatable, dimension(:) :: qout_gwsubbas  ! flow from the GW bucket
        integer, allocatable, dimension(:,:) :: qinflowbase     ! inflow/baseflow from QW to stream network

        contains

        procedure :: init => groundwater_output_struct_init
        procedure :: destroy => groundwater_output_struct_destroy
    end type groundwater_output_struct

    contains

    !constructor for the ground water struct
    subroutine groundwater_output_struct_init(this,ixrt,jxrt)
        implicit none
        class (groundwater_output_struct), intent(inout) :: this ! object being initialized
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

    end subroutine groundwater_output_struct_init

    ! destructor for the ground water struct
    subroutine groundwater_output_struct_destroy(this)
        implicit none
        class (groundwater_output_struct), intent(inout) :: this ! object being destroyed

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

    end subroutine groundwater_output_struct_destroy
end module module_groundwater_output
