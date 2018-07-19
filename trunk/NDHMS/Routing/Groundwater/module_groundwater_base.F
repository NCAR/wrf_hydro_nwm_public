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

    type, abstract :: groundwater_base_output_struct

    end type

    type, abstract :: groundwater_base_static_struct

    end type

    type, abstract :: groundwater_base_data_struct

        type(groundwater_base_input_struct), pointer :: input

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
    end subroutine groundwater_base_input_struct_destroy

end module module_groundwater_base
