! This module defines and instantiates objects
! for a rfc forecasts persistence levelpool type
! reservoir. The rfc forecasts reservoir type
! inherits input and output types from the
! reservoir base module and calls instantiation
! of these into sub-objects. The rfc forecasts
! reservoir type also points to types for
! rfc forecasts parameters and state and calls
! instantiation of these into sub-objects.
! There is also a subroutine to run rfc forecasts reservoir
! that is derived from the reservoir base
! type interface to run reservoir.

module module_rfc_forecasts

    use module_rfc_forecasts_parameters, only: rfc_forecasts_parameters_interface
    use module_rfc_forecasts_state, only: rfc_forecasts_state_interface
    use module_reservoir_base
    implicit none

    ! Extend/derive rfc forecasts type from the abstract base
    ! type for reservoirs.
    type, extends(reservoir_base_type) :: rfc_forecasts_type

        ! Define pointers to sub-types / sub-objects to and
        ! held by a level pool reservoir object.
        type (rfc_forecasts_parameters_interface), pointer :: parameters => null()
        type (rfc_forecasts_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => rfc_forecasts_type_init
        procedure :: destroy => rfc_forecasts_type_destroy
        procedure :: run => run_rfc_forecasts_reservoir

    end type rfc_forecasts_type

contains

    ! RFC Forecasts Constructor
    subroutine rfc_forecasts_type_init(this, water_elevation,  &
        lake_area, weir_elevation, weir_coeffecient, &
        weir_length, orifice_elevation, orifice_coefficient, &
        orifice_area, lake_max_water_elevation, initial_fractional_depth, &
        lake_number)
        implicit none
        class(rfc_forecasts_type), intent(inout) :: this ! object being initialized
        real,    intent(inout) :: water_elevation           ! meters AMSL
        real,    intent(in)    :: lake_area      		    ! area of lake (km^2)
        real,    intent(in)    :: weir_elevation            ! bottom of weir elevation (meters AMSL)
        real,    intent(in)    :: weir_coeffecient          ! weir coefficient
        real,    intent(in)    :: weir_length               ! weir length (meters)
        real,    intent(in)    :: orifice_elevation         ! orifice elevation (meters AMSL)
        real,    intent(in)    :: orifice_coefficient       ! orifice coefficient
        real,    intent(in)    :: orifice_area              ! orifice area (meters^2)
        real,    intent(in)    :: lake_max_water_elevation  ! max water elevation (meters)
        real,    intent(in)    :: initial_fractional_depth  ! initial fraction water depth
        integer, intent(in)    :: lake_number               ! lake number

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write an error
                write(0,*) "Failure to allocate rfc forecasts input structure"
            else
                ! initialize the input structure
                call this%input%init()
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write an error
                write(0,*) "Failure to allocate rfc forecasts output structure"
            else
                ! initialize the output structure
                call this%output%init()
            end if

            ! try to allocate parameters
            allocate ( this%parameters )
            if ( .not. associated(this%parameters) ) then
                ! if the parameters structure could not be created write an error
                write(0,*) "Failure to allocate rfc forecasts parameters structure"
            else
                ! initialize rfc forecasts parameters
                call this%parameters%init()
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created write an error
                write(0,*) "Failure to allocate rfc forecasts state structure"
            else
                ! initialize rfc forecasts state
                call this%state%init()
            end if
            this%pointer_allocation_guard = .true.

        end if

    end subroutine rfc_forecasts_type_init

    ! RFC Forecasts Destructor
    subroutine rfc_forecasts_type_destroy(this)
        implicit none
        class(rfc_forecasts_type), intent(inout) :: this ! object being destroyed
    end subroutine rfc_forecasts_type_destroy

    ! Subroutine for running reservoir for a rfc forecasts reservoir
    subroutine run_rfc_forecasts_reservoir(this, previous_timestep_inflow, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period)
        implicit none
        class(rfc_forecasts_type), intent(inout) :: this
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds

    end subroutine run_rfc_forecasts_reservoir

end module module_rfc_forecasts
