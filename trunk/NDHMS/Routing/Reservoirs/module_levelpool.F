module module_levelpool

    use module_levelpool_parameters
    use module_levelpool_state
    use module_reservoir_base


    implicit none


    type, extends(reservoir_base_struct) :: levelpool_struct

        !type (levelpool_parameters_interface), pointer :: parameters => null()
        !type (levelpool_state_interface), pointer :: state => null()

        type (levelpool_parameters_interface) :: parameters
        type (levelpool_state_interface) :: state





        logical :: pointer_allocation_guard = .false.


        contains

        procedure :: init => levelpool_struct_init
        procedure :: destroy => levelpool_struct_destroy
        procedure :: run_release => run_levelpool_release

    end type levelpool_struct


    contains



    !I don't need to pass in inflow, lat_in, prev_in, and outflow because they are derived from base input/output structs
    !Level Pool Constructor
    subroutine levelpool_struct_init(this, water_elevation,  &    !Need to check 2 different sources of routing period
        reservoir_area, weir_elevation, weir_coeffecient, &     !reservoir_area of reservoir maybe later a static param to reservoir??
        weir_length, orifice_elevation, orifice_coffecient, &
        orifice_area, max_depth, lake_number)   !Need to figure out connecting this with LP params

        implicit none
        class(levelpool_struct), intent(inout) :: this ! object being initialized
        real, intent(INOUT) :: water_elevation
        !real, intent(IN)    :: routing_period            ! routing period [s]
        real, intent(IN)    :: reservoir_area            ! area of reservoir (km^2)
        real, intent(IN)    :: weir_elevation            ! bottom of weir elevation
        real, intent(IN)    :: weir_coeffecient          ! weir coeff.
        real, intent(IN)    :: weir_length               ! weir length (m)
        real, intent(IN)    :: orifice_elevation         ! orifice elevation
        real, intent(IN)    :: orifice_coffecient        ! orifice coeff.
        real, intent(IN)    :: orifice_area              ! orifice area (m^2)
        real, intent(IN)    :: max_depth         ! max depth of reservoir before overtop (m)
        integer, intent(IN) :: lake_number               ! lake number



        if (this%pointer_allocation_guard .eqv. .false. ) then

            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write and error
                write(0,*) "Failure to allocate level pool input structure"
            else
                ! initialize the input structure
                call this%input%init()
            end if



            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write and error
                write(0,*) "Failure to allocate level pool output structure"
            else
                ! initialize the output structure
                call this%output%init()
            end if


            ! initialize levelpool parameters
            call this%parameters%init( reservoir_area, weir_elevation, weir_coeffecient, &
                weir_length, orifice_elevation, &
                orifice_coffecient, orifice_area, &
                max_depth, lake_number)



            ! initialize levelpool state
            call this%state%init( water_elevation )


            ! try to allocate levelpool parameters
            !allocate ( this%parameters )
            !if ( .not. associated(this%parameters) ) then
            !    if the parameters structure could not be created write an error
            !    write(0,*) "Failure to allocate levelpool parameters structure"
            !else
            !    ! initialize the parameters structure
            !    call this%parameters%init( reservoir_area, weir_elevation, weir_coeffecient, &
            !        weir_length, orifice_elevation, &
            !        orifice_coffecient, orifice_area, &
            !        max_depth, lake_number)
            !end if
            !this%pointer_allocation_guard = .true.



            ! try to allocate levelpool state
            !allocate ( this%state )
            !if ( .not. associated(this%state) ) then
            !     if the parameters structure could not be created write an error
            !    write(0,*) "Failure to allocate levelpool state structure"
            !else
            !    ! initialize the state structure
            !    call this%state%init( water_elevation )
            !end if
            !this%pointer_allocation_guard = .true.


        end if


    end subroutine levelpool_struct_init



    !Level Pool Destructor
    subroutine levelpool_struct_destroy(this)
        implicit none

        class(levelpool_struct), intent(inout) :: this ! object being destroyed

    end subroutine levelpool_struct_destroy




    subroutine run_levelpool_release(reservoir_data)   !Reservoir static data needed???
    !subroutine run_levelpool_release()   !Nothing passed
        implicit none
        class(levelpool_struct), intent(inout) :: reservoir_data
        !class(levelpool_struct)


        !I think I should call LP physics code here

        !first call to LP physics

        !second call to LP physics

    end subroutine run_levelpool_release















end module module_levelpool











