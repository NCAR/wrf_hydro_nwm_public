module module_reservoir_data
    use module_reservoir_routing_base
    use module_levelpool_parameters

    implicit none


    type, extends(reservoir_base_data_struct) :: reservoir_struct


	type (levelpool_parameters_interface), pointer :: parameters => null()
	type (levelpool_interface), pointer :: levelpool => null()


    contains

	procedure :: init => reservoir_struct_init
	procedure :: destroy => reservoir_struct_destroy
	procedure :: run_release = run_reservoir_release

    end type reservoir_struct


contains

    !constructor for the reservoir struct
    !take out dynamic arguments to LP init
    subroutine reservoir_struct_init(this, water_elevation, routing_period, inflow_at_previous_timestep, lateral_inflow, area_of_reservoir, &
       	bottom_of_weir_elevation, weir_coeffecient, weir_length, orifice_elevation, orifice_coffecient, orifice_area, max_depth_of_reservoir, lake_number)
	
	implicit none
	class (reservoir_struct), intent(inout) :: this ! object being initialized


        !real, intent(INOUT) :: water_elevation       ! water elevation height (m)   DO WE WANT AN INOUT VARIABLE TO THIS STRUCTURE??
        real, intent(IN)    :: routing_period            ! routing period [s]
        !real, intent(IN)    :: inflow_at_previous_timestep       ! inflow at previous timestep (cms)
        !real, intent(IN)    :: inflow_at_current_timestep          ! inflow at current timestep (cms)
        !real, intent(OUT)   :: outflow_at_current_timestep          ! outflow at current timestep    CMS???
        !real, intent(IN)    :: lateral_inflow                   ! lateral inflow
        real, intent(IN)    :: area_of_reservoir                 ! area of reservoir (km^2)
        real, intent(IN)    :: bottom_of_weir_elevation          ! bottom of weir elevation
        real, intent(IN)    :: weir_coeffecient                  ! weir coeff.
        real, intent(IN)    :: weir_length                      ! weir length (m)
        real, intent(IN)    :: orifice_elevation                ! orifice elevation
        real, intent(IN)    :: orifice_coffecient                ! orifice coeff.
        real, intent(IN)    :: orifice_area                     ! orifice area (m^2)
        real, intent(IN)    :: max_depth_of_reservoir          ! max depth of reservoir before overtop (m)                     
        integer, intent(IN) :: lake_number                      ! lake number


	this%routing_period = routing_period
	this%area_of_reservoir = area_of_reservoir
	this%bottom_of_weir_elevation = bottom_of_weir_elevation
	this%weir_coeffecient = weir_coeffecient
	this%weir_length = weir_length
	this%orifice_elevation = orifice_elevation
	this%orifice_coffecient = orifice_coffecient
	this%orifice_area = orifice_area
	this%max_depth_of_reservoir = max_depth_of_reservoir
	this%lake_number = lake_number




	!constructor chaining
	!take out dynamic arguments to LP init
	call levelpool_parameters_init(water_elevation, this%routing_period, inflow_at_previous_timestep, lateral_inflow, this%area_of_reservoir, &
       	this%bottom_of_weir_elevation, this%weir_coeffecient, this%weir_length, this%orifice_elevation, this%orifice_coffecient, this%orifice_area, &
	this%max_depth_of_reservoir, this%lake_number)


    end subroutine reservoir_struct_init


    !destructor for the reservoir struct
    subroutine reservoir_struct_destroy(this)
	implicit none
	class (reservoir_struct), intent(inout) :: this ! object being destroyed
    end subroutine reservoir_struct_destroy


    subroutine run_reservoir_release(reservoir_data)
	implicit none
	class (reservoir_struct), intent(inout) :: reservoir_data

	
	!Choose LP/ML/P here or somewhere else??
	call reservoir_release()  !???


    end subroutine







