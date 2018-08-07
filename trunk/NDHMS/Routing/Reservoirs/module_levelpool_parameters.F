module module_levelpool_parameters

  use module_reservoir_routing_base
  implicit none
  
  type, extends(levelpool_parameters_struct) :: levelpool_parameters_interface
  
  
  contains
  
    procedure :: init => levelpool_parameters_init
    procedure :: destroy => levelpool_parameters_destroy
    
  end type levelpool_parameters_interface
  
  
  contains
  
    ! allocate levelpool parameters
    ! I will take out the dynamic parameters below
    subroutine levelpool_parameters_init(this, water_elevation, routing_period, inflow_at_previous_timestep, lateral_inflow, area_of_reservoir, &
       bottom_of_weir_elevation, weir_coeffecient, weir_length, orifice_elevation, orifice_coffecient, orifice_area, max_depth_of_reservoir, lake_number)   
    !pass in more parameters here??
        
        implicit none
        class(levelpool_parameters_interface), intent(inout) :: this ! the type object being initialized
    
    
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



    end subroutine levelpool_parameters_init
    
    
    subroutine levelpool_parameters_destroy(this)                          ! NEED THIS SUBROUTINE??
      implicit none    
      class(levelpool_parameters_interface), intent(inout) :: this ! the type object being destroyed
    
    
    
    end subroutine levelpool_parameters_destroy
    
    
end module module_levelpool_parameters
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
