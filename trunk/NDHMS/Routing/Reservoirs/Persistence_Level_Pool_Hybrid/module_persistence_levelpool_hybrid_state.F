! This module defines and instantiates objects
! for a hybrid type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given hybrid
! reservoir object and not other modules or areas
! of the system.

#define seconds_in_day 86400

module module_persistence_levelpool_hybrid_state

    use module_levelpool
    use module_reservoir_base
    implicit none

    ! Extend/derive hybrid state from the abstract base
    ! struct for reservoir state.
    type, extends(reservoir_base_state_struct) :: hybrid_state_interface
        real    :: water_elevation              ! meters AMSL
        real*4  :: current_storage        ! cubic meters
        real    :: gage_discharge
        !integer :: timeslice_file_offset_seconds
        integer :: weight_update_time_interval
        integer :: weight_update_time
        integer :: timeslice_update_time_interval
        integer :: timeslice_update_time
        integer :: new_day_time
        integer :: current_time
        integer :: current_day
        integer :: persistence_weight_index
        real    :: levelpool_current_weight
        real    :: persistence_current_weight

        type (levelpool_struct), pointer :: levelpool_ptr   ! pointer to levelpool object

    contains

        procedure :: init => hybrid_state_init
        procedure :: destroy => hybrid_state_destroy

    end type hybrid_state_interface

contains

    !Hybrid State Constructor
    subroutine hybrid_state_init(this, water_elevation, lake_area, lake_max_water_elevation, orifice_elevation, initial_fractional_depth)
        implicit none
        class(hybrid_state_interface), intent(inout) :: this ! the type object being initialized
        real, intent(in) :: water_elevation        ! meters AMSL
        real, intent(in) :: lake_area              ! area of lake (km^2)
        real, intent(in) :: lake_max_water_elevation     ! max water elevation (meters)
        real, intent(in) :: orifice_elevation      ! orifice elevation (meters AMSL)
        real, intent(in) :: initial_fractional_depth


        ! Assign the water elevation value passed in to a particular hybrid reservoir
        ! state object's variable for water elevation
        !this%water_elevation = water_elevation


        print *, orifice_elevation, " + (( ", lake_max_water_elevation, " - ", orifice_elevation, " ) * ", initial_fractional_depth, ")"

        print *, orifice_elevation + ((lake_max_water_elevation - orifice_elevation) * initial_fractional_depth)

        this%water_elevation = orifice_elevation + ((lake_max_water_elevation - orifice_elevation) * initial_fractional_depth)






        this%gage_discharge = 0.0

        !OLD
        !this%previous_time_storage = water_elevation * lake_area * 1.0E6


        print *, 'STORAGE INIT'
print *, 'this%water_elevation'
print *, this%water_elevation
print *, 'orifice_elevation'
print *, orifice_elevation
print *, 'lake_area'
print *, lake_area
print *, 'lake_max_water_elevation'
print *, lake_max_water_elevation
print *, 'initial_fractional_depth'
print *, initial_fractional_depth


        this%current_storage = (this%water_elevation - orifice_elevation) * lake_area * 1.0E6

print *, 'this%current_storage'
print *, this%current_storage
print *, '------------------------------'
print *, '------------------------------'


        ! Initialize time variables
        this%weight_update_time_interval = seconds_in_day
        this%weight_update_time = 0
        !this%timeslice_update_time_interval = seconds_in_day
        !this%timeslice_update_time_interval = HUGE(0) ! set to this for forecast modes right now

        ! maybe figure a way to set to smthng else below
        this%timeslice_update_time_interval = 1e8 ! set to this for forecast modes right now
        this%timeslice_update_time = 0
        this%current_time = 0
        this%current_day = 0
        this%new_day_time = seconds_in_day
        this%persistence_weight_index = 0
        this%levelpool_current_weight = 0.0
        this%persistence_current_weight = 0.0


    end subroutine hybrid_state_init

    !Hybrid State Destructor
    subroutine hybrid_state_destroy(this)
        implicit none
        class(hybrid_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine hybrid_state_destroy

end module module_persistence_levelpool_hybrid_state
