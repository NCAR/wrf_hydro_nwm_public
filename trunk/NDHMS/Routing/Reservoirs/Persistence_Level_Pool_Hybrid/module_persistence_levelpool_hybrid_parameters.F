! This module defines and instantiates objects
! for a hybrid type reservoir's
! parameters/properties. Parameters holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.
module module_persistence_levelpool_hybrid_parameters

    !use netcdf
    use module_reservoir_utilities
    use module_reservoir_base
    implicit none

    ! Extend/derive hybrid parameters from the abstract base
    ! struct for reservoir properties.
    type, extends(reservoir_base_properties_struct) :: hybrid_parameters_interface
        real    :: min_storage                  ! minimum storage (cubic meters)
        real    :: max_storage                  ! maximum storage (cubic meters)
        real    :: lake_area                    ! area of reservoir (km^2)
        real    :: orifice_elevation            ! orifice elevation (meters AMSL)
        integer :: lake_number                  ! lake number
        integer :: gage_id
        real, allocatable, dimension(:) :: persistence_weighted_coefficients

    contains

        procedure :: init => hybrid_parameters_init
        procedure :: destroy => hybrid_parameters_destroy

    end type hybrid_parameters_interface

contains

    ! Hybrid Parameters Constructor
    subroutine hybrid_parameters_init(this, lake_area, lake_max_water_elevation, orifice_elevation, lake_number, &
        gage_id, persistence_parameter_file)
        implicit none
        class(hybrid_parameters_interface), intent(inout) :: this ! the type object being initialized
        real, intent(in)    :: lake_area                    ! area of lake (km^2)
        real, intent(in)    :: lake_max_water_elevation     ! max water elevation (meters)
        real, intent(in)    :: orifice_elevation            ! orifice elevation (meters AMSL)
        integer, intent(in) :: lake_number                  ! lake number
        integer, intent(in) :: gage_id
        character(len=*), intent(in) :: persistence_parameter_file
        integer                 :: ncid, var_id, lake_id_index
        integer                 :: status                   ! status of reading NetCDF
        real, allocatable, dimension(:,:) :: temp_real_2D_array
        integer                 :: number_of_weights, number_of_lakes

        this%lake_area = lake_area

        this%orifice_elevation = orifice_elevation

        this%lake_number = lake_number

        this%gage_id = gage_id

        this%min_storage = 0.0

        !!DOUBLE CHECK THIS??
        this%max_storage = (lake_max_water_elevation - orifice_elevation) * lake_area * 1.0E6

        print *, 'lake_number'
        print *, lake_number

        print*, 'this%max_storage'
        print*, this%max_storage



print *, 'orifice_elevation'
print *, orifice_elevation
print *, 'lake_area'
print *, lake_area
print *, 'lake_max_water_elevation'
print *, lake_max_water_elevation



        ! Open Persistence Reservoir Parameter NetCDF file
        status = nf90_open(path = persistence_parameter_file, mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open persistence reservoir parameter file")

        ! Read relevant parameters from Persistence Parameter NetCDF
        call read_netcdf_lake_id(ncid, lake_number, "lake_id", lake_id_index)

        call read_persistence_netcdf_real_2D_parameters(ncid, lake_id_index, "persistence_coefficients", var_id, number_of_weights, number_of_lakes)

        allocate(this%persistence_weighted_coefficients(number_of_weights))


        allocate(temp_real_2D_array(number_of_weights, number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_real_2D_array)
        if (status /= nf90_noerr) call handle_err(status, "persistence_coefficients")

print *, 'temp_real_2D_array'
print *, temp_real_2D_array

        this%persistence_weighted_coefficients(:) = temp_real_2D_array(:, lake_id_index)

print *, 'this%persistence_weighted_coefficients'
print *, this%persistence_weighted_coefficients


        if(allocated(temp_real_2D_array)) deallocate(temp_real_2D_array)


    end subroutine hybrid_parameters_init

    ! Hybrid Parameters Destructor
    subroutine hybrid_parameters_destroy(this)
        implicit none
        class(hybrid_parameters_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine hybrid_parameters_destroy

end module module_persistence_levelpool_hybrid_parameters
