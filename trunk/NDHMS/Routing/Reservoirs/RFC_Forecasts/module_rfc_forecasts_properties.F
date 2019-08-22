! This module defines and instantiates objects
! for an rfc forecasts type reservoir's
! parameters/properties. Properties holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.

module module_rfc_forecasts_properties
    use module_reservoir_utilities, only: read_netcdf_lake_id, &
                                          read_persistence_netcdf_gage_id, &
                                          read_persistence_netcdf_real_2D_parameters, &
                                          handle_err
    use module_reservoir, only: reservoir_properties
    implicit none

    ! Extend/derive rfc forecasts properties from the abstract base
    ! type for reservoir properties.
    type, extends(reservoir_properties) :: rfc_forecasts_properties_interface
        integer :: lake_number                  ! lake number

    contains

        procedure :: init => rfc_forecasts_properties_init
        procedure :: destroy => rfc_forecasts_properties_destroy

    end type rfc_forecasts_properties_interface

contains

    ! RFC Forecasts Properties Constructor
    subroutine rfc_forecasts_properties_init(this, lake_number, rfc_forecasts_parameter_file)
        implicit none
        class(rfc_forecasts_properties_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in)          :: lake_number                  ! lake number
        character(len=*), intent(in) :: rfc_forecasts_parameter_file
        integer                      :: ncid, var_id, lake_id_index
        integer                      :: status                        ! status of reading NetCDF

        this%lake_number = lake_number

        ! Open RFC Reservoir Parameter NetCDF file
        status = nf90_open(path = rfc_forecasts_parameter_file, mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open rfc forecasts parameter file")

        ! Read relevant properties from Persistence Parameter NetCDF
        call read_netcdf_lake_id(ncid, lake_number, "lake_id", lake_id_index)

    end subroutine rfc_forecasts_properties_init

    ! RFC Forecasts Properties Destructor
    subroutine rfc_forecasts_properties_destroy(this)
        implicit none
        class(rfc_forecasts_properties_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine rfc_forecasts_properties_destroy

end module module_rfc_forecasts_properties
