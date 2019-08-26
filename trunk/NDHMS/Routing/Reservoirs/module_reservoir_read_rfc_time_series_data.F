module module_reservoir_read_rfc_time_series_data
    use netcdf
    implicit none

    ! The time_series_data is a singleton in that the object is instantiated only once per
    ! processor by the first reservoir on that processor that needs it. The object is then used
    ! by every other reservoir on that processor that needs it.
    type :: time_series_data_type

        logical :: initialized = .FALSE.

    contains

        procedure :: init => time_series_data_init
        procedure :: destroy => time_series_data_destroy

    end type time_series_data_type

    type (time_series_data_type) :: time_series_data

contains

   ! Time Series Data Type Constructor
    subroutine time_series_data_init(this)
        implicit none
        class(time_series_data_type), intent(inout) :: this ! object being initialized

        ! Return from subroutine if this singleton is already initialized
        if (this%initialized) return
        this%initialized = .true.

    end subroutine time_series_data_init


    ! Time Series Data Type Destructor
    subroutine time_series_data_destroy(this)

        implicit none
        class(time_series_data_type), intent(inout) :: this ! object being destroyed

    end subroutine time_series_data_destroy


end module module_reservoir_read_rfc_time_series_data
