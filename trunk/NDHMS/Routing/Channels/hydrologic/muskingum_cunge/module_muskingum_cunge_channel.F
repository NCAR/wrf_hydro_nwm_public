
module module_muskingum_cunge_channel

    use module_hydrologic_base

    type, extends(hydrologic_base_properties) :: muskingum_cunge_properties

    end type

    type, extends(hydrologic_base_params) :: muskingum_cunge_params
        real :: musk !< the nuskingum k paramater
        real :: musx !< the muskingum x parameter
    end type

    type, extends(hydrologic_channel_base_struct) :: muskingum_cunge_channel_struct

        type(channel_base_geometry), pointer :: my_geometry

    contains

        procedure :: init => muskingum_cunge_init
        procedure :: destroy => muskingum_cunge_destroy
        procedure :: run_timestep => muskingum_cunge_run_timestep

    end type muskingum_cunge_channel_struct

contains

    subroutine muskingum_cunge_init(this)
        implicit none
        class(muskingum_cunge_channel_struct), intent(inout) :: this

        this%geometry => this%my_geometry
    end subroutine muskingum_cunge_init

    subroutine muskingum_cunge_destroy(this)
        implicit none
        class(muskingum_cunge_channel_struct), intent(inout)  :: this
    end subroutine muskingum_cunge_destroy

    subroutine muskingum_cunge_run_timestep(this)
        implicit none
        class(muskingum_cunge_channel_struct), intent(inout)  :: this
    end subroutine muskingum_cunge_run_timestep

end module module_muskingum_cunge_channel
