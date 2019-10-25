
module module_dynamic_wave_channel

    use module_hydrualic_channel_base

    type, extends(hydraulic_channel_base_struct) :: dynamic_wave_channel_struct

    contains

        procedure :: init => dynamic_wave_init
        procedure :: destroy => dynamic_wave_destroy
        procedure :: run_timestep => dynamic_wave_run_timestep

    end type dynamic_wave_channel_struct

contains

    subroutine dynamic_wave_init(this)
        implicit none
        class(dynamic_wave_channel_struct), intent(inout) :: this
    end subroutine dynamic_wave_init

    subroutine dynamic_wave_destroy(this)
        implicit none
        class(dynamic_wave_channel_struct), intent(inout)  :: this
    end subroutine dynamic_wave_destroy

    subroutine dynamic_wave_run_timestep(this)
        implicit none
        class(dynamic_wave_channel_struct), intent(inout)  :: this
    end subroutine dynamic_wave_run_timestep

end module module_dynamic_wave_channel
