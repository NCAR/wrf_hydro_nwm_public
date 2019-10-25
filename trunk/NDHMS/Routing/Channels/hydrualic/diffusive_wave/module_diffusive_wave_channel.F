
module module_diffusive_wave_channel

    use module_hydrualic_channel_base

    type, extends(hydraulic_channel_base_struct) :: diffusive_wave_channel_struct

    contains

        procedure :: init => diffusive_wave_init
        procedure :: destroy => diffusive_wave_destroy
        procedure :: run_timestep => diffusive_wave_run_timestep

    end type diffusive_wave_channel_struct

contains

    subroutine diffusive_wave_init(this)
        implicit none
        class(diffusive_wave_channel_struct), intent(inout) :: this
    end subroutine diffusive_wave_init

    subroutine diffusive_wave_destroy(this)
        implicit none
        class(diffusive_wave_channel_struct), intent(inout)  :: this
    end subroutine diffusive_wave_destroy

    subroutine diffusive_wave_run_timestep(this)
        implicit none
        class(diffusive_wave_channel_struct), intent(inout)  :: this
    end subroutine diffusive_wave_run_timestep

end module module_diffusive_wave_channel
