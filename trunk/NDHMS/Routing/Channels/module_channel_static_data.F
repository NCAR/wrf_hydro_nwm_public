module module_channel_static_data

    type channel_static_data_struct
        integer :: num_lakes             !< modeled number of lakes

        integer :: channel_option        !< selected channel option
        integer :: channel_only          !< flag for channel only routing option
        integer :: channel_bucket_only   !< flag for the channel bucket only routing option

        integer :: overland_routing      !< flag to enable disable overland routing physics
        integer :: subsurface_routing    !< flag to enable disable subsurface (vedos zone) physics

        integer :: cold_start            !< flag indicating if this run was a cold start or resume from restart file

        integer :: dt
        integer :: dtct
        integer :: dtrt_ch

    end type channel_static_data_struct

end module module_channel_static_data
