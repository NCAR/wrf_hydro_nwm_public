
module module_hydrologic_base

    use module_channel_base

    !> @brief class that holds physical properties common to most hydrologic classes
    type, abstract, extends(channel_base_properties) :: hydrologic_base_properties

    end type hydrologic_base_properties

    !> @brief class that holds abstract parameters common to most hydrologic classes
    type, abstract, extends(channel_base_params) :: hydrologic_base_params

    end type hydrologic_base_params

    !> @breif class that hold state that is common to most hydrologic classes
    type, abstract, extends(channel_base_state) :: hydrologic_base_state

    end type hydrologic_base_state

    !> @brief class that holds inputs that are common to most hydrologic classes
    type, abstract, extends(channel_base_input) :: hydrologic_base_input

    end type hydrologic_base_input

    !> @brief classes that hold outputs that are common to most hydrologic classes
    type, abstract, extends(channel_base_output) :: hydrologic_base_output

    end type hydrologic_base_output

    !> @brief the base type of channel representations that use a hydrologic physics scheme
    type, abstract, extends(channel_base_struct) :: hydrologic_channel_base_struct

    end type hydrologic_channel_base_struct

end module module_hydrologic_base
