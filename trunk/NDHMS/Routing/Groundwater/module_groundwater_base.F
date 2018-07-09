module module_groundwater_base

    type, abstract ::  groundwater_base_state_struct

    end type

    type, abstract ::  groundwater_base_properties_struct

    end type

    type, abstract :: groundwater_base_basin_struct

    end type

    type, abstract :: groundwater_base_input_struct

    end type

    type, abstract :: groundwater_base_output_struct

    end type

    type, abstract :: groundwater_base_static_struct

    end type

    type, abstract :: groundwater_base_data_struct

        contains

        procedure (run_groundwater_time_step_interface), deferred ::run_timestep

    end type

    abstract interface
        subroutine run_groundwater_time_step_interface(ground_water_data)
            import :: groundwater_base_data_struct
            class(groundwater_base_data_struct), intent(inout) :: ground_water_data
        end subroutine
    end interface

end module module_groundwater_base
