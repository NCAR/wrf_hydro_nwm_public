module module_nhd_data
    use module_nhd_state
    use module_nhd_basin
    use module_nhd_properties
    use module_groundwater_output
    use module_groundwater_input
    use module_groundwater_static_data
    use module_groundwater_base
    implicit none

    type, extends(groundwater_base_data_struct) :: nhd_groundwater_struct

        type (nhd_state_interface), pointer :: state => null()
        type (nhd_properties_interface), pointer :: properties => null()
        type (nhd_basin_interface), pointer :: basin => null()
        type (groundwater_input_struct), pointer :: input => null()
        type (groundwater_output_struct), pointer :: output => null()
        type (groundwater_static_struct), pointer :: statuc => null()

    contains

        procedure :: init => nhd_groundwater_struct_init
        procedure :: destroy => nhd_groundwater_struct_destroy
        procedure :: run_timestep => run_nhd_groundwater_timestep
    end type nhd_groundwater_struct

contains

    !constructor for the ground water struct
    subroutine nhd_groundwater_struct_init(this)
        implicit none
        class (nhd_groundwater_struct), intent(inout) :: this ! object being initialized
    end subroutine nhd_groundwater_struct_init

    ! destructor for the ground water struct
    subroutine nhd_groundwater_struct_destroy(this)
        implicit none
        class (nhd_groundwater_struct), intent(inout) :: this ! object being destroyed
    end subroutine nhd_groundwater_struct_destroy

    subroutine run_nhd_groundwater_timestep(ground_water_data)
        class(nhd_groundwater_struct), intent(inout) :: ground_water_data


    end subroutine
end module module_nhd_data
