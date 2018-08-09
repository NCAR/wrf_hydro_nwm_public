module module_levelpool

    !use module_levelpool_parameters
    !use module_levelpool_state
    use module_reservoir_base


    implicit none


    type, extends(reservoir_base_struct) :: levelpool_struct

        !type (levelpool_parameters_interface), pointer :: parameters => null()
        !type (levelpool_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.


        contains

        !procedure :: init => levelpool_base_struct_init
        !procedure :: destroy => levelpool_base_struct_destroy
        procedure :: run_release => run_levelpool_release

    end type levelpool_struct


    contains


    subroutine run_levelpool_release(reservoir_data)   !Reservoir static data needed???
    !subroutine run_levelpool_release()   !Nothing passed
        implicit none
        class(levelpool_struct), intent(inout) :: reservoir_data
        !class(levelpool_struct)


        !I think I should call LP physics code here

    end subroutine run_levelpool_release














end module module_levelpool











