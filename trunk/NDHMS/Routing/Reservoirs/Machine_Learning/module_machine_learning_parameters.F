! This module defines and instantiates objects
! for a machine learning type reservoir's
! parameters/properties. Parameters holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.

module module_machine_learning_parameters

    use netcdf
    use module_reservoir_utilities
    use module_reservoir_base
    implicit none

    ! Extend/derive machine learning parameters from the abstract base
    ! struct for reservoir properties.
    type, extends(reservoir_base_properties_struct) :: machine_learning_parameters_interface

        real    :: min_storage                  ! minimum storage
        real    :: max_storage                  ! maximum storage
        real    :: min_release                  ! minimum release
        real    :: max_release                  ! maximum release
        integer :: model_id	                    ! machine learning model number
        integer :: model_index	                ! machine learning model index
        integer :: lake_number                  ! lake number
        real    :: reservoir_area               ! area of reservoir (km^2)

    contains

        procedure :: init => machine_learning_parameters_init
        procedure :: destroy => machine_learning_parameters_destroy

    end type machine_learning_parameters_interface

contains

    !Machine Learning Parameters Constructor

    !subroutine machine_learning_parameters_init(this, machine_learning_parameter_file, &
    !    reservoir_area, lake_number, number_of_unique_ml_models)
    subroutine machine_learning_parameters_init(this, reservoir_area, lake_number, number_of_unique_ml_models)
        implicit none
        class(machine_learning_parameters_interface), intent(inout) :: this ! the type object being initialized
        !character(len=256), intent(in) :: machine_learning_parameter_file
        real, intent(in)        :: reservoir_area           ! area of reservoir (km^2)
        integer, intent(in)     :: lake_number              ! lake number
        integer, intent(inout)  :: number_of_unique_ml_models
        integer                 :: ncid, var_id, lake_id_index
        integer                 :: status                   ! status of reading NetCDF

        ! Assign the values passed in to a particular machine learning reservoir
        ! parameter object's variables.
        this%reservoir_area = reservoir_area
        this%lake_number = lake_number

        ! Machine Learning Reservoir Parm NC file

        print *, 'before open reservoir parm'

        status = nf90_open(path = "ML_RESERVOIR_PARM_str.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !status = nf90_open(path = machine_learning_parameter_file, mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        if (status /= nf90_noerr) call handle_err(status, "ML_RESERVOIR_PARM.nc")

        print *, 'after open reservoir parm'

        ! Read relevant parameters from machine learning parameter NetCDF
        call read_machine_learning_netcdf_lake_id(ncid, lake_number, "lake_id", lake_id_index)
        call get_model_id(ncid, lake_id_index, number_of_unique_ml_models, this%model_id)
        call get_model_names(ncid, "model_name")
        !call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "maximum_release", this%max_release)
        !call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "minimum_release", this%min_release)
        !call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "maximum_storage", this%max_storage)
        !call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "minimum_storage", this%min_storage)

    end subroutine machine_learning_parameters_init


    !Machine Learning Parameters Destructor
    subroutine machine_learning_parameters_destroy(this)
        implicit none
        class(machine_learning_parameters_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine machine_learning_parameters_destroy

    ! Get the model id and determine the number of unique machine learning models
    subroutine get_model_id(ncid, lake_id_index, number_of_unique_ml_models, model_id)
        integer, intent(in)     :: ncid, lake_id_index
        integer, intent(inout)  :: number_of_unique_ml_models
        integer, intent(out)    :: model_id
        integer                 :: var_id, number_of_lakes, model_ids_index, unique_models_index
        integer                 :: status       ! status of reading NetCDF
        integer, allocatable, dimension (:) :: model_ids
        integer, allocatable, dimension (:) :: unique_models

        call read_machine_learning_netcdf_model_ids(ncid, "model_id", var_id, number_of_lakes)
        allocate(model_ids(number_of_lakes))
        allocate(unique_models(number_of_lakes))
        status = nf90_get_var(ncid, var_id, model_ids)
        if(status /= nf90_NoErr) call handle_err(status, "model_id")
        model_id = model_ids(lake_id_index)

        ! Loop through the model_ids array to determine the number of unique machine learning
        ! models, which is then returned to module_machine_learning. These lines are only
        ! executed for the first machine learning reservoir initialization.
        if (number_of_unique_ml_models == 0) then
            ! Find out how many unique models here
            do model_ids_index = 1, number_of_lakes
                do unique_models_index = 1, number_of_lakes
                    if (model_ids(model_ids_index) == unique_models(unique_models_index)) then
                        exit
                    else if (unique_models(unique_models_index) == 0) then
                        unique_models(unique_models_index) = model_ids(model_ids_index)
                        exit
                    end if
                end do
            end do

            ! Loop through the unique_models array to count the number of unique machine
            ! learning models.
            do unique_models_index = 1, number_of_lakes
                if (unique_models(unique_models_index) == 0) then
                    exit
                else
                    number_of_unique_ml_models = number_of_unique_ml_models + 1
                end if
            end do
        end if

        if(allocated(model_ids)) deallocate(model_ids)
        if(allocated(unique_models)) deallocate(unique_models)

    end subroutine get_model_id


end module module_machine_learning_parameters
