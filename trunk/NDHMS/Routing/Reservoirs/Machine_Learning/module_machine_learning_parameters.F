! This module defines and instantiates objects
! for a machine learning type reservoir's
! parameters/properties. Parameters holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.

module module_machine_learning_parameters

    use netcdf
    use module_reservoir_utilities
    !use module_machine_learning_utilities
    use module_reservoir_base
    !use module_io_utilities
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

        integer, allocatable, dimension (:) :: lake_id
        integer, allocatable, dimension (:) :: model_id_array
        integer, allocatable, dimension (:) :: unique_models_array
        real, allocatable, dimension (:)    :: minimum_storage
        real, allocatable, dimension (:)    :: maximum_storage
        real, allocatable, dimension (:)    :: minimum_release
        real, allocatable, dimension (:)    :: maximum_release

        integer :: ncid, var_id, lake_id_index, number_of_lakes
        integer :: status       ! Status of reading NetCDF
        integer :: idx_mod_id_arr, idx_unique_arr


        ! Assign the values passed in to a particular machine learning reservoir
        ! parameter object's variables.
		!this%model_id = model_id
		!this%model_index = model_index

        this%reservoir_area = reservoir_area

        this%lake_number = lake_number

        ! Machine Learning Reservoir Parm NC file
        ! LEAVE THIS HARD CODED????

        print *, 'before open reservoir parm'

        status = nf90_open(path = "ML_RESERVOIR_PARM.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !status = nf90_open(path = machine_learning_parameter_file, mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        if (status /= nf90_noerr) call handle_err(status, "ML_RESERVOIR_PARM.nc")

        print *, 'after open reservoir parm'


        call read_machine_learning_netcdf_lake_id(ncid, lake_number, "lake_id", lake_id_index)


        call read_machine_learning_netcdf_model_ids(ncid, "model_id", var_id, number_of_lakes)
        allocate(model_id_array(number_of_lakes))
        allocate(unique_models_array(number_of_lakes))
        status = nf90_get_var(ncid, var_id, model_id_array)
        if(status /= nf90_NoErr) call handle_err(status, "model_id")
        this%model_id = model_id_array(lake_id_index)

        call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "maximum_release", this%max_release)

        call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "minimum_release", this%min_release)

        call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "maximum_storage", this%max_storage)

        call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "minimum_storage", this%min_storage)

        call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "maximum_release", this%max_release)

        call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "maximum_release", this%max_release)

        call read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, "maximum_release", this%max_release)


        print *, '1.1'


print *, 'model_id_array'
print *, model_id_array
print *, 'unique_models_array'
print *, unique_models_array




        !CHECK RECORDINGS ABOUT BELOW. IF I NEED TO REMOVE ONE OF THE LINES?????

        if (number_of_unique_ml_models == 0) then

            ! Find out how many unique models here
            do idx_mod_id_arr = 1, number_of_lakes
                do idx_unique_arr = 1, number_of_lakes

                    if (model_id_array(idx_mod_id_arr) == unique_models_array(idx_unique_arr)) then
                        exit

                    else if (unique_models_array(idx_unique_arr) == 0) then
                        unique_models_array(idx_unique_arr) = model_id_array(idx_mod_id_arr)
                        exit
                    end if

                end do
            end do

                    print *, '1.2'

            !CHANGE ARRAY NAME TO UNIQUE MODELS
            !number_of_unique_ml_models = 0
            do idx_unique_arr = 1, number_of_lakes
                if (unique_models_array(idx_unique_arr) == 0) then
                    unique_models_array(idx_unique_arr) = model_id_array(idx_mod_id_arr)
                    exit

                else
                    number_of_unique_ml_models = number_of_unique_ml_models + 1
                end if
            end do
        end if

        print *, '1.3'

        if(allocated(model_id_array)) deallocate(model_id_array)
        if(allocated(unique_models_array)) deallocate(unique_models_array)



        print *, '1.4'

    end subroutine machine_learning_parameters_init


    !Machine Learning Parameters Destructor
    subroutine machine_learning_parameters_destroy(this)
        implicit none
        class(machine_learning_parameters_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine machine_learning_parameters_destroy














end module module_machine_learning_parameters
