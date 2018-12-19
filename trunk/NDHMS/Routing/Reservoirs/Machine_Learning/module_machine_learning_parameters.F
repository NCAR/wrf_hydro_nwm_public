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

        real :: min_storage                                   ! minimum storage
        real :: max_storage                                   ! maximum storage
        real :: min_release                                   ! minimum release
        real :: max_release                                   ! maximum release
		integer :: model_id								      ! machine learning model number
		integer :: model_index								  ! machine learning model index
        integer :: lake_number                                ! lake number
        real    :: reservoir_area

    contains

        procedure :: init => machine_learning_parameters_init
        procedure :: destroy => machine_learning_parameters_destroy

    end type machine_learning_parameters_interface

contains

    !Machine Learning Parameters Constructor

    !subroutine machine_learning_parameters_init(this, model_id, model_index, lake_number)
    !subroutine machine_learning_parameters_init(this, model_index, lake_number)
    !subroutine machine_learning_parameters_init(this, machine_learning_parameter_file, &
    !    reservoir_area, lake_number, number_of_unique_models)
        subroutine machine_learning_parameters_init(this, reservoir_area, lake_number, number_of_unique_models)


        implicit none
        class(machine_learning_parameters_interface), intent(inout) :: this ! the type object being initialized

		!integer, intent(in) :: model_id				! machine learning model number
		!integer, intent(in) :: model_index				! machine learning model index in model array
        !character(len=256), intent(in) :: machine_learning_parameter_file
        real, intent(in)     :: reservoir_area          ! area of reservoir (km^2)
        integer, intent(in) :: lake_number              ! lake number
        integer, intent(inout) :: number_of_unique_models


        integer :: lake_index, lake_id_index

        integer :: idx_mod_id_arr, idx_unique_arr

        integer, allocatable, dimension (:) :: lake_id
        integer, allocatable, dimension (:) :: model_id_array
        integer, allocatable, dimension (:) :: unique_models_array
        real, allocatable, dimension (:)    :: minimum_storage
        real, allocatable, dimension (:)    :: maximum_storage
        real, allocatable, dimension (:)    :: minimum_release
        real, allocatable, dimension (:)    :: maximum_release

        integer :: Nlakes, ncid
        integer :: lake_idVarId, model_idVarId
        integer :: minimum_storageVarId, maximum_storageVarId
        integer :: minimum_releaseVarId, maximum_releaseVarId
        character, external :: nf90_strerror1
        integer, dimension(nf90_max_var_dims) :: dimIDs
        integer :: status       ! Status of reading NetCDF


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
        if (status /= nf90_noerr) call handle_err(status)

        print *, 'after open reservoir parm'

        !call read_netcdf_real_1d_array(ncid, "maximum_release", maximum_release)


print *, 'maximum_release'
print *, maximum_release


        status = nf90_inq_varid(ncid, "lake_id", lake_idVarId)
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_inq_varid(ncid, "model_id", model_idVarId)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inq_varid(ncid, "maximum_release", maximum_releaseVarId)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inq_varid(ncid, "minimum_release", minimum_releaseVarId)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inq_varid(ncid, "maximum_storage", maximum_storageVarId)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inq_varid(ncid, "minimum_storage", minimum_storageVarId)
        if(status /= nf90_NoErr) call handle_err(status)



        status = nf90_inquire_variable(ncid, lake_idVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)
        allocate(lake_id(Nlakes))


        status = nf90_inquire_variable(ncid, model_idVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)
        allocate(model_id_array(Nlakes))

        allocate(unique_models_array(Nlakes))


        status = nf90_inquire_variable(ncid, maximum_releaseVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)
        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)
        allocate(maximum_release(Nlakes))

        status = nf90_inquire_variable(ncid, minimum_releaseVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)
        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)
        allocate(minimum_release(Nlakes))

        status = nf90_inquire_variable(ncid, maximum_storageVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)
        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)
        allocate(maximum_storage(Nlakes))

        status = nf90_inquire_variable(ncid, minimum_storageVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)
        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)
        allocate(minimum_storage(Nlakes))


        status = nf90_get_var(ncid, lake_idVarId, lake_id)
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, model_idVarId, model_id_array)
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, maximum_releaseVarId, maximum_release)
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, minimum_releaseVarId, minimum_release)
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, maximum_storageVarId, maximum_storage)
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, minimum_storageVarId, minimum_storage)
        if (status /= nf90_noerr) call handle_err(status)



        do lake_index = 1, Nlakes
            if (lake_id(lake_index) == this%lake_number) then
                lake_id_index = lake_index
            end if
        end do

        this%model_id    = model_id_array(lake_id_index)
        this%min_storage = minimum_storage(lake_id_index)
        this%max_storage = maximum_storage(lake_id_index)
        this%min_release = minimum_release(lake_id_index)
        this%max_release = maximum_release(lake_id_index)



        !CHECK RECORDINGS ABOUT BELOW. IF I NEED TO REMOVE ONE OF THE LINES?????

        if (number_of_unique_models == 0) then

            ! Find out how many unique models here
            do idx_mod_id_arr = 1, Nlakes
                do idx_unique_arr = 1, Nlakes

                    if (model_id_array(idx_mod_id_arr) == unique_models_array(idx_unique_arr)) then
                        exit

                    else if (unique_models_array(idx_unique_arr) == 0) then
                        unique_models_array(idx_unique_arr) = model_id_array(idx_mod_id_arr)
                        exit
                    end if

                end do
            end do

            !CHANGE ARRAY NAME TO UNIQUE MODELS
            !number_of_unique_models = 0
            do idx_unique_arr = 1, Nlakes
                if (unique_models_array(idx_unique_arr) == 0) then
                    unique_models_array(idx_unique_arr) = model_id_array(idx_mod_id_arr)
                    exit

                else
                    number_of_unique_models = number_of_unique_models + 1
                end if
            end do
        end if

        print *, "5"


        deallocate(lake_id)
        deallocate(model_id_array)
        deallocate(unique_models_array)
        deallocate(minimum_storage)
        deallocate(maximum_storage)
        deallocate(minimum_release)
        deallocate(maximum_release)

    end subroutine machine_learning_parameters_init


    !Machine Learning Parameters Destructor
    subroutine machine_learning_parameters_destroy(this)
        implicit none
        class(machine_learning_parameters_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine machine_learning_parameters_destroy




    subroutine open_netcdf(file_path, ncid)
        character(len=256), intent(in) :: file_path
        integer, intent(out) ::ncid
        integer :: status

        status = nf90_open(path = file_path, mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        if (status /= nf90_noerr) call handle_err(status)

    end subroutine open_netcdf


    subroutine read_netcdf_real_1d_array(ncid, netcdf_array_name, array_out)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        real, dimension(:), intent(out) :: array_out
        real, allocatable, dimension(:) :: temp_array
        integer :: var_id, array_length, status
        integer, dimension(nf90_max_var_dims) :: dimIDs

        print *, 'before inq varid'

        print *, netcdf_array_name, ncid

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        !status = nf90_inq_varid(ncid, "maximum_release", var_id)
        if (status /= nf90_noerr) call handle_err(status)


        print *, 'after inq varid'

        status = nf90_inquire_variable(ncid, var_id, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        print *, 'after inq var'

        !status = nf90_inquire_dimension(ncid, dimIDs(1), len = array_length)
        !if(status /= nf90_NoErr) call handle_err(status)

        print *, 'after inq dimension'
        print *, array_length

        !allocate(temp_array(array_length))

        print *, 'after allocate temp array'

        status = nf90_get_var(ncid, var_id, array_out)
        if (status /= nf90_noerr) call handle_err(status)

        !print *, 'temp_array'
        !print *, temp_array

        !array_out = temp_array

        print *, 'array_out'
        print *, array_out


        deallocate(temp_array)


    end subroutine read_netcdf_real_1d_array



    subroutine get_netcdf_1d_array_dimension



    end subroutine get_netcdf_1d_array_dimension




end module module_machine_learning_parameters
