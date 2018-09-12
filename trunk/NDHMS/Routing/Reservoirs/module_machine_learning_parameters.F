! This module defines and instantiates objects
! for a machine learning type reservoir's
! parameters/properties. Parameters holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.
module module_machine_learning_parameters

    use module_reservoir_base
    implicit none

    !subroutine machine_learning_struct_init(this, water_elevation,  &
    !    previous_day_water_elevation, previous_day_inflow, &
    !    previous_day_release, current_storage, previous_day_storage, &
    !    min_release, max_release, min_storage, max_storage, &
    !    lake_number)

    ! Extend/derive machine learning parameters from the abstract base
    ! struct for reservoir properties.
    type, extends(reservoir_base_properties_struct) :: machine_learning_parameters_interface
        real :: min_storage                                   ! minimum storage
        real :: max_storage                                   ! maximum storage
        real :: min_release                                   ! minimum release
        real :: max_release                                   ! maximum release
        integer :: lake_number                                ! lake number

    contains

        procedure :: init => machine_learning_parameters_init
        procedure :: destroy => machine_learning_parameters_destroy

    end type machine_learning_parameters_interface

contains

    !Machine Learning Parameters Constructor
    subroutine machine_learning_parameters_init(this, lake_number)
        use netcdf
        implicit none
        class(machine_learning_parameters_interface), intent(inout) :: this ! the type object being initialized

        integer, intent(in) :: lake_number              ! lake number

        integer :: lake_index, lake_id_index

        integer, allocatable, dimension (:) :: lake_id
        real, allocatable, dimension (:)    :: minimum_storage
        real, allocatable, dimension (:)    :: maximum_storage
        real, allocatable, dimension (:)    :: minimum_release
        real, allocatable, dimension (:)    :: maximum_release


        integer :: Nlakes
        integer :: ncid, biasVarId, minimum_of_featuresVarId, maximum_of_featuresVarId, weightsVarId, input_namesVarId
        integer :: activation_functionsVarId, number_of_nodesVarId, minimum_storageVarId, maximum_storageVarId, lake_idVarId, lake_id_single_VarId
        integer :: minimum_releaseVarId, maximum_releaseVarId
        character, external :: nf90_strerror1
        integer, dimension(nf90_max_var_dims) :: dimIDs
        integer :: elevation_acapVarId, area_acapVarId, storage_acapVarId

        integer :: status       ! Status of reading NetCDF


        ! Assign the values passed in to a particular machine learning reservoir
        ! parameter object's variables.
        this%lake_number = lake_number


        !Reservoir Parm NC file
        status = nf90_open(path = "RESERVOIR_PARM.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !print *, "status", status
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_inq_varid(ncid, "lake_id", lake_idVarId)


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


        this%min_storage = minimum_storage(lake_id_index)
        this%max_storage = maximum_storage(lake_id_index)
        this%min_release = minimum_release(lake_id_index)
        this%max_release = maximum_release(lake_id_index)

        deallocate(lake_id)
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

end module module_machine_learning_parameters



! The functions that have been used for reading NetCDF file!
function nf90_inq_varid(ncid, name, varid)
    integer, intent(in) :: ncid
    character (len = *), intent( in) :: name
    real, intent(out) :: varid
    integer :: nf90_inq_varid
end function nf90_inq_varid


function nf90_get_var(ncid, varid, values)
    integer, intent( in) :: ncid, varid
    real, intent(out) :: values
    integer :: nf90_get_var
end function nf90_get_var


function nf90_strerror(ncerr)
    integer, intent( in) :: ncerr
    character(len = 80) :: nf90_strerror
end function nf90_strerror

subroutine handle_err(status)
    implicit none
    integer, intent ( in) :: status
    character :: nf90_strerror
    integer  :: nf90_noerr
    if(status /= nf90_noerr) then
        !print *, trim(nf90_strerror(status))
        !print *, nf90_strerror(status) !FIXME this line deactivated, but we need to check on it!
        stop "Stopped"
    end if
end subroutine handle_err

function nf90_open(path, mode, ncid, chunksize)
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: mode
    integer,             intent(  out) :: ncid
    integer, optional,   intent(inout) :: chunksize
    integer                            :: nf90_open
end function nf90_open

function nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids, nAtts)
    integer,                         intent( in) :: ncid, varid
    character (len = *),   optional, intent(out) :: name
    integer, dimension(*), optional, intent(out) :: dimids
    integer                                      :: nf90_inquire_variable
end function nf90_inquire_variable


function nf90_inquire_dimension(ncid, dimid, name, len)
    integer,                       intent( in) :: ncid, dimid
    character (len = *), optional, intent(out) :: name
    integer,             optional, intent(out) :: len
    integer                                    :: nf90_inquire_dimension
end function nf90_inquire_dimension
