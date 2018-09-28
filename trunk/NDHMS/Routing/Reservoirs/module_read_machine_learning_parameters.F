module module_read_machine_learning_parameters

    implicit none

contains

    subroutine read_machine_learning_parameters()
        use netcdf

        implicit none

        integer :: ncid, status, biasVarId, minimum_of_featuresVarId, maximum_of_featuresVarId, weightsVarId, input_namesVarId
        integer :: activation_functionsVarId, number_of_nodesVarId, minimum_storageVarId, maximum_storageVarId, lake_idVarId
        integer :: minimum_releaseVarId, maximum_releaseVarId
        character, external :: nf90_strerror1
        integer, dimension(nf90_max_var_dims) :: dimIDs
        integer :: elevation_acapVarId, area_acapVarId, storage_acapVarId

        status = nf90_open(path = "17all_res_acf_all_relu_nc.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !print *, "status", status
        if (status /= nf90_noerr) call handle_err(status)


        !Can we simplify all of the inquire_dimension calls and the like into a simpler list call to the functions???

        !We can probably take out the writing of results to text files???





    end subroutine read_machine_learning_parameters



end module module_read_machine_learning_parameters
