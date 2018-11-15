module module_reservoir_io

    use module_reservoir_base
    use module_io_utilities
    use netcdf

    use module_machine_learning

    implicit none




contains


    subroutine reservoir_write_to_restart_file2(reservoirs, q_types, inflows, outflows, water_elevations, old_date, iret, ncid, NF_FLOAT, dimid_lakes, varid, nlakes, lake_index)
    !subroutine reservoir_write_to_restart_file2(reservoirs, q_type, iret, ncid, NF_FLOAT, dimid_lakes, varid, nlakes, lake_index)
        implicit none
        class (reservoir_base_container), dimension(:), intent(in):: reservoirs
        integer, intent(inout) :: iret, ncid, varid
        integer, intent(in) :: NF_FLOAT, nlakes
        integer, intent(in) :: lake_index(nlakes)
        integer, dimension(:), intent(in) :: dimid_lakes
        real, dimension(:), intent(in) :: q_types, inflows, outflows, water_elevations
        character(len=*), intent(in) :: old_date


        integer :: e
        integer :: lake_index_for_q_type

        character(len=2) :: tmpStr
        character(len=64) :: outFile   ! NEED TO verify WHAT THIS LENGTH SHOULD BE


        real, allocatable, dimension(:) :: temp_machine_learning_current_time_storage_array


        outFile = "reservoir_rst."//old_date(1:16)


!#ifdef WRFIO_NCD_LARGE_FILE_SUPPORT
       iret = nf90_create(trim(outFile), ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid)
!#ifdef HYDRO_D
       write(6,*) "yyywww using large netcdf file definition. "
       call flush(6)
!#endif



            !dd = nf90_def_var(aa,"machine_learning_current_time_storage",ee,1,cc) ! builds okay

            !iret = nf_def_var(ncid,"qlakei",NF_FLOAT,1,(/dimid_lakes/),varid)


        ! use below
        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes(1)/), varid)


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Works but need to verify dimid_lakes
        iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes/), varid)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, dimid_lakes, varid)
        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes/), varid)

        !!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Builds w/ e set as an integer
        !iret = nf90_def_var(1, "machine_learning_current_time_storage", 2, 1, e)

        ! Builds now b/c varid set to inout instead of just in
        !iret = nf90_def_var(1, "machine_learning_current_time_storage", 2, 1, varid)
        !!!!!!!!!!!!!!!!!!!!!




        allocate (temp_machine_learning_current_time_storage_array(nlakes))

        temp_machine_learning_current_time_storage_array = 0.0

        do lake_index_for_q_type = 1, nlakes
            if (q_types(lake_index_for_q_type) == 2.0) then

                !temp_current_storage_array(lake_index) = rt_domain(did)%reservoirs(lake_index)%ptr%reservoir_base_state_struct%current_storage

                !builds
                !temp_machine_learning_current_time_storage_array(lake_index) = rt_domain(did)%reservoirs(lake_index)%ptr%input%inflow

                print *, 'restart setting storage for reservoir at index: ', lake_index_for_q_type

                select type (ml => reservoirs(lake_index_for_q_type)%ptr)
                    type is (machine_learning_struct)
                        print *, 'entered select'

                        temp_machine_learning_current_time_storage_array(lake_index_for_q_type) = ml%state%current_time_storage
                end select


            end if
        end do






        !call test123()


        call w_rst_crt_nc1_lake(ncid, nlakes, temp_machine_learning_current_time_storage_array, "machine_learning_current_time_storage", &
               lake_index)






    end subroutine reservoir_write_to_restart_file2















end module module_reservoir_io
