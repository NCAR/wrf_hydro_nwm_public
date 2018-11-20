module module_reservoir_io

    use module_reservoir_base
    use module_io_utilities
    use netcdf

    use module_machine_learning

    implicit none




contains






    !subroutine reservoir_write_to_restart_file2(reservoirs, q_types, inflows, outflows, water_elevations, old_date, iret, ncid, NF_FLOAT, dimid_lakes, varid, nlakes, lake_index)
    subroutine reservoir_write_to_restart_file2(reservoirs, q_types, inflows, outflows, &
    water_elevations, old_date, since_date, did, iret, ncid, NF_FLOAT, NF_INT, NF_REAL, NF_GLOBAL, DTCT, his_out_counts, channel_only, &
    channel_bucket_only, dimid_lakes, varid, nlakes, lake_index)
        implicit none
        class (reservoir_base_container), dimension(:), intent(in):: reservoirs
        integer, intent(inout) :: did, iret, ncid, varid
        integer, intent(in) :: NF_FLOAT, NF_INT, NF_REAL, NF_GLOBAL, nlakes, his_out_counts, channel_only, channel_bucket_only
        integer, intent(in) :: lake_index(nlakes)
        !integer, dimension(:), intent(in) :: dimid_lakes
        integer, intent(out) :: dimid_lakes
        real, intent(in) :: DTCT
        real, dimension(:), intent(in) :: q_types, inflows, outflows, water_elevations
        character(len=*), intent(in) :: old_date, since_date


        integer :: e
        integer :: lake_index_for_q_type

        character(len=1) :: did_string
        character(len=2) :: tmpStr
        character(len=64) :: out_file   ! NEED TO verify WHAT THIS LENGTH SHOULD BE


        real, allocatable, dimension(:) :: temp_machine_learning_current_time_storage_array


        !write(did_string,'(I5)') did

        !out_file = "reservoir_rst."//old_date(1:16)//did_string

        out_file = "reservoir_rst."//old_date(1:16)//"_DOMAIN1"



        print *, 'reservoir restart write in reservoir_io'


       iret = nf90_create(trim(out_file), ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid)






!#ifdef WRFIO_NCD_LARGE_FILE_SUPPORT
       !iret = nf90_create(trim(out_file), ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid)
!#ifdef HYDRO_D
       !write(6,*) "yyywww using large netcdf file definition. "
       !call flush(6)
!#endif
!#else


       !iret = nf90_create(trim(out_file), NF90_CLOBBER, ncid)



          !iret = nf90_def_dim(ncid, "lakes", nlakes, (/dimid_lakes(1)/))
          iret = nf90_def_dim(ncid, "lakes", nlakes, dimid_lakes)




            !dd = nf90_def_var(aa,"machine_learning_current_time_storage",ee,1,cc) ! builds okay

            !iret = nf_def_var(ncid,"qlakei",NF_FLOAT,1,(/dimid_lakes/),varid)



        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes(1)/), varid)


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Works but need to verify dimid_lakes
        iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes/), varid)

        iret = nf90_def_var(ncid, "inflows", NF_FLOAT, (/dimid_lakes/), varid)

        iret = nf90_def_var(ncid, "outflows", NF_FLOAT, (/dimid_lakes/), varid)

!        iret = nf90_def_var(ncid, "water_elevations", NF_FLOAT, (/dimid_lakes/), varid)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, dimid_lakes, varid)
        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes/), varid)

        !!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Builds w/ e set as an integer
        !iret = nf90_def_var(1, "machine_learning_current_time_storage", 2, 1, e)

        ! Builds now b/c varid set to inout instead of just in
        !iret = nf90_def_var(1, "machine_learning_current_time_storage", 2, 1, varid)
        !!!!!!!!!!!!!!!!!!!!!




       !         put global attribute
       !iret = nf90_put_att_int(ncid,NF_GLOBAL,"his_out_counts",NF_INT, 1,his_out_counts)
       !iret = nf90_put_att_text(ncid,NF_GLOBAL,"Restart_Time",19,old_date)
       !iret = nf90_put_att_text(ncid,NF_GLOBAL,"Since_Date",19,since_date)
       !iret = nf90_put_att_real(ncid,NF_GLOBAL,"DTCT",NF_REAL, 1,DTCT)
       !iret = nf90_put_att_int(ncid, NF_GLOBAL, "channel_only", NF_INT, 1, &
       !     channel_only)
       !iret = nf90_put_att_int(ncid, NF_GLOBAL, "channel_bucket_only", NF_INT, 1, &
       !     channel_bucket_only)



       !         put global attribute
       iret = nf90_put_att(ncid,NF_GLOBAL,"his_out_counts",his_out_counts)
       iret = nf90_put_att(ncid,NF_GLOBAL,"Restart_Time",old_date)
       iret = nf90_put_att(ncid,NF_GLOBAL,"Since_Date",since_date)
       iret = nf90_put_att(ncid,NF_GLOBAL,"DTCT",DTCT)
       iret = nf90_put_att(ncid, NF_GLOBAL, "channel_only", channel_only)
       iret = nf90_put_att(ncid, NF_GLOBAL, "channel_bucket_only", channel_bucket_only)



        iret = nf90_enddef(ncid)


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



        print *, 'outflows in reservoir_io'

        print *, outflows


        call test123()


        call w_rst_crt_nc1_lake(ncid, nlakes, temp_machine_learning_current_time_storage_array, "machine_learning_current_time_storage", &
               lake_index)


        call w_rst_crt_nc1_lake(ncid, nlakes, inflows, "inflows", &
               lake_index)


        call w_rst_crt_nc1_lake(ncid, nlakes, outflows, "outflows", &
               lake_index)

        !call w_rst_crt_nc1_lake(ncid, nlakes, water_elevations, "water_elevations", &
        !       lake_index)





!DEALLOCATE TEMP ML ARRAYS!!!

        iret = nf90_close(ncid)


    end subroutine reservoir_write_to_restart_file2





    subroutine reservoir_read_from_restart_file2(reservoirs, in_file, q_types, inflows, outflows, water_elevations, iret, ncid, NF_FLOAT, NF_GLOBAL, nlakes, lake_index)
        implicit none
        class (reservoir_base_container), dimension(:), intent(in):: reservoirs
        character(len=*) in_file
        integer, intent(inout) :: iret, ncid
        integer, intent(in) :: NF_FLOAT, NF_GLOBAL, nlakes
        integer, intent(in) :: lake_index(nlakes)
        !integer, dimension(:), intent(in) :: dimid_lakes
        !integer, intent(out) :: dimid_lakes
        real, dimension(:), intent(in) :: q_types
        real, dimension(:), intent(out) :: inflows, outflows, water_elevations

        !character(len=*), intent(in) :: old_date

        character(len = 32) :: time_stamp
        character(len = 14) :: reservoir_file_prefix
        character(len = 64) :: reservoir_restart_file_name

        character(len = 64) :: since_date

        integer :: e
        integer :: lake_index_for_q_type, NF_NOWRITE

        character(len=2) :: tmpStr
        !character(len=64) :: out_file   ! NEED TO verify WHAT THIS LENGTH SHOULD BE


        real, allocatable, dimension(:) :: temp_machine_learning_current_time_storage_array



        read (in_file(11: ), *) time_stamp


        print *, 'time_stamp'
        print *, time_stamp

        reservoir_file_prefix = 'reservoir_rst.'

        reservoir_restart_file_name = reservoir_file_prefix//time_stamp

        print *, 'reservoir_restart_file_name'
        print *, reservoir_restart_file_name


        !open a netcdf file
        iret = nf90_open(trim(reservoir_restart_file_name), NF_NOWRITE, ncid)


        if (iret /= 0) then
           write(*,'("Problem opening file: ''", A, "''")') &
                trim(reservoir_restart_file_name)
           call hydro_stop("In reservoir_read_from_restart_file() - Problem opening file")
        endif



        allocate (temp_machine_learning_current_time_storage_array(nlakes))


        temp_machine_learning_current_time_storage_array = 0.0


       !iret = NF90_GET_ATT(ncid, NF_GLOBAL, 'his_out_counts', rt_domain(did)%his_out_counts)
       !iret = NF90_GET_ATT(ncid, NF_GLOBAL, 'DTCT', nlst_rt(did)%DTCT)
       iret = NF90_GET_ATT(ncid,NF_GLOBAL,"Since_Date",since_date)


        call read_rst_crt_nc(ncid,outflows, nlakes, "outflows")

        print *, 'outflows from read'
        print *, outflows



        call read_rst_crt_nc(ncid, temp_machine_learning_current_time_storage_array, nlakes, "machine_learning_current_time_storage")


        print *, 'temp_machine_learning_current_time_storage_array from read'
        print *, temp_machine_learning_current_time_storage_array




        do lake_index_for_q_type = 1, nlakes
            if (q_types(lake_index_for_q_type) == 2.0) then

                !temp_current_storage_array(lake_index) = rt_domain(did)%reservoirs(lake_index)%ptr%reservoir_base_state_struct%current_storage

                !builds
                !temp_machine_learning_current_time_storage_array(lake_index) = rt_domain(did)%reservoirs(lake_index)%ptr%input%inflow

                print *, 'restart getting storage for reservoir at index: ', lake_index_for_q_type

                select type (ml => reservoirs(lake_index_for_q_type)%ptr)
                    type is (machine_learning_struct)
                        print *, 'entered select'

                        ml%state%current_time_storage = temp_machine_learning_current_time_storage_array(lake_index_for_q_type)
                end select


            end if
        end do


     iret =  nf90_close(ncid)


    end subroutine reservoir_read_from_restart_file2



end module module_reservoir_io
