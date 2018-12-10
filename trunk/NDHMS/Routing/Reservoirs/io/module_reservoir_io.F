module module_reservoir_io

    use module_reservoir_base
    use module_io_utilities
    use netcdf

    use module_machine_learning

    !use mpi, only: MPI_INFO_NULL
    !NEED THEN IFDEF PARALLEL LIKE IN MODULE_HRLDAS_NETCDF_IO.F??????


    implicit none




contains






    !subroutine reservoir_write_to_restart_file2(reservoirs, q_types, inflows, outflows, water_elevations, old_date, iret, ncid, NF_FLOAT, dimid_lakes, varid, nlakes, lake_index)
    subroutine reservoir_write_to_restart_file2(reservoirs, q_types, inflows, outflows, &
    water_elevations, old_date, since_date, did, iret, ncid, NF_FLOAT, NF_INT, NF_REAL, NF_GLOBAL, DTCT, his_out_counts, channel_only, &
    channel_bucket_only, dimid_lakes, varid, nlakes, lake_index, local_lake_index, lake_ids, nlinksize)
        implicit none
        class (reservoir_base_container), dimension(:), intent(inout):: reservoirs
        integer, intent(inout) :: did, iret, ncid, varid
        integer, intent(in) :: NF_FLOAT, NF_INT, NF_REAL, NF_GLOBAL, nlakes, his_out_counts, channel_only, channel_bucket_only
        integer, intent(in) :: lake_index(nlakes)  ! this is the rt_domain%lake_index
        !integer, dimension(:), intent(in) :: dimid_lakes
        integer, intent(out) :: dimid_lakes
        real, intent(in) :: DTCT
        integer, dimension(:), intent(in) :: local_lake_index ! this is the rt_domain%LAKEIDX, which might be the same as rt_domain%lake_index. Not sure.
        integer, dimension(:), intent(in) :: lake_ids
        real, dimension(:), intent(in) :: q_types, inflows, outflows, water_elevations
        character(len=*), intent(in) :: old_date, since_date
        integer, intent(in) :: nlinksize

        integer :: e
        integer :: lake_index_for_q_type

        integer :: lake_id

        character(len=32) :: lake_id_string
        character(len=64) :: reservoir_name


        character(len=1) :: did_string
        character(len=2) :: tmpStr
        character(len=64) :: out_file   ! NEED TO verify WHAT THIS LENGTH SHOULD BE


        real, allocatable, dimension(:) :: temp_machine_learning_current_time_storage_array, outflow, machine_learning_current_time_storage

        real, allocatable, dimension(:) :: temp_reservoir_array


        real :: inflow, water_elevation



       ! MPI stuff: number of processors, rank of this processor, and error
       ! code.
       integer :: p, my_rank, ierr




        integer :: array_size

        if (nlakes <= 0) return


       ! Initialize MPI, learn local rank and total number of processors.
       !call MPI_Init(ierr)
       !call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
       !call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)




        !write(did_string,'(I5)') did

        !out_file = "reservoir_rst."//old_date(1:16)//did_string

        out_file = "reservoir_rst."//old_date(1:16)//"_DOMAIN1"



        print *, 'reservoir restart write in reservoir_io'


       ! original
       !iret = nf90_create(trim(out_file), ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid)


        !!!!!!!!!
        !Creates reservoir restart files
       !iret = nf90_create(trim(out_file), ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid, MPI_COMM_WORLD, info = MPI_INFO_NULL)

        iret = nf90_create(trim(out_file), ior(NF90_CLOBBER, NF90_NETCDF4), ncid, comm = MPI_COMM_WORLD, info = MPI_INFO_NULL)

        !!!!!!!


        !call check( nf90_create(trim(out_file), NF90_NETCDF4, ncid, comm = MPI_COMM_WORLD, info = MPI_INFO_NULL) )

        !iret = nf90_create(trim(out_file), NF90_NETCDF4, ncid, comm = MPI_COMM_WORLD, info = MPI_INFO_NULL)

       !iret = nf90_create(trim(out_file), ior(NF90_CLOBBER, NF90_NETCDF4), ncid, MPI_COMM_WORLD, info = MPI_INFO_NULL)



        !iret = nf90_var_par_access(ncid, varid, nf90_collective)


        ! setting the 'comm =' does not create reservoir restarts
       !iret = nf90_create(trim(out_file), ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid, comm = MPI_COMM_WORLD)


        !iret = nf90_create(trim(out_file), ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid, MPI_COMM_WORLD, MPI_INFO_NULL)



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

        iret = nf90_def_var(ncid, "water_elevations", NF_FLOAT, (/dimid_lakes/), varid)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        print*, 'dimid_lakes'
        print*, dimid_lakes

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


        allocate (temp_machine_learning_current_time_storage_array(1))

        allocate (outflow(1))
        allocate (machine_learning_current_time_storage(1))


        !allocate (outflow(nlakes))

        outflow = 0.0
        machine_learning_current_time_storage = 0.0



        temp_machine_learning_current_time_storage_array = 0.0



        print *, 'q_types'
        print *, q_types

        print *, 'lake ids'
        print *, lake_ids


        print *, 'local lake index array vals'

        !print *, local_lake_index


        print *, 'lake_index array'
        print *, lake_index

        print *, 'nlakes'
        print *, nlakes


        print *, 'local lake index array size'
        !print *, shape(local_lake_index)




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        do lake_index_for_q_type = 1, nlinksize
            if (local_lake_index(lake_index_for_q_type) >= 0) then
                print *, 'lake idx greater than 0'
                !print *, lake_index_for_q_type

                !print *, local_lake_index(lake_index_for_q_type)


            lake_id = local_lake_index(lake_index_for_q_type)





!!!!!!!!!!!!!!!!!!
            !inflow = inflows(lake_id)

            outflow(1) = outflows(lake_id)

            !water_elevation = water_elevations(lake_id)

!!!!!!!!!!!!!!!!!!!

            print *, 'outflows'
            print *, outflows


            print *, 'outflow'
            print *, outflow






            !temp_reservoir_array(1) = inflow

            !temp_reservoir_array(2) = outflow

            !temp_reservoir_array(3) = water_elevation

            !temp_reservoir_array(4) = 0.0


            if (q_types(lake_id) == 2.0) then

                            !temp_current_storage_array(lake_index) = rt_domain(did)%reservoirs(lake_index)%ptr%reservoir_base_state_struct%current_storage

                            !builds
                            !temp_machine_learning_current_time_storage_array(lake_index) = rt_domain(did)%reservoirs(lake_index)%ptr%input%inflow

                            print *, 'restart setting storage for reservoir at index: ', lake_id


                            !allocate( machine_learning_struct :: reservoirs(lake_index_for_q_type)%ptr)
                            !allocate( machine_learning_struct :: reservoirs(lake_index_for_q_type)%ptr)


                            select type (ml => reservoirs(lake_id)%ptr)
                                type is (machine_learning_struct)
  !                                  print *, 'reservoir io entered select'

                                    !temp_machine_learning_current_time_storage_array(lake_index_for_q_type) = ml%state%current_time_storage


                                    !temp_reservoir_array(4) = ml%state%current_time_storage

                                    !machine_learning_current_time_storage = ml%state%current_time_storage



                                    !!!!!!!!!!!1
                                    !temp_machine_learning_current_time_storage_array(1) = ml%state%current_time_storage

                                    machine_learning_current_time_storage(1) = ml%state%current_time_storage
                                    !!!!!!!

                                    !print *, 'temp_machine_learning_current_time_storage_array'
                                    !print *, temp_machine_learning_current_time_storage_array


                                    ! Different approach
                                    !ml%state%current_time_storage = temp_machine_learning_current_time_storage_array(lake_index_for_q_type)


                                    !print *, 'reservoir io select current time storage'
                                    !print *, ml%state%current_time_storage
                                    !print *, machine_learning_current_time_storage


                                    !print *, 'select outflow'
                                    !print *, ml%output%outflow

                            end select


                            call w_rst_crt_nc1_lake(ncid, 1, machine_learning_current_time_storage, lake_id, "machine_learning_current_time_storage", &
                            lake_index)

                            !temp_machine_learning_current_time_storage_array(lake_index_for_q_type) = machine_learning_current_time_storage

                            !print *, 'reservoir io current time storage'
                            !print *, machine_learning_current_time_storage




        !call w_rst_crt_nc1_lake(ncid, 1, temp_machine_learning_current_time_storage_array, lake_id, "machine_learning_current_time_storage", &
        !       lake_index)





            end if  ! for q_types




        !call w_rst_crt_nc1_lake(ncid, 4, temp_reservoir_array, reservoir_name, &
        !       lake_index)


        !!!!!!!!!!!!!!!
        !call w_rst_crt_nc1_lake(ncid, 1, temp_machine_learning_current_time_storage_array, lake_id, "machine_learning_current_time_storage", &
        !       lake_index)


        !call w_rst_crt_nc1_lake(ncid, nlakes, temp_machine_learning_current_time_storage_array, lake_id, "machine_learning_current_time_storage", &
        !       lake_index)


        !call w_rst_crt_nc1_lake(ncid, nlakes, temp_machine_learning_current_time_storage_array, lake_id, "machine_learning_current_time_storage", &
        !       lake_index)


        !call w_rst_crt_nc1_lake(ncid, nlakes, inflows, "inflows", &
        !       lake_index)


        !call w_rst_crt_nc1_lake(ncid, nlakes, outflows, "outflows", &
        !       lake_index)




!!!!!!!!!!!!!!
        !call w_rst_crt_nc1_lake(ncid, nlakes, outflow, lake_id, "outflow", &
        !       lake_index)



!if (lake_id == 3) then
        call w_rst_crt_nc1_lake(ncid, 1, outflow, lake_id, "outflows", &
               lake_index)


!end if
!!!!!!!!!!!!!!1
     !call w_rst_crt_nc1_lake(ncid, nlakes, outflows, 1, "outflows", &
     !          lake_index)



        !call w_rst_crt_nc1_lake(ncid, nlakes, water_elevations, "water_elevations", &
        !       lake_index)





        end if ! for local_lake_index
    end do




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






        !call w_rst_crt_nc1_lake(ncid, nlakes, temp_machine_learning_current_time_storage_array, "machine_learning_current_time_storage", &
        !       lake_index)


        !call w_rst_crt_nc1_lake(ncid, nlakes, inflows, "inflows", &
        !       lake_index)


        !call w_rst_crt_nc1_lake(ncid, nlakes, outflows, "outflows", &
        !       lake_index)

        !call w_rst_crt_nc1_lake(ncid, nlakes, water_elevations, "water_elevations", &
        !       lake_index)




        !call w_rst_crt_nc1_lake(ncid, nlakes, outflows, 1, "outflows", &
        !       lake_index)


        iret = nf90_close(ncid)

        !print *, 'temp_machine_learning_current_time_storage_array'
        !print *, temp_machine_learning_current_time_storage_array



        if(allocated(temp_machine_learning_current_time_storage_array)) deallocate(temp_machine_learning_current_time_storage_array)

        if(allocated(temp_reservoir_array)) deallocate(temp_reservoir_array)



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

                        ml%state%previous_time_release = outflows(lake_index_for_q_type)

                end select


            end if
        end do


     iret =  nf90_close(ncid)


    end subroutine reservoir_read_from_restart_file2


   subroutine check(status)
     integer, intent ( in) :: status

     if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 2
     end if
   end subroutine check



end module module_reservoir_io
