module module_reservoir_io

    use module_reservoir_base
    use module_reservoir_utilities
    use netcdf
    use mpi, only: MPI_INFO_NULL, mpi_comm_world
    implicit none

contains

    subroutine create_reservoir_restart_file(current_date, since_date, DTCT, &
               did, number_of_lakes, NF_FLOAT, dimid_lakes, ncid)

        character(len=*), intent(in)   :: current_date, since_date
        real, intent(in)               :: DTCT
        integer, intent(in)            :: did, number_of_lakes, NF_FLOAT
        integer, intent(out)           :: dimid_lakes, ncid

        character(len=32) :: lake_id_string
        character(len=64) :: reservoir_name
        character(len=64) :: out_file
        integer           :: iret, varid
        character(len=1)  :: did_string

        write(did_string,'(I1)') did

        ! Construct file name based upon the current date/time
        out_file = "reservoir_rst."//current_date(1:16)//"_DOMAIN"//did_string

        ! Create restart file allowing parallel writes
        call check(nf90_create( trim(out_file), IOR(NF90_NETCDF4, NF90_MPIPOSIX), ncid, comm=MPI_COMM_WORLD, &
        info=MPI_INFO_NULL), 'create_reservoir_restart_file', trim(out_file))

        ! Create lake dimension
        call check(nf90_def_dim(ncid, "lakes", number_of_lakes, dimid_lakes), 'create_reservoir_restart_file', trim(out_file))

        call check(nf90_def_var(ncid, "inflows", NF_FLOAT, (/dimid_lakes/), varid), 'create_reservoir_restart_file', trim(out_file))
        call check(nf90_def_var(ncid, "outflows", NF_FLOAT, (/dimid_lakes/), varid), 'create_reservoir_restart_file', trim(out_file))
        call check(nf90_def_var(ncid, "water_elevations", NF_FLOAT, (/dimid_lakes/), varid), 'create_reservoir_restart_file', trim(out_file))

        call check(nf90_put_att(ncid, NF90_GLOBAL, "Restart_Time", current_date), 'create_reservoir_restart_file', trim(out_file))
        call check(nf90_put_att(ncid, NF90_GLOBAL, "Since_Date", since_date), 'create_reservoir_restart_file', trim(out_file))
        call check(nf90_put_att(ncid, NF90_GLOBAL, "DTCT", DTCT), 'create_reservoir_restart_file', trim(out_file))

        call check(nf90_enddef(ncid), 'create_reservoir_restart_file', trim(out_file))

    end subroutine create_reservoir_restart_file

    subroutine open_reservoir_restart_file_for_read(hydro_restart_file_name, reservoir_restart_file_passed, NF_NOWRITE, ncid)

        character(len=*), intent(in)   :: hydro_restart_file_name, reservoir_restart_file_passed
        integer, intent(in)            :: NF_NOWRITE
        integer, intent(out)           :: ncid

        character(len = 32) :: time_stamp
        character(len = 14) :: reservoir_file_prefix
        character(len = 64) :: reservoir_restart_file_name

        if (reservoir_restart_file_passed == '') then
            read (hydro_restart_file_name(11: ), *) time_stamp
            reservoir_file_prefix = 'reservoir_rst.'
            reservoir_restart_file_name = reservoir_file_prefix//time_stamp
        else
            reservoir_restart_file_name = reservoir_restart_file_passed
        end if

        call check(nf90_open(trim(reservoir_restart_file_name), NF_NOWRITE, ncid), &
        "open_reservoir_restart_file_for_read", trim(reservoir_restart_file_name))

    end subroutine open_reservoir_restart_file_for_read


    subroutine close_reservoir_restart_file(ncid)
        integer, intent(in) :: ncid
        call check(nf90_close(ncid), 'close_reservoir_restart_file')

    end subroutine close_reservoir_restart_file

end module module_reservoir_io
