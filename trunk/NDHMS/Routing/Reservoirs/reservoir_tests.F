program reservoir_unit_tests
    use module_levelpool_tests
    use module_levelpool
    use module_machine_learning_tests
    use module_machine_learning

       !include 'mpif.h'


    implicit none

    logical :: rv = .false.




       ! This is the name of the data file we will create.
       character (len = *), parameter :: FILE_NAME = "simple_xy_par.nc"

       ! We are writing 2D data.
       integer, parameter :: NDIMS = 2

       ! When we create netCDF files, variables and dimensions, we get back
       ! an ID for each one.
       integer :: ncid, varid, dimids(NDIMS)
       integer :: x_dimid, y_dimid

       ! These will tell where in the data file this processor should
       ! write.
       integer :: start(NDIMS), count(NDIMS)

       ! This is the data array we will write. It will just be filled with
       ! the rank of this processor.
       integer, allocatable :: data_out(:)

       ! MPI stuff: number of processors, rank of this processor, and error
       ! code.
       integer :: p, my_rank, ierr

       ! Loop indexes, and error handling.
       integer :: x, y, stat

       ! Initialize MPI, learn local rank and total number of processors.
       call MPI_Init(ierr)
       call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
       call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)

       ! Create some pretend data. We just need one row.
       allocate(data_out(p), stat = stat)
       if (stat .ne. 0) stop 3
       do x = 1, p
          data_out(x) = my_rank
       end do


print *, 'before create'

print *, MPI_COMM_WORLD

print *, MPI_INFO_NULL

       ! Create the netCDF file. The NF90_NETCDF4 flag causes a
       ! HDF5/netCDF-4 file to be created. The comm and info parameters
       ! cause parallel I/O to be enabled.

       !call check( nf90_create(FILE_NAME, NF90_NETCDF4, ncid, comm = MPI_COMM_WORLD, &
       !     info = MPI_INFO_NULL) )

        call check( nf90_create(FILE_NAME, OR(NF90_CLOBBER, NF90_NETCDF4), ncid, comm = MPI_COMM_WORLD, &
            info = MPI_INFO_NULL) )



       !call check( nc_create_par(FILE_NAME, NF90_NETCDF4, ncid, comm = MPI_COMM_WORLD, &
       !     info = MPI_INFO_NULL) )



print *, 'after create'


       ! Define the dimensions. NetCDF will hand back an ID for
       ! each. Metadata operations must take place on all processors.
       call check( nf90_def_dim(ncid, "x", p, x_dimid) )
       call check( nf90_def_dim(ncid, "y", p, y_dimid) )

       ! The dimids array is used to pass the IDs of the dimensions of
       ! the variables. Note that in fortran arrays are stored in
       ! column-major format.
       dimids =  (/ y_dimid, x_dimid /)

       ! Define the variable. The type of the variable in this case is
       ! NF90_INT (4-byte integer).
       call check( nf90_def_var(ncid, "data", NF90_INT, dimids, varid) )

       ! End define mode. This tells netCDF we are done defining
       ! metadata. This operation is collective and all processors will
       ! write their metadata to disk.
       call check( nf90_enddef(ncid) )

       ! Write the pretend data to the file. Each processor writes one row.
       start = (/ 1, my_rank + 1/)
       count = (/ p, 1 /)
       call check( nf90_put_var(ncid, varid, data_out, start = start, &
            count = count) )

       ! Close the file. This frees up any internal netCDF resources
       ! associated with the file, and flushes any buffers.
       call check( nf90_close(ncid) )

       ! Free my local memory.
       deallocate(data_out)

       ! MPI library must be shut down.
       call MPI_Finalize(ierr)

       if (my_rank .eq. 0) print *, "*** SUCCESS writing example file ", FILE_NAME, "! "








    rv = test_levelpool()


    !rv = .false.

    rv = test_machine_learning()














    contains

    !------------------------------------------------------------------------------!
    !                              test_levelpool()                                !
    ! this function verifies that the constructor for the levelpool type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_levelpool() result(rv)
        implicit none

        logical rv                        ! test result

        type (levelpool_struct) :: levelpool_reservoir_data
        real :: water_elevation = 2.
        integer :: call_status = 0

        print *, "calling init for levelpool_struct"
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 12., 14., 16., 18., 20)

        print *, "testing data in levelpool_struct"
        call_status = levelpool_data_info(levelpool_reservoir_data)

    end function test_levelpool




    !------------------------------------------------------------------------------!
    !                              test_machine_learning()                                !
    ! this function verifies that the constructor for the machine_learning type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_machine_learning() result(rv)
        implicit none

        logical rv                        ! test result

        type (machine_learning_struct) :: machine_learning_reservoir_data

        type (machine_learning_model_struct) :: machine_learning_model_data

        integer :: call_status = 0

        real :: outflow, water_elevation

        water_elevation = 0.0

        print *, "calling init for machine_learning_struct"
        !call machine_learning_reservoir_data%init("2017-08-25_17:24:22" , 1, 166758723)
        !call machine_learning_reservoir_data%init("2017-08-25_17:24:22" , 1, 1233515)
        call machine_learning_reservoir_data%init(water_elevation, 0.0, "2017-09-29_17:24:22" , 1, 166758723)



        print *, "calling init for machine_learning_model_struct"
        !call machine_learning_reservoir_data%init("2017-08-25_17:24:22" , 1, 166758723)
        call machine_learning_model_data%init(2)



        print *, "testing data in machine_learning_struct"
        call_status = machine_learning_data_info(machine_learning_reservoir_data)


        print *, "testing data in machine_learning_model_struct"
        call_status = machine_learning_model_data_info(machine_learning_model_data)


        print *, "calling release for machine_learning_struct"
        !call machine_learning_reservoir_data%run_machine_learning_release(reservoir_data, previous_timestep_inflow, &
        !inflow, lateral_inflow, water_elevation, outflow, routing_period)

        call machine_learning_reservoir_data%run_release(0.0, &
        192.46272, 0.0, water_elevation, outflow, 86400.0)

        print *, "outflow"
        print *, outflow


    !TEST FOR NEGATVIE INFLOW AND THINGS THAT OUTPUT THE PRE/POST COND WARNS/MODS FOR TIMESTEP/UPDATE

    end function test_machine_learning




       subroutine check(status)
         integer, intent ( in) :: status

         if(status /= nf90_noerr) then
           print *, trim(nf90_strerror(status))
           stop 2
         end if
       end subroutine check


end program




