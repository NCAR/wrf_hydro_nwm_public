program reservoir_unit_tests
    !use module_levelpool_tests
    !use module_levelpool
    use module_persistence_levelpool_hybrid_tests
    use module_persistence_levelpool_hybrid

    implicit none

    logical :: rv1, rv2          ! test result

    rv1 = .false.
    rv2 = .false.

    !rv1 = test_levelpool()
    rv1 = .true.


    rv2 = test_persistence_levelpool_hybrid()

    if (rv1 .and. rv2) then
        print *, "========================================================================"
        print *, 'All Reservoir Tests Passed'
        print *, "========================================================================"

    else
        print *, "========================================================================"
        print *, 'Not All Reservoir Tests Passed'
        print *, "========================================================================"
    end if


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
    !                              test_levelpool()                                !
    ! this function verifies that the constructor for the levelpool type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_persistence_levelpool_hybrid() result(rv)

        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (persistence_levelpool_hybrid_struct) :: persistence_levelpool_hybrid_reservoir_data

        real :: outflow, water_elevation

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.

        print *, "calling init for persistence_levelpool_hybrid_struct"
        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 123.829002380371, 63.6739971160889 , &
        0.4, 10.0, 50.3799997965495, 0.1, 1.0, 66.0199966430664, 0.9, 166758723, "hybrid_persistence_levelpool_ACF.nc", &
        "2010-10-01_07:00:00", ".", 12, 86400)


        print *, "testing data in persistence_levelpool_hybrid_struct"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)


        !print *, "calling release for persistence_levelpool_hybrid_struct"

        !call persistence_levelpool_hybrid_reservoir_data%run_release(0.0, &
        !192.46272, 0.0, water_elevation, outflow, 300.0)

        !print *, "outflow"
        !print *, outflow

        !if (outflow == 120.170197) then
        !    rv3 = .true.
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Passed'
        !    print *, "========================================================================"
        !else
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Failed'
        !    print *, 'Outflow should be 120.170197'
        !    print *, "========================================================================"
        !end if


        !if (rv1 .and. rv2 .and. rv3) then
        !    rv = .true.
        !end if

        if (rv1) then
            rv = .true.
        end if


    end function test_persistence_levelpool_hybrid


! stop the job due to the fatal error.
      subroutine HYDRO_stop(msg)
#ifdef MPP_LAND
        use module_mpp_land
#endif
        character(len=*) :: msg
        integer :: ierr
        ierr = 1
#ifndef NCEP_WCOSS
!#ifdef HYDRO_D  !! PLEASE NEVER UNCOMMENT THIS IFDEF, it's just one incredibly useful string.
      write(6,*) "The job is stopped due to the fatal error. ", trim(msg)
      call flush(6)
!#endif
#else
     write(*,*) "FATAL ERROR: ", trim(msg)
     write(78,*) "FATAL ERROR: ", trim(msg)
      call flush(78)
      close(78)
#endif
#ifdef MPP_LAND
#ifndef HYDRO_D
      print*, "---"
      print*, "FATAL ERROR! Program stopped. Recompile with environment variable HYDRO_D set to 1 for enhanced debug information."
      print*, ""
#endif

!        call mpp_land_sync()
!        write(my_id+90,*) msg
!        call flush(my_id+90)

         call mpp_land_abort()
         call MPI_finalize(ierr)
#else
         stop "FATAL ERROR: Program stopped. Recompile with environment variable HYDRO_D set to 1 for enhanced debug information."
#endif

     return
     end  subroutine HYDRO_stop




end program
