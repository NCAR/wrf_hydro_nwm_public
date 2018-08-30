program reservoir_unit_tests
    use module_levelpool_tests
    use module_levelpool

    implicit none

    logical :: rv = .false.

    rv = test_levelpool()

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

        real :: x, y, z = 0.0
        integer :: m = 0
        CHARACTER(20) :: first
        integer :: total_non_levelpool_reservoirs, index_non_levelpool_reservoirs, q_type
        integer :: ios, reservoir_index
        character(len=200) :: line
        character(len=200), allocatable :: non_levelpool_reservoirs(:)

        integer, allocatable :: all_reservoirs(:)
        integer :: total_reservoirs = 20
        allocate(all_reservoirs(total_reservoirs))
        all_reservoirs = 1


        print *, "calling init for levelpool_struct"
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 12., 14., 16., 18., 20)

        print *, "testing data in levelpool_struct"
        call_status = levelpool_data_info(levelpool_reservoir_data)


        ! Test file i/o
        print *, "testing file i/o"


        ! open(unit=1, file="test_input.txt")

        open(unit=1, file="test_input.txt", iostat=ios)
        if ( ios /= 0 ) stop "Error opening file data.dat"

        total_non_levelpool_reservoirs = 0

        do
            read (1, *, iostat=ios) index_non_levelpool_reservoirs, q_type
            if (ios /= 0) exit

            print *, "index_non_levelpool_reservoirs: ", index_non_levelpool_reservoirs
            print *, "q_type: ", q_type

            all_reservoirs(index_non_levelpool_reservoirs) = q_type

            !total_non_levelpool_reservoirs = total_non_levelpool_reservoirs + 1
        end do


        do reservoir_index = 1, total_reservoirs
            print*, all_reservoirs(reservoir_index)
        end do


        !print*, "File contains ", total_non_levelpool_reservoirs, "commands"

        !allocate(non_levelpool_reservoirs(total_non_levelpool_reservoirs))

        !rewind(1)

        !do index_non_levelpool_reservoirs = 1, total_non_levelpool_reservoirs
        !    read(1, '(A)') non_levelpool_reservoirs(index_non_levelpool_reservoirs)



        !end do

        !close(1)


        !do index_non_levelpool_reservoirs = 1, total_non_levelpool_reservoirs
        !    print*, non_levelpool_reservoirs(index_non_levelpool_reservoirs)
        !end do


        !read (1,*) x, y, z
        !read (1,*) m, n, first

        !print *, "testing file i/o"
        !print *, "x: ", x
        !print *, "y: ", y
        !print *, "m: ", m
        !print *, "first: ", first

    end function test_levelpool

end program




