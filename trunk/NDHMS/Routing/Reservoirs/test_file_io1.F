


        ! open(unit=1, file="test_input.txt")

        open(unit=1, file="test_input.txt", iostat=ios)
        if ( ios /= 0 ) stop "Error opening file data.dat"

        total_non_levelpool_reservoirs = 0

        do
            read(1, "(A)", iostat=ios) line
            if (ios /= 0) exit




            total_non_levelpool_reservoirs = total_non_levelpool_reservoirs + 1
        end do


        print*, "File contains ", total_non_levelpool_reservoirs, "commands"

        allocate(non_levelpool_reservoirs(total_non_levelpool_reservoirs))

        rewind(1)

        do index_non_levelpool_reservoirs = 1, total_non_levelpool_reservoirs
            read(1, '(A)') non_levelpool_reservoirs(index_non_levelpool_reservoirs)



        end do

        close(1)


        do index_non_levelpool_reservoirs = 1, total_non_levelpool_reservoirs
            print*, non_levelpool_reservoirs(index_non_levelpool_reservoirs)
        end do


        !read (1,*) x, y, z
        !read (1,*) m, n, first

        !print *, "testing file i/o"
        !print *, "x: ", x
        !print *, "y: ", y
        !print *, "m: ", m
        !print *, "first: ", first
