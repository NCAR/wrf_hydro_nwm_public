module module_read_q_type

    implicit none

contains

    !
    subroutine read_q_type(total_lakes, q_type_array)
        implicit none

        integer, intent(in) :: total_lakes
        integer, intent(inout), dimension(total_lakes) :: q_type_array
        integer :: reservoir_index, q_type, ios

        open(unit=1, file="q_types.txt", iostat=ios)            !Is this the right format we want to do???
        if ( ios /= 0 ) stop "Error opening file q_types.txt"

        do
            read (1, *, iostat=ios) reservoir_index, q_type   !Is this the right format we want to do???
            if (ios /= 0) exit

            q_type_array(reservoir_index) = q_type
        end do

    end subroutine read_q_type

end module module_read_q_type
