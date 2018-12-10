module read_timslice_data


    implicit none



contains








subroutine setup_read_timeslice()

    implicit none



    !integer,           intent(in) :: hydroDT ! the number of seconds of hydro advance from lsmTime
    !integer :: ff
    integer :: ii, tt, oo
    character(len=19) :: hydroTime, obsHydroTime ! hydro model time and corresponding observation
    !character(len=19), dimension(nObsTimes) :: obsTimes ! obs times in the current window
    integer :: oldDiff, nShiftLeft

    logical, allocatable, dimension(:) :: theMask
    integer, allocatable, dimension(:) :: whObsMiss
    logical, allocatable, dimension(:) :: obsTimeStrAllocated

    integer :: nObsMiss
    integer :: did=1  !! jlm: assuming did=1
    integer :: nWhObsMiss

    integer :: nGages, nCellsInR, cc, nLinkAff, nStatic, iiImage


end subroutine setup_read_timeslice




end module read_timslice_data




