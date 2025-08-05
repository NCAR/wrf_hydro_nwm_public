program call_python_test_driver
  use, intrinsic :: iso_c_binding, only: c_int, c_double
  use call_py_fSCA, only: ml_fSCA
  implicit none
  integer :: i, j
  integer(c_int), parameter :: nx = 4, ny = 4
  real(c_double), dimension(nx, ny) :: fSCA, T2D, LWDOWN, &
       SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon

  fSCA = -1
  T2D = 1
  LWDOWN = 1
  SWDOWN = 1
  U2D = 1
  V2D = 1
  day_of_year = 72
  HGT = 100
  slope = 0
  aspect = 0

  do i=1,nx
     lat(i,:) = [37, 38, 39, 40]
  end do
  do j=1,ny
     lon(:,j) = [107, 106, 105, 104]
  end do

  call ml_fSCA(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
       HGT, slope, aspect, lat, lon, nx, ny)

  print *, "Fortran's fSCA:"
  do i=1,nx
  print *, fSCA(i,:)
  end do
  print *, "FIN"
end program call_python_test_driver
