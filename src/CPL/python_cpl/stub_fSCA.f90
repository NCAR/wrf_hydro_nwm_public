module call_py_fSCA
  use, intrinsic :: iso_c_binding, only: c_int, c_double
  implicit none
contains
  subroutine ml_fSCA(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
                HGT, slope, aspect, lat, lon, nx, ny)
    real(c_double), dimension(nx, ny), intent(inout) :: fSCA, T2D, LWDOWN, &
         SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
    integer(c_int), intent(in) :: nx, ny
    error stop &
         "Error: calling ml_fSCA without building CMake with -DPYTHON_FSCA"
  end subroutine ml_fSCA
end module call_py_fSCA
