module call_py_fSCA
  use, intrinsic :: iso_c_binding, only: c_int, c_double
  implicit none

  interface
     subroutine py_ml_fSCA(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
          HGT, slope, aspect, lat, lon, nx, ny) &
          bind(c, name="py_ml_fSCA")
       use, intrinsic :: iso_c_binding, only: c_double, c_int
       real(c_double), dimension(nx, ny), intent(inout) :: fSCA
       real(c_double), dimension(nx, ny), intent(in) :: T2D, LWDOWN, &
            SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
       integer(c_int), intent(in) :: nx, ny
     end subroutine py_ml_fSCA
  end interface

contains
  subroutine ml_fSCA(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
                HGT, slope, aspect, lat, lon, nx, ny)
    real(c_double), dimension(nx, ny), intent(inout) :: fSCA, T2D, LWDOWN, &
         SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
    integer(c_int), intent(in) :: nx, ny
    call py_ml_fSCA(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
         HGT, slope, aspect, lat, lon, nx, ny)
  end subroutine ml_fSCA
end module call_py_fSCA
