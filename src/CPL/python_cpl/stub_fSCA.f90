module call_py_fSCA
  use, intrinsic :: iso_c_binding, only: c_int, c_double
  implicit none

  interface ml_fSCA
     module procedure ml_fSCA_scalar   ! all scalars
     module procedure ml_fSCA_array    ! 2-D arrays
  end interface
contains
  subroutine ml_fSCA_scalar(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
                HGT, slope, aspect, lat, lon)
    real, intent(inout) :: fSCA
    real, intent(in) :: T2D, LWDOWN, &
         SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
    error stop &
         "Error: calling ml_fSCA without building CMake with -DPYTHON_ML_FSCA"
  end subroutine ml_fSCA_scalar

  subroutine ml_fSCA_array(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
                HGT, slope, aspect, lat, lon, nx, ny)
    real, dimension(nx, ny), intent(inout) :: fSCA
    real, dimension(nx, ny), intent(in) :: T2D, LWDOWN, &
         SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
    integer, intent(in) :: nx, ny
    ! real(c_double), dimension(nx, ny), intent(inout) :: fSCA, T2D, LWDOWN, &
    !      SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
    ! integer(c_int), intent(in) :: nx, ny
    error stop &
         "Error: calling ml_fSCA without building CMake with -DPYTHON_ML_FSCA"
  end subroutine ml_fSCA_array
end module call_py_fSCA
