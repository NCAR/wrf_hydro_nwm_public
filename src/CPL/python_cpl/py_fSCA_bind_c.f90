module call_py_fSCA
  use, intrinsic :: iso_c_binding, only: c_int, c_double
  implicit none

  interface
     subroutine py_ml_fSCA_array(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, &
          day_of_year, HGT, slope, aspect, lat, lon, nx, ny) &
          bind(c, name="py_ml_fSCA_array")
       use, intrinsic :: iso_c_binding, only: c_double, c_int
       real(c_double), dimension(nx, ny), intent(inout) :: fSCA
       real(c_double), dimension(nx, ny), intent(in) :: T2D, LWDOWN, &
            SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
       integer(c_int), intent(in) :: nx, ny
     end subroutine py_ml_fSCA_array

     subroutine py_ml_fSCA_scalar(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, &
          day_of_year, HGT, slope, aspect, lat, lon) &
          bind(c, name="py_ml_fSCA_scalar")
       use, intrinsic :: iso_c_binding, only: c_double, c_int
       real(c_double), intent(inout) :: fSCA
       real(c_double), intent(in) :: T2D, LWDOWN, &
            SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
     end subroutine py_ml_fSCA_scalar

  end interface

  interface ml_fSCA
     module procedure ml_fSCA_scalar   ! all scalars
     module procedure ml_fSCA_array    ! 2-D arrays
  end interface

contains
  subroutine ml_fSCA_array(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
                HGT, slope, aspect, lat, lon, nx, ny)
    real, dimension(nx, ny), intent(inout) :: fSCA
    real, dimension(nx, ny), intent(in) :: T2D, LWDOWN, &
         SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
    integer, intent(in) :: nx, ny
    real(c_double), dimension(nx, ny)  :: FSCA_c, T2D_c, LWDOWN_c, SWDOWN_c, U2D_c, V2D_c
    real(c_double), dimension(nx, ny)  :: HGT_c, lat_c, lon_c
    real(c_double), dimension(nx, ny)  :: aspect_c
    real(c_double), dimension(nx, ny)  :: slope_c, day_of_year_c
    integer(c_int)  :: nx_c, ny_c

    fSCA_c = -1
    T2D_c = real(T2D, c_double)
    LWDOWN_c = real(LWDOWN, c_double)
    SWDOWN_c = real(SWDOWN, c_double)
    U2D_c = real(U2D, c_double)
    V2D_c = real(V2D, c_double)
    day_of_year_c = real(day_of_year, c_double)
    HGT_c = real(HGT, c_double)
    slope_c= real(slope, c_double)
    aspect_c= real(aspect, c_double)
    lat_c = real(lat, c_double)
    lon_c = real(lon, c_double)
    nx_c = real(nx, c_double)
    ny_c = real(ny, c_double)

    call py_ml_fSCA_array(fSCA_c, T2D_c, LWDOWN_c, SWDOWN_c, U2D_c, V2D_c, &
              day_of_year_c, HGT_c, slope_c, aspect_c, lat_c, lon_c, nx_c, ny_c)

    fSCA = real(fSCA_c)
    if (any(fSCA == -1.0)) error stop "py_ml_fSCA returned bad value"

  end subroutine ml_fSCA_array

  subroutine ml_fSCA_scalar(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
                HGT, slope, aspect, lat, lon)
    real, intent(inout) :: fSCA
    real, intent(in) :: T2D, LWDOWN, &
         SWDOWN, U2D, V2D, day_of_year, HGT, slope, aspect, lat, lon
    real(c_double)  :: FSCA_c, T2D_c, LWDOWN_c, SWDOWN_c, U2D_c, V2D_c
    real(c_double)  :: HGT_c, lat_c, lon_c
    real(c_double)  :: aspect_c
    real(c_double)  :: slope_c
    real(c_double)  :: day_of_year_c

    fSCA_c = -1.0
    T2D_c = real(T2D, c_double)
    LWDOWN_c = real(LWDOWN, c_double)
    SWDOWN_c = real(SWDOWN, c_double)
    U2D_c = real(U2D, c_double)
    V2D_c = real(V2D, c_double)
    day_of_year_c = real(day_of_year, c_double)
    HGT_c = real(HGT, c_double)
    slope_c= real(slope, c_double)
    aspect_c= real(aspect, c_double)
    lat_c = real(lat, c_double)
    lon_c = real(lon, c_double)

    call py_ml_fSCA_scalar(fSCA_c, T2D_c, LWDOWN_c, SWDOWN_c, U2D_c, V2D_c, &
              day_of_year_c, HGT_c, slope_c, aspect_c, lat_c, lon_c)

    fSCA = real(fSCA_c)
    if (fSCA == -1.0) error stop "py_ml_fSCA returned bad value"

  end subroutine ml_fSCA_scalar
end module call_py_fSCA
