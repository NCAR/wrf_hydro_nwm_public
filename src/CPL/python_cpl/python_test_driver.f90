program call_python_test_driver
  use call_py_fSCA, only: ml_fSCA
  implicit none
  real :: fSCA, T2D, LWDOWN, &
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

  lat = 35.0612092158282
  lon = -111.783129501614

  call ml_fSCA(fSCA, T2D, LWDOWN, SWDOWN, U2D, V2D, day_of_year, &
       HGT, slope, aspect, lat, lon)

  print *, "Fortran's fSCA:", fSCA
  print *, "FIN"
end program call_python_test_driver
