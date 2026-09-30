program print_domain_decomposition
  use wrf_hydro_tools, only: get_domain_decomposition
  implicit none
  integer, parameter :: np = 4
  integer :: x_np, y_np, nx, ny
  integer :: start_x(np), start_y(np), end_x(np), end_y(np)


  print *, "Reporting domain decomposition"

  call get_domain_decomposition(np, x_np, y_np, nx, ny, &
       start_x, start_y, end_x, end_y)

  print *, "np =", np, "x_np =", x_np, "y_np =", y_np
  print *, "nx =", nx
  print *, "start_x =", start_x
  print *, "end_x =  ", end_x
  print *, ""
  print *, "ny =", ny
  print *, "start_y =", start_y
  print *, "end_y =  ", end_y

  print *, "Finished reporting domain decomposition"
end program print_domain_decomposition
