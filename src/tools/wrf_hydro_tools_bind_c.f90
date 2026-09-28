module wrf_hydro_tools_bind_c
  use iso_c_binding, only: c_int
  implicit none

contains
  ! Get the start and end indices of each rank's subdomain for np MPI ranks.
  function get_domain_decomposition_c(np, x_np, y_np, nx, ny, &
       start_x, start_y, end_x, end_y) &
       result(res) &
       bind(c, name="get_domain_decomposition")
    use wrf_hydro_tools, only: get_domain_decomposition
    integer(c_int), intent(in) :: np
    integer(c_int), intent(out) :: x_np, y_np, nx, ny
    integer(c_int), intent(out) :: start_x(np), start_y(np)
    integer(c_int), intent(out) :: end_x(np), end_y(np)
    integer(c_int) :: res
    call get_domain_decomposition(np, x_np, y_np, nx, ny, &
         start_x, start_y, end_x, end_y)
    res = 1
  end function get_domain_decomposition_c
end module wrf_hydro_tools_bind_c
