module wrf_hydro_tools
  implicit none

contains

  ! developed to run by an external program
  ! report domain decomposition for whole domain
  subroutine get_domain_decomposition(np, x_np, y_np, nx, ny, &
       start_x, start_y, end_x, end_y)
    implicit none
    integer, intent(in) :: np
    integer, intent(out) :: x_np, y_np, nx, ny
    integer, intent(out) :: start_x(np), start_y(np), end_x(np), end_y(np)
    integer :: i, j, rank, index
    do i = 1, np
       rank = i - 1 ! convert to MPI ranking
       call get_rank_domain_decomposition(rank, np, x_np, y_np, &
            nx, ny, start_x(i), start_y(i))
    end do

    ! calculate ending points
    do j=1, y_np
       do i=1, x_np
          index = (j-1)*x_np + (i-1)
          ! print*, index, ": i", i, "x_np", x_np
          if (i < x_np) then
             end_x(index+1) = start_x(index+1+1) - 1
          else
             end_x(index+1) = nx
          end if

          if (j < y_np) then
             end_y(index+1) = start_y(index+1+x_np) - 1
          else
             end_y(index+1) = ny
          end if

          print*, "index", index, "i", i, "j", j, "end_x", end_x(index+1)
          ! if (i < x_np) then
          ! end if
          ! end_x(i) = start_x(i+1)
       end do
    end do

    print *, "fin"
  end subroutine get_domain_decomposition


  ! developed to run by an external program
  ! report domain decomposition for specific rank given np total ranks
  subroutine get_rank_domain_decomposition(rank, np, x_np, y_np, global_nx, global_ny, start_x, start_y)
    use module_hrldas_netcdf_io, only: read_dim
    use config_base, only: noah_lsm, init_noah_lsm_and_wrf_hydro
    use module_mpp_land, only: mpp_land_par_ini, getnx_ny
    implicit none

    integer, intent(in) :: rank, np
    integer, intent(out) :: x_np, y_np, global_nx, global_ny, start_x, start_y
    integer :: in_global_nx, in_global_ny, aggfactrt
    integer :: i
    ! DEBUGGING: cyclic not used right now
    integer :: ndim, ierr
    ! integer, dimension(0:1) :: dims, coords
    ! logical :: cyclic(0:1), reorder
    ! cyclic(2) = [.false., .false.]
    ! reorder = .false.

    character(len=128), parameter :: hrldas_setup_file = "namelist.hrldas"
    integer :: ix_tmp, jx_tmp

    ! mpp_land_par_ini variables
    integer :: left_id, right_id, up_id, down_id
    integer :: left_right_np, up_down_np ! define total process in two dimensions.
    integer :: left_right_p ,up_down_p ! the position of the current process in the logical topography.
    integer :: local_nx,local_ny
    integer :: global_rt_nx, global_rt_ny
    integer :: local_rt_nx,local_rt_ny,rt_aggfactrt
    integer :: nprocs, n, j, nx, ny, max

    ! calculate_start_p variables
    integer :: local_startx, local_starty, t_nx, t_ny


    call init_noah_lsm_and_wrf_hydro()

    ! module_NoahMP_hrldas_driver.F calls these
    call read_dim(noah_lsm%hrldas_setup_file, ix_tmp, jx_tmp)
#ifdef HYDRO_D
    print *, "Opened ", trim(noah_lsm%hrldas_setup_file)
    print*, "ix_tmp =", ix_tmp, "jx_tmp =", jx_tmp
    print *, "np =", np
#endif
    ! REPLICATED LATER
    ! call mpp_land_par_ini(1,ix_tmp,jx_tmp,1)
    ! call getLocalXY(ix_tmp,jx_tmp,noah_lsm%xstart, &
    !                   noah_lsm%ystart,noah_lsm%xend,noah_lsm%yend)



    ! get hrldas_setup_file


    ! --- from read_dim()



    ! --- from MPP_LAND_INIT(ix_tmp,jx_tmp) ---
    ! --- from LOG_MAP2d ---
    ! call getNX_NY(np, left_right_np,up_down_np)
    ! - setting in variables
    nprocs = np
    nx = left_right_np
    ny = up_down_np
    ! - setting global variables
    global_nx = ix_tmp
    global_ny = jx_tmp
    ! - getNX_NY body
    n = global_nx * global_ny
    if( nprocs .ge. n ) then
       error stop "Error: number of processes greater than number of cells in the land grid"
    end if

    max = nprocs
    do j = 1, nprocs
       if( mod(nprocs,j) .eq. 0 ) then
          i = nprocs/j
          if( i .le. global_nx ) then
             if( abs(i-j) .lt. max) then
                if( j .le. global_ny ) then
                   max = abs(i-j)
                   nx = i
                   ny = j
                end if
             end if
          end if
       end if
    end do
    ! out variables
    left_right_np = nx
    up_down_np = ny
    ! --- end getNX_NY ---

    !   ### get the row and column of the current process in the logical topography.
    !   ### left --> right, 0 -->left_right_np -1
    !   ### up --> down, 0 --> up_down_np -1
    left_right_p = mod(rank , left_right_np)
    up_down_p = rank / left_right_np

    !   ### get the neighbors.  -1 means no neighbor.
    down_id = rank - left_right_np
    up_id =   rank + left_right_np
    if (up_down_p .eq. 0) down_id = -1
    if (up_down_p .eq. (up_down_np-1) ) up_id = -1

    left_id = rank - 1
    right_id = rank + 1
    if (left_right_p .eq. 0) left_id = -1
    if (left_right_p .eq. (left_right_np-1) ) right_id =-1

    ! --- DEBUG: testing removal for printing
    ! IO_id = 0
    ! ndim = 2
    ! dims(0) = up_down_np      ! rows
    ! dims(1) = left_right_np   ! columns

    ! call MPI_Cart_create(HYDRO_COMM_WORLD, ndim, dims, &
    !    cyclic, reorder, cartGridComm, ierr)

    ! call MPI_CART_GET(cartGridComm, 2, dims, cyclic, coords, ierr)

    ! p_up_down = coords(0)
    ! p_left_right = coords(1)
    ! np_up_down = up_down_np ! not needed in report_domain_decomp
    ! np_left_right = left_right_np ! not needed in report_domain_decomp
    ! --- end log_map2d ---


    ! --- from MPP_LAND_PAR_INI ---
    ! - MPP_LAND_PAR_INI(over_lap,in_global_nx,in_global_ny,AGGFACTRT)
    ! - call MPP_LAND_PAR_INI(1,ix_tmp,jx_tmp,1) in module_NoahMP_hrldas_driver.F
    in_global_nx = ix_tmp
    in_global_ny = jx_tmp
    aggfactrt = 1


    global_nx = in_global_nx
    global_ny = in_global_ny
    rt_aggfactrt = aggfactrt
    global_rt_nx = in_global_nx * aggfactrt
    global_rt_ny = in_global_ny * aggfactrt

    local_nx = int(global_nx / left_right_np)
    !if (global_nx .ne. (local_nx*left_right_np) ) then
    if (mod(global_nx, left_right_np) .ne. 0) then
       do i = 1, mod(global_nx, left_right_np)
          if(left_right_p .eq. i ) then
             local_nx = local_nx + 1
          end if
       end do
    end if

    local_ny = int(global_ny / up_down_np)
    !if (global_ny .ne. (local_ny * up_down_np) ) then
    if (mod(global_ny,up_down_np) .ne. 0 ) then
       do i = 1, mod(global_ny,up_down_np)
          if( up_down_p .eq. i) then
             local_ny = local_ny + 1
          end if
       end do
    end if

    local_rt_nx = local_nx*aggfactrt+2
    local_rt_ny = local_ny*aggfactrt+2
    if(left_id.lt.0) &
         local_rt_nx = local_rt_nx -1
    if(right_id.lt.0) &
         local_rt_nx = local_rt_nx -1
    if(up_id.lt.0) &
         local_rt_ny = local_rt_ny -1
    if(down_id.lt.0) &
         local_rt_ny = local_rt_ny -1

#ifdef HYDRO_D
    print *, ""
    print *, "rank", rank, "of", np
    print *, "left_right_np =", left_right_np, &
         "up_down_np =", up_down_np
    ! print *, "rank =", rank, "global_rt_nx =", global_rt_nx, &
    !      "global_rt_ny =", global_rt_ny
    print *, "rank =", rank, "global_nx =", global_nx, "global_ny =", global_ny
    print *, "rank =", rank, "local_nx =", local_nx, "local_ny =", local_ny
    ! print *, "rank =", rank, "local_rt_nx =", local_rt_nx, "local_rt_ny =", local_rt_ny
    ! print *, "rank =", rank,
#endif

    ! ! --- DEBUG: testing removal for printing
    ! ! call get_local_size(local_nx, local_ny,local_rt_nx,local_rt_ny)

    ! - calculate_start_p()
    local_startx = int(global_nx/left_right_np) * left_right_p+1
    local_starty = int(global_ny/up_down_np) * up_down_p+1

    t_nx = 0
    do i = 1, mod(global_nx,left_right_np)
       if(left_right_p .gt. i ) then
          t_nx = t_nx + 1
       end if
    end do
    local_startx = local_startx + t_nx

    t_ny = 0
    do i = 1, mod(global_ny,up_down_np)
       if( up_down_p .gt. i) then
          t_ny = t_ny + 1
       end if
    end do
    local_starty = local_starty + t_ny

    if(left_id .lt. 0) local_startx = 1
    if(down_id .lt. 0) local_starty = 1

    ! print *, "rank =", rank, "local_startx =", local_startx, &
    !      "local_starty =", local_starty

    ! set output variables
    x_np = left_right_np
    y_np = up_down_np
    start_x = local_startx
    start_y = local_starty


    ! ! call calculate_offset_vectors()

    ! ! in_global_nx = local_nx
    ! ! in_global_ny = local_ny
  end subroutine get_rank_domain_decomposition
end module wrf_hydro_tools
