program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_COMPONENT_NAME, BMI_MAX_VAR_NAME
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_TYPE_NAME, BMI_MAX_UNITS_NAME
  use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
  use bmi_modflow_mod, only : modflow6, bmi_modflow, BMI_LENCOMPONENTNAME
  use iso_c_binding, only : c_char, C_NULL_CHAR
  use iso_fortran_env, only: output_unit
  use mf6bmiUtil, only:  BMI_LENVARADDRESS
  use mf6bmiGrid, only: get_grid_nodes_per_face
  use mpi
  implicit none

  type(bmi_wrf_hydro_nwm) :: wrf_hydro
  type(bmi_modflow) :: modflow

  character(len=BMI_MAX_COMPONENT_NAME), pointer :: model_name
  character(len=256), pointer :: mf_model_name
  character(len=BMI_MAX_VAR_NAME) :: time_unit
  character(len=BMI_MAX_VAR_NAME) :: mf_time_unit
  double precision :: wrfh_end_time, wrfh_current_time, wrfh_time_step
  double precision :: mf_end_time, mf_current_time, mf_time_step
  double precision :: current_time, end_time
  double precision :: time_step, time_step_conv
  integer :: i, bmi_status

  ! soldrain
  integer :: soldrain_grid, soldrain_rank, soldrain_size
  real, allocatable :: soldrain(:,:), soldrain_flat(:)
  real, allocatable :: soldrain_flat_daysum(:), soldrain_flat_daysum_flip(:)
  real :: soldrainavesum, dxdy=250. ! this needes to be input automatically
  integer, allocatable :: soldrain_grid_shape(:)
  integer :: soldrain_grid_shape_const(2)

  ! moddrain
  integer :: moddrain_grid, moddrain_rank, moddrain_size
  real, allocatable :: moddrain(:,:), SIMVALS_flipped(:,:)
  real, allocatable :: moddrain_flat(:), moddrain_flat_daysum(:)
  integer, allocatable :: moddrain_grid_shape(:)
  integer :: moddrain_grid_shape_const(2)

  ! modflow
  integer :: modflow_output_item_count
  integer :: x_grid, x_rank, x_size
  integer, allocatable :: x_grid_shape(:)
  integer :: x_grid_shape_const(1)
  integer :: nx, ny, ii, jj, kk
  double precision, allocatable :: x(:,:), x_flat(:)
  double precision, allocatable :: SIMVALS_flat(:), SIMVALS_flat_flipped(:)
  integer :: rch_grid, rch_rank
  double precision, allocatable :: rch_flat(:), rch_flat_flipped(:)
  double precision, allocatable :: grid_x(:), grid_y(:)

  ! double precision, allocatable :: x(:,:), x_flat(:)
  ! integer :: rch_grid, rch_rank
  ! double precision, allocatable :: rch_flat(:), &
  !      sy_flat(:), ss_flat(:), &
  !      top_flat(:), SIMVALS_flat(:), &
  !      SIMVALS_flat_flipped(:), rch_flat_flipped(:)

  ! MPI
  integer :: WRF_HYDRO_COMM, MODFLOW_COMM, comm
  integer :: ierr, rank, np, color
  logical :: wrf_hydro_rank, modflow_rank

  ! "to get working" additions
  integer, allocatable :: nodes_per_face(:)
  character(len=2024) :: msg

  print *, "----------------------------------------"
  print *, "   Starting WRF-Hydro/MODFLOW BMI ...   "
  print *, "----------------------------------------"

  comm = MPI_COMM_WORLD
  wrf_hydro = wrf_hydro_nwm()
  modflow = modflow6()

  call MPI_Init(ierr)
  call stat_check(wrf_hydro%parallel_initialize(comm))
  call stat_check(modflow%parallel_initialize(comm))


  ! error stop "DEBUGGING STOP"
  ! print *, "--- after parallel initialize ---"
  ! call stat_check(wrf_hydro%get_component_name(model_name))
  ! call stat_check(modflow%get_component_name(mf_model_name))

  ! print * , "--- Starting ", trim(model_name), " and ", trim(mf_model_name), " ---"

  ! ! call MPI_Init(ierr)

  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, np, ierr)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! initialize model
  call stat_check(wrf_hydro%initialize("no config file"))
  call stat_check(modflow%initialize(""))

  ! remove debug statement

  msg = "rank : initialized wrf-hydro and modflow"
  call print_parallel(rank, np, msg)

  ! get timing components
  call stat_check(wrf_hydro%get_start_time(wrfh_current_time), .false.)
  call stat_check(wrf_hydro%get_end_time(wrfh_end_time), .false.)
  call stat_check(wrf_hydro%get_time_step(wrfh_time_step), .false.)
  call stat_check(wrf_hydro%get_time_units(time_unit))

  call stat_check(modflow%get_start_time(mf_current_time), .false.)
  call stat_check(modflow%get_time_step(mf_time_step))
  ! call stat_check(modflow%get_end_time(mf_end_time), .false.)
  call stat_check(modflow%get_time_step(mf_time_step), .false.)

  if (rank == 0) then
     print *, "wrf-hydro: start, end, step =", wrfh_current_time, &
          wrfh_end_time, wrfh_time_step
     print *, "modflow: start, step =", mf_current_time, &
          mf_time_step
  end if

  ! print *, "modflow_output_item_count", modflow_output_item_count

  ! --- setup soldrain variables
  call stat_check(wrf_hydro%get_var_grid("soldrain", soldrain_grid))
  call stat_check(wrf_hydro%get_grid_rank(soldrain_grid, soldrain_rank))
  call stat_check(wrf_hydro%get_grid_shape(soldrain_grid, soldrain_grid_shape))
  soldrain_grid_shape_const = soldrain_grid_shape
  call stat_check(wrf_hydro%get_grid_size(soldrain_grid, soldrain_size))

  ! --- setup moddrain variables
  call stat_check(wrf_hydro%get_var_grid("moddrain", moddrain_grid))
  call stat_check(wrf_hydro%get_grid_rank(moddrain_grid, moddrain_rank))
  call stat_check(wrf_hydro%get_grid_shape(moddrain_grid, moddrain_grid_shape))
  moddrain_grid_shape_const = moddrain_grid_shape
  call stat_check(wrf_hydro%get_grid_size(moddrain_grid, moddrain_size))

  ! --- setup modflow x variable
  call stat_check(modflow%get_var_grid("X", x_grid))
  call stat_check(modflow%get_grid_rank(x_grid, x_rank))
  call stat_check(modflow%get_grid_shape(x_grid, x_grid_shape))
  x_grid_shape_const = x_grid_shape
  call stat_check(modflow%get_grid_size(x_grid, x_size))

  ! --- setup modflow recharge variable
  call stat_check(modflow%get_var_grid("RECHARGE", rch_grid))
  call stat_check(modflow%get_grid_rank(rch_grid, rch_rank))
  call stat_check(modflow%get_grid_x(rch_grid, grid_x))
  call stat_check(modflow%get_grid_y(rch_grid, grid_y))

  nx = size(grid_x) - 1
  ny = size(grid_y) - 1

  allocate(x_flat(x_size))
  allocate(rch_flat(x_size))
  allocate(rch_flat_flipped(x_size))
  allocate(soldrain_flat(soldrain_size))
  allocate(soldrain_flat_daysum(soldrain_size))
  allocate(soldrain_flat_daysum_flip(x_size))
  allocate(soldrain(soldrain_grid_shape(1), soldrain_grid_shape(2)))
  allocate(moddrain(moddrain_grid_shape(1), moddrain_grid_shape(2)))
  allocate(SIMVALS_flipped(moddrain_grid_shape(1), moddrain_grid_shape(2)))

  ! print *, rank, "after alloc variables"
  ! call MPI_Barrier(MPI_COMM_WORLD, ierr)
  ! error stop "MOD VARS DEBUGGING"

  end_time = 4

  if (rank == 0) then
     print *, "TESTING: Setting end_time to", end_time
  end if

  if (rank == 0) then
     print *, "wrf_hydro: Setting current_time ", current_time
     print *, "wrf_hydro: Setting end_time     ", end_time
     print *, "wrf_hydro: Setting time_step    ", time_step
     print *, " "
     print *, "modflow:   Setting mf_current_time to ", mf_current_time
     print *, "modflow:   Setting mf_time_step to    ", mf_time_step

     print *, "x_grid       , x_rank      ", x_grid, x_rank
     print *, "x_grid_shape , x_size      ", x_grid_shape, x_size
     print *, "size(grid_x) , size(grid_y)", size(grid_x), size(grid_y)
     print *, " "
  end if

  do while (current_time < end_time)
     ! update models
     call stat_check(wrf_hydro%update())
     call stat_check(modflow%update())
     ! update current_time
     call stat_check(wrf_hydro%get_current_time(current_time))
     call stat_check(modflow%get_current_time(mf_current_time))
     call stat_check(modflow%get_time_step(mf_time_step))
     time_step_conv = time_step / mf_time_step

     soldrainavesum = 0.
     soldrain_flat_daysum(:) = 0.
     soldrain_flat_daysum_flip(:) = 0.

     ! get current values
     call stat_check(modflow%get_value("X", x_flat))
     call stat_check(modflow%get_value("RECHARGE", rch_flat))

     call stat_check(modflow%get_grid_flipped("SIMVALS", SIMVALS_flat_flipped))
     ! 1-D to 2-D before setting WRF-Hydro grid
     ! DRN is negative in MODFLOW
     SIMVALS_flipped = reshape(-SIMVALS_flat_flipped, moddrain_grid_shape_const)

     call stat_check(wrf_hydro%get_value("soldrain", soldrain_flat))

     soldrainavesum = soldrainavesum + SUM(soldrain_flat)/size(soldrain_flat)
     soldrain_flat_daysum = soldrain_flat_daysum + soldrain_flat

     if (rank == 0) then
        print *, " "
        print *, "****************************************"
     end if
     do i=0,np
        if (i == rank) then
           print *, "--- Values for rank ", rank, "---"
           print *, "wrf_hydro: Setting current_time ", current_time
           print *, "wrf_hydro: Setting end_time     ", end_time
           print *, "wrf_hydro: Setting time_step    ", time_step
           print *, " "
           print *, "modflow:   Setting mf_current_time to ", mf_current_time
           print *, "modflow:   Setting mf_time_step to    ", mf_time_step

           print *, " "
           print *, "X ave: ", SUM(x_flat)/size(x_flat)
           print *, "RCHA ave     : ", SUM(rch_flat)/size(x_flat)*dxdy*dxdy
           print *, "RCHA min, max: ", minval(rch_flat)*dxdy*dxdy, maxval(rch_flat)*dxdy*dxdy

           print *, "SIMVALS_flipped ave     : ", SUM(SIMVALS_flat_flipped)/size(soldrain_flat)
           print *, "SIMVALS_flipped min, max: ", minval(SIMVALS_flat_flipped), maxval(SIMVALS_flat_flipped)

           print *, "soldrain ave:             ", SUM(soldrain_flat)/size(soldrain_flat)
           print *, "soldrain sum of ave:      ", soldrainavesum
           print *, "soldrain_flat_daysum ave: ", &
                SUM(soldrain_flat_daysum)/size(soldrain_flat_daysum)
        end if
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
     end do
     if (rank == 0) then
        print *, " "
        print *, "****************************************"
        print *, " "
     end if

     do while (current_time < mf_current_time .and. &
          current_time < end_time)
        call stat_check(wrf_hydro%get_current_time(current_time))
        call stat_check(modflow%get_current_time(mf_current_time))
        time_step_conv = time_step / mf_time_step
        write(msg,*) "[", int(current_time), "/", int(mf_current_time), "]", &
             " time_steps =", real(time_step), real(mf_time_step), &
             "conv", real(time_step_conv)
        call print_parallel(rank, np, msg)

        call stat_check(wrf_hydro%get_value("soldrain", soldrain_flat))
        ! soldrain = reshape(soldrain_flat, soldrain_grid_shape_const)
        soldrainavesum = soldrainavesum + SUM(soldrain_flat)/size(soldrain_flat)
        soldrain_flat_daysum = soldrain_flat_daysum + soldrain_flat
        print *, rank, ": soldrain ave: ", SUM(soldrain_flat)/size(soldrain_flat)
        print *, rank, ": soldrain sum of ave: ", soldrainavesum
        print *, rank, ": soldrain_flat_daysum ave: ", &
             SUM(soldrain_flat_daysum)/size(soldrain_flat_daysum)

        !unit change: m3/hour to m -----> I think it is m3/hour to m/hour
        moddrain = (SIMVALS_flipped*24./dxdy/dxdy) * time_step_conv
        call stat_check(wrf_hydro%set_value("moddrain", pack(moddrain, .true.)))


        ! update current_time
        call stat_check(wrf_hydro%update())
        print *, " "
     end do

     if (rank == 0) then
        print *, " "
        print *, "==========="
        print *, " "
     end if
     do i=0,np
        if (i == rank) then
           print *, rank, "rank : soldrain_flat_daysum ave unit: m3perhour: ", &
                SUM(soldrain_flat_daysum*dxdy*dxdy/24./1.E3)/size(soldrain_flat_daysum)
           print *, rank, "rank : soldrain_flat_daysum ave unit: mperhour:  ", &
                SUM(soldrain_flat_daysum/24./1.E3)/size(soldrain_flat_daysum)
           print *, rank, "rank: moddrain_flat        ave unit: mperhour:  ", &
                SUM(SIMVALS_flat_flipped/dxdy/dxdy)/size(SIMVALS_flat_flipped)
           print *, rank, "rank: RCHA                 ave unit: mperhour:  ", &
                SUM(rch_flat)/size(x_flat)
           print *, " "
        end if
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
     end do
     if (rank == 0) then
        print *, "==========="
        print *, " "
     end if

     kk=1
     do ii = ny, 1, -1
        do jj = 1, nx
           soldrain_flat_daysum_flip(kk)=soldrain_flat_daysum((ii-1)*nx+jj)
           kk = kk + 1
        end do
     end do

     call stat_check(modflow%set_value("RECHARGE", soldrain_flat_daysum_flip/24./1.E3))

  end do


  call stat_check(modflow%finalize())
  call stat_check(wrf_hydro%finalize())

  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  if (rank == 0) then
     print *, "--- Model Run Finished ---"
  end if

contains

  subroutine print_parallel(rank, np, msg)
    integer, intent(in) :: rank, np
    character(len=*), intent(in) :: msg
    integer :: i

    do i=0,np
       if (i == rank) then
          print *, rank, " rank :", trim(msg)
       end if
       call MPI_Barrier(MPI_COMM_WORLD, ierr)
    end do
  end subroutine print_parallel

end program wrf_hydro_nwm_bmi_driver
