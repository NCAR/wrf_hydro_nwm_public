program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_COMPONENT_NAME, BMI_MAX_VAR_NAME
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_TYPE_NAME, BMI_MAX_UNITS_NAME
  use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
  use bmi_modflow_mod, only : modflow6, bmi_modflow, BMI_LENCOMPONENTNAME
  use iso_c_binding, only : c_char, C_NULL_CHAR
  use mf6bmiUtil, only:  BMI_LENVARADDRESS
  use mf6bmiGrid, only: get_grid_nodes_per_face
  use BaseModelModule, only: BaseModelType, GetBaseModelFromList
  use BndModule, only: BndType, GetBndFromList
  use ListsModule, only: basemodellist
  use GwfModule, only: GwfModelType
  use KindModule, only: I4B

  implicit none

  class(BaseModelType), pointer :: baseModel => null()
  type(GwfModelType), pointer :: gwfmodel => null()
  class(BndType), pointer :: packobj => null()
  class(BndType), pointer :: packobjrch => null()
  type(bmi_wrf_hydro_nwm) :: wrf_hydro
  type(bmi_modflow) :: modflow

  character(len=BMI_MAX_COMPONENT_NAME), pointer :: model_name
  character(len=256), pointer :: mf_model_name
  character(len=BMI_MAX_VAR_NAME) :: time_unit
  character(len=BMI_MAX_VAR_NAME) :: mf_time_unit
  double precision :: end_time, current_time, mf_current_time
  double precision :: time_step, mf_time_step, time_step_conv
  integer :: i, bmi_status
  integer(I4B) :: ngwfpack, iterm, ip, nxny, idx1, idx2

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
  integer :: x_grid, x_rank, x_size, rch_size
  integer :: rch_grid, rch_rank
  integer, allocatable :: x_grid_shape(:)
  integer :: x_grid_shape_const(1)
  integer :: nx, ny, ii, jj, kk, mm
  double precision, allocatable :: x(:,:), x_flat(:)
  double precision, allocatable :: SIMVALS_flat(:), SIMVALS_flat_flipped(:)  
  double precision, allocatable :: rch_flat(:), rch_soldrain_red(:)
  double precision, allocatable :: grid_x(:), grid_y(:)

  print *, "----------------------------------------"
  print *, "   Starting WRF-Hydro/MODFLOW BMI ...   "  
  print *, "----------------------------------------"


  wrf_hydro = wrf_hydro_nwm()
  modflow = modflow6()
  call stat_check(wrf_hydro%get_component_name(model_name))
  call stat_check(modflow%get_component_name(mf_model_name))

  ! print * , "--- Starting ", trim(model_name), " and ", trim(mf_model_name), " ---"

  ! initialize model
  call stat_check(wrf_hydro%initialize("no config file"))
  call stat_check(modflow%initialize(""))

  ! get timing components
  call stat_check(wrf_hydro%get_start_time(current_time))
  call stat_check(wrf_hydro%get_end_time(end_time))
  call stat_check(wrf_hydro%get_time_step(time_step))
  call stat_check(modflow%get_time_step(mf_time_step))
  call stat_check(wrf_hydro%get_time_units(time_unit))
  ! call stat_check(wrf_hydro%get_start_time(mf_current_time))
  call stat_check(modflow%get_start_time(mf_current_time))
  ! call stat_check(modflow%get_time_units(mf_time_unit)) ! hardcoded, need to update
  ! call stat_check(modflow%get_output_item_count(modflow_output_item_count))
  print *, "time steps:", time_step, mf_time_step

  ! print *, "modflow_output_item_count", modflow_output_item_count

  ! --- setup soldrain variables
  call stat_check(wrf_hydro%get_var_grid("soldrain", soldrain_grid))
  call stat_check(wrf_hydro%get_grid_rank(soldrain_grid, soldrain_rank))
  call stat_check(wrf_hydro%get_grid_shape(soldrain_grid, soldrain_grid_shape))
  soldrain_grid_shape_const = soldrain_grid_shape
  call stat_check(wrf_hydro%get_grid_size(soldrain_grid, soldrain_size))
  ! --- done setting up soldrain variables

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

  call stat_check(modflow%get_var_grid("RECHARGE", rch_grid))
  call stat_check(modflow%get_grid_rank(rch_grid, rch_rank))
  call stat_check(modflow%get_grid_size(rch_grid, rch_size))
  call stat_check(modflow%get_grid_x(rch_grid, grid_x))
  call stat_check(modflow%get_grid_y(rch_grid, grid_y))  

  nx = size(grid_x) - 1
  ny = size(grid_y) - 1
  nxny = nx*ny

  allocate(x_flat(x_size))
  allocate(rch_flat(rch_size))
  allocate(rch_soldrain_red(rch_size))
  allocate(soldrain_flat(soldrain_size))
  allocate(soldrain_flat_daysum(soldrain_size))
  allocate(soldrain_flat_daysum_flip(soldrain_size))
  allocate(soldrain(soldrain_grid_shape(1), soldrain_grid_shape(2)))
  allocate(moddrain(moddrain_grid_shape(1), moddrain_grid_shape(2)))
  allocate(SIMVALS_flipped(moddrain_grid_shape(1), moddrain_grid_shape(2)))

  ! allocate(x(x_grid_shape(1), x_grid_shape(2)))
  ! --- done setting up x variable

  ! end_time = 4
  ! print *, "TESTING: Setting end_time to", end_time
  print *, " "
  print *, "****************************************"
  print *, " "
  print *, "wrf_hydro: Setting current_time ", current_time
  print *, "wrf_hydro: Setting end_time     ", end_time
  print *, "wrf_hydro: Setting time_step    ", time_step
  print *, " "
  print *, "modflow:   Setting mf_current_time to ", mf_current_time
  print *, "modflow:   Setting mf_time_step to    ", mf_time_step
  
  print *, "x_grid       , x_rank      ", x_grid, x_rank
  print *, "x_size, rch_size, soldrain_size", x_size, rch_size, soldrain_size
  print *, "x_grid_shape , x_size      ", x_grid_shape, x_size
  print *, "size(grid_x) , size(grid_y)", size(grid_x), size(grid_y)
  print *, " "
  print *, "****************************************"
  print *, " "

  baseModel => GetBaseModelFromList(basemodellist, 1)

  select type (baseModel)
  type is (GwfModelType)
	   gwfmodel => baseModel
  end select
  ngwfpack = gwfmodel%bndlist%Count()

  do ip = 1, ngwfpack
  
     packobj => GetBndFromList(gwfmodel%bndlist, ip)

     print *, "gwfmodel%bndlist%Count(), ngwfpack ******", ngwfpack	 
	 print *, "trim(packobj%packName) ******", trim(packobj%packName)

     if (trim(packobj%packName) == "RCH") then
	    packobjrch => GetBndFromList(gwfmodel%bndlist, ip)
	 end if
  end do


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

	print *, " "
    print *, "****************************************"
	print *, " "
    print *, "wrf_hydro: Setting current_time ", current_time
    print *, "wrf_hydro: Setting end_time     ", end_time
    print *, "wrf_hydro: Setting time_step    ", time_step
    print *, " "
    print *, "modflow:   Setting mf_current_time to ", mf_current_time
    print *, "modflow:   Setting mf_time_step to    ", mf_time_step

	print *, " "
	print *, "X ave: ", SUM(x_flat)/size(x_flat)
	print *, "RCHA ave     : ", SUM(rch_flat)/size(rch_flat)*dxdy*dxdy
	print *, "RCHA min, max: ", minval(rch_flat)*dxdy*dxdy, maxval(rch_flat)*dxdy*dxdy

	print *, "SIMVALS_flipped ave     : ", SUM(SIMVALS_flat_flipped)/size(soldrain_flat)
	print *, "SIMVALS_flipped min, max: ", minval(SIMVALS_flat_flipped), maxval(SIMVALS_flat_flipped)	

	print *, "soldrain ave:             ", SUM(soldrain_flat)/size(soldrain_flat)
	print *, "soldrain sum of ave:      ", soldrainavesum
	print *, "soldrain_flat_daysum ave: ", &
		      SUM(soldrain_flat_daysum)/size(soldrain_flat_daysum)

	print *, " "
    print *, "****************************************"
	print *, " "

    do while (current_time < mf_current_time .and. &
          current_time < end_time)
        call stat_check(wrf_hydro%get_current_time(current_time))
        call stat_check(modflow%get_current_time(mf_current_time))
        time_step_conv = time_step / mf_time_step
        print *, "[", int(current_time), "/", int(mf_current_time), "]", &
                 " time_steps =", real(time_step), real(mf_time_step), &
                 "conv", real(time_step_conv)

        call stat_check(wrf_hydro%get_value("soldrain", soldrain_flat))
        ! soldrain = reshape(soldrain_flat, soldrain_grid_shape_const)
        soldrainavesum = soldrainavesum + SUM(soldrain_flat)/size(soldrain_flat)
		soldrain_flat_daysum = soldrain_flat_daysum + soldrain_flat
	    print *, "soldrain ave: ", SUM(soldrain_flat)/size(soldrain_flat)
	    print *, "soldrain sum of ave: ", soldrainavesum
		print *, "soldrain_flat_daysum ave: ", &
		          SUM(soldrain_flat_daysum)/size(soldrain_flat_daysum)

		!unit change: m3/hour to m -----> I think it is m3/hour to m/hour
        moddrain = (SIMVALS_flipped*24./dxdy/dxdy) * time_step_conv
        call stat_check(wrf_hydro%set_value("moddrain", pack(moddrain, .true.)))

        ! --- update soldrain value
        ! soldrain = x_flat * time_step_conv
        ! soldrain = soldrain + 0.01 ! test update_value
        ! call stat_check(wrf_hydro%set_value("soldrain", pack(soldrain, .true.)))

        ! update current_time
        call stat_check(wrf_hydro%update())
		print *, " "
    end do

	print *, " "
    print *, "==========="
	print *, " "
	print *, "soldrain_flat_daysum ave unit: m3perhour: ", &
				SUM(soldrain_flat_daysum*dxdy*dxdy/24./1.E3)/size(soldrain_flat_daysum)
	print *, "soldrain_flat_daysum ave unit: mperhour:  ", &
				SUM(soldrain_flat_daysum/24./1.E3)/size(soldrain_flat_daysum)
	print *, "moddrain_flat        ave unit: mperhour:  ", &
				SUM(SIMVALS_flat_flipped/dxdy/dxdy)/size(SIMVALS_flat_flipped)
	print *, "RCHA                 ave unit: mperhour:  ", SUM(rch_flat)/size(rch_flat)
	print *, " "
    print *, "==========="
	print *, " "

	soldrain_flat_daysum_flip(:) = 0.
	rch_soldrain_red(:) = 0.

    kk=1
    do ii = ny, 1, -1
    do jj = 1, nx
       soldrain_flat_daysum_flip(kk)=soldrain_flat_daysum((ii-1)*nx+jj)
       kk = kk + 1
	end do	
	end do

    if (rch_size < nxny) then

    do mm = 1, rch_size
	   idx1 = packobjrch%nodelist(mm)
	   idx2 = packobjrch%dis%get_nodeuser(idx1)
	   
	   rch_soldrain_red(mm) = soldrain_flat_daysum_flip(idx2)

    end do
       call stat_check(modflow%set_value("RECHARGE", rch_soldrain_red/24./1.E3))	
	else
       call stat_check(modflow%set_value("RECHARGE", soldrain_flat_daysum_flip/24./1.E3))
    end if

  end do


  call stat_check(modflow%finalize())
  call stat_check(wrf_hydro%finalize())
  print *, "--- FIN ---"

end program wrf_hydro_nwm_bmi_driver
