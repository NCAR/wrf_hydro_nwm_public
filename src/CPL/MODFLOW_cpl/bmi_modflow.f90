submodule (bmi_modflow_mod) bmi_modflow_smod
  implicit none
  integer, parameter :: STUB_I = -1
  integer, dimension(1), parameter :: STUB_1D_I = [-1]
  double precision, parameter :: STUB_D = -1.0
  double precision, dimension(1), parameter :: STUB_1D_D = [-1.0]
  character(len=11), parameter :: STUB_C = "STUB RESULT"

  interface set_var_at_indices
     module procedure :: set_2d_var_at_indices_int
     module procedure :: set_3d_var_at_indices_int
     module procedure :: set_2d_var_at_indices_float
     module procedure :: set_3d_var_at_indices_float
     module procedure :: set_2d_var_at_indices_double
     module procedure :: set_3d_var_at_indices_double
  end interface set_var_at_indices

  interface index_to_multidim
     module procedure :: index_to_multidim_2d
     module procedure :: index_to_multidim_3d
  end interface index_to_multidim
contains

  ! --- BMI Function Implementation ---

  ! --------------------------------------------------------------------

  module procedure parallel_initialize
    use mf6xmi, only: xmi_initialize_mpi
    bmi_status = xmi_initialize_mpi(comm)
  end procedure ! parallel_initialize

  ! Get the name of the model.
  module procedure modflow_component_name
    use mf6bmi, only: bmi_get_component_name
    implicit none
    character(c_char) :: model_name(BMI_MAX_COMPONENT_NAME)
    character(len=256), allocatable, target :: f_name
    integer :: res, i
    integer, allocatable :: end(:)
    res = bmi_get_component_name(model_name)
    end = findloc(model_name, c_null_char)
    f_name = transfer(model_name(1:end(1)-1), f_name)
    name => f_name
    bmi_status = res
  end procedure ! modflow_component_name

  ! Perform startup tasks for the model.
  module procedure modflow_initialize
    use mf6bmi, only: bmi_initialize
    use SimVariablesModule, only: simfile
    use SimVariablesModule, only: simulation_mode
    use mpi
    integer :: unit, err
    logical :: found
    character(len=256) :: line
    character(len=64) :: foo
    integer :: s_start, s_end, s_len
    integer :: rank, ierr, i
    ! TODO: make sure the following would work
    ! if (trim(simulation_mode) /= 'parallel')
    !   bmi_status = bmi_initialize()
    ! end if

    ! GOALZ :: get third
    !   gwf6  ex-gwf-fhb.nam  ex-gwf-fhb
    !   gwf6  modflow_subset.nam  modflow_subset
    print *, "=== BMI: STARTING MODFLOW INIT ==="
    print *, "simulation_mode =", trim(simulation_mode)

    ! read simfile to extract component name
    open(newunit=unit, file=simfile, status='old', action='read', iostat=err)
    if (err .ne. 0) then
       print *, 'Error opening file: ', trim(simfile), " error:", err
       error stop 1
    end if

    found = .false.
    do
        read(unit, '(A)', iostat=err) line
        if (err .ne. 0) exit

        ! Check for "BEGIN models" line
        if (line == 'BEGIN models') then
           found = .true.
           exit
        end if
    end do

    if (found .eqv. .false.) then
       print *, "ERROR: Did not find 'BEGIN models' in", simfile
       error stop 1
    end if

    ! TODO: make sure the following would work
    ! if (trim(simulation_mode) == 'parallel')
    !   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    !   do i=0,rank
    !     read(unit, '(A)', iostat=err) line
    !   end do
    ! else
    !  read(unit, '(A)', iostat=err) line
    ! end if
    print *, "simulation_mode =", trim(simulation_mode)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    do i=0,rank
       read(unit, '(A)', iostat=err) line
    end do
    close(unit)

    ! handle component name
    s_end = len(trim(line))
    s_start = index(trim(line), " ", back=.true.)
    s_start = s_start + 1
    s_len = s_end-s_start + 1
    allocate(character(s_len) :: f_component_name)
    f_component_name = line(s_start:s_end)//c_null_char
    call capitalize_f_str(f_component_name, s_len)
    ! f_component_name = "EX-GWF-FHB"//c_null_char
    c_component_name = f_to_c_str(f_component_name )

    ! handle subcomponent, same for all
    ! allocate(character(len("")+1) :: f_subcomponent_name)
    ! f_subcomponent_name = ""//c_null_char
    ! c_subcomponent_name = f_to_c_str(f_subcomponent_name )
  end procedure ! modflow_initialize

  ! Advance the model one time step.
  module procedure modflow_update
    use mf6bmi, only: bmi_update
    bmi_status = bmi_update()
  end procedure ! modflow_update

  ! Perform teardown tasks for the model.
  module procedure modflow_finalize
    use mf6bmi, only: bmi_finalize
    bmi_status = bmi_finalize()
  end procedure ! modflow_finalize

  ! Count a model's input variables.
  module procedure modflow_input_item_count
    use mf6bmi, only: get_input_item_count
    integer(c_int) :: count_c
    bmi_status = get_input_item_count(count_c)
    count = count_c
  end procedure ! modflow_input_item_count

  ! Count a model's output variables.
  module procedure modflow_output_item_count
    use mf6bmi, only: get_output_item_count
    integer(c_int) :: count_c
    bmi_status = get_output_item_count(count_c)
    count = count_c
  end procedure ! modflow_output_item_count

  ! --------------------------------------------------------------------
  !  The functions below this need to be implemented
  ! --------------------------------------------------------------------

  ! Get the grid identifier for the given variable.
  module procedure modflow_var_grid
    bmi_status = BMI_SUCCESS
    select case(trim(name))
    case("X")
       grid = 1
    case("head")
       grid = 2
    case("RECHARGE")
       grid = 3
    case("SIMVALS")
       grid = 7
    case default
       grid = -1
       bmi_status = BMI_FAILURE
       print *, "WARNING: variable ", trim(name), " not found in modflow_var_grid"
    end select
  end procedure ! modflow_var_grid

  ! ! this function is need so that there isn't an object clash between the
  ! ! variable rank and the intrinsic function rank
  ! function get_grid_rank(grid) result(var_rank)
  !   integer :: var_rank
  !   var_rank = STUB_I
  !   ! bmi_status = BMI_FAILURE
  ! end function get_grid_rank

  ! ! this function is need so that there isn't an object clash between the
  ! ! variable shape and the intrinsic function shape
  ! function get_grid_shape(grid) result(grid_shape)
  !   integer :: grid_shape
  !   grid_shape = STUB_I
  !   ! bmi_status = BMI_FAILURE
  ! end function get_grid_shape

  ! ! this function is need so that there isn't an object clash between the
  ! ! variable size and the intrinsic function size
  ! function get_var_size(grid) result(var_size)
  !   integer :: var_size
  !   var_size = STUB_I
  !   ! bmi_status = BMI_FAILURE
  ! end function get_var_size

  ! Get the grid type as a string.
  module procedure modflow_grid_type
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_type

  ! Set new values for an integer model variable.
  module procedure modflow_set_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_int

  ! Set new values for a real model variable.
  module procedure modflow_set_float
    ! bmi_status = BMI_FAILURE
    use mf6bmi, only: set_value_double
    character(c_char), dimension(BMI_LENVARADDRESS) :: c_var_address
    type(c_ptr) :: c_var_ptr
    double precision, pointer :: f_var_ptr(:)
    double precision, target, allocatable, dimension(:) :: f_var
    integer, allocatable :: grid_shape(:)
    integer :: grid, grid_size
    bmi_status = this%get_var_grid(name, grid)
    bmi_status = get_modflow_var_address(c_var_address, grid)
    bmi_status = this%get_grid_size(grid, grid_size)
    allocate(f_var(grid_size))
    ! f_var = reshape(src, shape(f_var))
    f_var = pack(src, .true.)
    f_var_ptr => f_var
    c_var_ptr = c_loc(f_var_ptr)

    bmi_status = set_value_double(c_var_address, c_var_ptr)
    ! dest = pack(f_var, .true.)
  end procedure ! modflow_set_float

  ! Set new values for a double model variable.
  module procedure modflow_set_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_double

  ! Get a copy of values (flattened!) of the given integer variable.
  module procedure modflow_get_int
    use mf6bmi, only: get_value_int
    character(c_char), dimension(BMI_LENVARADDRESS) :: c_var_address
    type(c_ptr) :: c_var_ptr
    integer, pointer :: f_var_ptr(:)
    integer, target, allocatable, dimension(:) :: f_var
    integer, allocatable :: grid_shape(:)
    integer :: grid, grid_size
    bmi_status = this%get_var_grid(name, grid)
    bmi_status = get_modflow_var_address(c_var_address, grid)
    bmi_status = this%get_grid_size(grid, grid_size)
    print *, "There you go! grid_size of ibounds: ", grid, grid_size
    allocate(f_var(grid_size))
    f_var_ptr => f_var
    c_var_ptr = c_loc(f_var_ptr)

    bmi_status = get_value_int(c_var_address, c_var_ptr)
    dest = pack(f_var, .true.)
  end procedure ! modflow_get_int

  ! Get a copy of values (flattened!) of the given real variable.
  module procedure modflow_get_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_float

  ! Get a copy of values (flattened!) of the given double variable.
  module procedure modflow_get_double
    use mf6bmi, only: get_value_double
    character(c_char), dimension(BMI_LENVARADDRESS) :: c_var_address
    type(c_ptr) :: c_var_ptr
    double precision, pointer :: f_var_ptr(:)
    double precision, target, allocatable, dimension(:) :: f_var
    integer, allocatable :: grid_shape(:)
    integer :: grid, grid_size
    bmi_status = this%get_var_grid(name, grid)
    bmi_status = get_modflow_var_address(c_var_address, grid)
    bmi_status = this%get_grid_size(grid, grid_size)
    allocate(f_var(grid_size))
    f_var_ptr => f_var
    c_var_ptr = c_loc(f_var_ptr)

    bmi_status = get_value_double(c_var_address, c_var_ptr)
    dest = pack(f_var, .true.)
  end procedure ! modflow_get_double

  ! Get the grid flipped when array orientation is opposite between two components.
  ! here betweeh MODFLOW and WRF-Hydro  module procedure modflow_grid_flipped
  module procedure modflow_grid_flipped
    use KindModule, only: I4B
    use BaseDisModule, only: DisBaseType
    use DrnModule, only: DrnType
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
    use BndModule, only: BndType, GetBndFromList
    use GwfModule, only: GwfModelType
    use GwtModule, only: GwtModelType

    double precision, allocatable :: grid_x(:), grid_y(:)
    double precision, allocatable, dimension(:) :: src, src_, src_flipped
    character(len=LENBOUNDNAME) :: bndName
    integer(I4B) :: ngwfpack, iterm
    integer(I4B) :: grid, grid_size, nx, ny, i, ii, jj, kk, nxny, ip, idx1, idx2, idx3

    class(BaseModelType), pointer :: baseModel => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    class(BndType), pointer :: packobj => null()
    class(NumericalModelType), pointer :: numericalModel

    bmi_status = this%get_var_grid(name, grid)
    bmi_status = this%get_grid_size(grid, grid_size)

    bmi_status = this%get_grid_x(grid, grid_x)
    bmi_status = this%get_grid_y(grid, grid_y)

    nx = size(grid_x) - 1
    ny = size(grid_y) - 1
    nxny = nx*ny

    allocate(src(grid_size))

    print *, "nx, ny, ", nx, ny, grid_size

    if (name .eq. "SIMVALS") then
        bndName = "DRN"
    else
       print *, "ERROR: bndName", bndName, " does not support flipping!"
       error stop "bndName is not found"

    end if

    bmi_status = this%get_value("SIMVALS", src)
    print *, "SIMVALS ave, min, max: ", SUM(src)/size(src), minval(src), maxval(src)

    allocate(src_(nxny))
    allocate(src_flipped(nxny))
    allocate(dst(nxny))
    src_        = 0.
    src_flipped = 0.

    print *, "grid_size_red, grid_size_ful: ", grid_size, nxny

    print *, "basemodellist%Count: ", basemodellist%Count()

    do i = 1, basemodellist%Count()
      baseModel => GetBaseModelFromList(basemodellist, i)
      numericalModel => GetNumericalModelFromList(basemodellist, i)

      print *, "model name elements: ", baseModel%name

      select type (baseModel)
      type is (GwfModelType)
         gwfmodel => baseModel
      end select
    end do

    ngwfpack = gwfmodel%bndlist%Count()
    iterm = 1
    do ip = 1, ngwfpack
       packobj => GetBndFromList(gwfmodel%bndlist, ip)
       print *, "gwfmodel%bndlist%Count(), ngwfpack", ngwfpack

       if (trim(packobj%packName) == bndName) then

          print *, "iterm, ip: ", iterm, ip
          print *, "packobj%packName ", trim(packobj%packName)

         ! mapping reduced size array to full array
         if (grid_size < nxny) then

            do ii = 1, grid_size
               idx1 = packobj%nodelist(ii)


               ! if MODFLOW idomain is all 1 (box) these two function below are the same
               ! if there are inactive domain cell ----> two functions are different
               idx2 = packobj%dis%get_nodeuser(idx1)
               idx3 = packobj%dis%get_nodenumber(idx1, 0)

               if (idx2 .ne. idx3) print *, "ATTENTION! get_nodeuser AND get_nodenumber ARE DIFFERENT!", &
                    idx2, idx3
               src_(idx2) = src(ii)
            end do
            src = src_
         end if
      end if
      iterm = iterm + 1
    end do

    kk=1
    do ii = ny, 1, -1
    do jj = 1, nx
       src_flipped(kk)=src((ii-1)*nx+jj)
       kk = kk + 1
    end do
  end do

  dst = src_flipped

  end procedure ! modflow_grid_flipped

  ! Set integer values at particular (one-dimensional) indices.
  module procedure modflow_set_at_indices_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_at_indices_int

  ! Set real values at particular (one-dimensional) indices.
  module procedure modflow_set_at_indices_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_at_indices_float

  ! Set double values at particular (one-dimensional) indices.
  module procedure modflow_set_at_indices_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_at_indices_double

  ! Get integer values at particular (one-dimensional) indices.
  module procedure modflow_get_at_indices_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_at_indices_int

  ! Get real values at particular (one-dimensional) indices.
  module procedure modflow_get_at_indices_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_at_indices_float

  ! Get double values at particular (one-dimensional) indices.
  module procedure modflow_get_at_indices_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_at_indices_double

  ! Advance the model until the given time.
  module procedure modflow_update_until
    bmi_status = BMI_FAILURE
  end procedure ! modflow_update_until

  !---------------------------------------------------------------------
  ! Should update how this is handeled in main_hrldas_driver
  !---------------------------------------------------------------------
  ! Start time of the model.
  module procedure modflow_start_time
    use mf6bmi, only: get_start_time
    bmi_status = get_start_time(time)
  end procedure ! modflow_start_time

  ! End time of the model.
  module procedure modflow_end_time
    use mf6bmi, only: get_start_time
    bmi_status = get_start_time(time)
  end procedure ! modflow_end_time

  ! Time step of the model.
  module procedure modflow_time_step
    use mf6bmi, only: get_time_step
    bmi_status = get_time_step(time_step)
  end procedure ! modflow_time_step

  ! Time units of the model.
  module procedure modflow_time_units
    units = "d"
    bmi_status = BMI_SUCCESS
  end procedure ! modflow_time_units

  ! Current time of the model.
  module procedure modflow_current_time
    use mf6bmi, only: get_current_time
    bmi_status = get_current_time(time)
  end procedure ! modflow_current_time

  ! Get size of the given variable, in bytes.
  module procedure modflow_var_nbytes
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_nbytes

  ! Get number of dimensions of the computational grid.
  module procedure modflow_grid_rank
    use mf6xmi, only: get_var_rank
    character(kind=c_char), dimension(BMI_LENVARADDRESS) :: c_var_address
    integer(c_int) :: var_rank
    bmi_status = BMI_SUCCESS
    bmi_status = get_modflow_var_address(c_var_address, grid)
    if (bmi_status .ne. BMI_SUCCESS) &
         print *, "STOP, modflow get_var_address call unsuccessful"
    bmi_status = get_var_rank(c_var_address, var_rank)
    rank = var_rank
    if (bmi_status .ne. BMI_SUCCESS) &
         print *, "STOP, modflow get_var_rank call unsuccessful"
  end procedure ! modflow_grid_rank

  ! Get the dimensions of the computational grid.
  module procedure modflow_grid_shape
    use mf6bmi, only: get_var_shape
    character(kind=c_char), dimension(BMI_LENVARADDRESS) :: c_var_address
    integer :: grid_rank
    integer(c_int) :: var_rank
    integer(c_int), allocatable :: c_var_shape(:)

    bmi_status = get_modflow_var_address(c_var_address, grid)
    bmi_status = this%get_grid_rank(grid, grid_rank)
    allocate(c_var_shape(grid_rank))
    bmi_status = get_var_shape(c_var_address, c_var_shape)
    allocate(shape(grid_rank))
    shape = c_var_shape
  end procedure ! modflow_grid_shape

  ! Get the data type of the given variable as a string.
  module procedure modflow_var_type
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_type

  function calc_grid_size(grid_shape) result(size_i)
    integer, intent(in) :: grid_shape(:)
    integer :: size_i
    integer :: i
    size_i = 1
    do i=1,size(shape(grid_shape))
       size_i = size_i * grid_shape(i)
    end do
  end function calc_grid_size

  ! Get the total number of elements in the computational grid.
  module procedure modflow_grid_size
    integer, allocatable :: grid_shape(:)
    integer :: i
    bmi_status = this%get_grid_shape(grid, grid_shape)
    size = calc_grid_size(grid_shape)
  end procedure ! modflow_grid_size

  !---------------------------------------------------------------------
  ! Implemented but should be improved
  !---------------------------------------------------------------------

  ! List a model's input variables.
  module procedure modflow_input_var_names
    bmi_status = BMI_FAILURE
  end procedure ! modflow_input_var_names

  ! TODO: the following implementations are first takes for ldas, how do we add
  !       the other parts of the model?
  ! List a model's output variables.
  module procedure modflow_output_var_names
    use mf6bmi, only: get_output_var_names
    use mf6bmiUtil, only:  BMI_LENVARADDRESS
    ! ! get_output_var_names
    ! character(kind=c_char, len=1), allocatable :: names_c(:)
    ! character(len=BMI_MAX_VAR_NAME), target, allocatable :: names_f(:)
    integer :: count
    integer :: i, res, start, new_start, i_start, end_i
    ! integer, allocatable :: end_c(:)
    res = this%get_output_item_count(count)

    ! allocate(names_c(count * BMI_LENVARADDRESS))
    ! allocate(names_f(count))
    ! res = get_output_var_names(names_c)
    ! print *, "~~~~~~~ PRINTING VAR NAMES ~~~~~~~"
    ! start=1
    ! print *, names_c(1:9), "---", BMI_LENVARADDRESS
    ! ! print *, names_c(11:14), "---"

    ! do i=1,1
    !    end_c = index(names_c(1:100), c_null_char)
    ! end do
    ! i_start = 1
    ! do i=i_start,100
    !    if (end_c(i) == 1) then
    !       end_i=i-2
    !       exit
    !    end if
    ! end do
    ! print *, names_c(start:end_i)

    ! start = end_i + BMI_LENVARADDRESS
    ! i_start = start
    ! do i=i_start,100
    !    if (end_c(i) == 1) then
    !       end_i=i-2
    !       exit
    !    end if
    ! end do

    ! print *, names_c(start:end_i)
    ! print *, "DONE PRINTING"
    ! names => names_f
    bmi_status = BMI_SUCCESS
  end procedure ! modflow_output_var_names

  module procedure modflow_var_units
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_units

  ! Get a reference to the given integer variable.
  module procedure modflow_get_ptr_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_ptr_int

  ! Get a reference to the given real variable.
  module procedure modflow_get_ptr_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_ptr_float

  ! Get a reference to the given double variable.
  module procedure modflow_get_ptr_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_ptr_double

  ! Describe where a variable is located: node, edge, or face.
  module procedure modflow_var_location
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_locatio

  ! Get distance between nodes of the computational grid.
  module procedure modflow_grid_spacing
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_spacing

  ! Get coordinates of the origin of the computational grid.
  module procedure modflow_grid_origin
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_origin

  ! Get the x-coordinates of the nodes of a computational grid.
  module procedure modflow_grid_x
    use mf6bmiGrid, only: get_var_grid
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use KindModule, only: I4B
    use mf6bmiUtil, only: get_model_name
    use ConstantsModule, only: LENMODELNAME

    ! use mf6bmiGrid, only: bmif_get_var_grid
    character(c_char), dimension(BMI_LENVARADDRESS) :: c_var_address
    integer(kind=c_int) :: var_grid
    real(kind=c_double), allocatable :: c_grid_x(:)
    integer :: grid_rank
    integer(I4B), dimension(:), pointer, contiguous :: grid_shape_x
    ! integer(I4B), dimension(1) :: grid_shape_x
    integer(I4B) :: x_size, i
    character(len=LENMODELNAME) :: model_name

    bmi_status = get_modflow_var_address(c_var_address, grid)
    bmi_status = get_var_grid(c_var_address, var_grid)

    model_name = get_model_name(var_grid)

    bmi_status = this%get_grid_rank(grid, grid_rank)

    call mem_setptr(grid_shape_x, "MSHAPE", create_mem_path(model_name, 'DIS'))
    ! print *, "grid_shape_x: ", grid_shape_x

    x_size = grid_shape_x(size(grid_shape_x)) + 1
    ! print *, "grid_shape_x(size(grid_shape_x))", grid_shape_x(size(grid_shape_x)), x_size

    allocate(c_grid_x(x_size))
    allocate(x(x_size))

    c_grid_x(1:x_size) = [(i, i=0, x_size - 1)]
    x = c_grid_x
  end procedure ! modflow_grid_x

  ! Get the y-coordinates of the nodes of a computational grid.
  module procedure modflow_grid_y
    use mf6bmiGrid, only: get_var_grid
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use KindModule, only: I4B
    use mf6bmiUtil, only: get_model_name
    use ConstantsModule, only: LENMODELNAME

    ! use mf6bmiGrid, only: bmif_get_var_grid
    character(c_char), dimension(BMI_LENVARADDRESS) :: c_var_address
    integer(kind=c_int) :: var_grid
    real(kind=c_double), allocatable :: c_grid_y(:)
    integer :: grid_rank
    integer(I4B), dimension(:), pointer, contiguous :: grid_shape_y
    ! integer(I4B), dimension(1) :: grid_shape_y
    integer(I4B) :: y_size, i
    character(len=LENMODELNAME) :: model_name

    bmi_status = get_modflow_var_address(c_var_address, grid)
    bmi_status = get_var_grid(c_var_address, var_grid)

    model_name = get_model_name(var_grid)

    bmi_status = this%get_grid_rank(grid, grid_rank)

    call mem_setptr(grid_shape_y, "MSHAPE", create_mem_path(model_name, 'DIS'))
    ! print *, "grid_shape_y: ", grid_shape_y

    y_size = grid_shape_y(size(grid_shape_y) - 1) + 1
    ! print *, "grid_shape_y(size(grid_shape_y) - 1)", grid_shape_y(size(grid_shape_y) - 1), y_size

    allocate(c_grid_y(y_size))
    allocate(y(y_size))

    c_grid_y(1:y_size) = [(i, i=y_size - 1, 0, -1)]
    ! print *, c_grid_y

    y = c_grid_y
  end procedure ! modflow_grid_y

  ! Get the z-coordinates of the nodes of a computational grid.
  module procedure modflow_grid_z
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_z

  ! Get the number of nodes in an unstructured grid.
  module procedure modflow_grid_node_count
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_node_count

  ! Get the number of edges in an unstructured grid.
  module procedure modflow_grid_edge_count
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  module procedure modflow_grid_face_count
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_face_count

  ! Get the edge-node connectivity.
  module procedure modflow_grid_edge_nodes
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_edge_nodes

  ! Get the face-edge connectivity.
  module procedure modflow_grid_face_edges
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_face_edges

  ! Get the face-node connectivity.
  module procedure modflow_grid_face_nodes
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_face_nodes

  ! Get the number of nodes for each face.
  module procedure modflow_grid_nodes_per_face
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_nodes_per_face

  ! ------------------------------------
  ! Non-BMI procedures
  ! ------------------------------------

  ! --- following set of subroutines handles the conversion of 1d flat index
  !     to 2d or 3d dimensions ---
  subroutine index_to_multidim_3d(index, nx, ny, i, j, k)
    integer, intent(in) :: index, nx, ny
    integer, intent(out) :: i,j,k
    i = modulo(index-1, nx)
    j = modulo((index-1) / nx, ny)
    k = (index-1)  / (ny * nx)
    i = i + 1
    j = j + 1
    k = k + 1
  end subroutine index_to_multidim_3d

  subroutine index_to_multidim_2d(index, nx, i, j)
    integer, intent(in) :: index, nx
    integer, intent(out) :: i,j
    i = modulo(index-1, nx)
    j = (index-1) / nx
    i = i + 1
    j = j + 1
  end subroutine index_to_multidim_2d

  ! --- following set of subroutines set the indices of a 2d or 3d
  !     variable for its given type ---
  subroutine set_2d_var_at_indices_int(var, inds, src)
    integer, intent(inout) :: var(:,:)
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: index, ii, i, j, var_shape(2), nx
    var_shape = shape(var)
    nx = var_shape(1)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, i, j)
       var(i,j) = src(ii)
    end do
  end subroutine set_2d_var_at_indices_int

  subroutine set_3d_var_at_indices_int(var, inds, src)
    integer, intent(inout) :: var(:,:,:)
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: index, ii, i, j, k, var_shape(3), nx, ny
    var_shape = shape(var)
    nx = var_shape(1)
    ny = var_shape(2)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, ny, i, j, k)
       var(i,j,k) = src(ii)
    end do
  end subroutine set_3d_var_at_indices_int

  subroutine set_2d_var_at_indices_float(var, inds, src)
    real, intent(inout) :: var(:,:)
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: index, ii, i, j, var_shape(2), nx
    var_shape = shape(var)
    nx = var_shape(1)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, i, j)
       var(i,j) = src(ii)
    end do
  end subroutine set_2d_var_at_indices_float

  subroutine set_3d_var_at_indices_float(var, inds, src)
    real, intent(inout) :: var(:,:,:)
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: index, ii, i, j, k, var_shape(3), nx, ny
    var_shape = shape(var)
    nx = var_shape(1)
    ny = var_shape(2)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, ny, i, j, k)
       var(i,j,k) = src(ii)
    end do
  end subroutine set_3d_var_at_indices_float

  subroutine set_2d_var_at_indices_double(var, inds, src)
    double precision, intent(inout) :: var(:,:)
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: index, ii, i, j, var_shape(2), nx
    var_shape = shape(var)
    nx = var_shape(1)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, i, j)
       var(i,j) = src(ii)
    end do
  end subroutine set_2d_var_at_indices_double

  subroutine set_3d_var_at_indices_double(var, inds, src)
    double precision, intent(inout) :: var(:,:,:)
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: index, ii, i, j, k, var_shape(3), nx, ny
    var_shape = shape(var)
    nx = var_shape(1)
    ny = var_shape(2)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, ny, i, j, k)
       var(i,j,k) = src(ii)
    end do
  end subroutine set_3d_var_at_indices_double

  ! Model introspection.
  ! module procedure print_model_info
  ! end procedure ! print_model_info

  ! Check the status and update bmi_status if necessary
  module procedure stat_check
    if (bmi_status == BMI_FAILURE) print *, "MODFLOW WHY??"
    if (status .ne. BMI_SUCCESS) then
       print *, " --- WARNING!! BMI_STATUS FAILED ---"
       bmi_status = BMI_FAILURE
    end if
  end procedure

  ! Get memory use per array element, in bytes.
  module procedure modflow_var_itemsize
    use mf6xmi, only: get_var_itemsize
    ! integer :: res
    ! integer :: integer_var
    ! real :: real_var
    ! double precision :: double_var
    ! logical :: logical_var
    ! character(BMI_MAX_TYPE_NAME) :: var_type
    character(kind=c_char), dimension(BMI_LENVARADDRESS) :: c_var_address
    integer :: var_size
    bmi_status = get_var_itemsize(c_var_address, var_size)
    size = var_size
    ! res = this%get_var_type(name, var_type)
    ! select case(var_type)
    ! case("integer")
    !    size = sizeof(integer_var)
    ! case("real")
    !    size = sizeof(real_var)
    ! case("double precision")
    !    size = sizeof(double_var)
    ! case("logical")
    !    size = sizeof(logical_var)
    ! case default
    !    size = 0
    !    bmi_status = BMI_FAILURE
    ! end select
  end procedure ! modflow_var_itemsize

  function f_to_c_str(f_str) result(c_str)
    character(len=BMI_MAX_COMPONENT_NAME) :: f_str
    character(c_char) :: c_str(BMI_MAX_COMPONENT_NAME)
    integer :: i, name_len
    name_len = len(trim(f_str))
    do i=1,name_len
       c_str(i) = f_str(i:i)
    end do
    c_str(name_len+1) = C_NULL_CHAR
  end function f_to_c_str

  subroutine capitalize_f_str(f_str, length)
    character(len=BMI_MAX_COMPONENT_NAME), intent(inout) :: f_str
    integer, intent(in) :: length
    character :: a
    integer :: i, str_len
    do i = 1, length
       a = f_str(i:i)
       if (a >= 'a' .and. a <= 'z') then
          f_str(i:i) = achar(iachar(a) - 32)
       end if
    end do
  end subroutine capitalize_f_str

  ! components and variables are hardcoded right now for testing purposes
  function get_modflow_var_address(c_var_address, grid) result(bmi_status)
    use mf6xmi, only: get_var_address
    character(c_char), dimension(BMI_LENVARADDRESS), intent(out) :: c_var_address
    integer, intent(in) :: grid
    integer :: bmi_status
    character(c_char), allocatable :: c_var_name(:)
    character(len=:), allocatable :: f_var_name
    integer, dimension(:), allocatable :: s_end

    character(len=:), allocatable :: f_subcomponent_name
    character(c_char), allocatable :: c_subcomponent_name(:)

    if (grid == 1) then
       allocate(character(len("X")+1) :: f_var_name)
       allocate(character(len("")+1) :: f_subcomponent_name)
       f_var_name = "X"//c_null_char
       f_subcomponent_name = ""//c_null_char
    else if (grid == 2) then
       allocate(character(len("head")+1) :: f_var_name)
       f_var_name = "head"//c_null_char
    else if (grid == 3) then
       allocate(character(len("RECHARGE")+1) :: f_var_name)

       ! This (subcomponent/package name; here RCH) should be read
       ! from modflow_subset.nam (f_component_name//".nam")
       ! similar to f_component_name above

       allocate(character(len("RCH")+1) :: f_subcomponent_name)
       f_var_name = "RECHARGE"//c_null_char
       f_subcomponent_name = "RCH"//c_null_char
    else if (grid == 7) then
       allocate(character(len("SIMVALS")+1) :: f_var_name)
       allocate(character(len("DRN")+1) :: f_subcomponent_name)
       f_var_name = "SIMVALS"//c_null_char
       f_subcomponent_name = "DRN"//c_null_char
    else
       print *, "ERROR: grid", grid, "was not found"
       error stop "grid is not found"
    end if
    c_var_name = f_to_c_str(f_var_name )
    c_subcomponent_name = f_to_c_str(f_subcomponent_name )

    bmi_status = get_var_address(&
         c_component_name(1:len(f_component_name)), &
         c_subcomponent_name(1:len(f_subcomponent_name)), &
         c_var_name(1:len(f_var_name)), &
         c_var_address)
  end function
end submodule bmi_modflow_smod
