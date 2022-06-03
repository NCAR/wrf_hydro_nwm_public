module state_module
  implicit none

  type state_type
     !SNOW VARIABLES
     REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  TSNOXY    ! snow temperature [K] ** REFACTOR THIS!
     REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  ZSNSOXY   ! snow layer depth [m] ** REFACTOR THIS!
     REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SNICEXY   ! snow layer ice [mm] ** REFACTOR THIS!
     REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SNLIQXY   ! snow layer liquid water [mm] ** REFACTOR THIS!
     REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNOW      ! snow water equivalent [mm] ** (sometime) PROGNOSTIC VARIABLE
     REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNOWH     ! physical snow depth [m] ** (sometime) PROGNOSTIC VARIABLE
   contains
     procedure :: init => init_undefined
  end type state_type

contains
  !Are we actually initializing?
  subroutine init_undefined(this)
    use config_base, only: noah_lsm
    implicit none
    
    class(state_type) :: this
    integer, parameter :: NSNOW = 3 !As definined in module_NoahMP,hrldas_driver.F. TODO Getting from a config later
    REAL, PARAMETER :: undefined_real = 9.9692099683868690E36 ! TODO Getting from a config later
    integer :: xstart, xend, ystart, yend, nsoil
    
    xstart = noah_lsm%xstart
    xend = noah_lsm%xend
    ystart = noah_lsm%ystart
    yend = noah_lsm%yend
    nsoil = noah_lsm%nsoil
    
    ALLOCATE ( this%TSNOXY    (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND), source = undefined_real )  ! snow temperature [K]
    ALLOCATE ( this%ZSNSOXY   (XSTART:XEND,-NSNOW+1:NSOIL,YSTART:YEND), source = undefined_real )  ! snow layer depth [m]
    ALLOCATE ( this%SNICEXY   (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND), source = undefined_real )  ! snow layer ice [mm]
    ALLOCATE ( this%SNLIQXY   (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND), source = undefined_real )  ! snow layer liquid water [mm]
    ALLOCATE ( this%SNOW      (XSTART:XEND,YSTART:YEND), source = undefined_real )  ! snow water equivalent [mm]
    ALLOCATE ( this%SNOWH     (XSTART:XEND,YSTART:YEND), source = undefined_real )  ! physical snow depth [m]
    
  end subroutine init_undefined

end module state_module
