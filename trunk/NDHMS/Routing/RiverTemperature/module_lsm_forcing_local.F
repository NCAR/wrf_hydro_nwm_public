

module module_lsm_forcing

	! -- Added by Reza
	! -- to module_lsm_forcing.F
	! --------------------------------------------------------------------------

	implicit none
        !INTEGER, parameter :: IX   = 3
        !INTEGER, parameter :: JX   = 3
        !Didn't want to use the IXRT and JXRT 
        !INTEGER, parameter :: IXRT = 6
        !INTEGER, parameter :: JXRT = 6

	type :: weather_2D_inputs
		!INTEGER :: IX,JX, IXRT, JXRT
                INTEGER :: IX, JX
		REAL, DIMENSION(:,:), allocatable   :: T2		! Air temperature
		REAL, DIMENSION(:,:), allocatable   :: q2x		! Humidity
		REAL, DIMENSION(:,:), allocatable   :: u		! Near surface wind in the u-component
		REAL, DIMENSION(:,:), allocatable   :: short		! Shortwave radiation.

		contains
		procedure :: init => weather_2D_inputs_init
		procedure :: destroy => weather_2D_inputs_destroy
                
                ! For debugging               
                procedure :: test => test_function
                procedure :: inputs => weather_2D_inputs_variables
	end type weather_2D_inputs

	! --------------------------------------------------------------------------
        contains

        subroutine test_function(this)
          class(weather_2D_inputs), intent(inout) :: this
          ! REAL, DIMENSION(IX,JX):: short		!
          !Shortwave radiation.
          !integer :: IX
          print *, "in test_function"
          !print *, "this.IX is ", this.IX
          !print *, "this.JX is ", this.JX
          !print *, "IX is ", IX
          !print *, "JX is ", JX
          this.short = reshape ((/500.,500.,500., &
               500.,500.,500., &
               500.,500.,500./), shape(this.short))

        end subroutine

	!subroutine weather_2D_inputs_init(this, IX, JX, IXRT, JXRT, T2, q2x, u, short)
        subroutine weather_2D_inputs_init(this, IX, JX, T2, q2x, u, short)
        

		implicit none
		class(weather_2D_inputs), intent(inout) :: this         ! the type object being initialized
		INTEGER, INTENT(in)                :: IX,JX             ! , IXRT, JXRT
		REAL, INTENT(in), DIMENSION(:,:) :: T2		! Air temperature
		REAL, INTENT(in), DIMENSION(:,:) :: q2x		! Humidity
		REAL, INTENT(in), DIMENSION(:,:) :: u			! Near surface wind in the u-component
		REAL, INTENT(in), DIMENSION(:,:) :: short		! Shortwave radiation

		! Assign the values passed in to a particular
		this%IX        = IX
		this%JX        = JX
		!this%IXRT      = IXRT
		!this%JXRT      = JXRT
		this%T2        = T2
		this%q2x       = q2x
		this%u         = u
		this%short     = short

	end subroutine weather_2D_inputs_init

	! --------------------------------------------------------------------------
	! weather_2D_inputs Destructor

	subroutine weather_2D_inputs_destroy (this)

		implicit none
		! the type object being destroyed

		class(weather_2D_inputs), intent(inout) :: this

	end subroutine weather_2D_inputs_destroy

	! --------------------------------------------------------------------------

	!subroutine weather_2D_inputs_variables(this, T2, q2x, u, short)
        subroutine weather_2D_inputs_variables(this)
        
        implicit none
        class(weather_2D_inputs), intent(inout) :: this         ! the type object being initialized
       
	!REAL, INTENT(out), DIMENSION(IX,JX):: T2		! Air temperature
	!REAL, INTENT(out), DIMENSION(IX,JX):: q2x		! Humidity
	!REAL, INTENT(out), DIMENSION(IX,JX):: u		        ! Near surface wind in the u-component
	!REAL, INTENT(out), DIMENSION(IX,JX):: short		! Shortwave radiation.

	!this.IX = IX
        !this.JX = JX

        this.T2 = reshape ((/18.,18.,18., &
			18.,18.,18., &
			18.,18.,18./), shape(this.T2))
	this.q2x = reshape ((/85.,85.,85., &
			 85.,85.,85., &
			 85.,85.,85./), shape(this.q2x))
	this.u = reshape ((/1.,1.,1., &
		       1.,1.,1., &
		       1.,1.,1./), shape(this.u))
	this.short = reshape ((/500.,500.,500., &
			   500.,500.,500., &
			   500.,500.,500./), shape(this.short))

        end subroutine weather_2D_inputs_variables

End module module_lsm_forcing
