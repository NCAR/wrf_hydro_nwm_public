! This module defines and instantiates objects
! for a level pool type reservoir. The level
! pool reservoir struct inherits input and
! output stucts from the reservoir base
! module and calls instantiation of these into
! sub-objects. The level pool reservoir struct
! also points to stucts for level pool parameters
! and state and calls instantiation of these into
! sub-objects. This module also contains the
! subroutine to run level pool release that is
! derived from the reservoir base struct interface
! to run release. Running level pool release will
! then call the LEVELPOOL subroutine, which
! processes the given inputs, parameters, and
! state for a particular level pool reservoir and
! returns the output/outflow.
module module_levelpool

    use module_levelpool_parameters
    use module_levelpool_state
    use module_reservoir_base
    use module_levelpool_tests
    implicit none

    ! Extend/derive level pool struct from the abstract base
    ! struct for reservoirs.
    type, extends(reservoir_base_struct) :: levelpool_struct

        ! Define pointers to sub-structs / sub-objects to and
        ! held by a level pool reservoir object.
        type (levelpool_parameters_interface), pointer :: parameters => null()
        type (levelpool_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => levelpool_struct_init
        procedure :: destroy => levelpool_struct_destroy
        procedure :: run_release => run_levelpool_release
    end type levelpool_struct

contains

    !Level Pool Constructor
    subroutine levelpool_struct_init(this, water_elevation,  &
        reservoir_area, weir_elevation, weir_coeffecient, &
        weir_length, orifice_elevation, orifice_coefficient, &
        orifice_area, max_depth, lake_number)
        implicit none
        class(levelpool_struct), intent(inout) :: this ! object being initialized
        real, intent(inout) :: water_elevation           ! meters AMSL
        real, intent(in)    :: reservoir_area            ! area of reservoir (km^2)
        real, intent(in)    :: weir_elevation            ! bottom of weir elevation (meters AMSL)
        real, intent(in)    :: weir_coeffecient          ! weir coefficient
        real, intent(in)    :: weir_length               ! weir length (meters)
        real, intent(in)    :: orifice_elevation         ! orifice elevation (meters AMSL)
        real, intent(in)    :: orifice_coefficient       ! orifice coefficient
        real, intent(in)    :: orifice_area              ! orifice area (meters^2)
        real, intent(in)    :: max_depth                 ! max depth of reservoir before overtop (meters)
        integer, intent(in) :: lake_number               ! lake number

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write an error
                write(0,*) "Failure to allocate level pool input structure"
            else
                ! initialize the input structure
                call this%input%init()
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write an error
                write(0,*) "Failure to allocate level pool output structure"
            else
                ! initialize the output structure
                call this%output%init()
            end if

            ! try to allocate parameters
            allocate ( this%parameters )
            if ( .not. associated(this%parameters) ) then
                ! if the parameters structure could not be created write an error
                write(0,*) "Failure to allocate levelpool parameters structure"
            else
                ! initialize levelpool parameters
                call this%parameters%init( reservoir_area,  &
                    weir_elevation, weir_coeffecient, weir_length, &
                    orifice_elevation, orifice_coefficient, &
                    orifice_area, max_depth, lake_number )
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created write an error
                write(0,*) "Failure to allocate state parameters structure"
            else
                ! initialize levelpool state
                call this%state%init( water_elevation )
            end if
            this%pointer_allocation_guard = .true.
        end if

    end subroutine levelpool_struct_init

    !Level Pool Destructor
    subroutine levelpool_struct_destroy(this)
        implicit none
        class(levelpool_struct), intent(inout) :: this ! object being destroyed
    end subroutine levelpool_struct_destroy

    ! Subroutine for running release for a level pool reservoir,
    ! which will then call the LEVELPOOL method/subroutine for processing the
    ! inputs and returning the output.
    subroutine run_levelpool_release(reservoir_data, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period)
        implicit none
        class(levelpool_struct), intent(inout) :: reservoir_data
        real, intent(in)    :: inflow               ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow       ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation      ! meters
        real, intent(out)   :: outflow              ! cubic meters per second (cms)
        real, intent(in)    :: routing_period       ! seconds

        logical :: rv

        rv = levelpool_data_info(reservoir_data)

        !print *, "--DEBUG-- before call to LEVELPOOL"
        !flush(6)

        ! Update input variables
        reservoir_data%input%inflow = inflow
        reservoir_data%input%lateral_inflow = lateral_inflow

        ! Update state variables
        reservoir_data%state%water_elevation = water_elevation

        call LEVELPOOL(reservoir_data%parameters%lake_number,   &
        reservoir_data%input%previous_timestep_inflow,          &
        reservoir_data%input%inflow,                            &
        reservoir_data%output%outflow,                          &
        reservoir_data%input%lateral_inflow,                    &
        routing_period,                                         &
        reservoir_data%state%water_elevation,                   &
        reservoir_data%parameters%reservoir_area,               &
        reservoir_data%parameters%weir_elevation,               &
        reservoir_data%parameters%max_depth,                    &
        reservoir_data%parameters%weir_coeffecient,             &
        reservoir_data%parameters%weir_length,                  &
        reservoir_data%parameters%orifice_elevation,            &
        reservoir_data%parameters%orifice_coefficient,          &
        reservoir_data%parameters%orifice_area                  &
        )

        ! Update output variable returned from this subroutine
        outflow = reservoir_data%output%outflow

        ! Set current inflow to previous_timestep_inflow
        reservoir_data%input%previous_timestep_inflow = inflow

    end subroutine run_levelpool_release

    ! ------------------------------------------------
    !   SUBROUTINE LEVELPOOL
    ! ------------------------------------------------

    subroutine LEVELPOOL(ln,qi0,qi1,qo1,ql,dt,H,ar,we,maxh,wc,wl,oe,oc,oa)

    !! ----------------------------  argument variables
    !! All elevations should be relative to a common base (often belev(k))

    real, intent(inout) :: H       ! water elevation height (m)
    real, intent(in)    :: dt      ! routing period [s]
    real, intent(in)    :: qi0     ! inflow at previous timestep (cms)
    real, intent(in)    :: qi1     ! inflow at current timestep (cms)
    real, intent(out)   :: qo1     ! outflow at current timestep
    real, intent(in)    :: ql      ! lateral inflow
    real, intent(in)    :: ar      ! area of reservoir (km^2)
    real, intent(in)    :: we      ! bottom of weir elevation
    real, intent(in)    :: wc      ! weir coeff.
    real, intent(in)    :: wl      ! weir length (m)
    real, intent(in)    :: oe      ! orifice elevation
    real, intent(in)    :: oc      ! orifice coeff.
    real, intent(in)    :: oa      ! orifice area (m^2)
    real, intent(in)    :: maxh    ! max depth of reservoir before overtop (m)
    integer, intent(in) :: ln      ! lake number

    !!DJG Add lake option switch here...move up to namelist in future versions...
    integer :: LAKE_OPT            ! Lake model option (move to namelist later)
    real    :: Htmp                ! Temporary assign of incoming lake el. (m)

    !! ----------------------------  local variables
    real :: sap                    ! local surface area values
    real :: discharge              ! storage discharge m^3/s
    real :: tmp1, tmp2
    real :: dh, dh1, dh2, dh3      ! height function and 3 order RK
    real :: It, Itdt_3, Itdt_2_3
    real :: maxWeirDepth           !maximum capacity of weir
    !! ----------------------------  subroutine body: from chow, mad mays. pg. 252
    !! -- determine from inflow hydrograph


    !!DJG Set hardwire for LAKE_OPT...move specification of this to namelist in
    !future versions...
    LAKE_OPT = 2
    Htmp = H   !temporary set of incoming lake water elevation...


    !!DJG IF-block for lake model option  1 - outflow=inflow, 2 - Chow et al level
    !pool, .....
    if (LAKE_OPT.eq.1) then     ! If-block for simple pass through scheme....

       qo1 = qi1                 ! Set outflow equal to inflow at current time
       H = Htmp                  ! Set new lake water elevation to incoming lake el.

    else if (LAKE_OPT.eq.2) then   ! If-block for Chow et al level pool scheme

       It = qi0
       Itdt_3   = (qi0 + (qi1 + ql))/3
       Itdt_2_3 = (qi0 + (qi1 + ql))/3 + Itdt_3
       maxWeirDepth =  maxh - we

       !-- determine Q(dh) from elevation-discharge relationship
       !-- and dh1
       dh = H - we
       if (dh .gt. maxWeirDepth) then
          dh = maxWeirDepth
       endif

       if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
          tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
          tmp2 = wc * wl * (dh ** 3./2.)
          discharge = tmp1 + tmp2

          if (H .gt. 0.0) then
             sap = (ar * 1.0E6 ) * (1 + (H - we) / H)
          else
             sap  = 0.0
          endif

       else if ( H .gt. oe ) then     !! only orifice flow,not full
          discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
          sap = ar * 1.0E6
       else
          discharge = 0.0
          sap = ar * 1.0E6
       endif

       if (sap .gt. 0) then
          dh1 = ((It - discharge)/sap)*dt
       else
          dh1 = 0.0
       endif

       !-- determine Q(H + dh1/3) from elevation-discharge relationship
       !-- dh2
       dh = (H+dh1/3) - we
       if (dh .gt. maxWeirDepth) then
          dh = maxWeirDepth
       endif

       if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
          tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
          tmp2 = wc * wl * (dh ** 3./2.)
          discharge = tmp1 + tmp2

          if (H .gt. 0.0) then
             sap = (ar * 1.0E6 ) * (1 + (H - we) / H)
          else
             sap  = 0.0
          endif

       else if ( H .gt. oe ) then     !! only orifice flow,not full
          discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
          sap = ar * 1.0E6
       else
          discharge = 0.0
          sap = ar * 1.0E6
       endif

       if (sap .gt. 0.0) then
          dh2 = ((Itdt_3 - discharge)/sap)*dt
       else
          dh2 = 0.0
       endif

       !-- determine Q(H + 2/3 dh2) from elevation-discharge relationship
       !-- dh3
       dh = (H + (0.667*dh2)) - we
       if (dh .gt. maxWeirDepth) then
          dh = maxWeirDepth
       endif

       if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
          tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
          tmp2 = wc * wl * (dh ** 3./2.)
          discharge = tmp1 + tmp2

          if (H .gt. 0.0) then
             sap = (ar * 1.0E6 ) * (1 + (H - we) / H)
          else
             sap = 0.0
          endif

       else if ( H .gt. oe ) then     !! only orifice flow,not full
          discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
          sap = ar * 1.0E6
       else
          discharge = 0.0
          sap = ar * 1.0E6
       endif

       if (sap .gt. 0.0) then
          dh3 = ((Itdt_2_3 - discharge)/sap)*dt
       else
          dh3 = 0.0
       endif

       !-- determine dh and H
       dh = (dh1/4.) + (0.75*dh3)
       H = H + dh

       !-- compute final discharge
       dh = H - we
       if (dh .gt. maxWeirDepth) then
          dh = maxWeirDepth
       endif
       if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
          tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
          tmp2 = wc * wl * (dh ** 3./2.)
          discharge = tmp1 + tmp2

          if (H .gt. 0.0) then
             sap = (ar * 1.0E6 ) * (1 + (H - we) / H)
          else
             sap = 0.0
          endif

       else if ( H .gt. oe ) then     !! only orifice flow,not full
          discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
          sap = ar * 1.0E6
       else
          discharge = 0.0
          sap = ar * 1.0E6
       endif

       if(H .ge. maxh) then  ! overtop condition
          discharge = qi1
          H = maxh
       endif

       qo1  = discharge  ! return the flow rate from reservoir

    23 format('botof H dh orf wr Q',f8.4,2x,f8.4,2x,f8.3,2x,f8.3,2x,f8.2)
    24 format('ofonl H dh sap Q ',f8.4,2x,f8.4,2x,f8.0,2x,f8.2)


    else   ! ELSE for LAKE_OPT....
    endif  ! ENDIF for LAKE_OPT....

    return

    ! ----------------------------------------------------------------
    end subroutine LEVELPOOL
    ! ----------------------------------------------------------------


    function levelpool_data_info(levelpool_data) result(rv)
    implicit none
    type (levelpool_struct) :: levelpool_data
    integer :: rv
    logical, dimension(1) :: ptr_state
    logical, dimension(1) :: data_state

    rv = 0

    ! Check to see if the levelpool_state data structure exists
    print *, "Checking pointer association on data%state "
    if ( associated(levelpool_data%state) ) then
        print *, "PASSED"
        ptr_state(1) = .true.
    else
        print *, "FAILED"
        ptr_state(1) = .false.
    end if
    print *, " "

    ! Check to see if the levelpool_parameters data structure exists
    print *, "Checking pointer association on data%parameters "
    if ( associated(levelpool_data%parameters) ) then
        print *, "PASSED"
        ptr_state(2) = .true.
    else
        print *, "FAILED"
        ptr_state(2) = .false.
    end if
    print *, " "

    ! Check to see if the levelpool_input data structure exists
    print *, "Checking pointer association on data%input "
    if ( associated(levelpool_data%input) ) then
        print *, "PASSED"
        ptr_state(3) = .true.
    else
        print *, "FAILED"
        ptr_state(3) = .false.
    end if
    print *, " "

    ! Now check the data members of each substructure
    if ( ptr_state(1) ) then
        data_state(1) = test_levelpool_state(levelpool_data%state)
    end if

    if ( ptr_state(2) ) then
        data_state(2) = test_levelpool_parameters(levelpool_data%parameters)
    end if

    if ( ptr_state(3) ) then
        data_state(3) = test_input(levelpool_data%input)
    end if





    if ( all(ptr_state) .and. all(data_state) ) then
        print *, "========================================================================"
        print *, "All Tests Passed"
        print *, "========================================================================"
    end if
    rv = 1

    !flush(6)
end function levelpool_data_info


end module module_levelpool
