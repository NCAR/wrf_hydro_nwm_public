module module_levelpool

    use module_levelpool_parameters
    use module_levelpool_state
    use module_reservoir_base


    implicit none


    type, extends(reservoir_base_struct) :: levelpool_struct

        !type (levelpool_parameters_interface), pointer :: parameters => null()
        !type (levelpool_state_interface), pointer :: state => null()

        type (levelpool_parameters_interface) :: parameters
        type (levelpool_state_interface) :: state





        logical :: pointer_allocation_guard = .false.


        contains

        procedure :: init => levelpool_struct_init
        procedure :: destroy => levelpool_struct_destroy
        procedure :: run_release => run_levelpool_release

    end type levelpool_struct


    contains



    !I don't need to pass in inflow, lat_in, prev_day_in, and outflow because they are derived from base input/output structs
    !Level Pool Constructor
    subroutine levelpool_struct_init(this, water_elevation,  &
        reservoir_area, weir_elevation, weir_coeffecient, &
        weir_length, orifice_elevation, orifice_coffecient, &
        orifice_area, max_depth, lake_number)
        implicit none
        class(levelpool_struct), intent(inout) :: this ! object being initialized
        real, intent(INOUT) :: water_elevation
        real, intent(IN)    :: reservoir_area            ! area of reservoir (km^2)
        real, intent(IN)    :: weir_elevation            ! bottom of weir elevation
        real, intent(IN)    :: weir_coeffecient          ! weir coeff.
        real, intent(IN)    :: weir_length               ! weir length (m)
        real, intent(IN)    :: orifice_elevation         ! orifice elevation
        real, intent(IN)    :: orifice_coffecient        ! orifice coeff.
        real, intent(IN)    :: orifice_area              ! orifice area (m^2)
        real, intent(IN)    :: max_depth         ! max depth of reservoir before overtop (m)
        integer, intent(IN) :: lake_number               ! lake number



        if (this%pointer_allocation_guard .eqv. .false. ) then

            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write and error
                write(0,*) "Failure to allocate level pool input structure"
            else
                ! initialize the input structure
                call this%input%init()
            end if



            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write and error
                write(0,*) "Failure to allocate level pool output structure"
            else
                ! initialize the output structure
                call this%output%init()
            end if


            ! initialize levelpool parameters
            call this%parameters%init( reservoir_area, weir_elevation, weir_coeffecient, &
                weir_length, orifice_elevation, &
                orifice_coffecient, orifice_area, &
                max_depth, lake_number )



            ! initialize levelpool state
            call this%state%init( water_elevation )


            ! try to allocate levelpool parameters
            !allocate ( this%parameters )
            !if ( .not. associated(this%parameters) ) then
            !    if the parameters structure could not be created write an error
            !    write(0,*) "Failure to allocate levelpool parameters structure"
            !else
            !    ! initialize the parameters structure
            !    call this%parameters%init( reservoir_area, weir_elevation, weir_coeffecient, &
            !        weir_length, orifice_elevation, &
            !        orifice_coffecient, orifice_area, &
            !        max_depth, lake_number)
            !end if
            !this%pointer_allocation_guard = .true.



            ! try to allocate levelpool state
            !allocate ( this%state )
            !if ( .not. associated(this%state) ) then
            !     if the parameters structure could not be created write an error
            !    write(0,*) "Failure to allocate levelpool state structure"
            !else
            !    ! initialize the state structure
            !    call this%state%init( water_elevation )
            !end if
            !this%pointer_allocation_guard = .true.


        end if


    end subroutine levelpool_struct_init



    !Level Pool Destructor
    subroutine levelpool_struct_destroy(this)
        implicit none

        class(levelpool_struct), intent(inout) :: this ! object being destroyed

    end subroutine levelpool_struct_destroy




    subroutine run_levelpool_release(reservoir_data, inflow, lateral_inflow, routing_period)   !inflow_previous_timestep also???
        implicit none
        class(levelpool_struct), intent(inout) :: reservoir_data

        real, intent(IN)    :: inflow
        real, intent(IN)    :: lateral_inflow
        real, intent(IN)    :: routing_period      ! routing period [s]


        !Update input variables
        reservoir_data%input%inflow = inflow
        reservoir_data%input%lateral_inflow = lateral_inflow

        !Update state variables

        !call LEVELPOOL(reservoir_data%parameters%lake_number,   &
        !reservoir_data%input%inflow_previous_timestep,          &
        !reservoir_data%input%inflow,                            &
        !reservoir_data%output%outflow,                          &
        !reservoir_data%input%lateral_inflow,                    &
        !routing_period,                                         &
        !reservoir_data%state%water_elevation,                   &
        !reservoir_data%parameters%reservoir_area,               &
        !reservoir_data%parameters%weir_elevation,               &
        !reservoir_data%parameters%max_depth,                    &
        !reservoir_data%parameters%weir_coeffecient,             &
        !reservoir_data%parameters%weir_length,                  &
        !reservoir_data%parameters%orifice_elevation,            &
        !reservoir_data%parameters%orifice_coffecient,           &
        !reservoir_data%,parameters%orifice_area                 &
        !)


    end subroutine run_levelpool_release





    ! ------------------------------------------------
    !   SUBROUTINE LEVELPOOL
    ! ------------------------------------------------

    subroutine LEVELPOOL(ln,qi0,qi1,qo1,ql,dt,H,ar,we,maxh,wc,wl,oe,oc,oa)

    !! ----------------------------  argument variables
    !! All elevations should be relative to a common base (often belev(k))

    real, intent(INOUT) :: H       ! water elevation height (m)
    real, intent(IN)    :: dt      ! routing period [s]
    real, intent(IN)    :: qi0     ! inflow at previous timestep (cms)
    real, intent(IN)    :: qi1     ! inflow at current timestep (cms)
    real, intent(OUT)   :: qo1     ! outflow at current timestep
    real, intent(IN)    :: ql      ! lateral inflow
    real, intent(IN)    :: ar      ! area of reservoir (km^2)
    real, intent(IN)    :: we      ! bottom of weir elevation
    real, intent(IN)    :: wc      ! weir coeff.
    real, intent(IN)    :: wl      ! weir length (m)
    real, intent(IN)    :: oe      ! orifice elevation
    real, intent(IN)    :: oc      ! orifice coeff.
    real, intent(IN)    :: oa      ! orifice area (m^2)
    real, intent(IN)    :: maxh    ! max depth of reservoir before overtop (m)
    integer, intent(IN) :: ln      ! lake number

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











end module module_levelpool











