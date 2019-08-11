! This module defines and instantiates objects
! for a level pool type reservoir. The level
! pool reservoir type inherits input and
! output stucts from the reservoir base
! module and calls instantiation of these into
! sub-objects. The level pool reservoir type
! also points to stucts for level pool parameters
! and state and calls instantiation of these into
! sub-objects. This module also contains the
! subroutine to run level pool reservoir that is
! derived from the reservoir base type interface
! to run reservoir. Running level pool will
! then call the LEVELPOOL subroutine, which
! processes the given inputs, parameters, and
! state for a particular level pool reservoir and
! returns the output/outflow.

module module_levelpool

    use module_levelpool_parameters, only: levelpool_parameters_interface
    use module_levelpool_state, only: levelpool_state_interface
    use module_reservoir_utilities, only: create_levelpool_diagnostic_log_file, &
                                          log_levelpool_diagnostic_data
    use module_reservoir_base
    implicit none

    ! Extend/derive level pool type from the abstract base
    ! type for reservoirs.
    type, extends(reservoir_base_type) :: levelpool_type

        ! Define pointers to sub-types / sub-objects to and
        ! held by a level pool reservoir object.
        type (levelpool_parameters_interface), pointer :: parameters => null()
        type (levelpool_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => levelpool_type_init
        procedure :: destroy => levelpool_type_destroy
        procedure :: run => run_levelpool_reservoir

    end type levelpool_type

contains

    !Level Pool Constructor
    subroutine levelpool_type_init(this, water_elevation,  &
        lake_area, weir_elevation, weir_coeffecient, &
        weir_length, orifice_elevation, orifice_coefficient, &
        orifice_area, max_depth, lake_number)
        implicit none
        class(levelpool_type), intent(inout) :: this ! object being initialized
        real, intent(inout) :: water_elevation           ! meters AMSL
        real, intent(in)    :: lake_area      		 ! area of lake (km^2)
        real, intent(in)    :: weir_elevation            ! bottom of weir elevation (meters AMSL)
        real, intent(in)    :: weir_coeffecient          ! weir coefficient
        real, intent(in)    :: weir_length               ! weir length (meters)
        real, intent(in)    :: orifice_elevation         ! orifice elevation (meters AMSL)
        real, intent(in)    :: orifice_coefficient       ! orifice coefficient
        real, intent(in)    :: orifice_area              ! orifice area (meters^2)
        real, intent(in)    :: max_depth                 ! max depth of reservoir before overtop (meters)
        integer, intent(in) :: lake_number               ! lake number

#ifdef RESERVOIR_D
        ! Create diagnostic log file only for development/debugging purposes
        call create_levelpool_diagnostic_log_file(lake_number)
#endif

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
                call this%parameters%init( lake_area,  &
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

    end subroutine levelpool_type_init


    !Level Pool Destructor
    subroutine levelpool_type_destroy(this)
        implicit none
        class(levelpool_type), intent(inout) :: this ! object being destroyed
    end subroutine levelpool_type_destroy


    ! Subroutine for running a level pool reservoir,
    ! which will then call the LEVELPOOL method/subroutine for processing the
    ! inputs and returning the output.
    subroutine run_levelpool_reservoir(this, previous_timestep_inflow, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period)
        implicit none
        class(levelpool_type), intent(inout) :: this
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds

        ! Update input variables
        this%input%inflow = inflow
        this%input%lateral_inflow = lateral_inflow

        ! Update state variables
        this%state%water_elevation = water_elevation

        call LEVELPOOL(this%parameters%lake_number,   &
        previous_timestep_inflow,                               &
        this%input%inflow,                            &
        this%output%outflow,                          &
        this%input%lateral_inflow,                    &
        routing_period,                                         &
        this%state%water_elevation,                   &
        this%parameters%lake_area,          		    &
        this%parameters%weir_elevation,               &
        this%parameters%max_depth,                    &
        this%parameters%weir_coeffecient,             &
        this%parameters%weir_length,                  &
        this%parameters%orifice_elevation,            &
        this%parameters%orifice_coefficient,          &
        this%parameters%orifice_area                  &
        )

        ! Update output variable returned from this subroutine
        outflow = this%output%outflow

        ! Set current inflow to previous_timestep_inflow
        this%input%previous_timestep_inflow = inflow

        ! Update water_elevation variable returned from this subroutine
        water_elevation = this%state%water_elevation

#ifdef RESERVOIR_D
        ! Log diagnostic data only for development/debugging purposes
        call log_levelpool_diagnostic_data(this%parameters%lake_number, inflow, water_elevation, outflow)
#endif

    end subroutine run_levelpool_reservoir

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
    real :: dh, dh1, dh2, dh3      ! Depth in weir, and height function for 3 order RK
    real :: It, Itdt_3, Itdt_2_3   ! inflow hydrographs
    real :: maxWeirDepth           !maximum capacity of weir
    !real :: hdiff_vol, qdiff_vol   ! water balance check variables
    !! ----------------------------  subroutine body: from chow, mad mays. pg. 252
    !! -- determine from inflow hydrograph


    !!DJG Set hardwire for LAKE_OPT...move specification of this to namelist in
    !future versions...
    LAKE_OPT = 2
    Htmp = H   !temporary set of incoming lake water elevation...
    !hdiff_vol = 0.0
    !qdiff_vol = 0.0

    !!DJG IF-block for lake model option  1 - outflow=inflow, 2 - Chow et al level
    !pool, .....
    if (LAKE_OPT.eq.1) then     ! If-block for simple pass through scheme....

       qo1 = qi1                 ! Set outflow equal to inflow at current time
       H = Htmp                  ! Set new lake water elevation to incoming lake el.

    else if (LAKE_OPT.eq.2) then   ! If-block for Chow et al level pool scheme

       It = qi0
       Itdt_3   = qi0 + ((qi1 + ql - qi0) * 0.33)
       Itdt_2_3 = qi0 + ((qi1 + ql - qi0) * 0.67)
       maxWeirDepth =  maxh - we

       !assume vertically walled reservoir
       !remove this when moving to a variable head area volume
       sap = ar * 1.0E6

       !-- determine Q(dh) from elevation-discharge relationship
       !-- and dh1
       dh = H - we
       if (dh .gt. maxWeirDepth) then
          dh = maxWeirDepth
       endif

       if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
          tmp1 = oc * oa * sqrt(2. * 9.81 * ( H - oe ) )
          tmp2 = wc * wl * (dh ** (3./2.))
          discharge = tmp1 + tmp2
       else if ( H .gt. oe ) then     !! only orifice flow,not full
          discharge = oc * oa * sqrt(2. * 9.81 * ( H - oe ) )
       else
          discharge = 0.0
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
          tmp1 = oc * oa * sqrt(2. * 9.81 * ( (H+dh1/3.) - oe ) )
          tmp2 = wc * wl * (dh ** (3./2.))
          discharge = tmp1 + tmp2
       else if ( (H+dh1/3) .gt. oe ) then     !! only orifice flow,not full
          discharge = oc * oa * sqrt(2. * 9.81 * ( (H+dh1/3.) - oe ) )
       else
          discharge = 0.0
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
          tmp1 = oc * oa * sqrt(2. * 9.81 * ( (H+dh2*0.667) - oe ) )
          tmp2 = wc * wl * (dh ** (3./2.))
          discharge = tmp1 + tmp2
       else if ( (H+dh2*0.667) .gt. oe ) then     !! only orifice flow,not full
          discharge = oc * oa * sqrt(2. * 9.81 * ( (H+dh2*0.667) - oe ) )
       else
          discharge = 0.0
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
          tmp1 = oc * oa * sqrt(2. * 9.81 * ( H - oe ) )
          tmp2 = wc * wl * (dh ** (3./2.))
          discharge = tmp1 + tmp2
       else if ( H .gt. oe ) then     !! only orifice flow,not full
          discharge = oc * oa * sqrt(2. * 9.81 * ( H - oe ) )
       else
          discharge = 0.0
       endif

       if(H .ge. maxh) then  ! overtop condition
          discharge = qi1 - (((maxh - Htmp) * sap) / dt)
          H = maxh
       endif

       qo1  = discharge  ! return the flow rate from reservoir

    !#ifdef HYDRO_D
    !#ifndef NCEP_WCOSS
    !   ! Water balance check
    !   qdiff_vol = (qi1+ql-qo1)*dt !m3
    !   hdiff_vol = (H-Htmp)*sap    !m3
    !22 format(f8.4,2x,f8.4,2x,f8.4,2x,f8.4,2x,f8.4,2x,f6.0,2x,f20.1,2x,f20.1)
    !   open (unit=67, &
    !     file='lake_massbalance_out.txt', status='unknown',position='append')
    !   write(67,22) Htmp, H, qi1, ql, qo1, dt, qdiff_vol, hdiff_vol
    !   close(67)
    !#endif
    !#endif

    23 format('botof H dh orf wr Q',f8.4,2x,f8.4,2x,f8.3,2x,f8.3,2x,f8.2)
    24 format('ofonl H dh sap Q ',f8.4,2x,f8.4,2x,f8.0,2x,f8.2)


    else   ! ELSE for LAKE_OPT....
    endif  ! ENDIF for LAKE_OPT....

    return

    ! ----------------------------------------------------------------
    end subroutine LEVELPOOL
    ! ----------------------------------------------------------------

end module module_levelpool
