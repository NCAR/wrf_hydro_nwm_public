module module_levelpool
  
  use module_reservoir_base
  use module_levelpool_parameters
  
  implicit none
  
  
  type, extends(reservoir_base_state_struct) :: levelpool_base_struct
  
      type (levelpool_parameters_interface), pointer :: parameters => null()
      
  contains
  
    procedure :: init => levelpool_base_struct_init
    procedure :: destroy => levelpool_base_struct_destroy
    
  end type levelpool_base_struct
  
contains

  !constructor for the levelpool base struct
  subroutine levelpool_base_struct_init(this)
      implicit none
      class (levelpool_base_struct), intent(inout) :: this ! object being initialized
  end subroutine levelpool_base_struct_init
  
  !destructor for the levelpool base struct
  subroutine levelpool_base_struct_destroy(this)
      implicit none
      class (levelpool_base_struct), intent(inout) :: this ! object being destroyed
  end subroutine levelpool_base_struct_destroy
  
  !!!!!!!!!ADD 'THIS' FOR ACCESSING OBJECT PARAMETERS
  
  !Same name for module and subroutine okay?? 
  subroutine levelpool()
        
    implicit none
    
    !! ----------------------------  argument variables
    !! All elevations should be relative to a common base (often belev(k))
    
    !!!Declarations...
    !real, intent(INOUT) :: waterElevation      ! water elevation height (m)   DO WE WANT AN INOUT VARIABLE TO THIS STRUCTURE??
    !real, intent(IN)    :: routingPeriodSeconds            ! routing period [s]
    !real, intent(IN)    :: inflowAtPreviousTimestepCMS     ! inflow at previous timestep (cms)
    !real, intent(IN)    :: inflowAtCurrentTimestepCMS      ! inflow at current timestep (cms)
    !real, intent(OUT)   :: outflowAtCurrentTimestep        ! outflow at current timestep    CMS???
    !real, intent(IN)    :: lateralInflow                   ! lateral inflow
    !real, intent(IN)    :: areaOfReservoirKMsquared        ! area of reservoir (km^2)
    !real, intent(IN)    :: bottomOfWeirElevation           ! bottom of weir elevation
    !real, intent(IN)    :: weirfCoffecient                 ! weir coeff.
    !real, intent(IN)    :: weirLength                      ! weir length (m)
    !real, intent(IN)    :: orificeElevation                ! orifice elevation
    !real, intent(IN)    :: orificeCoffecient               ! orifice coeff.
    !real, intent(IN)    :: orificeAreaMetersSquared        ! orifice area (m^2)
    !real, intent(IN)    :: maxDepthOfReservoirBeforeOvertopMeters    ! max depth of reservoir before overtop (m)                     
    !integer, intent(IN) :: lakeNumber                      ! lake number

    
    
  !!ACCESS THE PARTICULAR RESERVOIR FROM THE RES ARRAY THEN USE % TO ACCESS PARTICULAR VARIABLES/PARAMETERS FROM THE RESERVOIR STRUCTURE
        
  !!DJG Add lake option switch here...move up to namelist in future versions...
  integer :: LAKE_OPT            ! Lake model option (move to namelist later)
  real    :: waterElevationHeightMetersTemp                ! Temporary assign of incoming lake el. (m)
  
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
  water_elevation_Temp = this%water_elevation  !temporary set of incoming lake water elevation...
   
  
  !!DJG IF-block for lake model option  1 - outflow=inflow, 2 - Chow et al level
  !pool, .....
  if (LAKE_OPT.eq.1) then     ! If-block for simple pass through scheme....
     
     outflowAtCurrentTimestep = inflowAtCurrentTimestepCMS                 ! Set outflow equal to inflow at current time      
     waterElevationHeightMeters = waterElevationHeightMetersTemp                  ! Set new lake water elevation to incoming lake el.
     
  else if (LAKE_OPT.eq.2) then   ! If-block for Chow et al level pool scheme
     
     It = inflowAtPreviousTimestepCMS
     Itdt_3   = (inflowAtPreviousTimestepCMS + (inflowAtCurrentTimestepCMS + lateralInflow))/3
     Itdt_2_3 = (inflowAtPreviousTimestepCMS + (inflowAtCurrentTimestepCMS + lateralInflow))/3 + Itdt_3
     maxWeirDepth = maxDepthOfReservoirBeforeOvertopMeters - bottomOfWeirElevation   
     
     !-- determine Q(dh) from elevation-discharge relationship
     !-- and dh1
     dh = waterElevationHeightMeters - bottomOfWeirElevation
     if (dh .gt. maxWeirDepth) then 
        dh = maxWeirDepth 
     endif
     
     if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = orificeCoffecient * orificeAreaMetersSquared * sqrt(2 * 9.81 * ( waterElevationHeightMeters - orificeElevation ) )
        tmp2 = weirfCoffecient * weirLength * (dh ** 2./3.)
        discharge = tmp1 + tmp2
        
        if (waterElevationHeightMeters .gt. 0.0) then
           sap = (areaOfReservoirKMsquared * 1.0E6 ) * (1 + (waterElevationHeightMeters - bottomOfWeirElevation) / waterElevationHeightMeters)
        else
           sap  = 0.0
        endif
        
     else if ( waterElevationHeightMeters .gt. orificeElevation ) then     !! only orifice flow,not full
        discharge = orificeCoffecient * orificeAreaMetersSquared * sqrt(2 * 9.81 * ( waterElevationHeightMeters - orificeElevation ) )
        sap = areaOfReservoirKMsquared * 1.0E6
     else
        discharge = 0.0
        sap = areaOfReservoirKMsquared * 1.0E6
     endif
     
     if (sap .gt. 0) then 
        dh1 = ((It - discharge)/sap)*routingPeriodSeconds
     else
        dh1 = 0.0
     endif
     
     !-- determine Q(H + dh1/3) from elevation-discharge relationship
     !-- dh2
     dh = (waterElevationHeightMeters+dh1/3) - bottomOfWeirElevation
     if (dh .gt. maxWeirDepth) then 
        dh = maxWeirDepth 
     endif
     
     if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = orificeCoffecient * orificeAreaMetersSquared * sqrt(2 * 9.81 * ( waterElevationHeightMeters - orificeElevation ) )
        tmp2 = weirfCoffecient * weirLength * (dh ** 2./3.) 
        discharge = tmp1 + tmp2
        
        if (waterElevationHeightMeters .gt. 0.0) then 
           sap = (areaOfReservoirKMsquared * 1.0E6 ) * (1 + (waterElevationHeightMeters - bottomOfWeirElevation) / waterElevationHeightMeters)
        else
           sap  = 0.0
        endif
        
     else if ( waterElevationHeightMeters .gt. orificeElevation ) then     !! only orifice flow,not full
        discharge = orificeCoffecient * orificeAreaMetersSquared * sqrt(2 * 9.81 * ( waterElevationHeightMeters - orificeElevation ) )
        sap = areaOfReservoirKMsquared * 1.0E6
     else
        discharge = 0.0
        sap = areaOfReservoirKMsquared * 1.0E6
     endif
     
     if (sap .gt. 0.0) then 
        dh2 = ((Itdt_3 - discharge)/sap)*routingPeriodSeconds
     else
        dh2 = 0.0
     endif
     
     !-- determine Q(waterElevationHeightMeters + 2/3 dh2) from elevation-discharge relationship
     !-- dh3
     dh = (waterElevationHeightMeters + (0.667*dh2)) - bottomOfWeirElevation
     if (dh .gt. maxWeirDepth) then 
        dh = maxWeirDepth 
     endif
     
     if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = orificeCoffecient * orificeAreaMetersSquared * sqrt(2 * 9.81 * ( waterElevationHeightMeters - orificeElevation ) )
        tmp2 = weirfCoffecient * weirLength * (dh ** 2./3.) 
        discharge = tmp1 + tmp2
        
        if (waterElevationHeightMeters .gt. 0.0) then
           sap = (areaOfReservoirKMsquared * 1.0E6 ) * (1 + (waterElevationHeightMeters - bottomOfWeirElevation) / waterElevationHeightMeters)
        else
           sap = 0.0
        endif
        
     else if ( waterElevationHeightMeters .gt. orificeElevation ) then     !! only orifice flow,not full
        discharge = orificeCoffecient * orificeAreaMetersSquared * sqrt(2 * 9.81 * ( waterElevationHeightMeters - orificeElevation ) )
        sap = areaOfReservoirKMsquared * 1.0E6
     else
        discharge = 0.0
        sap = areaOfReservoirKMsquared * 1.0E6
     endif
     
     if (sap .gt. 0.0) then 
        dh3 = ((Itdt_2_3 - discharge)/sap)*routingPeriodSeconds
     else
        dh3 = 0.0
     endif
      
     !-- determine dh and H
     dh = (dh1/4.) + (0.75*dh3)
     waterElevationHeightMeters = waterElevationHeightMeters + dh
     
     !-- compute final discharge
     dh = waterElevationHeightMeters - bottomOfWeirElevation
     if (dh .gt. maxWeirDepth) then 
        dh = maxWeirDepth 
     endif
     if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = orificeCoffecient * orificeAreaMetersSquared * sqrt(2 * 9.81 * ( waterElevationHeightMeters - orificeElevation ) )
        tmp2 = weirfCoffecient * weirLength * (dh ** 2./3.)
        discharge = tmp1 + tmp2
        
        if (waterElevationHeightMeters .gt. 0.0) then 
           sap = (areaOfReservoirKMsquared * 1.0E6 ) * (1 + (waterElevationHeightMeters - bottomOfWeirElevation) / waterElevationHeightMeters)
        else
           sap = 0.0
        endif
        
     else if ( waterElevationHeightMeters .gt. orificeElevation ) then     !! only orifice flow,not full
        discharge = orificeCoffecient * orificeAreaMetersSquared * sqrt(2 * 9.81 * ( waterElevationHeightMeters - orificeElevation ) )
        sap = areaOfReservoirKMsquared * 1.0E6
     else
        discharge = 0.0
        sap = areaOfReservoirKMsquared * 1.0E6
     endif
     
     if(waterElevationHeightMeters .ge. maxDepthOfReservoirBeforeOvertopMeters) then  ! overtop condition
        discharge = inflowAtCurrentTimestepCMS
        waterElevationHeightMeters = maxDepthOfReservoirBeforeOvertopMeters
     endif
     
     !outflowAtCurrentTimestep  = discharge  ! return the flow rate from reservoir
     this%reservoir_outflow  = discharge  ! return the flow rate from reservoir

  23 format('botof waterElevationHeightMeters dh orf wr Q',f8.4,2x,f8.4,2x,f8.3,2x,f8.3,2x,f8.2)
  24 format('ofonl waterElevationHeightMeters dh sap Q ',f8.4,2x,f8.4,2x,f8.0,2x,f8.2)
     
     
  else   ! ELSE for LAKE_OPT....
  endif  ! ENDIF for LAKE_OPT....
  
  return
   
  ! ----------------------------------------------------------------
  end subroutine LEVELPOOL
  ! ----------------------------------------------------------------
      
      
end module module_levelpool
    
    
    
    
    
    
    
    
    


