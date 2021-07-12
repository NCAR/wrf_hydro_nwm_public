!  Program Name: module_river_temperature
!  Author(s)/Contact(s): Reza Abdi (rabdi@ucar.edu)
!  Abstract:
!  History Log: First attept, Mar. 21
!
!  Usage:
!  Parameters: 
!  Input Files:
!        
!  Output Files:
!        
!  Condition codes:
!
!  User controllable options: <if applicable>

MODULE module_river_temperature

	
	use module_channel_routing, only: temperature_sim_H_inputs
	use disaggregateWeatherModule,  only: fineWeatherData	
	use module_RT_data, only: rt_domain
	
	IMPLICIT NONE
		
type  :: river_temperature
		type (temperature_sim_H_inputs), pointer :: hydraulics_sim => null() 
		type (fineWeatherData), pointer :: weather_sim => null() 

     logical :: pointer_allocation_guard = .false.
		
	   contains
	   procedure :: init => river_temperature_init
     procedure :: destroy => river_temperature_destroy
     procedure :: run => river_temperature_run
		
end type river_temperature

REAL, parameter ::  rho_water = 1000.
REAL, parameter ::  c_water = 4182.
        
contains

!subroutine river_temperature_init(this, NLINKS, IXRT, JXRT, reach_number, ChanCondConst, QSTRMVOLRT,  &
!					  QINFLOWBASE, QSUM,  XS_Peremeter, Bw, Tw, velocity, HLINK, ChSSlp, &
!					  CHANXI, CHANYJ, ZELEV, CHANLEN, CH_NETLNK, QLINK, DT_STEPS, DTCT, node_area, TO_NODE, TYPEL, &
!           IX, JX,  T2, q2x, u, short, CH_NETRT, LAKE_MSKRT, nsoil, ims,ime, jms,jme, TMN, TSK, TSLB, &
!            Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D, SolarRad_fine_1D, TMN_1D_fine, TSK_1D_fine,TSLB_1D_fine, T_QSUM)
	    
	    
	subroutine river_temperature_init(this, Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D, SolarRad_fine_1D, &
	                                  TMN_1D_fine, TSK_1D_fine,TSLB_1D_fine, T_QSUM, reach_number)

		implicit none
		
		class(river_temperature), INTENT(inout)                          :: this              ! the type object being initialized
		INTEGER,              INTENT(in)                                 :: reach_number	
                !REAL,                 INTENT(in),  DIMENSION(IX, JX)             :: T2, q2x, u, short
   	        !INTEGER, INTENT(in)                                              :: nsoil, ims,ime, jms,jme
   	        !REAL, INTENT(in), DIMENSION(ix,jx)                               :: TMN
                !REAL, INTENT(in), DIMENSION(ix,jx)                               :: TSK
                !REAL, INTENT(in), DIMENSION(ix,nsoil,jx)                         :: TSLB
                REAL, INTENT(inout), DIMENSION(NLINKS)             :: Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D, SolarRad_fine_1D, TMN_1D_fine, TSK_1D_fine,TSLB_1D_fine
                !REAL, INTENT(inout), DIMENSION(NLINKS)             :: T_QSUM        
		character(len=20)                                                :: Reach_number_string
		
		if (this%pointer_allocation_guard .eq. .false. ) then
		! try to allocate input
                        ! allocate ( this%input )
                        !if ( .not. associated(this%input) ) then
                        ! if the input structure could not be created, call hydro_stop.
                        !        write(Reach_number_string, "(I15)") reach_number
                        !        call hydro_stop("ERROR: Failure to allocate temeprature module input structure for reach " &
                        !        // trim(ADJUSTL(Reach_number_string)) // ".")
                        !else
                        ! initialize the input structure
                        !        call this%input%init()
                        !end if

		 ! try to allocate output
                        !allocate ( this%output )
                        !if ( .not. associated(this%output) ) then
                        ! if the output structure could not be created, call hydro_stop.
                        !        write(Reach_number_string, "(I15)") reach_number
                        !        call hydro_stop("ERROR: Failure to allocate temeprature module output structure for reach " &
                        !        // trim(ADJUSTL(Reach_number_string)) // ".")
                        !else
                        ! initialize the output structure
                        !        call this%output%init()
                        !end if
			
                        ! try to allocate hydraulics_sim
		    !allocate ( this%hydraulics_sim )
		    !if ( .not. associated(this%hydraulics_sim) ) then
				! if the output structure could not be created, call hydro_stop.
                                !write(Reach_number_string, "(I15)") reach_number
                               ! call hydro_stop("ERROR: Failure to allocate temeprature module the hydraulics_sim information for reach " &
                               ! // trim(ADJUSTL(Reach_number_string)) // ".")	
			  !else
                ! initialize hydraulics_sim
                !call this%hydraulics_sim%init(NLINKS, IXRT, JXRT, ChanCondConst, QSTRMVOLRT,  &
		!		               		     QINFLOWBASE, QSUM, XS_Peremeter, Bw, Tw, velocity, HLINK, ChSSlp, &
		!		      	        	     CHANXI, CHANYJ, ZELEV, CHANLEN, CH_NETLNK, QLINK, DT_STEPS, DTCT, node_area, TO_NODE, TYPEL)

                 ! For debugging and testing purposes, I am gonna run the following subroutine
                 ! call this%hydraulics_sim%inputs_hyd()
		 !	   end if
							
			   ! try to allocate weather_sim
			   allocate ( this%weather_sim )
			   if ( .not. associated(this%weather_sim) ) then
			   ! if the output structure could not be created, call hydro_stop.
                                write(Reach_number_string, "(I15)") reach_number
                                !call hydro_stop("ERROR: Failure to allocate temeprature module the weather_sim information for reach " &
                                !// trim(ADJUSTL(Reach_number_string)) // ".")	
			    else
          ! initialize weather_sim                         	
                !call this%weather_sim%init(IX, JX, IXRT, JXRT, T2, q2x, u, short, NLINKS, CH_NETRT, &
		!			                   CHANXI, CHANYJ, LAKE_MSKRT, TMN, TSK, TSLB, nsoil, reach_number, &
                !             ims,ime, jms,jme, &
                !             Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D, SolarRad_fine_1D, TMN_1D_fine, TSK_1D_fine,TSLB_1D_fine)
		!--updated version for the WRF-Hydro combined phase
		!-- Maybe I don't need to initialize something in weather_sim%init()!
		!call this%weather_sim%init(Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D, SolarRad_fine_1D, TMN_1D_fine, TSK_1D_fine,TSLB_1D_fine)
                ! Run the subroutine to diaggregate
                !call this%weather_sim%run()
					 					
     			end if
			    this%pointer_allocation_guard = .true.
		end if
		
end subroutine river_temperature_init
! --------------------------------------------------------------------------
! river_temperature Destructor
subroutine river_temperature_destroy(this)
        implicit none
        class(river_temperature), intent(inout) :: this ! object being destroyed
 end subroutine river_temperature_destroy

! --------------------------------------------------------------------------
subroutine river_temperature_run(this, did)
		
	implicit none
  class(river_temperature), INTENT(inout)             :: this
  INTEGER                                             :: dx
  ! Embeddedness and Substrate particle size (mm) respectively
	REAL,    DIMENSION(:), allocatable                  :: C_Em, P_size                    
	REAL,    DIMENSION(:), allocatable                  :: shadeF, sur_albedo, viewToSky, cloud   ! Shading factor and surface albedo for the reach
	REAL,    INTENT(INOUT), DIMENSION(:)                :: T_QSUM
  
  INTEGER                                             ::  did  ! for the rt_domain
         
  !-- For the T_bed, I considered the temperature I am using for the hyporheic exchange,  this%weather_sim%TSLB_1D_fine
  !-- I hard-coded the following vectors here but they need to be imported to the model as inputs, for the next phase.
  dx = 250
  C_Em = (/0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3/)
  P_size = (/60,60,60,60,60,60,60,60,60,60,60,60/)
  shadeF = (/0,0,0,0,0,0,0,0,0,0,0,0/)             ! No shade; if = 1, full shade
  sur_albedo = (/0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2/) 
  viewToSky = (/1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1./)
  cloud = (/0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1/)

	 

  call river_TEMPERATURE_SOLVER(dx,                	        &
				rt_domain(did)%DT_STEPS,        &
                                rt_domain(did)%IXRT,            &                 
                                rt_domain(did)%JXRT,            &
                                rt_domain(did)%Tair_fine_1D,    &
				rt_domain(did)%RelH_fine_1D,    &
				rt_domain(did)%windSpd_fine_1D, &
					                      Cloud,      &
				rt_domain(did)%TSLB_1D_fine,    &
				rt_domain(did)%SolarRad_fine_1D,&
				rt_domain(did)%XS_Peremeter,    &
				rt_domain(did)%HLINK,	        & 	 	
				rt_domain(did)%ChSSlp,	        &
				rt_domain(did)%ZELEV,	        &
                                rt_domain(did)%velocity,        &
				rt_domain(did)%Tw,	        &
				rt_domain(did)%QSUM,	        &
				rt_domain(did)%QINFLOWBASE,     & 
				rt_domain(did)%QSTRMVOLRT,      &
				rt_domain(did)%TMN_1D_fine,     &
				rt_domain(did)%TSLB_1D_fine,    &
				rt_domain(did)%TSK_1D_fine,     &
                                			      sur_albedo,  &
                					      shadeF,      &
					                      viewToSky,   &
				rt_domain(did)%ChanCondConst,   &
                                			      C_Em,	   &
	                				      P_size,      &
				rt_domain(did)%CHANXI,         &
				rt_domain(did)%CHANYJ,	       &
				rt_domain(did)%CH_NETLNK,      &
                                rt_domain(did)%CH_NETRT,       &
                                rt_domain(did)%QLINK,	       &
				rt_domain(did)%DT_STEPS,       & 
				rt_domain(did)%DTCT,	       &
				rt_domain(did)%node_area,      &
				rt_domain(did)%TO_NODE,	       &
				rt_domain(did)%TYPEL,	       &
				rt_domain(did)%T_QSUM,	       &
                                rt_domain(did)%NLINKS          &
                 		)
	
	
end subroutine river_temperature_run
	
! ------------------------------------------------
!   SUBROUTINE RIVER_TEMPERATURE_SOLVER
! ------------------------------------------------
Subroutine river_TEMPERATURE_SOLVER(dx, dt, IXRT, JXRT,  T_a, H, V_wind, C_L, T_bed, s_in, Pw_river, y_w, S_0, z_elv, V_w, W_w, Q_str, Q_GW, &
								Q_Trib, T_GW, T_hyp, T_Trib, albedo, SF, vts, k_s_0, &
								C_E, P_s, CHANXI, CHANYJ, CH_NETLNK,CH_NETRT, QLINK, DT_STEPS, DTCT, node_area, TO_NODE, TYPEL, T_QSUM_local, totLength)
		implicit none
		
		! -------- DECLARATIONS ------------------------
		INTEGER, INTENT(IN)  :: dx			! Space intervals which should be based on the HYDRO grid size  -===================> CHANLEN 
		INTEGER, INTENT(IN)  :: dt			! time intervals which in most of cases should be 1 hour           - ??? should be based on the hydraulic routing module
		!INTEGER, INTENT(IN) :: t 			! the current and previous time steps for the simulation, t=2
		INTEGER, INTENT(IN)  :: totLength          	! Total length (# of cells) of the domain for simulation -===========================> NLINKS
		INTEGER, INTENT(IN)  :: IXRT, JXRT
		REAL, INTENT(IN), DIMENSION(:)    :: T_a		! Air temperature 						               - As an input
		REAL, INTENT(IN), DIMENSION(:)    :: H		        ! Humidity (%) - relative humidity			                       - As an input
		REAL, INTENT(IN), DIMENSION(:)    :: V_wind	        ! Wind speed (m/s)							       - As an input
		REAL, INTENT(IN), DIMENSION(:)	  :: C_L	        ! Cloudiness (0 to 1)					                       - As an input - should find it from somewhere!
		REAL, INTENT(IN), DIMENSION(:)	  :: T_bed	        ! Conduction Layer Temperature (C)			                       - As an input
		REAL, INTENT(IN), DIMENSION(:)    :: s_in		! Incoming sortwave radiation, as an input
		REAL, INTENT(IN), DIMENSION(:)    :: Pw_river	        ! Wetted perimeter in the river channle for the desird grid (m)                 =====> XS_Peremeter
		REAL, INTENT(IN), DIMENSION(:)    :: y_w		! Water Column Depth (m)                                    	                =====> HLINK
		REAL, INTENT(IN), DIMENSION(:)	  :: S_0		! Stream gradient (unitless)							=====> ChSSlp
    REAL, INTENT(IN), DIMENSION(:)	  :: z_elv		! Elevation of station where met. data was obtained				=====> ZELEV
		REAL, INTENT(IN), DIMENSION(:)	  :: V_w		! Average Flow Velocity (m/s)							=====> velocity
		REAL, INTENT(IN), DIMENSION(:)	  :: W_w		! Wetted width (Top; m)								=====> Tw
													
		! Discharge data, instream, groundwatr flow, triburaty
		REAL, INTENT(IN), DIMENSION(:)	  :: Q_str		! Instream flow in the river channel - from sll sources (csm)                   =====> QSUM
		REAL, INTENT(IN), DIMENSION(:,:)  :: Q_GW		! Groundwater flow in the gridded based (cms)					=====> QINFLOWBASE
		REAL, INTENT(IN), DIMENSION(:,:)  :: Q_Trib		! Tributarty surface inflow in the gridded based (cms)			        =====> QSTRMVOLRT
		REAL, INTENT(IN), DIMENSION(:)	  :: T_GW		! Groundwater flow temperature in the gridded based (C)		         	- As an input
		REAL, INTENT(IN), DIMENSION(:)	  :: T_hyp		! Hyporheic Flow temperature in the gridded based (C)		         	- As an input
		REAL, INTENT(IN), DIMENSION(:)	  :: T_Trib		! Tributarty surface inflow temperature (C)					- As an input
		! Shading related parameters
		REAL, INTENT(IN), DIMENSION(:)    :: albedo		! albedo at the stream surface (0 to 1)						- ???
		REAL, INTENT(IN), DIMENSION(:)    :: SF			! Shading factor for each grid of the reach					- ???
		REAL, INTENT(IN), DIMENSION(:)    :: vts		! View to sky factor (1-SF)								

		! Fluxes
		REAL                              :: SW_rad		! Shortwave radiation flux (W/m2)
		REAL                              :: LW_rad		! Longwave radiation flux (W/m2)
		REAL                              :: LH		        ! Latent heat energy flux (W/m2)
		REAL                              :: SH		        ! Sensible heat energy flux (W/m2)
		REAL                              :: CH		        ! Conduction flux (W/m2)
		REAL,  DIMENSION(totLength)       :: Net_HeatFlux	! Net heat flux from 5 sources (W/m2)			
		
		! For hyporheic exchange incflow calculation & Porosity of the conduction layer (unitless)
		REAL, DIMENSION(IXRT,JXRT)        :: Q_hyp		! Darcy’s Law for Calculating for Hyporheic Flow (Domenico and Schwartz 1990)
		REAL, DIMENSION(IXRT,JXRT)        :: Disp_coef
    REAL		          	  :: A_s		! Cross-Sectional Across Seepage Face (m2)	
		REAL			          :: k_s		! Composite Substrate Hydraulic Conductivity (m/s) 
		REAL, INTENT(IN),    DIMENSION(:) :: C_E		! Embeddedness                                                                    - As an input
		REAL, INTENT(IN),    DIMENSION(:) :: P_s		! Substrate particle size (mm)
		REAL, INTENT(IN),    DIMENSION(:) :: k_s_0		! Dominant Substrate Hydraulic Conductivity (m/s)                                 ===========> ChanCondConst
		
		REAL                              :: h_D		! Hydraulic Head for Darcy Calculation at location i (m)
		REAL                              :: h_D_dn		! Hydraulic Head for Darcy Calculation at location i+1 (m)
		REAL                              :: Dummy1		! For calcluating the hydraulic conductiveity 
		REAL                              :: Dummy2		! For calcluating the hydraulic conductiveity 
		REAL                              :: d_h		! Change in hydraulic head per unit distance
		! Two variables for the calculations
		!REAL, INTENT(OUT)                :: rho_water          ! Density of water (kg/m3)
		!REAL, INTENT(OUT)                :: c_water	        ! Specific heat capacity of water (J/kgC)
		! Component of the main equaion of the simulated water temperature
		REAL                              :: Advection  	!
		REAL                              :: Dispersion 	! 
		REAL                              :: up
		REAL                              :: FluxTemp	        !
		!REAL                              :: Disp_coef      	! Dispersion Coefficient (m2/s)
		REAL                              :: Shear_Velocity	! Shear Velocity (m/s)
		REAL                              :: SlopeWS		! Water surface slope
	
		!REAL, INTENT(IN), DIMENSION(totLength)	  :: Z			                    ! Side slope (fraction)
		REAL,                   DIMENSION(totLength)          :: TLateral
		INTEGER, INTENT(IN),    DIMENSION(:)                  :: CHANXI, CHANYJ
		INTEGER, INTENT(IN),    DIMENSION(:,:)                :: CH_NETLNK
    INTEGER, INTENT(IN),    DIMENSION(:,:)                :: CH_NETRT
		REAL,                   DIMENSION(totLength)          :: QLateral
		REAL,    INTENT(IN),    DIMENSION(totLength,2)        :: QLINK
		INTEGER, INTENT(in)                                   :: DT_STEPS               !-- number of timestep in routing
		REAL,    INTENT(in)                                   :: DTCT
		REAL ,   INTENT(in),    DIMENSION(:)                  :: node_area
		INTEGER, INTENT(in),    DIMENSION(:)                  :: TO_NODE
		INTEGER, INTENT(IN),    DIMENSION(:)                  :: TYPEL
		REAL,    INTENT(INOUT), DIMENSION(:)                  :: T_QSUM_local	            ! -- FINAL PRODUCT
		INTEGER,                DIMENSION(totLength)          :: BC_Grid		    ! -- This is the new way I am thinking of to detec the upstream grids
		INTEGER                                               :: neighbours, i, j
    ! Simulated water temeprature (C)
		REAL,                   DIMENSION(totLength,2)        :: T_w_sim	! Simulatd water temperature for the reach in space & time (C)
    REAL                                                  :: Q_strTot, T_strTot 
				
    ! ++++++++++++++++++++++++++++update this part
		! Assigning the upstream water temeprature boundary condition to the main matrix
		
		Disp_coef = 0.
                ! Calculating hyporheic fluxes
		Do i = 1, totLength
			! Find hyporheic fluxes
			! Ratio Conductivity of dominant sunstrate
			Dummy1 = k_s_0(CH_NETLNK(CHANXI(i),CHANYJ(i))) * (1.0 - C_E(i))
                        
      ! Ratio Conductivity of sand - low range
			! 0.00002 is the embedded Substrate Hydraulic Conductivity for Silt/Sand (m/s)
			Dummy2 = 0.00002 * C_E(i)
			! True cond. (m/s); Composite Substrate Hydraulic Conductivity
			k_s = Dummy1 + Dummy2
			
			! For the last grid where there is no water surface in the downstream
			if (CH_NETLNK(CHANXI(i),CHANYJ(i)) .EQ. totLength) then
				!h_D = y_w(CH_NETLNK(CHANXI(i),CHANYJ(i)))
                                d_h = 0
                        else
                                h_D = y_w(CH_NETLNK(CHANXI(i),CHANYJ(i)))
                                h_D_dn = y_w(CH_NETLNK(CHANXI(i),CHANYJ(i))+1) - (dx * S_0(CH_NETLNK(CHANXI(i),CHANYJ(i))))
			        !Change in hydraulic head per unit distance
			        d_h = (h_D - h_D_dn) / dx

			end if
			
			!h_D = y_w(CH_NETLNK(CHANXI(i),CHANYJ(i)))

			! Cross-Sectional Area of the Seepage Face (m2)
			A_s = dx * S_0(CH_NETLNK(CHANXI(i),CHANYJ(i))) * Pw_river(CH_NETLNK(CHANXI(i),CHANYJ(i)))
			! Darcy’s Law for Calculating for Hyporheic Flow (cms 
      Q_hyp(CHANXI(i),CHANYJ(i)) = A_s * k_s * d_h 
                       
      ! Calculating dispersion coefficient
			! Calculate dispersion using equations adapted from SubMcCormick1 - HeatSourceModel source code
			! (Martin and McCutcheon 1999)
			! To enable simulating the cooling/warming effect of riffle-pool 
			SlopeWS = d_h
			if (SlopeWS < 0.0) then
				! For adverse slopes
				Shear_Velocity = V_w(CH_NETLNK(CHANXI(i),CHANYJ(i)))
			else
				Shear_Velocity = sqrt(9.8 * y_w(CH_NETLNK(CHANXI(i),CHANYJ(i))) * SlopeWS)
			end if
			! Calculating wetted width for the river channel (top with - m)
			!W_w = W_b(node) + 2.0 * Z(node) * y_w(node) => Will grab it from the routing module
			
			! Physical Dispersion Coefficient (Fischer et. al. 1979)
			! I Changed 0.011 to 0.06 in below eqn to account for natural (not straight) streams
			
      Disp_coef(CHANXI(i),CHANYJ(i)) = (0.06 * (V_w(CH_NETLNK(CHANXI(i),CHANYJ(i))) ** 2.0) * (W_w(CH_NETLNK(CHANXI(i),CHANYJ(i))) ** 2.0))/ &
						(y_w(CH_NETLNK(CHANXI(i),CHANYJ(i))) * Shear_Velocity)

			! A condition based on the adaptd methods from Martin & McCutcheon (1999) also considered with Boyd & Kasper (2003)
			! 3600 is the assumed dt in sectonds
			if ((Disp_coef(CHANXI(i),CHANYJ(i)) * ((3600.0 / dx) ** 2.0)) > 0.5) then
				Disp_coef(CHANXI(i),CHANYJ(i)) = (0.45 * (dx ** 2.0)) / 3600.0
                        end if

		end Do
	             
  
    !-- I have this long if statement for determining  the headwaters within the grid. 
    BC_Grid = 0
    T_QSUM_local = 0.

		if (dt .eq. 1) then	!-- To do this only one time!	
			neighbours = 0
			DO i = 1,IXRT  !rows
				DO j = 1,JXRT   !colsumns
                                        if ( CH_NETRT(i,j) .EQ. 0) then

					if ((i .gt. 1) .and. (i .lt. IXRT) .and. (j .gt. 1) .and. (j .lt. JXRT)) then
							if (CH_NETRT(i-1,j-1) .NE. -9999) then
                  neighbours = neighbours + 1
              end if
							if (CH_NETRT(i-1,j) .NE. -9999) then 
                  neighbours = neighbours + 1
              end if
							if (CH_NETRT(i-1,j+1) .NE. -9999) then 
                  neighbours = neighbours + 1
              end if
							if (CH_NETRT(i,j-1) .NE. -9999) then
                  neighbours = neighbours + 1
              end if
							if (CH_NETRT(i,j+1) .NE. -9999) then
                  neighbours = neighbours + 1
              end if
						 	if (CH_NETRT(i+1,j-1) .NE. -9999) then
                  neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
							if (CH_NETRT(i+1,j+1) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
					else if ((i .ne. 1) .and. (i .ne. IXRT) .and. (j .eq. 1)) then 
							if (CH_NETRT(i-1,j) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
							if (CH_NETRT(i-1,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i,j+1) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
							if (CH_NETRT(i+1,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
					else if ((i .eq. IXRT) .and. (j .ne. 1) .and. (j .ne. JXRT)) then
							if (CH_NETRT(i,j-1) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
							if (CH_NETRT(i-1,j-1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i-1,j) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
							if (CH_NETRT(i-1,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
					else if ((j .ne. 1) .and. (j.ne. JXRT) .and. (i .eq. 1)) then
							if (CH_NETRT(i,j-1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j+1) .NE. -9999) then 
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
					else if ((i .ne. 1) .and. (i .ne. IXRT) .and. (j .eq. JXRT)) then
							if (CH_NETRT(i-1,j) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i-1,j-1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i,j-1) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
							if (CH_NETRT(i+1,j) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j) .NE. -9999) then 
                 neighbours = neighbours + 1
              end if
					else if ((i .eq. 1) .and. (j .eq. 1)) then
							if (CH_NETRT(i,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
							if (CH_NETRT(i+1,j+1) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if			
					else if ((i .eq. IXRT) .and. (j .eq. 1)) then
							if (CH_NETRT(i-1,j) .NE. -9999) then
                 neighbours = neighbours + 1	
              end if
							if (CH_NETRT(i-1,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i,j+1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
					else if ((i .eq. 1) .and. (j .eq. JXRT)) then
							if (CH_NETRT(i,j-1) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j-1) .NE. -9999) then 
                 neighbours = neighbours + 1
              end if
							if (CH_NETRT(i+1,j) .NE. -9999) then
                 neighbours = neighbours + 1
              end if
					else if ((i .eq. IXRT) .and. (j .eq. JXRT)) then
							if (CH_NETRT(i-1,j-1) .NE. -9999) then
                neighbours = neighbours + 1
              end if
							if (CH_NETRT(i-1,j) .NE. -9999) then
                neighbours = neighbours + 1
              end if
							if (CH_NETRT(i,j-1) .NE. -9999) then
                neighbours = neighbours + 1	
              end if
					end if

					if ((neighbours .eq. 1) .and. (CH_NETLNK(i,j) .ne. totLength)) then
						BC_Grid(CH_NETLNK(i,j)) = 1
					end if

					neighbours = 0
          end if
				end do
			end do

      !-- Preparing the T_w_sim to be used in the first time step directly from the inputs from NoahMP
      T_w_sim = 0.
      T_w_sim(:,2) =  T_Trib - 273.
                        
		end if
		

		! Caclulating the temperature
		Do i = 1 , totLength
			! Calculate the heat fluxes
			SW_rad = SW_RAD_FLUX(CH_NETLNK(CHANXI(i),CHANYJ(i)), s_in, SF, albedo)
			LW_rad = LW_RAD_FLUX(CH_NETLNK(CHANXI(i),CHANYJ(i)), T_w_sim, T_a, H, C_L, vts)	
                        LH     = LatentHeat_FLUX(CH_NETLNK(CHANXI(i),CHANYJ(i)), SW_rad, LW_rad, T_w_sim, T_a, H, V_wind, z_elv)
			SH     = SensibleHeat_FLUX(CH_NETLNK(CHANXI(i),CHANYJ(i)), T_w_sim, LH, T_a, H, z_elv)
			CH     = BedSediment_FLUX(CH_NETLNK(CHANXI(i),CHANYJ(i)),  dx, T_w_sim, P_s, C_E, Pw_river, T_bed)
			! Net heat flux
			Net_HeatFlux(CH_NETLNK(CHANXI(i),CHANYJ(i))) = SW_rad + LW_rad + LH + SH + CH
			! Calculate mixture temperature for each node and time step 
			Advection = Net_HeatFlux(CH_NETLNK(CHANXI(i),CHANYJ(i))) / (rho_water * c_water * &
			            y_w(CH_NETLNK(CHANXI(i),CHANYJ(i))))
			
		
			! For the first time step, where the temperature is not calculated for that time step yet:
			! This is impprtant though! As, basically, here I am definig the upstream boundary condition.
			if ((BC_Grid(CH_NETLNK(CHANXI(i),CHANYJ(i))) .eq. 1) .and. (dt .eqv. 1)) then		! -- somethoing like this if statement.
                        ! => changed this one for my example, but not sure if it should be the same for the WRF-Hydro inputs, THE ISSUE WITH CH-NETLINK
                       T_w_sim(CH_NETLNK(CHANXI(i),CHANYJ(i)),1) =(Q_Trib(CHANXI(i),CHANYJ(i))*(T_Trib(i)-273.1)+ &                 
							         Q_GW(CHANXI(i),CHANYJ(i))*(T_GW(i)-273.1)+ &
							         Q_hyp(CHANXI(i),CHANYJ(i))*(T_hyp(i)-273.1))/ &
							        (Q_Trib(CHANXI(i),CHANYJ(i))+Q_GW(CHANXI(i),CHANYJ(i))+ &
							         Q_hyp(CHANXI(i),CHANYJ(i)))
			end if
                        

			! For the very upstream grid where there is no information in the prior grid:
			if (BC_Grid(CH_NETLNK(CHANXI(i),CHANYJ(i))) .eq. 1) then
				up = 0.
				Dispersion = (-1.0 * Disp_coef(CHANXI(i),CHANYJ(i)) * ((Net_HeatFlux(CH_NETLNK(CHANXI(i),CHANYJ(i))) / &
							 (rho_water * c_water * y_w(CH_NETLNK(CHANXI(i),CHANYJ(i))) * &
							  V_w(CH_NETLNK(CHANXI(i),CHANYJ(i))))) - up)) / dx
				FluxTemp = T_w_sim(CH_NETLNK(CHANXI(i),CHANYJ(i)),1) + (Advection + Dispersion) * (dx/V_w(CH_NETLNK(CHANXI(i),CHANYJ(i))))
				!BC_Grid = .FALSE.
			else	
				up = (Net_HeatFlux(CH_NETLNK(CHANXI(i),CHANYJ(i))-1) / &
					(rho_water * c_water * y_w(CH_NETLNK(CHANXI(i),CHANYJ(i))-1) * V_w(CH_NETLNK(CHANXI(i),CHANYJ(i))-1)))
				Dispersion = (-1.0 * Disp_coef(CHANXI(i),CHANYJ(i)) * ((Net_HeatFlux(CH_NETLNK(CHANXI(i),CHANYJ(i))) / &
			                 (rho_water * c_water * y_w(CH_NETLNK(CHANXI(i),CHANYJ(i))) * &
						      V_w(CH_NETLNK(CHANXI(i),CHANYJ(i))))) - up)) / dx
				FluxTemp = T_w_sim(CH_NETLNK(CHANXI(i),CHANYJ(i))-1,1) + (Advection + Dispersion) * (dx/V_w(CH_NETLNK(CHANXI(i),CHANYJ(i))))

			end if


			! -- We don't apply the lateral inflows to T_w_sim; They will be applied to the final porduct, T_QSUM later, 
			T_w_sim(CH_NETLNK(CHANXI(i),CHANYJ(i)),2) = FluxTemp
	   

			!-- getting a weighted average
			if (CH_NETLNK(CHANXI(i),CHANYJ(i)) .gt. 0) then
                !TLateral(i) = (Q_Trib(CH_NETLNK(CHANXI(i),CHANYJ(i)))*(T_Trib(i)-273.1)+ &             => changed this one for my example, but not sure if it should be the same for the WRF-Hydro inputs,
							  ! Q_GW(CH_NETLNK(CHANXI(i),CHANYJ(i)))*(T_GW(i)-273.1)+ &       => changed this one for my example, but not sure if it should be the same for the WRF-Hydro inputs,
                TLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))) = (Q_Trib(CHANXI(i),CHANYJ(i))*(T_Trib(i)-273.1)+ & 
                                                            Q_GW(CHANXI(i),CHANYJ(i))*(T_GW(i)-273.1)+ &
				                                                    Q_hyp(CHANXI(i),CHANYJ(i))*(T_hyp(i)-273.1))/ &
				                                                   (Q_Trib(CHANXI(i),CHANYJ(i))+Q_GW(CHANXI(i),CHANYJ(i))+ &
				                                                    Q_hyp(CHANXI(i),CHANYJ(i)))
			          !-- I am not about this part "(DT_STEPS*node_area(CH_NETLNK(CHANXI(i),CHANYJ(i)))/1000./DTCT)" here. 
                !-- I should check this when I wanna combine my code with the WRF-Hydro. I assume it's correct for the WRF-Hydro
				        QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))) = ((Q_Trib(CHANXI(i),CHANYJ(i)) + Q_GW(CHANXI(i),CHANYJ(i))) / &
									                                         (DT_STEPS*node_area(CH_NETLNK(CHANXI(i),CHANYJ(i)))/1000./DTCT)) + &
									                                          Q_hyp(CHANXI(i),CHANYJ(i))
														  
			end if

                              
      !-- Here I wanna detect the cells/grids discharging to the current cell
      !-- Not very efficient. Should check with Soren to see if I can find a better way
      Q_strTot = 0.
      T_strTot = 0.
      Do j = 1, totLength
         if (TO_NODE(j) .eq. i) then
             Q_strTot = Q_strTot + Q_str(j)
             T_strTot = (Q_strTot * T_strTot + Q_str(j) * T_w_sim(j,1)) / &
                        (Q_strTot +  Q_str(j))
          end if
      end do

			
			!-- inflow to node across each face  
			!if(TYPEL(i) .eq. 0) then                         ! -- If stream; only regular nodes have to attribute
			T_QSUM_local(i) = (Q_strTot * T_strTot + &
			                   QLINK(CH_NETLNK(CHANXI(i),CHANYJ(i)),1)*T_w_sim(i,2) + &
		         	           QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i)))*TLateral(i)) / &
		         	           (Q_strTot+QLINK(CH_NETLNK(CHANXI(i),CHANYJ(i)),1)+QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))))

			print *,"T_Qsub(1):", T_QSUM_local(1)
      print *,"stop running the code"
      stop

      ! Update the vector to be used in the next time step.		 
			T_w_sim(i,1) = T_QSUM_local(i)						
			
	
			! -- Q_str = QSUM(TO_NODE(i))                   : The amount of the water is gonna be sent to the grid in the downstream?
			! -- 						  QSUM's unit is volume, that means the QLateral also should be converted to the volume. 
			! -- T_QSUM(TO_NODE(i))                         : The temperature we are gonna attach to the QSUM to affect it's downstream
			! -- QLINK(i,1)                                 : Volume of the water that updates the QSUM and in the case of cells 9 & 13 go to 14 (my example),  
			!                                                 They could both update the flux of the cell #14. It's unit also is volume
			! -- T_w_sim(i,2)                               : Simulated water temperature based on the energy budget calculations, not affected by the lateral inlflows
			! -- QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i)))   : Lateral surface and subsurface inflows
			! -- TLateral(i)                                : Temperature of the lateral surface and subsurface inflows,(combined) 
			!						  based on the equation I showed earlier today


			! For now, the idea is to run the temperature module based on the time step we have for the hydraulics routing module but report the results only for every hour. 

		end DO
                
    !-- Updating the T_w_sim for the next timestep
    T_w_sim(:,2) = T_w_sim(:,1)
		
!#endif


end subroutine river_TEMPERATURE_SOLVER



! ------------------------------------------------
!   FUNCTION Shortwave radiation flux
! ------------------------------------------------
real function SW_RAD_FLUX(space, s_in, SF, albedo)
	! Shortwave radiation flux for the desired time step and grid
	implicit none
        ! The node we are calculating the flux for that
	INTEGER, INTENT(IN)                         :: space		
        REAL,    INTENT(IN),  DIMENSION(:)  :: s_in, SF, albedo
	
	! For more information, see Magnusson et al., (2012) and Maidment, (1993)
	SW_RAD_FLUX = s_in(space) * (1.0 - SF(space)) * (1.0 -  albedo(space))
	
end function SW_RAD_FLUX

! ------------------------------------------------
!   FUNCTION Longwave radiation flux
! ------------------------------------------------

real function LW_RAD_FLUX(space, T_w_sim, T_a, H, C_L, vts)
	! Longwav radiation flux for the desired time step and grid
	implicit none

	REAL    :: SBConst	! Stefan-Boltzman constant, 5.6696*10-8 (kg·s-3·K-4)
	REAL    :: StrTemp	! simulated water temperature for the back to atmosphere part of the LW flux
	REAL    :: satVapPre	! Saturation Vapor Pressure (mbar)
	REAL    :: VapPreReal  	! Real vapor pressure (mbar)
	REAL    :: emis		! Emissivity of the Atmosphere (unitless)
	REAL    :: LW_Atm	! Atmospheric longwave radiation
	REAL    :: LW_LC	! Landcover radiation
	REAL    :: LW_Back	! Back radiation
        INTEGER :: space        ! The node we are calculating the flux for that
        REAL,    INTENT(IN),  DIMENSION(:,:)  ::  T_w_sim
        REAL,    INTENT(IN),  DIMENSION(:)    ::  T_a, H, C_L, vts 
        
	SBConst = 0.000000056696;	! Initializing the Stefan-Boltzman constant

	if (space .EQ. 1) then
		StrTemp = T_w_sim(space,2)
	else
		StrTemp = T_w_sim(space-1,2)
	end if
	
	! Saturation Vapor Pressure (modified from Chapra 1997 and Raudkivi 1979)
	satVapPre = 0.611 * exp((17.27 * T_a(space)) / (T_a(space) + 237.3))
	! Vapor Pressure (Bedient and Huber 1992)
	VapPreReal = (H(space) / 100.0) * satVapPre
	! Atmospheric Emissivity (Dingman 2002, Kustas et al. 1994)
	emis = 1.72 * (((VapPreReal) / (T_a(space) + 273.2))**(1.0 / 7.0)) * &
		(1 + ((0.22 * C_L(space)) ** 2.0))		! See Dingman (2002) p 282
	
        ! Put a control here		
	if (emis > 0.96) then
            emis = 0.96
        end if
	
	! Atmospheric Longwave Radiation Flux Attenuated in Water Column 
	! (Benyahya et al.,2010; Boyd and Kasper, 2003; Westhoff et al., 2007, 2011)
	LW_Atm = 0.96 * emis * SBConst * ((T_a(space) + 273.2)**4.0) * vts(space)
	! Land cover longwave radiation (Boyd and Kasper, 2003; Westhoff et al., 2007)
	LW_LC = 0.96 * (1.0 - vts(space)) * 0.96 * SBConst * ((T_a(space) + 273.2) ** 4.0)
	! Back radiation
	! (Boyd and Kasper, 2003; Evans et al., 1998; Kim and Chapra, 1997; Ouellet et al., 2014; Westhoff et al., 2007)
	LW_Back = -0.96 * SBConst * (StrTemp ** 4.0)
	
	! Atmospheric, back, and landcover longwave radiation
	LW_RAD_FLUX = LW_Atm + LW_LC + LW_Back

end function LW_RAD_FLUX
	
	
! ------------------------------------------------
!   FUNCTION Latent heat flux
! ------------------------------------------------
real function LatentHeat_FLUX(space, SW_rad, LW_rad, T_w_sim, T_a, H, V_wind, z_elv)
	
	implicit none
	REAL    :: c_air = 1004.	! Heat capacity of air (J/kg deg C)
	REAL    :: rho_air = 1.2041	! Density of air at 20 deg C (kg/m^3)
	REAL    :: StrTemp		! simulated water temperature at the previous location
	REAL    :: L_e			! Latent Heat of Vaporization
	REAL    :: satVapPreLat		! Saturation Vapor Pressure (mbar)
	REAL    :: VapPreReal		! Real vapor pressure (mbar)
	REAL    :: s			! Slope of the saturation vapor pressure curve (kPa/C)
	REAL    :: r_a			! Aerodynamic resistance (s/m)
	REAL    :: Pa_air		! Air pressure (mbar)
	REAL    :: psy			! psychrometric constant(kPa/C)
	REAL    :: E			! Penman open water evaporation
	INTEGER :: space         	! The node we are calculating the flux for that
        REAL    :: SW_rad, LW_rad
        REAL,    INTENT(IN),  DIMENSION(:,:)  ::  T_w_sim
        REAL,    INTENT(IN),  DIMENSION(:)    ::  T_a, H, V_wind, z_elv 
	
	if (space .EQ. 1) then
		StrTemp = T_w_sim(space,2)
	else
		StrTemp = T_w_sim(space-1.0,2)
	end if
	
	! <<<<<<<<< Evaporation section >>>>>>>>
	! Calculate the latent heat of vaporization J/kg ((Maidment, 1993)
	L_e = 1000000 * (2.501 - (0.002361 * StrTemp))		
	! Calculate the slope of the saturation vapor pressure curve at a given air temperature
	! (modified from Chapra 1997 and Raudkivi 1979)
	satVapPreLat = 0.611 * exp((17.27 * T_a(space)) / (T_a(space) + 237.3))
	! Vapor Pressure Based on Saturation Vapor Pressure Using Stream Temperature
	! (Bedient and Huber 1992)
	VapPreReal = (H(space) / 100.0) * satVapPreLat
	! Slope of the saturation vapor pressure curve (kPa/C) is calculated as 
	! (Maidment, 1993)
	s = (4100.0 * satVapPreLat) / ((237.0 + T_a(space)) ** 2.0)
	! Calculate the aerodynamic resistance (s/m)
	! (Westhoff et al., 2007)
	r_a = 245.0 / ((0.54 * V_wind(space)) + 0.5)
	! Calculate air pressure
	Pa_air = 101.3 - (0.0105 * z_elv(space))
	! Calculate psychrometric constant(kPa / deg C) (based on air pressure(Pa), 
	! (value should be adjusted for different site elevations)
	! Ratio of water to dry air = .622, and the latent heat of water vaporization = 2.45E6 (J/kgC)
	! (Cuenca 1989)
	psy = (c_air * Pa_air) / (0.622 * 2450000.0)
	! Calculate the Penman open water evaporation
	! (Maidment, 1993; Westhoff et al., 2007)
	E = ((s * (SW_rad + LW_rad)) / (1000.0 * L_e * (s + psy))) &
		+ ((c_air * rho_air * psy * (satVapPreLat - VapPreReal)) / (1000.0 * L_e * r_a * (s + psy)))
	
	LatentHeat_FLUX = -rho_water * L_e * E
	

end function LatentHeat_FLUX


! ------------------------------------------------
!   FUNCTION Sensible heat flux
! ------------------------------------------------
real function SensibleHeat_FLUX(space, T_w_sim, LH, T_a, H, z_elv )
	
	implicit none
	REAL    :: c_air = 1004.	! Heat capacity of air (J/kgC)
	REAL    :: rho_air = 1.2041	! Density of air at 20 deg C (kg/m3)
	REAL    :: ews			! Saturation vapor pressure of the evaporating surface (KPa)
	REAL    :: ewa			! Actual vapor pressure of the evaporation surface (KPa)			
	REAL    :: StrTemp		! Simulated water temperature at the previous location
	REAL    :: Pa_air		! Air pressure (mbar)
	REAL    :: H_tmp		! Temporary value for assigning the humidity from the input
	REAL    :: B_r			! Bowen's ratio
	INTEGER :: space
        REAL    :: LH
        REAL,    INTENT(IN),  DIMENSION(:,:)  ::  T_w_sim
        REAL,    INTENT(IN),  DIMENSION(:)    ::  T_a, H, z_elv
       

	if (space .EQ. 1) then
		StrTemp = T_w_sim(space,2)
	else
		StrTemp = T_w_sim(space-1.0,2)
	endif
	
	! Saturation vapor pressure of the evaporating surface (KPa)
	! (Dingman, 1994; Maidment, 1993)
	ews = 0.61275 * exp((17.27 * StrTemp) / (237.3 + StrTemp))
	! Actual vapor pressure of the evaporation surface (KPa)
	! To handle the infinte problem in B_r equation, consider the umidity of 100 as 99
	H_tmp = H(space)
        if (H(space) .EQ. 100.0) then 
            H_tmp = 99.0 
        endif
	ewa = (H_tmp / 100.0) * ews
	! Adiabatic atmospheric pressure adjusted for elevation above sea level
	! (Westhoff et al., 2007)
	Pa_air = 101.3 * (((293 - (0.0065 * z_elv(space))) / 293)**5.256)
	! Bowen ratio
	! (Evans et al.,1998; Magnusson et al., 2012; Webb and Zhang, 1997; Westhoff et al., 2007)
	! (modified for units from Bedient and Huber 1992)
	B_r = 0.00061 * Pa_air * ((StrTemp - T_a(space)) / (ews - ewa))
		
	SensibleHeat_FLUX = B_r * LH

	
end function SensibleHeat_FLUX


! ------------------------------------------------
!   FUNCTION Conduction Flux
! ------------------------------------------------
real function BedSediment_FLUX(space, dx, T_w_sim, P_s, C_E, Pw_river, T_bed)
	
	implicit none

	REAL  :: StrTemp		! Simulated water temperature at the previous location
	REAL  :: Dummy1			! 
	REAL  :: Dummy2			!
	REAL  :: P			! Estimated porosity
	REAL  :: ThermalDiffuse		! Volumetric Weighted Thermal Diffusivity (m2/s)
	REAL  :: Sed_ThermalDiffuse	! Substrate Thermal Diffusivity (m2/s)
	REAL  :: H2O_ThermalDiffuse	! Water Thermal Diffusivity (m2/s)
	REAL  :: Ratio_Sediment		! To save the value of Volume_Sediment / Volume_cl
	REAL  :: Sed_Depth		! Sediment depth used in bed conduction (m)
	REAL  :: Volume_Sediment	! Substrate Volume in Conduction Layer (m3)
	REAL  :: Volume_cl		! Total Volume in Conduction Layer (m3)
	REAL  :: Volume_H2O		! Water Volume in Conduction Layer (m3)
	REAL  :: Density		! Volumetric Weighted Density (kg/m3)
	REAL  :: Sed_Density		! Substrate Density (kg/m3)
	REAL  :: HeatCapacity		! Volumetric Weighted Specific Heat Capacity (J/kgC)
	REAL  :: Sed_HeatCapacity	! Substrate Specific Heat Capacity (J/kgC)
        REAL  :: Ratio_H2O 
        INTEGER :: space, dx
        REAL,    INTENT(IN),  DIMENSION(:,:)  ::  T_w_sim
        REAL,    INTENT(IN),  DIMENSION(:)    ::  P_s, C_E, Pw_river, T_bed
	
	! Assigning the numbers
	Sed_ThermalDiffuse = 0.0000045
	H2O_ThermalDiffuse = 0.00000014331
	Sed_Density = 1600.0
	Sed_HeatCapacity = 2219.0

	if (space .EQ. 1) then
		StrTemp = T_w_sim(space,2)
	else
		StrTemp = T_w_sim(space-1.0,2)
	end if

	! ======================================================
	! Calculate Volumetric Ratio of Water and Substrate
	! Code uses this ratio to estimate conduction constants
	! First, calculating the porosity
	! Ratio Size of dominant substrate
	Dummy1 = P_s(space) * (1.0 - C_E(space))
	! Ratio Conductivity of sand - low range
	Dummy2 = 0.062 * C_E(space)
	! Estimated Porosity
	! (adopted from Bedient and Huber 1992)
	P = (0.3683 * ((Dummy1 + Dummy2)**(-0.0641)))
	! ======================================================
	! Variables used in bed conduction
	! Calculate the sediment depth (conduction layer)
	! (Assumes 100 Particles Thickness)
	! 0.1m < particleSize < 0.3m
	Sed_Depth = 10.0 * P_s(space) / 1000.0;
	if (Sed_Depth > 1.0) then
		Sed_Depth = 1.0;
	else if (Sed_Depth < 0.1) then
		Sed_Depth = 0.1
	end if
	! ======================================================
	
	Volume_Sediment = (1.0 - P) * Pw_river(space) * Sed_Depth * dx
	Volume_cl = Pw_river(space) * Sed_Depth * dx
	Ratio_Sediment = Volume_Sediment / Volume_cl
	Volume_H2O = P * Pw_river(space) * Sed_Depth * dx
	Ratio_H2O = Volume_H2O / Volume_cl
	ThermalDiffuse = (Sed_ThermalDiffuse * Ratio_Sediment) + (H2O_ThermalDiffuse * Ratio_H2O)
	Density = (Sed_Density * Ratio_Sediment) + (rho_water * Ratio_H2O)
	HeatCapacity = (Sed_HeatCapacity * Ratio_Sediment) + (c_water * Ratio_H2O)

	BedSediment_FLUX = ThermalDiffuse * Density * HeatCapacity * ((T_bed(space)-273.) - StrTemp) / (Sed_Depth / 2.0)


end function BedSediment_FLUX

end module module_river_temperature
