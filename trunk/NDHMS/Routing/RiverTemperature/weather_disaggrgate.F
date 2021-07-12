!===================================================================================================
! Subroutine Name:
!   subroutine disaggregateWeather
! Author(s)/Contact(s):
!   R. Abdi <rabdi><ucar><edu>
! Abstract:
!   Disaggregates states and parameters from coarse grid to fine grid for the weather data
! History Log:
! Usage:
! Parameters:
! Input Files:
! Output Files:
! Condition codes:
! User controllable options: None.
! Notes:
!===================================================================================================

MODULE disaggregateWeatherModule

use module_lsm_forcing, only: weather_2D_inputs
use config_base, only: noah_lsm, nlst
use module_NoahMP_hrldas_driver
use module_RT_data, only: rt_domain

IMPLICIT NONE
	
type  :: fineWeatherData
	    !INTEGER                                  :: NLINKS
            REAL, DIMENSION(:), allocatable          :: TAIR_FINE_1D, RELH_FINE_1D, WINDSPD_FINE_1D, SOLARRAD_FINE_1D
            REAL, DIMENSION(:), allocatable          :: TMN_1D_FINE, TSLB_1D_FINE, TSK_1D_FINE
		
	    !type (weather_2D_inputs), pointer        :: weather_2D => null() 
	    type (vectorizing_tools), pointer        :: vectorizer => null()
	    type (NoahMP_vectorizing_tools), pointer :: NMPvectorizer => null()

    logical :: pointer_allocation_guard = .false.
		
	contains

	procedure :: init => fineWeatherData_init
        procedure :: destroy => fineWeatherData_destroy
        procedure :: run => fineWeatherData_run
		
end type fineWeatherData
	
! --------------------------------------------------------------------------
contains 

subroutine fineWeatherData_init(this, Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D, &
				SolarRad_fine_1D, TMN_1D_fine, TSK_1D_fine,TSLB_1D_fine)
	implicit none
	class(fineWeatherData), intent(inout)       :: this                   ! the type object being initialized
	!character(len=20)                           :: Reach_number_string

	REAL,    INTENT(inout), DIMENSION(:)        :: Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D
 	REAL,    INTENT(inout), DIMENSION(:)        :: SolarRad_fine_1D, TMN_1D_fine, TSK_1D_fine,TSLB_1D_fine
	

	!this%Tair_fine_1D       = Tair_fine_1D
	!this%RelH_fine_1D       = RelH_fine_1D
	!this%windSpd_fine_1D    = windSpd_fine_1D
	!this%SolarRad_fine_1D   = SolarRad_fine_1D
	!this%TMN_1D_fine        = TMN_1D_fine
	!this%TSK_1D_fine        = TSK_1D_fine
	!this%TSLB_1D_fine       = TSLB_1D_fine
	

		
end subroutine fineWeatherData_init
! --------------------------------------------------------------------------

	! fineWeatherData Destructor
subroutine fineWeatherData_destroy(this)
      implicit none
      class(fineWeatherData), intent(inout) :: this ! object being destroyed
end subroutine fineWeatherData_destroy

! --------------------------------------------------------------------------

subroutine fineWeatherData_run(this, did)
	
	implicit none
  	class(fineWeatherData), intent(inout)       ::    this
  
	call disaggregateWeather(this,                             &
                           	 rt_domain(did)%IX,	           &
                           	 rt_domain(did)%JX,		   &
		                 rt_domain(did)%IXRT,     	   &
			       	 rt_domain(did)%JXRT,   	   &
				        this%weather_2D%T2,	   & 
				        this%weather_2D%short,     &
				        this%weather_2D%q2x,	   &
				        this%weather_2D%u,	   & 
                                 rt_domain(did)%NLINKS,		   &
				 rt_domain(did)%CH_NETRT, 	   &
			         rt_domain(did)%CHANXI,		   & 
				 rt_domain(did)%CHANYJ,		   &
				        this%vectorizer%LAKE_MSKRT,&
				 module_NoahMP_hrldas_driver%TMN,  &
				 module_NoahMP_hrldas_driver%TSK,  &
				 module_NoahMP_hrldas_driver%TSLB, &
				 noah_lsm%nsoil,                   &
				 nlst(did)%AGGFACTRT               &
                                 )
							
end subroutine fineWeatherData_run

! --------------------------------------------------------------------------

subroutine disaggregateWeather(this, IX, JX, IXRT, JXRT, Tair, SolarRad, RelH, windSpd, &
                               NLINKS, CH_NETRT, CHANXI, CHANYJ, LAKE_MSKRT, TMN, TSK, TSLB, nsoil, AGGFACTRT)

    implicit none
    class(fineWeatherData), intent(inout)                ::    this
    ! Input Variables ------------------------------------------------------------------------						   
    integer                    		       :: IX, JX, IXRT, JXRT, NLINKS, nsoil, AGGFACTRT
    REAL, INTENT(IN), DIMENSION(IX,JX)	       :: Tair, SolarRad, RelH, windSpd, TMN, TSK
    REAL, INTENT(IN), DIMENSION(IX,1:nsoil,JX) :: TSLB
    INTEGER,INTENT(in), DIMENSION(IXRT,JXRT)   :: CH_NETRT, LAKE_MSKRT
    INTEGER, INTENT(in), DIMENSION(NLINKS)     :: CHANXI, CHANYJ						   
    ! Local Variables ------------------------------------------------------------------------					   
    integer                    :: i, j         ! coarse grid loop indices
    integer                    :: IXXRT, JYYRT ! fine grid i,j coordinates
    integer                    :: AGGFACYRT, AGGFACXRT ! fine grid aggregation factors
    ! 
    !integer	               :: AGGFACTRT = 2			   ! disaggregation factor, I assume as 2, 500m to 2x250m; should be checked later
    ! 
    REAL,  DIMENSION(IXRT,JXRT)	:: Tair_fine, SolarRad_fine, RelH_fine, windSpd_fine, TMN_fine, TSK_fine, TSLB_fine

		do J=1,JX ! Start coarse grid j loop
			do I=1,IX ! Start coarse grid i loop
		
				! For now, I hard-coded the AGGFACTRT, the disaggregation factor as 4
				! => AGGFACTRT-1 = 4-1 = 3
				do AGGFACYRT= AGGFACTRT-1,0,-1 ! Start disagg fine grid j loop
					do AGGFACXRT= AGGFACTRT-1,0,-1 ! Start disagg fine grid i loop
				
						IXXRT = I * AGGFACTRT - AGGFACXRT ! Define fine grid i
						JYYRT = J * AGGFACTRT - AGGFACYRT ! Define fine grid j
						
						Tair_fine(IXXRT,JYYRT) = Tair(I,J)
						SolarRad_fine(IXXRT,JYYRT) = SolarRad(I,J)
						RelH_fine(IXXRT,JYYRT) = RelH(I,J)
						windSpd_fine(IXXRT,JYYRT) = windSpd(I,J)
						
						TMN_fine(IXXRT,JYYRT)  = TMN(I,J)
						TSK_fine(IXXRT,JYYRT)  = TSK(I,J)
						TSLB_fine(IXXRT,JYYRT) = TSLB(I,nsoil,J)
						
					end do
				end do
					
			end do
		end do
		
		
		DO i = 1,NLINKS

			if((CH_NETRT(CHANXI(i), CHANYJ(i) ) .eq. 0) .and. &
			   (LAKE_MSKRT(CHANXI(i),CHANYJ(i)) .lt.0)) then			
					this%Tair_fine_1D(i) = Tair_fine(CHANXI(i),CHANYJ(i))
					this%SolarRad_fine_1D(i) = SolarRad_fine(CHANXI(i),CHANYJ(i))
					this%RelH_fine_1D(i) = RelH_fine(CHANXI(i),CHANYJ(i))
					this%windSpd_fine_1D(i) = windSpd_fine(CHANXI(i),CHANYJ(i))
					this%TMN_1D_fine(i) = TMN_fine(CHANXI(i),CHANYJ(i))
					this%TSK_1D_fine(i) = TSK_fine(CHANXI(i),CHANYJ(i))
					this%TSLB_1D_fine(i) = TSLB_fine(CHANXI(i),CHANYJ(i))
					
			end if
		end do


end subroutine disaggregateWeather
	
	
END MODULE disaggregateWeatherModule
