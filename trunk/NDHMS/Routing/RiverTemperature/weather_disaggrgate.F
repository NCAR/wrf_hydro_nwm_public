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
!use module_channel_routing, only: vectorizing_tools 
use module_sf_noahmpdrv, only: NoahMP_vectorizing_tools

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

subroutine fineWeatherData_init(this, IX, JX,  T2, q2x, u, short,  &
					                         LAKE_MSKRT, TMN, TSK, TSLB, nsoil, reach_number, &
                                  ims,ime, jms,jme, &
                                  Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D, SolarRad_fine_1D, TMN_1D_fine,&
                                  TSK_1D_fine,TSLB_1D_fine)
	implicit none
	class(fineWeatherData), intent(inout)       :: this                   ! the type object being initialized
	INTEGER, INTENT(in)                         :: IX,JX, reach_number		! coarse grid i,j dims
	
	REAL,    INTENT(in), DIMENSION(ix,jx)       :: T2	                    ! Air temperature
	REAL,    INTENT(in), DIMENSION(ix,jx)       :: q2x	                  ! Humidity
	REAL,    INTENT(in), DIMENSION(ix,jx)       :: u	                    ! Near surface wind in the u-component
	REAL,    INTENT(in), DIMENSION(ix,jx)       :: short	                ! Shortwave radiation
	! -- for vectorizing
	
	INTEGER, INTENT(in), DIMENSION(IXRT,JXRT)   :: CH_NETRT
	 
	character(len=20)                           :: Reach_number_string
	! -- for the stuff that are coming from NoahMP
	INTEGER, INTENT(in)                         :: nsoil, ims,ime, jms,jme
	REAL,    INTENT(in), DIMENSION(ix,jx)       :: TMN
	REAL,    INTENT(in), DIMENSION(ix,jx)       :: TSK
	REAL,    INTENT(in), DIMENSION(ix,nsoil,jx) :: TSLB
	
	REAL,    INTENT(inout), DIMENSION(NLINKS)      :: Tair_fine_1D, RelH_fine_1D, windSpd_fine_1D
 	REAL,    INTENT(inout), DIMENSION(NLINKS)      :: SolarRad_fine_1D, TMN_1D_fine, TSK_1D_fine,TSLB_1D_fine
	

  this%Tair_fine_1D       = Tair_fine_1D
  this%RelH_fine_1D       = RelH_fine_1D
  this%windSpd_fine_1D    = windSpd_fine_1D
  this%SolarRad_fine_1D   = SolarRad_fine_1D
  this%TMN_1D_fine        = TMN_1D_fine
  this%TSK_1D_fine        = TSK_1D_fine
  this%TSLB_1D_fine       = TSLB_1D_fine
	
	if (this%pointer_allocation_guard .eq. .false. ) then
			! try to allocate input
               ! allocate (this%input)
               ! if ( .not. associated(this%input) ) then
                        ! if the input structure could not be created, call hydro_stop.
                        !write(Reach_number_string, "(I15)") reach_number
                        !call hydro_stop("ERROR: Failure to allocate temeprature module input structure for reach " &
                        !// trim(ADJUSTL(Reach_number_string)) // ".")
                !else
                ! initialize the input structure
                !call this%input%init()
                !end if
		
                ! try to allocate output
                !allocate ( this%output )
                !if ( .not. associated(this%output) ) then
                       ! if the output structure could not be created, call hydro_stop.
                       !write(Reach_number_string, "(I15)") reach_number
                       !call hydro_stop("ERROR: Failure to allocate temeprature module output structure for reach " &
                       !// trim(ADJUSTL(Reach_number_string)) // ".")
                !else
                       ! initialize the output structure
                       !call this%output%init()
                !end if

		! try to allocate weather_2D
		allocate ( this%weather_2D )
		if ( .not. associated(this%weather_2D) ) then
			! if the output structure could not be created, call hydro_stop.
                        write(Reach_number_string, "(I15)") reach_number
                        !call hydro_stop("ERROR: Failure to allocate temeprature module weather_2D for reach " &
                        !// trim(ADJUSTL(Reach_number_string)) // ".")	
		else
                ! initialize weather_2D
                        !call this%weather_2D%init(IX, JX, IXRT, JXRT, T2, q2x, u, short)	! 2D corse grid size based		
                        !call this%weather_2D%init(IX, JX, T2, q2x, u, short)	! 2D corse grid size based
                        call this%weather_2D%init(IX, JX, T2, q2x, u, short)

                        ! I am calling this next function for debugging/testing 
                        call this%weather_2D%inputs()
                        
		end if
			
		!! try to allocate vectorizer
		!allocate ( this%vectorizer )
		!if ( .not. associated(this%vectorizer) ) then
			!! if the output structure could not be created, call hydro_stop.
                        !write(Reach_number_string, "(I15)") reach_number
                        !!call hydro_stop("ERROR: Failure to allocate temeprature module vectorizer for reach " &
                        !!// trim(ADJUSTL(Reach_number_string)) // ".")	
		!else
                        !! initialize vectorizer
                        !call this%vectorizer%init(NLINKS, IXRT, JXRT, CH_NETRT, CHANXI, CHANYJ, LAKE_MSKRT)
                        
                        ! I am calling this next function for debugging/testing
                        !call this%vectorizer%inputs_vec()						
		!end if
			
		! try to allocate NMPvectorizer
		allocate ( this%NMPvectorizer)
		if ( .not. associated(this%NMPvectorizer) ) then
			! if the output structure could not be created, call hydro_stop.
                        write(Reach_number_string, "(I15)") reach_number
                        !call hydro_stop("ERROR: Failure to allocate temeprature module NMPvectorizer for reach " &
                        !// trim(ADJUSTL(Reach_number_string)) // ".")	
		else
                       ! initialize NMPvectorizer
                       call this%NMPvectorizer%init(ims,ime, jms,jme, TMN, TSK, TSLB, nsoil)

                       ! I am calling this next function for debugging/testing 
                       call this%NMPvectorizer%noahMP_inputs()						
		end if
		this%pointer_allocation_guard = .true.

             

	end if
		
end subroutine fineWeatherData_init
! --------------------------------------------------------------------------

	! fineWeatherData Destructor
subroutine fineWeatherData_destroy(this)
      implicit none
      class(fineWeatherData), intent(inout) :: this ! object being destroyed
end subroutine fineWeatherData_destroy

! --------------------------------------------------------------------------

subroutine fineWeatherData_run(this)
	
	implicit none
  class(fineWeatherData), intent(inout)       ::    this
  
	call disaggregateWeather(this,                      &
                           this%weather_2D%IX,				&
                           this%weather_2D%JX,				&
		                       this%vectorizer%IXRT,     	&
			       	             this%vectorizer%JXRT,   		&
				                   this%weather_2D%T2,				&
				                   this%weather_2D%short,    	&
				                   this%weather_2D%q2x,	     	&
				                   this%weather_2D%u,	     		&
                           this%vectorizer%NLINKS,		&
				                   this%vectorizer%CH_NETRT, 	&
			                     this%vectorizer%CHANXI,		&
				                   this%vectorizer%CHANYJ,		&
				                   this%vectorizer%LAKE_MSKRT,&
				                   this%NMPvectorizer%TMN,		&
				                   this%NMPvectorizer%TSK,		&
				                   this%NMPvectorizer%TSLB,   &
				                   this%NMPvectorizer%nsoil   &
                           )
							
end subroutine fineWeatherData_run

! --------------------------------------------------------------------------

subroutine disaggregateWeather(this, IX, JX, IXRT, JXRT, Tair, SolarRad, RelH, windSpd, &
                               NLINKS, CH_NETRT, CHANXI, CHANYJ, LAKE_MSKRT, TMN, TSK, TSLB, nsoil)

   	implicit none
    class(fineWeatherData), intent(inout)                ::    this
		! Input Variables ------------------------------------------------------------------------						   
		integer                    		         :: IX, JX, IXRT, JXRT, NLINKS, nsoil
		REAL, INTENT(IN), DIMENSION(IX,JX)	   :: Tair, SolarRad, RelH, windSpd, TMN, TSK
    REAL, INTENT(IN), DIMENSION(IX,1:nsoil,JX) :: TSLB
		INTEGER,INTENT(in), DIMENSION(IXRT,JXRT)   :: CH_NETRT, LAKE_MSKRT
		INTEGER, INTENT(in), DIMENSION(NLINKS)     :: CHANXI, CHANYJ						   
		! Local Variables ------------------------------------------------------------------------					   
		integer                    :: i, j         ! coarse grid loop indices
		integer                    :: IXXRT, JYYRT ! fine grid i,j coordinates
		integer                    :: AGGFACYRT, AGGFACXRT ! fine grid aggregation factors
		! 
    integer	                   :: AGGFACTRT = 2			   ! disaggregation factor, I assume as 2, 500m to 2x250m; should be checked later
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
