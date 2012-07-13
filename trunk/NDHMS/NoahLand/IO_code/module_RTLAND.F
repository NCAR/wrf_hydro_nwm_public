module module_rtland
  USE module_sf_Noahlsm_wrf_input_rt
! USE module_sf_Noahlsm_param_init_rt
  USE module_Noahlsm_utility_rt
  USE module_sf_noahlsm_rt
  USE module_sf_Noahlsm_param_init_rt
  USE module_date_utilities_rt
  USE module_Noah_distr_routing
  USE module_Noah_gw_baseflow
  USE module_Noah_channel_routing
  USE module_Noah_chan_param_init_rt

  IMPLICIT NONE


  ! Dummy parameterized dimension for maximum number of soil levels allowed
  INTEGER, PARAMETER :: NSOLDX = 100

  ! ZSOIL, set through the namelist, is the BOTTOM of each soil layer (m)
!  REAL, DIMENSION(NSOLDX) :: ZSOIL

  REAL :: T1V,TH2V

!-----------------------------------------------------------------
!  Dimensions from the input file:
!-----------------------------------------------------------------

!     IX: x-direction points, usually along latitude
!     JX: y-direction points, usually along longitude
  INTEGER :: IX
  INTEGER :: JX
  INTEGER :: LAND_CAT
  INTEGER :: SOIL_CAT

!-----------------------------------------------------------------
!  DECLARE VARIABLES FOR GRIDDED SIMULATION
!-----------------------------------------------------------------

! the following parameters are read from a namelist file

! setup model configuration
!   KHOUR: number of hours for the simulation
  INTEGER :: KHOUR
  INTEGER :: KDAY
  INTEGER :: HHTIME,MMTIME ! Real times for solar rad. calculations
! INTEGER :: NSOIL: number of soil layers for the LSM integration
! REAL    :: DT:    time-step in seconds
! REAL    :: ZLVL:  Level (m) at which atmospheric fields are taken to be valid
  INTEGER :: FORC_TYP   ! Type of forcing data specified in namelist file
! INTEGER :: RSTRT_SWC  ! Switch for resetting restart accumulation vars = 0. (0-no, 1-yes)
  INTEGER :: SNOW_ASSIM ! Switch for snow assimilation
  INTEGER :: TERADJ_SOLAR ! Switch for terrain adjustment of incoming solar radiation
  INTEGER :: HIRES_OUT  ! Switch for output routing grid data
  INTEGER :: KCOWS ! Time level counter for COWS simulation...
  CHARACTER(len=3)         :: DOY   !COWS Julian day for filename...
  CHARACTER(len=256)       :: product   ! spec. pcp. product (see FORC_TYP 5)
  CHARACTER(len=256)       :: dat_dir   ! data directory for alternate soil/veg
  integer istatus


! Unaccounted for variables when compiling w/ implicit none F90

!KWM  REAL   :: VEGMIN,VEGMAX
  REAL   :: SNOFAC,ALBED
  REAL   :: RHO,CHKFF,T14,AET,FUP,S
!KWM  REAL   :: F, H
!KWM  REAL   :: SNOFLX, SNMAX, SNOEVP

  INTEGER   :: i,j,k,ns,ierr,kx
  real :: Z

!DJG_DES  Month checks for GREENFAC and ALBEDO12M input
  INTEGER :: MM
  INTEGER :: DD 
  INTEGER :: DD_chk

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Gridded fields
#ifdef COUPLED 
  REAL,    allocatable, DIMENSION(:,:,:)   :: out_cpl
#endif
  INTEGER, allocatable, DIMENSION(:,:)   :: VEGTYP,SOLTYP
  REAL,    allocatable, DIMENSION(:,:)   :: TERRAIN, LATITUDE, LONGITUDE
  REAL,    allocatable, DIMENSION(:,:)   :: T2,XLONG,U,V,PRES,SHORT,PRCP1
  REAL,    allocatable, DIMENSION(:,:)   :: FPAR, LAI
!DJG_DES  Assign Arrays for GREENFRAC and ALBEDO12M
  REAL,    allocatable, DIMENSION(:,:)   :: GREENFRAC, ALBEDO12M
  REAL,    allocatable, DIMENSION(:,:)   :: CMC,SNODEP,WEASD,T1, SNOWC,ZNT
  REAL,    allocatable, DIMENSION(:,:)   :: ETPX,ETAX,SOILHX,CHX,RUNOFF1X,RUNOFF2X
!DJG
  INTEGER                                :: SOLVEG_INITSWC  !Switch to custom vegtyp

  REAL,    allocatable, DIMENSION(:,:)   :: LH_yw, SOILW_yw, SOILM_yw
  REAL,    allocatable, DIMENSION(:,:)   :: ETAKIN, QFX,QSFC
  REAL,    allocatable, DIMENSION(:,:)   :: RUNOFF3X,EDIRX,ECX,ETTX,SNMAXX,RCX,HX
  REAL,    allocatable, DIMENSION(:,:)   :: Q2X,SFCSPDX,ALBEDX,SMCMAX1,SMCWLT1
  REAL,    allocatable, DIMENSION(:,:)   :: SMCREF1,FX,RESX
  REAL,    allocatable, DIMENSION(:,:)   :: SNOFLXX,SNOEVPX,ACSNOM,ESNOW2D,ACRAIN,ACRAIN_0
  REAL,    allocatable, DIMENSION(:,:)   :: DRIP2D, DEWFALL, SOILMX, EMISS
  REAL,    allocatable, DIMENSION(:,:,:) :: SMC,STC,SH2OX,SICE
  REAL,    allocatable, DIMENSION(:,:,:) :: ZSOILX 
  REAL,    allocatable, DIMENSION(:,:)   :: PRCP_OUT, ZLVL_WRF, z0_wrf, TBOT_WRF
  integer ii,jj,ims, jms
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG   VARIABLES FOR ROUTING
  INTEGER, allocatable, DIMENSION(:,:)      :: BASIN_MSK,LAK_1K
  INTEGER, allocatable, DIMENSION(:,:)      :: g_LAK_1K
  INTEGER,    allocatable, DIMENSION(:,:,:) :: SO8LD_D
  REAL,    allocatable, DIMENSION(:,:)      :: SO8LD_Vmax
  REAL   Vmax
  REAL,    allocatable, DIMENSION(:,:)      :: SFCHEADRT,INFXSRT,LKSAT,ETPNDX
  REAL,    allocatable, DIMENSION(:,:)      :: SOLDEPRT
  REAL	  :: DXRT,DX,DTRT,GRDAREART,SUBFLORT,WATAVAILRT,QSUBDRYRT,QSUBBDRYTRT
  REAL    :: SFHEAD1RT,INFXS1RT,QSTRMVOLTRT,QBDRYTRT,SFHEADRT,ETPND1,INFXSRTOT
  REAL 	  :: LAKE_INFLOTRT,accsuminfxs,diffsuminfxs,RETDEPFRAC
  REAL    :: VERTKSAT,l3temp,l4temp,l3moist,l4moist,RNOF1TOT,RNOF2TOT,RNOF3TOT
  INTEGER :: IXRT,JXRT,vegct
  INTEGER :: AGGFACYRT, AGGFACXRT, KRT
  INTEGER :: SATLYRCHKRT,DT_FRACRT,SUBRTSWCRT,OVRTSWCRT,CHANRTSWCRT
  INTEGER :: IXXRT, JYYRT, LAKE_CT, STRM_CT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG   VARIABLES FOR GW/Baseflow
! INTEGER :: GWBASESWCRT,numbasns,GW_RESTART
  INTEGER, allocatable, DIMENSION(:,:)   :: GWSUBBASMSK  !GW basin mask grid
  REAL,    allocatable, DIMENSION(:,:)   :: SOLDRAIN     !time-step drainage
  REAL,    allocatable, DIMENSION(:)     :: z_gwsubbas   !depth in GW bucket
  REAL,    allocatable, DIMENSION(:)     :: qin_gwsubbas !flow to GW bucket
  REAL,    allocatable, DIMENSION(:)     :: qout_gwsubbas!flow from GW bucket
  REAL,    allocatable, DIMENSION(:)     :: gwbas_pix_ct !ct of strm pixels in
  REAL,    allocatable, DIMENSION(:)     :: z_q_bas_parm !GW bucket disch params
  INTEGER, allocatable, DIMENSION(:)     :: ct2_bas       !ct of lnd pixels in basn
  REAL,    allocatable, DIMENSION(:)     :: bas_pcp      !sub-basin avg'd pcp
  INTEGER                                :: bas,bas_id
  CHARACTER(len=19)                      :: header
  CHARACTER(len=1)                       :: jnk
  REAL, allocatable, DIMENSION(:)        :: gw_buck_coeff,gw_buck_exp,z_max  !GW bucket parameters
!DJG Switch for Deep Sat GW Init:
  INTEGER                                :: DEEPGWSPIN  !Switch to setup deep GW spinp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG,DNY   VARIABLES FOR CHANNEL ROUTING
!-- channel params
                                                      !-- Diffusion: 0 edge or pour; 1 interior; 2 lake
  INTEGER, allocatable, DIMENSION(:)   :: TYPEN      !type of link 0 strm 1 lake

  REAL,  DIMENSION(50)     :: BOTWID,HLINK_INIT,CHAN_SS,CHMann !Channel parms from table

  REAL                                 :: RETDEP_CHAN  ! Channel retention depth
  INTEGER :: MAXORDER !maximum stream order
!-- lake params
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG   VARIABLES FOR AGGREGATION/DISAGGREGATION
  REAL,    allocatable, DIMENSION(:)       :: SMCAGGRT,STCAGGRT,SH2OAGGRT
  REAL    :: INFXSAGG1RT,SFCHEADAGG1RT,SFCHEADAGGRT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG   VARIABLES FOR ONLINE MASS BALANCE CALCULATION
  INTEGER, PARAMETER :: double=8
  REAL(KIND=double)    :: DCMC,DSWE,DACRAIN,DSFCEVP,DCANEVP,DEDIR,DETT,DEPND,DESNO,DSFCRNFF
  REAL(KIND=double)    :: DSMCTOT,RESID,SUMEVP,DUG1RNFF,DUG2RNFF,SMCTOT1,SMCTOT2,DETP
  REAL(KIND=double)    :: suminfxsrt,suminfxs1,suminfxs2,dprcp_ts
  REAL(KIND=double)    :: CHAN_IN1,CHAN_IN2,LAKE_IN1,LAKE_IN2,zzz, CHAN_STOR,CHAN_OUT
  REAL(KIND=double)    :: CHAN_INV,LAKE_INV  !-channel and lake inflow in volume
  REAL(KIND=double)    :: DQBDRY
  REAL    :: QSTRMVOLTRT1,LAKE_INFLOTRT1,QBDRYTOT1,LSMVOL
  REAL(KIND=double),    allocatable, DIMENSION(:)   :: DSMC,SMCRTCHK
  REAL(KIND=double),    allocatable, DIMENSION(:,:)  :: CMC_INIT,SWE_INIT
!  REAL(KIND=double),    allocatable, DIMENSION(:,:,:) :: SMC_INIT
  REAL(KIND=double)            :: SMC_INIT,SMC_FINAL,resid2,resid1
  REAL(KIND=double)            :: chcksm1,chcksm2,CMC1,CMC2,prcp_in,ETATOT,dsmctot_av



!---------------------------------------------------------------------
! NEW VARIABLES ADDED DURING NOAH F90 UPGRADE CALL to SFLX
!---------------------------------------------------------------------


! INTENT(IN) to SFLX:
  INTEGER, PARAMETER :: ICE = 0     ! Sea-ice flag  (=1: sea-ice, =0: land)
  REAL    :: DT       ! Timestep (s) (DT should not exceed 3600 S, recommend 1800 s or less)
  REAL    :: ZLVL     ! Height (m) above ground of atmospheric forcing variables
  INTEGER :: NSOIL    ! Number of soil layers
  REAL, ALLOCATABLE, DIMENSION(:) :: SLDPTH ! The THICKNESS (m) of each soil layer 
  CHARACTER(LEN=4) :: LLANDUSE ! (=USGS, using USGS landuse classification)
  CHARACTER(LEN=4) :: LSOIL    ! (=STAS, using FAO/STATSGO soil texture classification)

  REAL :: LWDN      ! LW downward radiation (W m-2; positive, not net longwave)
  REAL :: SOLDN     ! Solar downward radiation (W m-2; positive, not net solar)
  REAL :: SFCPRS    ! Pressure (Pa) at height ZLVL m above ground
  REAL :: PRCP      ! Precip rate (kg m-2 s-1) (NOTE: this is a rate)
  REAL :: SFCTMP    ! Air temperature (K) at height ZLVL m above ground
  REAL :: Q2        ! Mixing ratio (kg kg-1) at height ZLVL m above ground 
  REAL :: SFCSPD    ! Wind speed (m s-1) at height ZLVL m above ground
  REAL :: PRCPRAIN  ! Liquid-precipitation rate (KG M-2 S-1) (not used)
  REAL :: TH2       ! Air potential temperature (K) at height ZLVL m above ground
  REAL :: Q2SAT     ! Sat mixing ratio (kg kg-1) at height ZLVL m above ground
  REAL :: DQSDT2    ! Slope of sat specific humidity curve  (kg kg-1 K-1) at T=SFCTMP
  INTEGER :: VEGTYPX! Vegetation type (integer index)
  INTEGER :: SOILTYP! Soil type (integer index)
  INTEGER ::SLOPETYP! Class of sfc slope (integer index)
  REAL :: SHDMIN    ! Minimum areal fractional coverage of green vegetation
  !         (dimensionless fraction 0.0-1.0) <= SHDFAC
  REAL :: SHDMAX    !UNDOCUMENTED IN SFLX
  REAL :: TBOT      ! Bottom soil temperature (local yearly-mean of surfacc air temperature)


  REAL WATHOLDCAP

! INTENT(INOUT) to/from SFLX:
  REAL :: SNCOVR  ! Fractional snow cover (dimensionless fraction, 0.0-1.0)
  REAL :: COSZ    ! Solar zenith angle (not used for now)
  REAL :: SOLARDIRECT ! Direct component of downward solar radiation (W m-2) (not used)
  REAL :: Z0      ! Time varying roughness length (m) as function of snow depth
  REAL :: CMCX    ! Canopy moisture content (m)
  REAL :: T1X     ! Ground/Canopy/Snowpack effective skin temperature (K)
  REAL, allocatable, DIMENSION(:)  :: STC1    ! Soil temp (K)                            
  REAL, allocatable, DIMENSION(:)  :: SMC1    ! Total soil moisture content (volumetric fraction)
  REAL, allocatable, DIMENSION(:)  :: SH2O    ! Unfrozen soil moisture content (volumetric fraction)
  !          NOTE: Frozen soil moisture = SMC - SH2O
  REAL :: SNOWH   ! Actual snow depth (m)                                      
  REAL :: SNEQV   ! Liquid water-equivalent snow depth (m)                     
  !       NOTE: snow density = SNEQV/SNOWH                         
  REAL :: ALBEDO  ! Surface albedo including snow effect (unitless fraction)   
  !      =snow-free albedo (ALB) when SNEQV=0, or                 
  !      =FCT(MSNOALB,ALB,VEGTYP,SHDFAC,SHDMIN) when SNEQV>0      
  REAL :: CH      ! Surface exchange coefficient for heat and moisture         
  !   (m s-1); NOTE: CH is technically a conductance since     
  !   it has been multiplied by wind speed.                    
  REAL :: CM      ! Surface exchange coefficient for momentum (m s-1); NOTE:   
  !   CM is technically a conductance since it has been        
  !   multiplied by wind speed.                                

! INTENT(OUT) from SFLX:
  REAL :: ETT     ! Total plant transpiration (W m-2)
  REAL :: ETA     ! Actual latent heat flux (W m-2: negative, if up from surface)     
!KWM ???  Is the note about the sign of ETA right ???? !KWM
  REAL :: SHEAT   ! Sensible heat flux (W m-2: negative, if upward from surface)
  REAL :: ETA_KINEMATIC ! Actual latent heat flux (kg m/s)
  REAL :: FDOWN   ! Radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN
  REAL :: EC      ! Canopy water evaporation (W m-2)
  REAL :: EDIR    ! Direct soil evaporation (W m-2)
  REAL, allocatable, DIMENSION(:) :: ET      ! Plant transpiration from a particular root (soil) layer (W m-2)
  REAL :: ESNOW   ! Sublimation from (or deposition to if <0) snowpack (W m-2)
  REAL :: DRIP    ! Through-fall of precip and/or dew in excess of canopy
  ! water-holding capacity (m)
  REAL :: DEW     ! Dewfall (or Frostfall for T<273.15) (m)

  REAL :: BETA    ! Ratio of actual to potential evap (dimensionless)
  REAL :: ETP     ! Potential Evaporation (w m-2)
  REAL :: SSOIL   ! Soil heat flux (W m-2: negative if downward from surface)
  REAL :: FLX1    ! Precip-snow sfc (W m-2)
  REAL :: FLX2    ! Freezing rain latent heat flux (W m-2)      
  REAL :: FLX3    ! Phase-change heat flux from snowmelt (W m-2)
  REAL :: SNOMLT  ! Snow melt (m) (water equivalent)
  REAL :: RUNOFF1 ! Surface runoff (m s-1), not infiltrating the surface
  REAL :: RUNOFF2 ! Subsurface runoff (m s-1), drainage out bottom of last
  !       soil layer (baseflow).  Note: RUNOFF2 is actually 
  !       the sum of RUNOFF2 and RUNOFF3
  REAL :: RUNOFF3 ! Numerical trunctation in excess of porosity (SMCMAX)
  ! for a given soil layer at the end of a time step (m s-1).
  REAL :: RC      ! Canopy resistance (s m-1)
  REAL :: PC      ! Plant coefficient (dimensionless fraction, 0.0-1.0) 
  ! where PC*ETP = actual transpiration
  REAL :: RCS     ! Incoming solar RC factor (dimensionless)
  REAL :: RCT     ! Air temperature RC factor (dimensionless)
  REAL :: RCQ     ! Atmos. vapor pressure deficit RC factor (dimensionless)
  REAL :: RCSOIL  ! Soil moisture RC factor (dimensionless)
  REAL :: SOILW   ! Available soil moisture in root zone (dimensionless fraction
  !      between SMCWLT and SMCMAX)
  REAL :: SOILM   ! Total soil column moisture content (frozen+unfrozen) (m)
  REAL :: Q1      ! Effective mixing ratio at surface (kg kg-1), used for
  !      diagnosing the mixing ratio at 2 meter for 
  !      coupled model

  real ::  snoalb, frzx
  logical :: local = .TRUE.
  integer :: kyw
  character*80, file1_yw
  character*80, file2_yw


!---------------------------------------------------------------------
!  DECLARE/Initialize constants
!---------------------------------------------------------------------

  REAL, PARAMETER :: R=287.04
  REAL, PARAMETER :: CPHEAT=1004.5
  INTEGER, PARAMETER :: LAND=1

  character(len=19) :: olddate, newdate, startdate
  CHARACTER(len=256) :: inflnm, outflnm,ywinflnm
  CHARACTER(len=256) :: inflnm2="init_channel_state.dat"
  character :: hgrid

! NAMELIST:

  CHARACTER(len=256) :: indir
  integer :: igrid
  character(len=256) :: range = ""
  character(len=256) :: wrfsi_static_flnm = ""
  character(len=256) :: wrfsi_finegrid_flnm = ""
  character(len=19)  :: restart_hdate = ""
  character(len=256) :: route_topo_f=""
  character(len=256) :: route_chan_f=""
  character(len=256) :: route_link_f=""
  character(len=256) :: route_lake_f=""
  character(len=256) :: route_direction_f=""
  character(len=256) :: route_order_f=""
  character(len=256) :: gwbasmskfil =""
  character(len=256) :: gwstrmfil =""

  integer            :: split_output_count
  logical            :: history_output
  

!DJG Modified namelist for routing and agg. variables

  integer :: START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN
  integer :: rt_option      ! 1 for 1 direction routing.
                            ! 2 for 2 direction routing.
  integer :: channel_option = 3  ! 1 for Muskingum, 2 kinematic, 3 diffusive wave

  real Z_tmp

  integer, parameter :: iunit = 10
  integer, parameter :: ounit = 11
  integer :: g_ixrt,g_jxrt,flag

  integer iywtmp

  real ZSOIL8(8)

  real out_dt, rst_dt

  namelist /NOAHLSM_OFFLINE/ INDIR, FORC_TYP, NSOIL, ZSOIL8, DT, TBOT, &
    START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN, DOY, &
    RESTART_HDATE,HISTORY_OUTPUT,SPLIT_OUTPUT_COUNT,IGRID,RANGE, &
    KHOUR, KDAY, ZLVL, Z, wrfsi_static_flnm, wrfsi_finegrid_flnm, &
    SUBRTSWCRT, out_dt, rst_dt,&
    OVRTSWCRT,CHANRTSWCRT,DTRT,DXRT,SLOPETYP,SNOW_ASSIM, &
    TERADJ_SOLAR,HIRES_OUT, route_lake_f, route_topo_f,route_chan_f, &
    route_link_f, route_direction_f, route_order_f, rt_option, &
    gwbasmskfil, gwstrmfil, channel_option, DEEPGWSPIN, SOLVEG_INITSWC


contains 

  subroutine rtland_init(ntime)
  implicit none
  INTEGER   :: NTIME, ix_tmp, jx_tmp, did

!wwwww  call WRF_LAND_INIT(1,2,3,4)  
  did = 1
!  nsoil = -999
!  zsoil = -999.
!  dt = -999.
!   tbot = -999.
!  start_year = -999
!  start_month = -999
!  start_day = -999
!  start_hour = -999
!   start_min = -999
!  khour = -999
!  kday = -999
!  zlvl = -999.
!  z = -999.
!  chan_out = 0.
!  history_output = .FALSE.


!    open(30, file="noah_wrfcode.namelist", form="FORMATTED")
!    read(30, NOAHLSM_OFFLINE, iostat=ierr)
!    close(30)
    ZSOIL(1:8) = ZSOIL8(1:8)


  write(6,*) "rt_option is :",rt_option
  write(6,*) "channel_option = ",channel_option
  call flush(6)
!yw  if(rt_option .eq. 0) call land_finish()



!---------------------------------------------------------------------
! END Declarations 
!---------------------------------------------------------------------


  LLANDUSE = 'USGS'
  LSOIL = 'STAS'

  ! Initialize namelist variables to dummy values, so we can tell
  ! if they have not been set properly.


  write(olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
       start_year, start_month, start_day, start_hour, start_min, 0

  startdate = olddate
  write(hgrid,'(I1)') igrid

  !!DJG Setup forcing data filenames here...
!!!
!!!Obsolete...  if (FORC_TYP.ne.3) then     ! Forctype not COWS...
! Temp filename fix, DJG 1/6/2012     ".LDASIN_DOMAIN"//hgrid
  if (FORC_TYP.eq.11) then     ! Minute filename forcing...
!    if (len_trim(range) == 0) then
       inflnm = trim(indir)//"/"//&
          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
          olddate(15:16)//".LDASIN_DOMAIN"//hgrid
    else
       inflnm = trim(indir)//"/"//&
          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
            ".LDASIN_DOMAIN"//hgrid
    endif
!  else     !for COWS only...
!    ix=60  !COWS
!    jx=40  !COWS
!    if (len_trim(range) == 0) then
!      inflnm = trim(indir)//"/"//&
!        "MET_LIS_CRO_2D_SANTEE_LU_1KM."//&
!        startdate(1:4)//DOY//startdate(12:13)//&
!        ".156hrfcst.radar"
!    else
!    endif
!  endif


  if (FORC_TYP.ne.3)  then    !normal read of wrfstatic data...
     call read_hrldas_hdrinfo(wrfsi_static_flnm, ix, jx, land_cat, soil_cat)
  else   !COWS
  endif


!define global variable and allocate the global array for IO_id node.
!following global variable moved back for debuggin purpose.




!DJG  SETTING DISAGGREGATION GRID SIZE


  write(olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
       start_year, start_month, start_day, start_hour, start_min, 0

  startdate = olddate
  olddate = startdate(1:16)

!......................... end of model configuration (later in a namelist) .....


  CALL LSM_PARM_INIT          !yyww need to change later

  write(6,*) "Finish call LSM_PARM_INIT"
  call flush(6)

!----------------------------------------------------------------------
! Allocate arrays for our gridded domain, now that we know the size
!----------------------------------------------------------------------
  allocate( VEGTYP   (IX,JX) )
  allocate( SOLTYP   (IX,JX) )
  allocate( TERRAIN  (IX,JX) )
  allocate( LATITUDE (IX,JX) )
  allocate( LONGITUDE(IX,JX) )
  allocate( T2       (IX,JX) )
  allocate( XLONG    (IX,JX) )
  allocate( U        (IX,JX) )
  allocate( V        (IX,JX) )
  allocate( PRES     (IX,JX) )
  allocate( SHORT    (IX,JX) )
  allocate( PRCP1    (IX,JX) )
  allocate( PRCP_OUT (IX,JX) )
  allocate( FPAR     (IX,JX) )
  allocate( LAI      (IX,JX) )
!DJG_DES Allocate arrays GREENFRAC and ALBEDO12M
  allocate( GREENFRAC(IX,JX) )
  allocate( ALBEDO12M(IX,JX) )
  allocate( CMC      (IX,JX) )
  allocate( SNODEP   (IX,JX) )
  allocate( WEASD    (IX,JX) )
  allocate( ZNT      (IX,JX) )
  allocate( SNOWC    (IX,JX) )
  allocate( T1       (IX,JX) )
  allocate( ETPX     (IX,JX) )
  allocate( ETAX     (IX,JX) )
  allocate( ETAKIN   (IX,JX) )
  allocate( SOILHX   (IX,JX) )
  allocate( LH_yw   (IX,JX) )
  allocate( SOILW_yw   (IX,JX) )
  allocate( SOILM_yw   (IX,JX) )
  allocate( CHX      (IX,JX) )
  allocate( RUNOFF1X (IX,JX) )
  allocate( RUNOFF2X (IX,JX) )
  allocate( RUNOFF3X (IX,JX) )
  allocate( EDIRX    (IX,JX) )
  allocate( ECX      (IX,JX) )
  allocate( ETTX     (IX,JX) )
  allocate( SNMAXX   (IX,JX) )
  allocate( RCX      (IX,JX) )
  allocate( HX       (IX,JX) )
  allocate( QFX      (IX,JX) )
  allocate( QSFC     (IX,JX) )
  allocate( Q2X      (IX,JX) )
  allocate( SFCSPDX  (IX,JX) )
  allocate( ALBEDX   (IX,JX) )
  allocate( SMCMAX1  (IX,JX) )
  allocate( SMCWLT1  (IX,JX) )
  allocate( SMCREF1  (IX,JX) )
  allocate( ACSNOM   (IX,JX) )
  allocate( ACRAIN   (IX,JX) )
  allocate( ACRAIN_0   (IX,JX) )
  allocate( ESNOW2D  (IX,JX) )
!KWM  allocate( SNOFLXX  (IX,JX) )
  allocate( SNOEVPX  (IX,JX) )
  allocate( FX       (IX,JX) )
  allocate( RESX     (IX,JX) )
  allocate( DRIP2D   (IX,JX) )
  allocate( DEWFALL  (IX,JX) )
  allocate( SOILMX   (IX,JX) )
  allocate( EMISS    (IX,JX) )
!DJG  allocate   few vars on native grird
  allocate( LKSAT    (IX,JX) )
  allocate( ETPNDX   (IX,JX) )
  allocate( SFCHEADRT  (IX,JX) )
  allocate( INFXSRT   (IX,JX) )
  allocate( SOLDRAIN (IX,JX) )
  allocate( BASIN_MSK   (IX,JX) )
  allocate( LAK_1K   (IX,JX) )
!KWM  allocate( PDDUM2   (IX,JX) )
!KWM  allocate( PCPDRP   (IX,JX) )
!KWM  allocate( SFCWATR2 (IX,JX) )

  allocate( SMC  (IX,JX,NSOIL) ) 
  allocate( SICE  (IX,JX,NSOIL) ) 
  allocate( STC  (IX,JX,NSOIL) )
  allocate( SH2OX(IX,JX,NSOIL) )
  allocate( ZSOILX(IX,JX,NSOIL) )
  allocate( ZLVL_WRF(IX,JX) )
  allocate( TBOT_WRF(IX,JX) )
  allocate( z0_WRF(IX,JX) )

  allocate( SH2O   (NSOIL) )
  allocate( SLDPTH (NSOIL) )
  allocate( SMC1   (NSOIL) )
  allocate( STC1   (NSOIL) )
  allocate( ET     (NSOIL) )

!DJG Allocate routing and disaggregation arrays

  allocate( SO8LD_D   	(IX,JX,3) )

  allocate(SO8LD_Vmax(IX,JX) )
  allocate( SMCAGGRT   	(NSOIL) )
  allocate( STCAGGRT   	(NSOIL) )
  allocate( SH2OAGGRT  	(NSOIL) )


! allocate channel routing and lake routing arrays

!DJG,DNY Allocate channel routing and lake routing arrays



  !DJG Allocate routing and disaggregation arrays
  allocate(GWSUBBASMSK  (IX,JX) )

! allocate channel routing and lake routing arrays

!DJG Allocate mass balance calc arrays
 
  allocate( DSMC        (NSOIL) )
  allocate( SMCRTCHK    (NSOIL) )
  allocate( CMC_INIT    (IX,JX) )
  allocate( SWE_INIT    (IX,JX) )
!  allocate( SMC_INIT    (IX,JX,NSOIL) )

  print *, "done allocating"

!----------------------------------------------------------------------
! Initialize gridded domain
!----------------------------------------------------------------------


  ! SLDPTH is the thickness of each layer
  SLDPTH(1) = -ZSOIL(1)
  do i = 2, nsoil
     sldpth(i) = zsoil(i-1)-zsoil(i)
  enddo

  ETAX=0.0 ! -999.9
  ETAKIN=0.0 ! -999.9
  ETPX=0.0 ! -999.9
  CHX=-999.9
  FX=-999.9
  HX=-999.9
  RESX=-999.9
  SOILHX=-999.9
  CMC=0.0
  T2=-999.9
  T1=-999.9
  PRCP1=-999.9
  PRCP_OUT=0.0

  RUNOFF1X=0.0 
  RUNOFF2X=0.0 
  RUNOFF3X=0.0 
  EDIRX=0.0
  ETTX=0.0
  SNOEVPX=-999.9
  SNODEP=-999.9

  STC=-999.9
  SH2OX=-999.9
  SMC=-999.9
  SICE=-999.9

  ECX=0.0
  ACSNOM  = 0.0
  ACRAIN  = 0.0
  ACRAIN_0  = 0.0
  ESNOW2D = 0.0
  DRIP2D = 0.0
  DEWFALL = 0.0
  SOILMX = -999.

!DJG Initialize routing and aggregation fields
  ETPNDX=0.0
!   SOLDEPRT = -1.0*ZSOIL(NSOIL)
  SFCHEADRT = 0.0
  INFXSRT = 0.0
  LAKE_CT = 0
  STRM_CT = 0
  SOLDRAIN = 0.0

  BASIN_MSK = 1

!DJG Initialize mass balance check variables...
  SMC_INIT=0.
  DSMC=0.
  DACRAIN=0.
  DSFCEVP=0.
  DCANEVP=0.
  DEDIR=0.
  DETT=0.
  DEPND=0.
  DESNO=0.
  DSFCRNFF=0.
  DQBDRY=0.
  SUMINFXS1=0.


!---------------------------------------------------------------------
! Initialize static surface data
!---------------------------------------------------------------------


!   OK     VEGTYP     VEGETATION TYPE (INTEGER INDEX)
!   OK     SOILTYP    SOIL TYPE (INTEGER INDEX)
!   OK     SLOPETYP   CLASS OF SFC SLOPE (INTEGER INDEX)
!          SHDMIN     MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!                (FRACTION= 0.0-1.0) <= SHDFAC
!   N/U    PTU        PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)
!                (NOT YET USED, BUT PASSED TO REDPRM FOR FUTURE USE IN
!                VEG PARMS)
!   OK     ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION), FOR JULIAN
!                DAY OF YEAR (USUALLY FROM TEMPORAL INTERPOLATION OF
!                MONTHLY MEAN VALUES' CALLING PROG MAY OR MAY NOT
!                INCLUDE DIURNAL SUN ANGLE EFFECT)
!   OK     SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW (E.G. FROM
!                ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)
!   OK     TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR
!                TEMPERATURE)
!   ??     Z0BRD      Background fixed roughness length (M)
!   ??     Z0         Time varying roughness length (M) as function of snow depth




!----------------------------------------------------------------------
! Read Landuse Type and Soil Texture and Other Information
!----------------------------------------------------------------------

!KWM         CALL READLAND(IX,JX,VEGTYP,SOLTYP, TERRAIN, LATITUDE,LONGITUDE)
!KWM  CALL READLAND_IHOP(DIR,IX,JX,VEGTYP,SOLTYP,TERRAIN,LATITUDE,LONGITUDE)
!KWM  CALL READLAND_RTFDDA(IUNIT,IX,JX,VEGTYP,SOLTYP,TERRAIN,LATITUDE,LONGITUDE)
!DJG  CALL READLAND_HRLDAS(wrfsi_static_flnm,IX,JX,LAND_CAT,SOIL_CAT,      &
!DJG       VEGTYP,SOLTYP,TERRAIN,LATITUDE,LONGITUDE)



  CALL READLAND_HRLDAS(wrfsi_static_flnm,IX,JX,LAND_CAT,SOIL_CAT,      &
      VEGTYP,SOLTYP,TERRAIN,LATITUDE,LONGITUDE,SOLVEG_INITSWC)

!DJG For DEEPGWSPINUP test...
!	IF (DEEPGWSPIN.EQ.1) THEN
!          SOLTYP = 1
!        END IF


!== quick fix for COWPENS so no bound error ======== july 08
 do j = 1, jx
    do i = 1, ix
          if(vegtyp(i,j) .eq. 0) vegtyp(i,j) = 7
    end do
 end do




!------------------------------------------------------------------------
!DJG Routing Processing
!------------------------------------------------------------------------
!DJG IF/then to get routing terrain fields if either routing module is 
!DJG   activated


!DJG    will later make this a function of SOLTYP and VEGTYP

!             RETDEPRT = 0.1   ! units (mm)
              RETDEP_CHAN = 0.001


!DJG Need to insert call for acquiring routing fields here...
!DJG     include as a subroutine in module module_Noahlsm_wrfcode_input.F
!DJG  Calculate terrain slopes 'SOXRT,SOYRT' from subgrid elevation 'ELRT'


!---------------------------------------------------------------------
!DJG,DNY  If channel routing activated...
!----------------------------------------------------------------------



!----------------------------------------------------------------------
! Initialize LSM Model State
!----------------------------------------------------------------------

!KWM         CALL READINIT(IX,JX,SMC,STC,SH2OX,ESDX,CMC,T1, SNODEP)
!KWM CALL READINIT_IHOP(DIR, IX, JX, NSOIL, SMC, STC, SH2OX, ESDX, CMC, T1, SNODEP)
!KWM  CALL READINIT_RTFDDA(IUNIT, IX, JX, NSOIL, OLDDATE, SMC, STC, SH2OX, CMC, T1, &
!KWM       WEASD, SNODEP)

   SMC = 0.25
   STC = 282.0
   SH2OX = SMC
   CMC = 0.000025
   T1 = 282.0
   WEASD = 0.
   SNODEP = 0.
   if(SNOW_ASSIM.eq.1) then
     CALL READSNOW_HRLDAS(inflnm,IX,JX,OLDDATE,WEASD,SNODEP)
   end if


!DJG Initialize additional soil layers (5-8) used in Noah-distrib.
!DJG  and adjust upper layers accordingly. This methodology assumes
!DJG  original init. data is coming from a 4 layer version of Noah
!DJG  as in EDAS or AFWA analyses.


!        print *, "debug soils...",SMC(100,100,1),SMC(100,100,2), &
!                    SMC(100,100,3),SMC(100,100,4),SMC(100,100,5), &
!                    SMC(100,100,6),SMC(100,100,8),SMC(100,100,8)

!#ifndef COUPLED  
! coupled model will read hrldas output.

!	DO J=1,JX
!!          DO I=1,IX
! 	    l3moist=SMC(I,J,3)
! 	    l4moist=SMC(I,J,4)
!            SMC(I,J,8)=l4moist
!!            SMC(I,J,7)=l4moist
!            SMC(I,J,6)=l4moist
!            SMC(I,J,5)=l4moist
!            SMC(I,J,4)=0.5*(l3moist+l4moist)
!            SMC(I,J,3)=0.5*(l3moist+SMC(I,J,2))
! 	    l3moist=SH2OX(I,J,3)
! 	    l4moist=SH2OX(I,J,4)
!            SH2OX(I,J,8)=l4moist
!            SH2OX(I,J,7)=l4moist
!            SH2OX(I,J,6)=l4moist
!            SH2OX(I,J,5)=l4moist
!            SH2OX(I,J,4)=0.5*(l3moist+l4moist)
!            SH2OX(I,J,3)=0.5*(l3moist+SH2OX(I,J,2))
!#ifdef COUPLED
!              TBOT = TBOT_WRF(i,j)
!#endif
!            l3temp=STC(I,J,3)
!            l4temp=STC(I,J,4)
!            STC(I,J,8)=0.5*(TBOT+l4temp)
!            STC(I,J,7)=l4temp
!!            STC(I,J,6)=l4temp
!            STC(I,J,5)=l4temp
!            STC(I,J,4)=0.5*(l3temp+l4temp)
!            STC(I,J,3)=0.5*(l3temp+STC(I,J,2))
!          ENDDO
!        ENDDO
!#endif


! *** Read lai, fveg etc.
!KWM        CALL READVEG(IX,JX,FPAR,XLAI)
!KWM  CALL READVEG_IHOP(DIR,IX,JX,FPAR,LAI)
!KWM  CALL READVEG_RTFDDA(IUNIT, IX, JX, OLDDATE, VEGTYP, FPAR, LAI)


  if(FORC_TYP.eq.2) then
        inflnm = trim(indir)//"/"//&
             "wrfout_d02_"//&
             olddate(1:4)//"-"//olddate(6:7)//"-"//olddate(9:10)//&
             "_"//olddate(12:13)//":00:00"
        print *, inflnm
  end if


!DJG_DES  CALL READVEG_HRLDAS(inflnm, IX, JX, OLDDATE, VEGTYP, FPAR, LAI,  &
  DD = start_day
  MM = start_month
  CALL READVEG_HRLDAS(inflnm,trim(wrfsi_static_flnm), IX, JX, OLDDATE, VEGTYP, FPAR, LAI,  &
         FORC_TYP,GREENFRAC, ALBEDO12M,MM,DD)


   write(6,*) "finish READVEG_HRLDAS"
   call flush(6)
!DJG now set in namelist file...  SLOPETYP = 8
   if(SLOPETYP .eq. 0) then
      SLOPETYP = 8
   endif

!  close(iunit)
!  rewind(iunit)

! Output in V3 format at time zero:
  SOILMX = -1.0 * SMC (:,:,1)* ZSOIL (1)
  DO K = 2,NSOIL
     SOILMX = SOILMX + SMC(:,:,K) * (ZSOIL (K-1) - ZSOIL (K))
  ENDDO
  SOILMX = SOILMX * 1.E3 ! Convert from m to mm
  HX = 0.0
  QFX = 0.0




!DJG Output Initial Conditions...
  



  NTIME=KHOUR*3600./nint(dt)


  write(6,*) "finish rtland_init"
  call flush(6)



    return
    end subroutine rtland_init   


end module module_rtland


     subroutine land_finish()
#ifdef MPP_LAND
         call mpp_land_finish()
#else 
         stop
#endif
     return
     end  subroutine land_finish  

      subroutine output_nc(array,idim,jdim, var_name,file_name)
      implicit none
#include <netcdf.inc>
          integer idim,jdim
          real array(idim,jdim)
          integer dim(2)
          character(len=*):: file_name,var_name
          integer   iret,ncid,varid,idim_id,jdim_id
          integer i,j
          iret = nf_create(trim(file_name), 0, ncid)
          iret = nf_def_dim(ncid, "idim", idim,idim_id)
          iret = nf_def_dim(ncid, "jdim", jdim,jdim_id)
          dim(1)=idim_id
          dim(2)=jdim_id
          iret = nf_put_att_real(ncid, NF_GLOBAL, &
              "missing_value", NF_FLOAT, 1, -1.E33)

          iret = nf_def_var(ncid,var_name,NF_FLOAT,2,dim,varid)
          iret = nf_enddef(ncid)

!   output
          iret = nf_inq_varid(ncid,var_name,varid)
          iret = nf_put_var_real(ncid,varid,array)
          iret=nf_close(ncid)
      return
      end subroutine output_nc

    subroutine time_seconds(i3)
          integer time_array(8)
          real i3
          call date_and_time(values=time_array)
          i3 = time_array(5) * 3600 + time_array(6) * 60 + &
                time_array(7) + 0.001 * time_array(8)
    return
    end subroutine time_seconds

    subroutine flush( i)
      integer i
    return
    end subroutine flush

