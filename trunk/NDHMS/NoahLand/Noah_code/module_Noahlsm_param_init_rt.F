MODULE module_sf_Noahlsm_param_init_rt2

! VEGETATION PARAMETERS
  INTEGER :: LUCATS , BARE
  integer, PARAMETER :: NLUS=50
  CHARACTER(LEN=4) LUTYPE
  INTEGER, DIMENSION(1:NLUS) :: NROTBL
  real, dimension(1:NLUS) ::  SNUPTBL, RSTBL, RGLTBL, HSTBL, LAITBL,        &
       ALBTBL, Z0TBL, SHDTBL, MAXALB, EMITBL
  REAL ::   TOPT_DATA,CMCMAX_DATA,CFACTR_DATA,RSMAX_DATA

! SOIL PARAMETERS
  INTEGER :: SLCATS
  INTEGER, PARAMETER :: NSLTYPE=30
  CHARACTER(LEN=4) SLTYPE
  REAL, DIMENSION (1:NSLTYPE) :: BB,DRYSMC,F11,                           &
       MAXSMC, REFSMC,SATPSI,SATDK,SATDW, WLTSMC,QTZ

! LSM GENERAL PARAMETERS
  INTEGER :: SLPCATS
  INTEGER, PARAMETER :: NSLOPE=30
  REAL, DIMENSION (1:NSLOPE) :: SLOPE_DATA
  REAL ::  SBETA_DATA,FXEXP_DATA,CSOIL_DATA,SALP_DATA,REFDK_DATA,           &
       REFKDT_DATA,FRZK_DATA,ZBOT_DATA,  SMLOW_DATA,SMHIGH_DATA,        &
       CZIL_DATA, MANN_DATA

!
CONTAINS
!
!-----------------------------------------------------------------
  SUBROUTINE LSM_PARM_INIT
!-----------------------------------------------------------------
    character(LEN=4) :: MMINLU, MMINSL

    MMINLU='USGS'
    MMINSL='STAS'
      call SOIL_VEG_GEN_PARM( MMINLU, MMINSL)
!   call mpp_land_bcast_int1(LUCATS)
!   call mpp_land_bcast_real(NLUS,ALBTBL)
!   call mpp_land_bcast_real(NLUS,Z0TBL)
!   call mpp_land_bcast_real(NLUS,SHDTBL)
!   call mpp_land_bcast_int(NLUS,NROTBL)
!   call mpp_land_bcast_real(NLUS,RSTBL)
!   call mpp_land_bcast_real(NLUS,RGLTBL)
!   call mpp_land_bcast_real(NLUS,HSTBL)
!   call mpp_land_bcast_real(NLUS,SNUPTBL)
!   call mpp_land_bcast_real(NLUS,LAITBL)
!   call mpp_land_bcast_real(NLUS,MAXALB)
!   call mpp_land_bcast_real(NLUS,EMITBL)
!   call mpp_land_bcast_real1(TOPT_DATA)
!   call mpp_land_bcast_real1(CMCMAX_DATA)
!   call mpp_land_bcast_real1(CFACTR_DATA)
!   call mpp_land_bcast_real1(RSMAX_DATA)
!   call mpp_land_bcast_int1(BARE)
!   call mpp_land_bcast_int1(SLCATS)

!   call mpp_land_bcast_real(NSLTYPE,BB)
!   call mpp_land_bcast_real(NSLTYPE,DRYSMC)
!   call mpp_land_bcast_real(NSLTYPE,F11)
!   call mpp_land_bcast_real(NSLTYPE,MAXSMC)
!   call mpp_land_bcast_real(NSLTYPE,REFSMC)
!   call mpp_land_bcast_real(NSLTYPE,SATPSI)
!   call mpp_land_bcast_real(NSLTYPE,SATDK)
!   call mpp_land_bcast_real(NSLTYPE,SATDW) 
!   call mpp_land_bcast_real(NSLTYPE,WLTSMC)
!   call mpp_land_bcast_real(NSLTYPE,QTZ)

!   call mpp_land_bcast_real(NSLOPE,SLOPE_DATA)
!   call mpp_land_bcast_real1(SBETA_DATA)
!   call mpp_land_bcast_real1(FXEXP_DATA)
!   call mpp_land_bcast_real1(CSOIL_DATA)
!   call mpp_land_bcast_real1(SALP_DATA)
!   call mpp_land_bcast_real1(REFDK_DATA)
!   call mpp_land_bcast_real1(REFKDT_DATA)
!   call mpp_land_bcast_real1(FRZK_DATA) 
!   call mpp_land_bcast_real1(ZBOT_DATA) 
!   call mpp_land_bcast_real1(CZIL_DATA)
!   call mpp_land_bcast_real1(MANN_DATA)
!   call mpp_land_bcast_real1(SMLOW_DATA)
!   call mpp_land_bcast_real1(SMHIGH_DATA)


!-----------------------------------------------------------------
  END SUBROUTINE LSM_PARM_INIT
!-----------------------------------------------------------------

  SUBROUTINE SOIL_VEG_GEN_PARM( MMINLU, MMINSL)
!-----------------------------------------------------------------

    IMPLICIT NONE

    integer :: LUMATCH, IINDEX, LC, NUM_SLOPE

    character(LEN=4) :: MMINLU, MMINSL
    character(LEN=4) :: LUTYPE, SLTYPE

!-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
!             ALBBCK: SFC albedo (in percentage)
!                 Z0: Roughness length (m)
!             SHDFAC: Green vegetation fraction (in percentage)
!  Note: The ALBEDO, Z0, and SHDFAC values read from the following table
!          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is
!          the monthly green vegetation data
!             CMXTBL: MAX CNPY Capacity (m)
!             NROTBL: Rooting depth (layer)
!              RSMIN: Mimimum stomatal resistance (s m-1)
!              RSMAX: Max. stomatal resistance (s m-1)
!                RGL: Parameters used in radiation stress function
!                 HS: Parameter used in vapor pressure deficit functio
!               TOPT: Optimum transpiration air temperature. (K)
!             CMCMAX: Maximum canopy water capacity
!             CFACTR: Parameter used in the canopy inteception calculati
!               SNUP: Threshold snow depth (in water equivalent m) that
!                     implies 100% snow cover
!                LAI: Leaf area index (dimensionless)
!             MAXALB: Upper bound on maximum albedo over deep snow
!                EMI: Emissivity (between 0.0 and 1.0)
!
!-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL
!

#ifdef COUPLED    
    OPEN(19, FILE='RT/VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD')
#else
    OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD')
#endif

    PRINT *, 'INPUT LANDUSE = ',MMINLU
    PRINT *, 'called from module_Noahlsm_param_init_rt.F'

    LUMATCH=0

    READ (19,*)
    READ (19,2000,END=2002)LUTYPE
    READ (19,*)LUCATS,IINDEX
2000 FORMAT (A4)

    IF(LUTYPE.EQ.MMINLU)THEN
       PRINT *, 'LANDUSE TYPE = ',LUTYPE,' FOUND',           &
            LUCATS,' CATEGORIES'
       LUMATCH=1
    ENDIF

    IF(LUTYPE.EQ.MMINLU)THEN
       DO LC=1,LUCATS
          READ (19,*)IINDEX,ALBTBL(LC),Z0TBL(LC),SHDTBL(LC),   &
               &     NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC), &
               &     SNUPTBL(LC),LAITBL(LC),MAXALB(LC),EMITBL(LC)
          !KWM
          MAXALB(LC)=MAXALB(LC)*0.01  !KWM Convert to fraction
       ENDDO

       READ (19,*)
       READ (19,*)TOPT_DATA
       READ (19,*)
       READ (19,*)CMCMAX_DATA
       READ (19,*)
       READ (19,*)CFACTR_DATA
       READ (19,*)
       READ (19,*)RSMAX_DATA
       READ (19,*)
       READ (19,*)BARE
    ENDIF

2002 CONTINUE

    CLOSE (19)

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
    OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD')

    MMINSL='STAS'                       !oct2
    PRINT *, 'INPUT SOIL TEXTURE CLASSIFICAION = ',MMINSL

    LUMATCH=0

    READ (19,*)
    READ (19,2000,END=2003)SLTYPE
    READ (19,*)SLCATS,IINDEX
    IF(SLTYPE.EQ.MMINSL)THEN
       PRINT *, 'SOIL TEXTURE CLASSIFICATION = ',SLTYPE,' FOUND', &
            SLCATS,' CATEGORIES'
       LUMATCH=1
    ENDIF
    IF(SLTYPE.EQ.MMINSL)THEN
       DO LC=1,SLCATS
          READ (19,*) IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
               &      REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
               &      WLTSMC(LC), QTZ(LC)
       ENDDO
    ENDIF

2003 CONTINUE

    CLOSE (19)

    IF(LUMATCH.EQ.0)THEN
       PRINT *,'SOIl TEXTURE IN INPUT FILE DOES NOT '
       PRINT *, 'MATCH SOILPARM TABLE'
       STOP 'INCONSISTENT OR MISSING SOILPARM FILE'
    ENDIF

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
    OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD')

    READ (19,*)
    READ (19,*)
    READ (19,*) NUM_SLOPE

    SLPCATS=NUM_SLOPE

    DO LC=1,SLPCATS
       READ (19,*)SLOPE_DATA(LC)
    ENDDO

    READ (19,*)
    READ (19,*)SBETA_DATA
    READ (19,*)
    READ (19,*)FXEXP_DATA
    READ (19,*)
    READ (19,*)CSOIL_DATA
    READ (19,*)
    READ (19,*)SALP_DATA
    READ (19,*)
    READ (19,*)REFDK_DATA
    READ (19,*)
    READ (19,*)REFKDT_DATA
    READ (19,*)
    READ (19,*)FRZK_DATA
    READ (19,*)
    READ (19,*)ZBOT_DATA
    READ (19,*)
    READ (19,*)CZIL_DATA
    READ (19,*)
    READ (19,*)MANN_DATA
    READ (19,*)
    READ (19,*)SMLOW_DATA
    READ (19,*)
    READ (19,*)SMHIGH_DATA
    CLOSE (19)

!-----------------------------------------------------------------
  END SUBROUTINE SOIL_VEG_GEN_PARM
!-----------------------------------------------------------------

! ----------------------------------------------------------------------




END MODULE module_sf_Noahlsm_param_init_rt2


