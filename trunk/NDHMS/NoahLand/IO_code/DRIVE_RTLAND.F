   SUBROUTINE lsm_rtland(DZ8W,QV3D,P8W3D,T3D,TSK,                      &
                  HFX,QFX,LH,GRDFLX, QGH,GSW,GLW,SMSTAV,SMSTOT, &
                  SFCRUNOFF, UDRUNOFF,IVGTYP,ISLTYP,VEGFRA,     &          
                  ALBEDO,ALBBCK,ZNT,Z0,TMN,XLAND,XICE, EMISS,   & 
                  SNOWC,QSFC,RAINBL,                            &
                  num_soil_layers,DT,DZS,ITIMESTEP,             &
                  SMOIS,TSLB,SNOW,CANWAT,                       &
                  CHS,CHS2,CQS2,CPM,ROVCP,                      & !H   
                  SH2O,SNOWH,                                   & !H 
                  SNOALB,SHDMIN,SHDMAX,                         & !I
                  ACSNOM,ACSNOW,                                & !O  
                  ids,ide, jds,jde, kds,kde,                    &
                  ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte                     )
!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------              
!----------------------------------------------------------------
! --- atmospheric (WRF generic) variables
!-- DT          time step (seconds)
!-- DZ8W        thickness of layers (m)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- FLHC        exchange coefficient for heat (m/s)
!-- FLQC        exchange coefficient for moisture (m/s)
!-- PSFC        surface pressure (Pa)
!-- XLAND       land mask (1 for land, 2 for water)
!-- QGH         saturated mixing ratio at 2 meter
!-- GSW         downward short wave flux at ground surface (W/m^2)
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- History variables 
!-- CANWAT      canopy moisture content (mm)
!-- TSK         surface temperature (K)
!-- TSLB        soil temp (k)
!-- SMOIS       total soil moisture content (volumetric fraction)
!-- SH2O        unfrozen soil moisture content (volumetric fraction)
!                note: frozen soil moisture (i.e., soil ice) = SMOIS - SH2O
!-- SNOWH       actual snow depth (m)
!-- SNOW        liquid water-equivalent snow depth (m)
!-- ALBEDO      time-varying surface albedo including snow effect (unitless fraction)
!-- ALBBCK      background surface albedo (unitless fraction)
!-- CHS          surface exchange coefficient for heat and moisture (m s-1);
!-- CHS2        2m surface exchange coefficient for heat  (m s-1);
!-- CQS2        2m surface exchange coefficient for moisture (m s-1);
! --- soil variables
!-- num_soil_layers   the number of soil layers
!-- ZS          depths of centers of soil layers   (m)
!-- DZS         thicknesses of soil layers (m)
!-- SLDPTH      thickness of each soil layer (m, same as DZS)
!-- TMN         soil temperature at lower boundary (K)
!-- SMCWLT      wilting point (volumetric)
!-- SMCDRY      dry soil moisture threshold where direct evap from 
!               top soil layer ends (volumetric)
!-- SMCREF      soil moisture threshold below which transpiration begins to
!                   stress (volumetric)
!-- SMCMAX      porosity, i.e. saturated value of soil moisture (volumetric)
!-- NROOT       number of root layers, a function of veg type, determined
!               in subroutine redprm.
!-- SMSTAV      Soil moisture availability for evapotranspiration ( 
!                   fraction between SMCWLT and SMCMXA)
!-- SMSTOT      Total soil moisture content frozen+unfrozen) in the soil column (mm)
! --- snow variables
!-- SNOWC       fraction snow coverage (0-1.0)
! --- vegetation variables
!-- SNOALB      upper bound on maximum albedo over deep snow 
!-- SHDMIN      minimum areal fractional coverage of annual green vegetation
!-- SHDMAX      maximum areal fractional coverage of annual green vegetation
!-- XLAI        leaf area index (dimensionless)
!-- Z0BRD       Background fixed roughness length (M)
!-- Z0          Background vroughness length (M) as function
!-- ZNT         Time varying roughness length (M) as function
!-- ALBD(IVGTPK,ISN) background albedo reading from a table
! --- LSM output
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- LH          upward moisture flux at the surface (W m-2)
!-- GRDFLX(I,J) ground heat flux (W m-2)
!-- FDOWN       radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN
!----------------------------------------------------------------------------
!-- EC          canopy water evaporation ((W m-2)
!-- EDIR        direct soil evaporation (W m-2)
!-- ET          plant transpiration from a particular root layer (W m-2)
!-- ETT         total plant transpiration (W m-2)
!-- ESNOW       sublimation from (or deposition to if <0) snowpack (W m-2)
!-- DRIP        through-fall of precip and/or dew in excess of canopy
!                 water-holding capacity (m)
!-- DEW         dewfall (or frostfall for t<273.15) (M)
! ----------------------------------------------------------------------
!-- BETA        ratio of actual/potential evap (dimensionless)
!-- ETP         potential evaporation (W m-2)
! ----------------------------------------------------------------------
!-- FLX1        precip-snow sfc (W m-2)
!-- FLX2        freezing rain latent heat flux (W m-2)
!-- FLX3        phase-change heat flux from snowmelt (W m-2)
! ----------------------------------------------------------------------
!-- ACSNOM      snow melt (mm) (water equivalent)
!-- ACSNOW      accumulated snow fall (mm) (water equivalent)
! ----------------------------------------------------------------------
!-- RUNOFF1     surface runoff (m s-1), not infiltrating the surface
!-- RUNOFF2     subsurface runoff (m s-1), drainage out bottom of last
!                  soil layer (baseflow)
!  important note: here RUNOFF2 is actually the sum of RUNOFF2 and RUNOFF3
!-- RUNOFF3     numerical trunctation in excess of porosity (smcmax)
!                  for a given soil layer at the end of a time step (m s-1).
! ----------------------------------------------------------------------
!-- RC          canopy resistance (s m-1)
!-- PC          plant coefficient (unitless fraction, 0-1) where PC*ETP = actual transp
!-- RSMIN       minimum canopy resistance (s m-1)
!-- RCS         incoming solar rc factor (dimensionless)
!-- RCT         air temperature rc factor (dimensionless)
!-- RCQ         atmos vapor pressure deficit rc factor (dimensionless)
!-- RCSOIL      soil moisture rc factor (dimensionless)

!-- EMISS       surface emissivity (between 0 and 1)

!-- ROVCP       R/CP
!               (R_d/R_v) (dimensionless)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!----------------------------------------------------------------

! IN only

   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                            TMN, &
                                                         XLAND, &
                                                          XICE, &
                                                        VEGFRA, &
                                                        SHDMIN, &
                                                        SHDMAX, &
                                                        SNOALB, &
                                                           GSW, &
                                                           GLW, &
                                                            Z0, &
                                                        ALBBCK, &
                                                        RAINBL, &
                                                        EMISS    

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
            INTENT(IN   )    ::                           QV3D, &
                                                         p8w3D, &
                                                          DZ8W, &
                                                          T3D
   REAL,     DIMENSION( ims:ime, jms:jme )                    , &
             INTENT(IN   )               ::               QGH,  &
                                                          CHS,   &
                                                          CPM

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                         IVGTYP, &
                                                        ISLTYP

   INTEGER, INTENT(IN)       ::     num_soil_layers,ITIMESTEP

   REAL,     INTENT(IN   )   ::     DT,ROVCP

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::DZS

! IN and OUT

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(INOUT)   ::                          SMOIS, & ! total soil moisture
                                                         SH2O,  & ! new soil liquid
                                                         TSLB     ! TSLB     STEMP

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            TSK, & !was TGB (temperature)
                                                           HFX, &     
                                                           QFX, &     
                                                            LH, &
                                                        GRDFLX, &
                                                          QSFC,&     
                                                          CQS2,&     
                                                          CHS2,&     
                                                          SNOW, & 
                                                         SNOWC, & 
                                                         SNOWH, & !new
                                                        CANWAT, & 
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOM, &
                                                        ACSNOW, &
                                                        ALBEDO, &
                                                           ZNT


! Local variables (moved here from driver to make routine thread safe, 20031007 jm)

      REAL, DIMENSION(1:num_soil_layers) ::  ET

      REAL  ::  BETA, ETP, SSOIL,EC, EDIR, ESNOW, ETT,        &
                FLX1,FLX2,FLX3, DRIP,DEW,FDOWN,RC,PC,RSMIN,XLAI,  &
                RCS,RCT,RCQ,RCSOIL
                                                                           
 
! DECLARATIONS - LOGICAL
! ----------------------------------------------------------------------
      LOGICAL, PARAMETER :: LOCAL=.false.
      LOGICAL :: FRZGRA, SNOWNG

      LOGICAL :: IPRINT
 
! ----------------------------------------------------------------------
! DECLARATIONS - INTEGER
! ----------------------------------------------------------------------
      INTEGER :: I,J, ICE,NSOIL,SLOPETYP,SOILTYP,VEGTYP
      INTEGER :: NROOT
      INTEGER :: KZ ,K
      INTEGER :: NS 
      INTEGER, PARAMETER :: NSOLD = 30
! ----------------------------------------------------------------------
! DECLARATIONS - REAL
! ----------------------------------------------------------------------

      REAL  :: SHMIN,SHMAX,DQSDT2,LWDN,PRCP,PRCPRAIN,           &
               Q2SAT,SFCPRS,SFCSPD,SFCTMP,SHDFAC,SNOALB1,             &
               SOLDN,TBOT,ZLVL, Q2K,ALBBRD, ALBEDOK, ETA, ETA_KINEMATIC,      &
               Z0K,RUNOFF1,RUNOFF2,RUNOFF3,SHEAT       

      REAL  :: EMISSI

      REAL  :: SNCOVR,SNEQV,SNOWHK,CMC, CHK,TH2

      REAL  :: SMCDRY,SMCMAX,SMCREF,SMCWLT,SNOMLT,SOILM,SOILW,Q1,T1 

      REAL  :: DUMMY,Z0BRD
!
      REAL  :: COSZ, SOLARDIRECT
!
      REAL, DIMENSION(1:num_soil_layers)::  SLDPTH, STC,SMC,SWC
!
      REAL, DIMENSION(1:NSOLD) ::     ZSOIL, RTDIS   
      REAL, PARAMETER  :: TRESH=.95E0, A2=17.67,A3=273.15,A4=29.65,   &
                          T0=273.16E0, ELWV=2.50E6,  A23M4=A2*(A3-A4)


! debug printout
         IPRINT=.false.
!FEI: temporaray arrays below need to be changed later by using SI

      SLOPETYP=2
!      SHDMIN=0.00           


      NSOIL=num_soil_layers

     DO NS=1,NSOIL
     SLDPTH(NS)=DZS(NS)
     ENDDO

   DO J=jts,jte

      IF(ITIMESTEP.EQ.1)THEN                                                 
        DO 50 I=its,ite
!*** initialize soil conditions for IHOP 31 May case
!         IF((XLAND(I,J)-1.5) < 0.)THEN                                
!            if (I==108.and.j==85) then
!                  DO NS=1,NSOIL
!                      SMOIS(I,NS,J)=0.10
!                      SH2O(I,NS,J)=0.10
!                  enddo
!             endif
!         ENDIF

!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS                   
          IF((XLAND(I,J)-1.5).GE.0.)THEN                                
! check sea-ice point                                                   
            IF(XICE(I,J).EQ.1..and.IPRINT)PRINT*,' sea-ice at water point, I=',I,  &
              'J=',J
!***   Open Water Case                                                  
            SMSTAV(I,J)=1.0                                             
            SMSTOT(I,J)=1.0                                             
            DO NS=1,NSOIL                                               
              SMOIS(I,NS,J)=1.0                                          
              TSLB(I,NS,J)=273.16                                          !STEMP
            ENDDO                                                       
          ELSE                                                          
            IF(XICE(I,J).EQ.1.)THEN                                     
!***        SEA-ICE CASE                                                
              SMSTAV(I,J)=1.0                                           
              SMSTOT(I,J)=1.0                                           
              DO NS=1,NSOIL                                             
                SMOIS(I,NS,J)=1.0                                        
              ENDDO                                                     
            ENDIF                                                       
          ENDIF                                                         
!                                                                       
   50   CONTINUE                                                        
      ENDIF                                                               ! end of initialization over ocean

!-----------------------------------------------------------------------
      DO 100 I=its,ite                                                    
        SFCPRS=P8w3D(i,1,j)
        Q2K=QV3D(i,1,j)
        Q2SAT=QGH(I,j)                                                  
        SFCTMP=T3D(i,1,j)
        ZLVL=0.5*DZ8W(i,1,j)
        TH2=SFCTMP+(0.0097545*ZLVL)
        EMISSI = EMISS(I,J)
        LWDN=GLW(I,J)*EMISSI
        SOLDN=GSW(I,J)/(1.0-albedo(i,j))
        PRCP=RAINBL(i,j)/DT 
        VEGTYP=IVGTYP(I,J)                                            
        SOILTYP=ISLTYP(I,J)                                            
        SHDFAC=VEGFRA(I,J)/100.                                       
        T1=TSK(I,J)
        CHK=CHS(I,J) 
        SHMIN=SHDMIN(I,J)/100. !NEW 
        SHMAX=SHDMAX(I,J)/100. !NEW
        SNOALB1=SNOALB(I,J)     !NEW
! convert snow depth from mm to meter
        SNEQV=SNOW(I,J)*0.001
!        SNOWHK=SNOWH(I,J)*0.001     ! check the unit of snowh which is assumed to be mm for now
        SNOWHK=SNOWH(I,J)     ! check the unit of snowh which is assumed to be m for now
        

!***                                                                    
        IF((XLAND(I,J)-1.5).GE.0.)THEN                 
! Open water points
!CC     Q2SAT=PQ0/SFCPRS*EXP(A2*(TGDSA(I)-A3)/(TGDSA(I)-A4))
!          HFX(I,J)=CHFF*(TGDSA(I)-TH(I))
!          QFX(I,J)=RHO(I)*CHS(I)*(Q2SAT-QV(I))
!          SFCEVP(I,J)=SFCEVP(I,J)+QFX(I,J)*DT

        ELSE
! Land or sea-ice case

          IF (XICE(I,J) .GT. 0.5) THEN                                  
             ICE=1                                                      
          ELSE                                                          
             ICE=0                                                      
          ENDIF                                                         
          DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2                             

          IF(ICE.EQ.0)THEN                                              
            TBOT=TMN(I,J)                                              
          ELSE                                                          
            TBOT=271.16                                                
          ENDIF                                                         
          IF(VEGTYP.EQ.25) SHDFAC=0.0001
          IF(SOILTYP.EQ.14.AND.XICE(I,J).EQ.0.)THEN                      
         IF(IPRINT)PRINT*,' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'          
         IF(IPRINT)PRINT*,i,j,'RESET SOIL in surfce.F'                      
            SOILTYP=7                                                    
          ENDIF                                                         
          CMC=CANWAT(I,J)                                                

!-------------------------------------------
!*** convert snow depth from mm to meter                                
!                                                                       
!          IF(RDMAXALB) THEN
!           SNOALB=ALBMAX(I,J)*0.01
!         ELSE
!           SNOALB=MAXALB(IVGTPK)*0.01
!         ENDIF
!         IF(RDBRDALB) THEN
!           ALBBRD=ALBEDO(I,J)*0.01
!         ELSE
!           ALBBRD=ALBD(IVGTPK,ISN)*0.01
!         ENDIF

!        SNOALB1=0.80
!        SHMIN=0.00
        ALBBRD=ALBBCK(I,J)                  
        Z0BRD=Z0(I,J)
!FEI: temporaray arrays above need to be changed later by using SI
      
          DO 70 NS=1,NSOIL                                              
            SMC(NS)=SMOIS(I,NS,J)                                       
            STC(NS)=TSLB(I,NS,J)                                          !STEMP
            SWC(NS)=SH2O(I,NS,J)
   70     CONTINUE                                                      
!
          if ( (SNEQV.ne.0..AND.SNOWHK.eq.0.).or.(SNOWHK.le.SNEQV) )THEN
            SNOWHK= 5.*SNEQV
          endif
!           print *, 'BEFORE SFLX, in Noahlsm_driver'
!           print*, 'EMISSI', EMISSI
!
          IF(IPRINT) THEN 
!                                                                       
       print*, 'BEFORE SFLX, in Noahlsm_driver'
       print*, 'ICE', ICE, 'DT',DT, 'ZLVL',ZLVL, 'NSOIL', NSOIL,   &
       'SLDPTH', SLDPTH, 'LOCAL',LOCAL, 'LUTYPE',&
        LUTYPE, 'SLTYPE',SLTYPE, 'LWDN',LWDN, 'SOLDN',SOLDN,      &
        'SFCPRS',SFCPRS, 'PRCP',PRCP,'SFCTMP',SFCTMP,'Q2K',Q2K,   &
         'TH2',TH2,'Q2SAT',Q2SAT,'DQSDT2',DQSDT2,'VEGTYP', VEGTYP,&
         'SOILTYP',SOILTYP, 'SLOPETYP',SLOPETYP, 'SHDFAC',SHDFAC,&
         'SHMIN',SHMIN, 'ALBBRD',ALBBRD,'SNOALB1',SNOALB1,'TBOT',&
          TBOT, 'Z0BRD',Z0BRD, 'Z0K',Z0K, 'CMC',CMC, 'T1',T1,'STC',&
          STC, 'SMC',SMC, 'SWC',SWC,'SNOWHK',SNOWHK,'SNEQV',SNEQV,&
          'ALBEDO',ALBEDO,'CHK',CHK,'ETA',ETA,'SHEAT',SHEAT,      &
          'ETA_KINEMATIC',ETA_KINEMATIC, 'FDOWN',FDOWN,'EC',EC,   &
          'EDIR',EDIR,'ET',ET,'ETT',ETT,'ESNOW',ESNOW,'DRIP',DRIP,&
          'DEW',DEW,'BETA',BETA,'ETP',ETP,'SSOIL',SSOIL,'FLX1',FLX1,&
          'FLX2',FLX2,'FLX3',FLX3,'SNOMLT',SNOMLT,'SNCOVR',SNCOVR,&
          'RUNOFF1',RUNOFF1,'RUNOFF2',RUNOFF2,'RUNOFF3',RUNOFF3, &
          'RC',RC, 'PC',PC,'RSMIN',RSMIN,'XLAI',XLAI,'RCS',RCS,  &
          'RCT',RCT,'RCQ',RCQ,'RCSOIL',RCSOIL,'SOILW',SOILW,     &
          'SOILM',SOILM,'Q1',Q1,'SMCWLT',SMCWLT,'SMCDRY',SMCDRY,&
          'SMCREF',SMCREF,'SMCMAX',SMCMAX,'NROOT',NROOT
           endif

       CALL SFLX (ICE,DT,ZLVL,NSOIL,SLDPTH,                       &    !C
                 LOCAL,                                           &    !L
                 LUTYPE, SLTYPE,                                  &    !CL
                 LWDN,SOLDN,SFCPRS,PRCP,SFCTMP,Q2K,DUMMY,         &    !F
                 DUMMY,DUMMY, DUMMY,                              &    !F PRCPRAIN not used
                 TH2,Q2SAT,DQSDT2,                                &    !I
                 VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHMIN,DUMMY,      &    !I   
                 ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI,        &    !S 
                 CMC,T1,STC,SMC,SWC,SNOWHK,SNEQV,ALBEDOK,CHK,dummy,&    !H
                 ETA,SHEAT, ETA_KINEMATIC,FDOWN,                  &    !O
                 EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                   &    !O
                 BETA,ETP,SSOIL,                                  &    !O
                 FLX1,FLX2,FLX3,                                  &    !O
                 SNOMLT,SNCOVR,                                   &    !O
                 RUNOFF1,RUNOFF2,RUNOFF3,                         &    !O
                 RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,             &    !O
                 SOILW,SOILM,Q1,                                  &    !D
                 SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT)


!           print *, 'AFTER SFLX, in Noahlsm_driver'
!           print*, 'EMISSI', EMISSI

          IF(IPRINT) THEN 
       print*, 'AFTER SFLX, in Noahlsm_driver'
       print*, 'ICE', ICE, 'DT',DT, 'ZLVL',ZLVL, 'NSOIL', NSOIL,   &
       'SLDPTH', SLDPTH, 'LOCAL',LOCAL, 'LUTYPE',&
        LUTYPE, 'SLTYPE',SLTYPE, 'LWDN',LWDN, 'SOLDN',SOLDN,      &
        'SFCPRS',SFCPRS, 'PRCP',PRCP,'SFCTMP',SFCTMP,'Q2K',Q2K,   &
         'TH2',TH2,'Q2SAT',Q2SAT,'DQSDT2',DQSDT2,'VEGTYP', VEGTYP,&
          'SOILTYP',SOILTYP, 'SLOPETYP',SLOPETYP, 'SHDFAC',SHDFAC,&
         'SHDMIN',SHMIN, 'ALBBRD',ALBBRD,'SNOALB',SNOALB1,'TBOT',&
          TBOT, 'Z0BRD',Z0BRD, 'Z0K',Z0K, 'CMC',CMC, 'T1',T1,'STC',&
          STC, 'SMC',SMC, 'SWc',SWC,'SNOWHK',SNOWHK,'SNEQV',SNEQV,&
          'ALBEDO',ALBEDOK,'CHK',CHK,'ETA',ETA,'SHEAT',SHEAT,      &
          'ETA_KINEMATIC',ETA_KINEMATIC, 'FDOWN',FDOWN,'EC',EC,   &
          'EDIR',EDIR,'ET',ET,'ETT',ETT,'ESNOW',ESNOW,'DRIP',DRIP,&
          'DEW',DEW,'BETA',BETA,'ETP',ETP,'SSOIL',SSOIL,'FLX1',FLX1,&
          'FLX2',FLX2,'FLX3',FLX3,'SNOMLT',SNOMLT,'SNCOVR',SNCOVR,&
          'RUNOFF1',RUNOFF1,'RUNOFF2',RUNOFF2,'RUNOFF3',RUNOFF3, &
          'RC',RC, 'PC',PC,'RSMIN',RSMIN,'XLAI',XLAI,'RCS',RCS,  &
          'RCT',RCT,'RCQ',RCQ,'RCSOIL',RCSOIL,'SOILW',SOILW,     &
          'SOILM',SOILM,'Q1',Q1,'SMCWLT',SMCWLT,'SMCDRY',SMCDRY,&
          'SMCREF',SMCREF,'SMCMAX',SMCMAX,'NROOT',NROOT
           endif
 
!***  UPDATE STATE VARIABLES 
          CANWAT(I,J)=CMC                                                
          SNOW(I,J)=SNEQV*1000.
!          SNOWH(I,J)=SNOWHK*1000.
          SNOWH(I,J)=SNOWHK                   ! SNOWHK is assumed in meters
          ALBEDO(I,J)=ALBEDOK
          ZNT(I,J)=Z0K
          TSK(I,J)=T1
          HFX(I,J)=SHEAT 
          QFX(I,J)=ETA_KINEMATIC
          LH(I,J)=ETA
          GRDFLX(I,J)=SSOIL
          SNOWC(I,J)=SNCOVR
          CHS2(I,J)=CQS2(I,J)
!      prevent diagnostic ground q (q1) from being greater than qsat(tsk)
!      as happens over snow cover where the cqs2 value also becomes irrelevant
!      by setting cqs2=chs in this situation the 2m q should become just qv(k=1)
          IF (Q1 .GT. QSFC(I,J)) THEN
            CQS2(I,J) = CHS(I,J)
          ENDIF
          QSFC(I,J)=Q1
!                                                                       
          DO 80 NS=1,NSOIL                                              
           SMOIS(I,NS,J)=SMC(NS)                                       
           TSLB(I,NS,J)=STC(NS)                                        !  STEMP
           SH2O(I,NS,J)=SWC(NS)
   80     CONTINUE                                                      
!       ENDIF                                                           

!***  DIAGNOSTICS                                                       
          SMSTAV(I,J)=SOILW                                            
          SMSTOT(I,J)=SOILM*1000.                                      
!         Convert the water unit into mm
          SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RUNOFF1*DT*1000.0
          UDRUNOFF(I,J)=UDRUNOFF(I,J)+(RUNOFF2+RUNOFF3)*DT*1000.0
          IF(SFCTMP<=T0)THEN
!            ACSNOW(I,J)=ACSNOW(I,J)+PREC(I)*DT
            ACSNOW(I,J)=ACSNOW(I,J)+PRCP*DT
          ENDIF
          IF(SNOW(I,J).GT.0.)THEN
            ACSNOM(I,J)=ACSNOM(I,J)+SNOMLT*1000.
          ENDIF

        ENDIF                                                           ! endif of land-sea test 

  100 CONTINUE                                                          ! of I loop

   ENDDO                                                                ! of J loop
!------------------------------------------------------
   END SUBROUTINE lsm_rtland
