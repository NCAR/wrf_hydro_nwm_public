MODULE module_sf_Noah_grads_output


  contains

	SUBROUTINE PRTHYD(KTIME,IX,JX,NSOIL,MDT,MIN,ETAX,ETPX,PRCPX,         &
         SMC,ZSOIL,RUNOFF1X,RUNOFF2X,RUNOFF3X,DT,EDIRX,ECX,            &
         ETTX,CMC,SLDPTH,SNOEVPX,SH2OX,ESDX,SNODEP,VEGTYP)
!FC         QBDRYT,QSTRMVOLT,QBDRY,QSTRMVOL,SFCHEAD,INFXS1,DH,            & 
!FC         CH_NET,PDDUM,PCPDRP,SFCWATR,QSUBDRY,QSUBDRYT)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC
!CC    NAME:  PRINT Hydrological variables
!CC
!CC    VARIABLES:
!CC    =========
!CC
!CC       LABEL     I/O     TYPE    ............DESCRIPTION...............
!CC
!CC       AET      LOC      RS      ACTUAL EVAPOTRANSPIRATIVE ENERGY
!CC                                 (J M-2 S-1)
!CC       AET2     LOC      RS      ACTUAL EVAPOTRANSPIRATION (MM)
!CC       CH       IOC      RS      DRAG COEF FOR HEAT/MOISTURE
!CC       CM       IOC      RS      DRAG COEFFICIENT FOR MOMENTUM
!CC       CMC      IOC      RS      CANOPY MOISTURE CONTENT (M)
!CC       DEW       IB      RS      DEWFALL AMOUNT  (M S-1)
!CC       DRIP      IB      RS      EXCESS CANOPY MOISTURE (M)
!CC       EC(EC1)   IB      RS      CANOPY EVAPORATION (M S-1)
!CC       EDIR(EDIR1)IB      RS      DIRECT SOIL EVAPORATION (M S-1)
!CC       ETA      IOC      RA      FINAL ACTUAL EVAPOTRANSP (KG M-2 S-1)
!CC       ETAS     IOC      RA      FINAL ACTUAL EVAPOTRANSP (mm)
!CC       ETP      IOC      RA      FINAL POTNTL EVAPOTRANSP (KG M-2 S-1)
!CC       ETPS     IOC      RA      FINAL POTNTL EVAPOTRANSP (mm)
!CC       ETT(ETT1) IB      RS      ACCUM PLANT TRANSPIRATION (M S-1)
!DG       ETPND             R       DIRECT EVAP OF PONDED WATER (M S-1)
!CC       F        IOC      RS      NET FLUX (TOT DOWNWARD RADIATION)
!CC       FLX1      IB      RS      1ST FLUX VALUE (W M-2)
!CC       FLX2      IB      RS      2ND FLUX VALUE (W M-2)
!CC       FLX3      IB      RS      3RD FLUX VALUE (W M-2)
!CC       FOG       IC      LS      INTERMEDIATE HRLY FOG FLAG
!CC       H        LOC      RS      SENSIBLE HEAT FLUX (W M-2)
!CC       HEAT     LOC      RS      SENSIBLE HEAT FLUX SUB-PRODUCT
!CC       HEMI      IC      IS      CURRENT HEMISPHERE (1=N, 2=S)
!CC       I         IC      IS      1/8 MESH I COORDINATE
!CC       ICLAMT    IC      IA      INTERMEDIATE HRLY CLD AMOUNT
!CC       ICLTYP    IC      IA      INTERMEDIATE HRLY CLD TYPE
!CC       J         IC      IS      1/8 MESH J COORDINATE
!CC       K        LOC      IS      LOOP INDEX
!CC       NSOIL     IC      IS      SOIL LAYER NUMBER
!CC       PET      LOC      RS      POTENTIAL EVAPOTRANSPIRATION (MM)
!CC       PRCP     IOC      RA      HALF HOURLY PRECIP AMT (KG M-2 S-1)
!CC       Q2       IOC      RS      MIXING RATIO AT 1ST MDL LVL ABV SKIN
!CC       Q2SAT    IOC      RS      SAT MXNG RATIO AT 1ST MDL LVL ABV SKIN
!CC       RES      LOC      RS      ENERGY BALANCE EQN RESIDUAL (W M-2)
!CC       RHO       IB      RS      SFC AIR DENSITY (KG M-3)
!CC       RIB       IB      RS      BULK RICHARDSON NUMBER
!CC       RLDOWN    IC      RS      DOWNWARD LONGWAVE RADIATION (W M-2)
!CC       RR       LOC      RS      SENSIBLE HEAT SUB-PRODUCT
!CC       RSOLIN    IC      RS      SOLAR RADIATION (W M-2)
!CC       RUNOFF1    IB      RS      GRND SFC RUNOFF (M )
!CC       RUNOFF2    IB      RS      UNDERGROUND  RUNOFF (M )
!CC       RUNOFF3    IB      RS      GRUNOFF WITHIN SOIL LAYERS (M )
!CC       SMC      IOC      RA      SOIL MOISTURE CONTENT (VOLUMETRIC)
!CC       ZSOIL    IOC      RA      SOIL LAYER DEPTH  ( M )
!CC
!CDG  OUTPUT for sfc routing
!CDG      QBDRYT                    Accum Flow leaving the flow domain bound.
!CDG                                (does NOT include channel flow)
!CDG      QSTRMVOLT                 Accum Flow into the stream network
!CDG      QSUBDRYT                  Accum Subsfc Flow leaving the flow domain 
!CDG      QSUBDRY                   Accum Subsfc Flow leaving the flow domain
!CDG      QBDRY                     Accum. Grid of flow leaving flow domain bound.
!CDG      QSTRMVOL                  Accum. Grid of flow into the stream network
!CDG      SFCHEAD                   Grid of surface hydrualic head (i.e. sfc water)
!CDG      INFXS1                    Grid of infiltration excess on a timestep
!CDG      DH                        Grid of change in surface head due to runoff
!CDG      CH_NET                    Channel network (0,1)
!CDG      PDDUM                     Infiltrated water
!CDG      PCPDRP                    Canopy drip
!CDG      SFCWATR                   Total sfc water


	IMPLICIT NONE


        INTEGER, PARAMETER :: NSOLD=8

!! PASSED VARIABLES

        CHARACTER(len=6) MDT
        CHARACTER(len=2) MIN
        INTEGER, INTENT(IN)            :: IX,JX, NSOIL, KTIME
        INTEGER, INTENT(IN), DIMENSION(IX,JX) ::  VEGTYP
!FC        INTEGER, INTENT(IN), DIMENSION(IX,JX) :: CH_NET
        REAL, INTENT(IN), DIMENSION(IX,JX,NSOLD) ::  SMC
        REAL, INTENT(IN), DIMENSION(IX,JX) :: SNODEP
        REAL, INTENT(IN), DIMENSION(NSOLD) :: ZSOIL
        REAL, INTENT(IN), DIMENSION(IX,JX) :: CMC
        REAL, INTENT(IN), DIMENSION(NSOLD) :: SLDPTH
        REAL, INTENT(IN), DIMENSION(IX,JX,NSOLD) :: SH2OX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: ETPX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: ETAX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: RUNOFF1X
        REAL, INTENT(IN), DIMENSION(IX,JX) :: RUNOFF2X
        REAL, INTENT(IN), DIMENSION(IX,JX) :: RUNOFF3X
        REAL, INTENT(IN), DIMENSION(IX,JX) :: EDIRX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: ECX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: ETTX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: PRCPX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: ESDX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: SNOEVPX
        REAL, INTENT(IN) :: DT
!FC        REAL, INTENT(IN) :: QBDRYT
!FC        REAL, INTENT(IN) :: QSTRMVOLT
!FC        REAL, INTENT(IN) :: QSUBDRYT
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: QBDRY
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: QSTRMVOL
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: SFCHEAD
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: INFXS1
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: DH
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: QSUBDRY
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: PDDUM
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: PCPDRP
!FC        REAL, INTENT(IN), DIMENSION(IX,JX) :: SFCWATR

!! LOCAL VARIABLES

        REAL, DIMENSION(IX,JX) :: CNET
        REAL, DIMENSION(IX,JX) :: SNODEP1
        REAL, DIMENSION(IX,JX) :: PRCPS
        REAL, DIMENSION(IX,JX) :: ETPS
        REAL, DIMENSION(IX,JX) :: ETAS
        REAL, DIMENSION(IX,JX) :: EDIR1S
        REAL, DIMENSION(IX,JX) :: EC1S
        REAL, DIMENSION(IX,JX) :: ESDS
        REAL, DIMENSION(IX,JX) :: ETT1S
        REAL, DIMENSION(IX,JX) :: ETPNDS
        REAL, DIMENSION(IX,JX) :: CMCS
        REAL, DIMENSION(IX,JX) :: RUNOFF11
        REAL, DIMENSION(IX,JX) :: RUNOFF22
        REAL, DIMENSION(IX,JX) :: RUNOFF33
        REAL, DIMENSION(IX,JX) :: TRUNOFF
        REAL, DIMENSION(IX,JX) :: SMC1
        REAL, DIMENSION(IX,JX) :: SMC2
        REAL, DIMENSION(IX,JX) :: SMC3
        REAL, DIMENSION(IX,JX) :: SMC4
        REAL, DIMENSION(IX,JX) :: SMC5
        REAL, DIMENSION(IX,JX) :: SMC6
        REAL, DIMENSION(IX,JX) :: SMC7
        REAL, DIMENSION(IX,JX) :: SMC8
        REAL, DIMENSION(IX,JX) :: SH1
        REAL, DIMENSION(IX,JX) :: SH2
        REAL, DIMENSION(IX,JX) :: SH3
        REAL, DIMENSION(IX,JX) :: SH4
        REAL, DIMENSION(IX,JX) :: PCPDRP1
        REAL, DIMENSION(IX,JX) :: INFILT
        REAL, DIMENSION(IX,JX) :: SFCWATR1
        INTEGER :: I,J,k
        INTEGER :: LWR4
        INTEGER, SAVE :: ICL
        REAL :: SS

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C
!C           Convert from M S-1 to MM
!C
!C      print*, ' ************** IN PRTHYD **********'
!C      print*,'ETP=', ETP, ' ETA=', ETA, 'RUNOFF1=', RUNOFF1,
!C     *        'RUNOFF2=', RUNOFF2, ' TRUNOFF=', TRUNOFF, ' SMC1=',
!C     *         SMC(1), ' SMC2=', SMC(2), 'ZSOIL1=', ZSOIL(1),
!C     *        ' ZSOIL2=', ZSOIL(2), ' DT=',DT
!C        STEST=EDIR1+EC1+ETT1
!C        ETA11=ETA/1000.0
!C        print*,' **** In PRTHYDRO ***'
!C        print*,' ETA1=',ETA11,' ESUM=',STEST,'EDIR1=',EDIR1,'EC1=',
!C     *   EC1,'ETT1=',ETT1
      ETPNDS = -999.
      SS = 1000.
      DO J=1,JX
        DO I=1,IX
        IF((VEGTYP(I,J).GT.0).AND.(VEGTYP(I,J).NE.16)) THEN        
          SNODEP1(I,J) = SNODEP(I,J)*SS
!CDG Remove erroneous precip *DT
          PRCPS(I,J) = PRCPX(I,J)
          ETPS(I,J) = ETPX(I,J)*DT
          ETAS(I,J) = ETAX(I,J)*DT
          EDIR1S(I,J)=EDIRX(I,J)/2.5E6*DT
          EC1S(I,J)=ECX(I,J)/2.5E6*DT
          ETT1S(I,J) = ETTX(I,J)/2.5E6*DT
!FC          ETPNDS(I,J)=ETPNDX(I,J)*SS*DT
          CMCS(I,J)=CMC(I,J)*SS
          RUNOFF11(I,J) = RUNOFF1X(I,J)*DT*SS
          RUNOFF22(I,J) = RUNOFF2X(I,J)*DT*SS
          RUNOFF33(I,J) = RUNOFF3X(I,J)*SS*DT
          TRUNOFF(I,J)=RUNOFF11(I,J)+RUNOFF22(I,J)
!+RUNOFF33(I,J)
!CDG Change Cnet from integer to real number
!FC          CNET(I,J)=REAL(CH_NET(I,J))
!          INFILT(I,J)=PDDUM(I,J)*DT*SS
!          PCPDRP1(I,J)=PCPDRP(I,J)*DT*SS
!          SFCWATR1(I,J)=SFCWATR(I,J)*DT*SS
          ESDS(I,J)=ESDX(I,J)*SS

!CDG Set Lake points = no data value -999.9

        ELSE
          SNODEP1(I,J) = -999.9
!CDG Remove erroneous precip *DT
          PRCPS(I,J) =-999.9
          ETPS(I,J) =-999.9
          ETAS(I,J) =-999.9
          EDIR1S(I,J)=-999.9
          EC1S(I,J)=-999.9
          ETT1S(I,J) = -999.9
          ETPNDS(I,J) = -999.9
          CMCS(I,J)=-999.9
          RUNOFF11(I,J) =-999.9
          RUNOFF22(I,J) =-999.9
          RUNOFF33(I,J) =-999.9
          TRUNOFF(I,J)=-999.9
          ESDS(I,J)=-999.9
!        INFILT(I,J)=-999.9
!        PCPDRP1(I,J)=-999.9
!        SFCWATR1(I,J)=-999.9
       ENDIF
       ENDDO
      ENDDO

       LWR4=11
       IF(KTIME==1) THEN	
!          FILE='../Results/Grads/HY'//MDT//MIN//'.grs',STATUS='NEW',       &
              ICL=1
              OPEN(UNIT=LWR4,                                            &
           FILE='../Results/Grads/HYDRO.grs',STATUS='NEW',       &
            ACCESS='DIRECT',FORM='UNFORMATTED',recl=IX*JX*4)
	ENDIF

!C
!C  ****** Write in an unformatted form for Grads output *********
!C
      WRITE(LWR4,REC=ICL) PRCPS
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) ETPS
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) ETAS
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) RUNOFF11
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) RUNOFF22
      ICL=ICL+1
!DG Temporary change to RUNOFF33 to check soil drainage
!      WRITE(LWR4,REC=ICL) TRUNOFF
      WRITE(LWR4,REC=ICL) RUNOFF33
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) EDIR1S
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) ETT1S
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) ETPNDS
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) EC1S
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) CMCS
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) SNODEP1
      ICL=ICL+1
      WRITE(LWR4,REC=ICL) ESDS
      ICL=ICL+1
      DO k=1,NSOIL
         WRITE(LWR4,REC=ICL) SMC(:,:,K)
         ICL=ICL+1
      ENDDO
      DO k=1,NSOIL
         WRITE(LWR4,REC=ICL) SH2OX(:,:,K)
         ICL=ICL+1
      ENDDO
!DG Output sfc routed data to Grads output
!FC      WRITE(LWR4,REC=ICL) QBDRY
!FC      ICL=ICL+1
!FC      WRITE(LWR4,REC=ICL) QSTRMVOL
!FC      ICL=ICL+1
!FC     WRITE(LWR4,REC=ICL) SFCHEAD
!FC      ICL=ICL+1
!FC      WRITE(LWR4,REC=ICL) INFXS1
!FC      ICL=ICL+1
!FC      WRITE(LWR4,REC=ICL) DH
!FC      ICL=ICL+1
!FC      WRITE(LWR4,REC=ICL) CNET
!FC      ICL=ICL+1
!FC      WRITE(LWR4,REC=ICL) INFILT
!FC      ICL=ICL+1
!FC      WRITE(LWR4,REC=ICL) PCPDRP1
!FC      ICL=ICL+1
!FC      WRITE(LWR4,REC=ICL) SFCWATR1
!FC      ICL=ICL+1
!FC      WRITE(LWR4,REC=ICL) QSUBDRY
!FC      ICL=ICL+1
!     CLOSE(UNIT=LWR4)

!DG OUTPUT DOMAIN TOTAL ACCUM. BDRY & CHANNEL RUNOFF VALUES

!        print *, 'Channel Q=',QSTRMVOLT,'BDRY Q=',QBDRYT,               &
!                      'LAKE Q=',QLAKVOLT

!
! 100  FORMAT('DATE',4X,'TIME',2X,'PRCP(mm)',5X,'POTEV',6x,'ACTEV',6X,
!     *       'SFRUN',6X,'GRDRUN',5X,'TOTRUN',
!     *       6X,'DIREV',5x,'PLNTN',6X,'CANEV',6x,'CANPM',6x,'NSOIL',
!     *       3x,'SOIM(1)',
!     *       4x,'SOIM(2)',3x,'SOIM(3)',3x,'SOIM(4)')
100  FORMAT('DATE',4X,'TIME',2X,'PRCP(mm)',5X,'POTEV',6x,'ACTEV',6X,    &
             'SFRUN',6X,'GRDRUN',5X,'TOTRUN',                           &
             4X,'DIREV',6x,'PLNTN',6X,'CANEV',6x,'CANPM',6x,'NSOIL',    &
             3x,'SOIM(1)',                                              &
             4x,'SOIM(2)',2X,'SOIM(3)',3X,'SOIM(4)')
!     *       4x,'SOIM(2)',/,2X,'SOIM(3)',3X,'SOIM(4)')
!     *       /,4X,'DIREV',6x,'PLNTN',6X,'CANEV',6x,'CANPM',6x,'NSOIL', &
210  FORMAT(I6,1X,I4,1X,F10.6,1x,E12.4)

! ---------------------------------------------------------------------
      END SUBROUTINE PRTHYD
! ---------------------------------------------------------------------


      SUBROUTINE PRTHM(KTIME,IX,JX,MDT,MIN,NSOIL,CHX,FX,SOILHX,               &
          CMC,SFCTMPX,T1,ETAX,ETPX,                                     &
          PRCP,STC,SMC,DT,HX,RES)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C
!C    NAME:  PRINT THERMODYNAMICS VARIABLES
!C
!C    CPC:   N/A
!C    ===
!C
!C    PURPOSE:  TO CALC AND WRITE OUT PARAMETERS TO AID IN SCIENTIFIC
!C    =======   TUNING AND DEBUGGING OF THE OSU SOIL HYDROLOGY MODEL.
!C              THIS ROUTINE IS ONLY INVOKED FOR POINTS SELECTED BY
!C              SPECIFICATION IN THE CONTROL FILE.
!C
!C    METHOD:   1. CALC NEEDED PARTIAL PRODUCTS AND SUMS.
!C    ======    2. WRITE OUT RESULTS TO PRINT$ FILE.
!C
!C    REFERENCES:  FUNCTIONAL DESCRIPTION, SUBSYSTEM SPECIFICATION
!C
!C    VARIABLES:
!C    =========
!C
!C       LABEL     I/O     TYPE    ............DESCRIPTION...............
!C
!C       AET      LOC      RS      ACTUAL EVAPOTRANSPIRATIVE ENERGY
!C                                 (J M-2 S-1)
!C       AET2     LOC      RS      ACTUAL EVAPOTRANSPIRATION (MM)
!C       CH       IOC      RS      DRAG COEF FOR HEAT/MOISTURE
!C       CM       IOC      RS      DRAG COEFFICIENT FOR MOMENTUM
!C       CMC      IOC      RS      CANOPY MOISTURE CONTENT (M)
!C       DEW       IB      RS      DEWFALL AMOUNT  (M S-1)
!C       DRIP      IB      RS      EXCESS CANOPY MOISTURE (M)
!C       EC        IB      RS      CANOPY EVAPORATION (M S-1)
!C       EDIR      IB      RS      DIRECT SOIL EVAPORATION (M S-1)
!C       ETA      IOC      RA      FINAL ACTUAL EVAPOTRANSP (KG M-2 S-1)
!C       ETP      IOC      RA      FINAL POTNTL EVAPOTRANSP (KG M-2 S-1)
!C       ETPS     IOC      RA      FINAL POTNTL EVAPOTRANSP (J M-2 S-1)
!C       ETT       IB      RS      ACCUM PLANT TRANSPIRATION (M S-1)
!C       F        IOC      RS      NET FLUX (TOT DOWNWARD RADIATION)
!C       FLX1      IB      RS      1ST FLUX VALUE (W M-2)
!C       FLX2      IB      RS      2ND FLUX VALUE (W M-2)
!C       FLX3      IB      RS      3RD FLUX VALUE (W M-2)
!C       FUP       IB      RS      UPWARD GRND LW RADIATION (W M-2)
!C       H        LOC      RS      SENSIBLE HEAT FLUX (W M-2)
!C       HEAT     LOC      RS      SENSIBLE HEAT FLUX SUB-PRODUCT
!C       I         IC      IS      1/8 MESH I COORDINATE
!C       ICLAMT    IC      IA      INTERMEDIATE HRLY CLD AMOUNT
!C       ICLTYP    IC      IA      INTERMEDIATE HRLY CLD TYPE
!C       J         IC      IS      1/8 MESH J COORDINATE
!C       K        LOC      IS      LOOP INDEX
!C       NSOIL     IC      IS      SOIL LAYER NUMBER
!C       PET      LOC      RS      POTENTIAL EVAPOTRANSPIRATION (MM)
!C       PRCP     IOC      RA      HALF HOURLY PRECIP AMT (KG M-2 S-1)
!C       Q2       IOC      RS      MIXING RATIO AT 1ST MDL LVL ABV SKIN
!C       Q2SAT    IOC      RS      SAT MXNG RATIO AT 1ST MDL LVL ABV SKIN
!C       RES      LOC      RS      ENERGY BALANCE EQN RESIDUAL (W M-2)
!C       RHO       IB      RS      SFC AIR DENSITY (KG M-3)
!C       RIB       IB      RS      BULK RICHARDSON NUMBER
!C       RLDOWN    IC      RS      DOWNWARD LONGWAVE RADIATION (W M-2)
!C       RR       LOC      RS      SENSIBLE HEAT SUB-PRODUCT
!C       RSOLIN    IC      RS      SOLAR RADIATION (W M-2)
!C       RUNOFF    IB      RS      GRND SFC RUNOFF (M S-1)
!C       S         IC      RS      GRND SFC FLUX (W M-2)
!C       SFCPRS   IOC      RS      SFC PRESSURE (PASCALS)
!C       SFCSPD    IC      RS      SFC WIND SPEED (M S-1)
!C       SFCTMP   IOC      RS      SFC TEMPERATURE (K)
!C       SMC      IOC      RA      SOIL MOISTURE CONTENT (VOLUMETRIC)
!C       SMCMAX   IOC      RS      MAXIMUM SOIL MOISTURE CONTENT LIMIT
!C       SNODEP   IOC      RA      SNOW DEPTH ( M )
!C       STC      IOC      RA      SOIL TEMPERATURE ( 5 CM AND 95 CM )
!C       T1       IOC      RS      SKIN (GRND SFC) TEMPERATURE (K)
!C       T14      LOC      RS      GRND SFC TEMP TO THE 4TH POWER (K+4)
!C       TIME     IOC      IS      1 HRLY TIME (LOOP INDEX)
!C       Z        IOC      RS      HT ABOVE GRND LVL (M)
!C       ZSOIL    IOC      RA      SOIL LAYER DEPTH  ( M )
!C



	IMPLICIT NONE

        INTEGER, INTENT(IN):: IX,JX, NSOIL, KTIME
        INTEGER, PARAMETER :: NSOLD=10
        CHARACTER(len=6) :: MDT
        CHARACTER(len=2) :: MIN
        REAL, INTENT(IN) :: DT
        REAL, INTENT(IN), DIMENSION(IX,JX) :: ETPX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: ETAX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: SOILHX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: CHX         
        REAL, INTENT(IN), DIMENSION(IX,JX) :: CMC
        REAL, INTENT(IN), DIMENSION(IX,JX) :: SFCTMPX
        REAL, INTENT(IN), DIMENSION(IX,JX) :: T1
        REAL, INTENT(IN), DIMENSION(IX,JX) :: HX               
        REAL, INTENT(IN), DIMENSION(IX,JX) :: PRCP
        REAL, INTENT(IN), DIMENSION(IX,JX,NSOLD) :: SMC
        REAL, INTENT(IN), DIMENSION(IX,JX,NSOLD) :: STC

        REAL, DIMENSION(IX,JX) :: AET
        REAL, DIMENSION(IX,JX) :: ETPS
        REAL, DIMENSION(IX,JX) :: RES
        REAL, DIMENSION(IX,JX) :: RNET
        REAL, DIMENSION(IX,JX) :: FX
        REAL :: T14
        REAL :: FUP
        INTEGER :: I,J, K
        INTEGER :: LWR4
        INTEGER, SAVE :: ICL

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CONVERT ACTUAL EVAPOTRANSPIRATION VALUE FROM  KG M-2 S-1  TO
!     W M-2  (BY MULTIPLYING BY 2.5E+6) FOR USE IN ENERGY BALANCE.
!     ALSO CHANGE ACTUAL AND POTENTIAL EVAPORTRANSPIRATION VALUES
!     FROM  KG M-2 S-1  TO  MM  (BY MULTIPLYING BY DT).
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      NOUT=10
      DO J=1,JX
       DO I=1,IX
       IF(T1(I,J).NE.-999.9) T14 = T1(I,J) * T1(I,J) * T1(I,J) * T1(I,J)
       IF(ETAX(I,J).NE.-999.9) AET(I,J) = 2.5E+6 * ETAX(I,J)
       IF(ETPX(I,J).NE.-999.9) ETPS(I,J) = 2.5E+6 * ETPX(I,J)
       FUP = 5.67E-8 * T14
       IF(FX(I,J).NE.-999.9) RNET(I,J) = FX(I,J) - FUP
       ENDDO
      ENDDO
!
!   Net radiation
!
!      IF(INDI.LE.1) THEN
!         WRITE(NOUT,100)
!         WRITE(NOUT1,100)
!      ENDIF
!*****laurent*****
!      WRITE(NOUT,200) FX,RNET,HX,SOILHX,AET,T1,ETPS,PRCP,CHX,RES
!     &,               SFCTMPX,STC1,STC2,STC3,STC4
!
!  ***** Write in an unformatted form for Grads output ********
!
       LWR4=12
	IF(KTIME==1) THEN
           ICL=1
!          FILE='../Results/Grads/TH'//MDT//MIN//'.grs',STATUS='NEW',     &
       		OPEN(UNIT=LWR4,                                          &
           FILE='../Results/Grads/THERMO.grs',STATUS='NEW',     &
          ACCESS='DIRECT',FORM='UNFORMATTED', RECL=IX*JX*4)
	ENDIF

!  debug line1
!    print*, 'In PRTHM ********'
!    print*, 'NSOIL=', NSOIL
!    print*,' FX(35,35)',FX(35,35)
!  debug line2
!    print*,' FX(71,74)',FX(71,74)

      WRITE(LWR4,REC=ICL) FX
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) RNET
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) HX
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) SOILHX
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) AET
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) T1
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) ETPS
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) PRCP
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) CHX
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) RES
       ICL=ICL+1
      WRITE(LWR4,REC=ICL) SFCTMPX
       ICL=ICL+1
      DO k=1,NSOIL
       WRITE(LWR4,REC=ICL) STC(:,:,K)
       ICL=ICL+1
      ENDDO
!     CLOSE(UNIT=LWR4)

998  continue

! 100  FORMAT('DATE',4X,'TIME',4X,'RDWN',3X,'RNET',3X,'SEFX',3X,'GRFX',
!     &      4X,'LTFX',4X,'SKTM',5X,'SFMX',4X,'POEV',4X,'PRCP',3X,
!     &      'NSOIL',3X,'STC1',
!     &      4X,'STC2',4X,'STC3',4X,'STC4')
100  FORMAT('DATE',4X,'TIME',4X,'RDWN',3X,'RNET',3X,'SEFX',3X,'GRFX',   &
            4X,'LTFX',4X,'SKTM',5X,'SFMX',4X,'POEV',3x,'PRCP',3X,       &
            'NSOIL',3X,'STC1',                                          & 
            4X,'STC2',4X,'STC3',4X,'STC4')

! -------------------------------------------------------------------
      END SUBROUTINE PRTHM
! -------------------------------------------------------------------
END MODULE module_sf_Noah_grads_output
