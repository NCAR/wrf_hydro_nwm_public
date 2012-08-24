
    subroutine lsm_exe(did,&
          kt, &
          LATITUDE,LONGITUDE, &
          SO8LD_D,SO8LD_Vmax, &
          SLDPTH, &
          SFCHEADRT, &
          stc,smc,sh2ox,  &
          INFXSRT, SMCMAX1,SMCREF1,LKSAT,SOLDRAIN,SMCWLT1, &
          VEGTYP , TERADJ_SOLAR,&
          zsoil,REFMANN_out,&
          ix,jx,NSOIL, &
          T2,q2x,u,v,&
          pres,xlong,&
          SHORT, prcp1, weasd, &
          snodep,t1,cmc , &
          znt, snowc,LH,       &
          HX,EtAKIN,&
          QSFC,SOILHX,ALBEDX, &
          soltyp,GREENFRAC,ALBEDO12M, &
          ETPX, acrain &
#ifdef COUPLED
          ,tbot_wrf,zlvl_wrf,z0_wrf &
#endif
          )  
#ifdef MPP_LAND
     use module_mpp_land
#endif
     use module_rtland,only: readveg_hrldas, sflx_rt, redprm, sfcdif_off, &
           caltmp,calhum
! , geth_newdate
     use module_rtland, only: &
          range,&
          ZLVL,Z0,CZIL,ice,COSZ,SOLARDIRECT, &
          indir,inflnm,&
          z0brd,igrid,&
          SLOPETYP, &
          MM,DD,DD_chk, &
          inflnm, &
          VEGTYPX,SOILTYP,VERTKSAT, &
          SFCTMP, SFCPRS,  TH2, T1V, TH2V, RHO, &
          Q2SAT, DQSDT2, SFCSPD,CM,CH, &
          LWDN,SOLDN,PRCP,Q2,         &    !F
          PRCPRAIN,                       &    !F
          SHDMIN,SHDMAX,SFHEAD1RT,                 &    !I
          TBOT,                                         &    !S
          CMCX,T1X,SNOWH,SNEQV,ALBEDO, &    !H
          ETA,SHEAT, ETA_KINEMATIC,FDOWN,                  &    !O
          EC,EDIR,ETT,ESNOW,DRIP,DEW,                   &    !O
          BETA,ETP,SSOIL,                                  &    !O
          FLX1,FLX2,FLX3,                                  &    !O
          SNOMLT,SNCOVR,                                   &    !O
          RUNOFF1,RUNOFF2,RUNOFF3,                         &    !O
          RC,PC,RCS,RCT,RCQ,RCSOIL,                        &    !O
          SOILW,SOILM,Q1,ETPND1,INFXS1RT, &
          smcmax, smcwlt, smcref, &
!! added tmp
          xlai,s    , &
          alb, REFMANN


       use module_namelist, only: nlst_lsm
       use module_diag_field, only: lsm_diag
#ifdef MPP_LAND
    use module_mpp_land, only: my_id, IO_id
#endif
     
     implicit none

     integer did 

!     character(len = 19) :: newdate 

     REAL, PARAMETER :: CPHEAT=1004.5
     integer :: ix,jx
     integer :: kt, i,j, k1
     integer :: flag

     real   fup, aet, t14, chkff
     real albed  !, alb


     integer :: nsoil

     real :: SHDFAC,ALBBRD, REFMANN_out

!    
     real, dimension(NSOIL) :: ET

     real,dimension(ix,jx):: t2,u,v,q2x,prcp1, short, &
             xlong, snodep,pres, &
             weasd, etpx
     real, dimension(ix,jx) :: znt,snowc,etakin,qsfc, &
             soilhx, HX,ALBEDX
     real,dimension(ix,jx) ::   LH, &
            t1,cmc
     real,dimension(ix,jx):: &
          INFXSRT, SMCMAX1, SMCREF1, LKSAT, SOLDRAIN,SMCWLT1, &
          SFCHEADRT
     real,dimension(ix,jx):: latitude, longitude,so8ld_vmax

     real,dimension(ix,jx,NSOIL)::  stc,smc,sh2ox

     INTEGER,dimension(ix,jx,3)::  SO8LD_D 

#ifdef COUPLED
     real,dimension(ix,jx):: tbot_wrf,zlvl_wrf,z0_wrf
#endif


!    integer, dimension(ix,jx) :: soltyp 
     integer, dimension(ix,jx) ::  vegtyp,soltyp
     real,dimension(ix,jx):: GREENFRAC, ALBEDO12M
     integer :: TERADJ_SOLAR

!
!!! temp array 
     real, dimension(NSOIL):: sldpth, zsoil
     real, dimension(NSOIL):: stc1,smc1,sh2o
     real :: snofac
!    integer :: flag,i,j
     real,dimension(ix,jx) ::  fpar, lai, &
        sfcspdx
!    real, dimension(NSOIL):: ET    

! output
     real,dimension(ix,jx) ::  acrain
!REDEM

!DJG_DES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG_DES Insert additional call to READVEG to update GREENFRAC and ALBEDO12M
!DJG_DES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!    print *, "DJG_DES Made it here...." 
#ifdef MPP_LAND
   flag = -1
   if(my_id.eq.IO_id) then
!      read(nlst_lsm(did)%olddate(6:7),"(I2.2)") MM ! Day of the month
!      read(nlst_lsm(did)%olddate(9:10),"(I2.2)") DD ! Day of the month
      if (nlst_lsm(did)%start_day.ne.DD_chk) flag = 99
   endif 
   call mpp_land_bcast_int1(flag) 


     if (flag .eq. 99) then   ! Get new GREENFRAC and ALBEDO12M values only if month changes...
       CALL MPP_READVEG_HRLDAS(inflnm,trim(nlst_lsm(did)%wrfsi_static_flnm),  &
               ix,jx, nlst_lsm(did)%OLDDATE, &
         VEGTYP, FPAR, LAI,nlst_lsm(did)%FORC_TYP,GREENFRAC, ALBEDO12M,MM,DD,  &
         global_nx,global_ny)
       DD_chk = nlst_lsm(did)%start_day
     end if

#else
     flag = -1
!     read(nlst_lsm(did)%olddate(6:7),"(I2.2)") MM ! Day of the month
!     read(nlst_lsm(did)%olddate(9:10),"(I2.2)") DD ! Day of the month
     if (nlst_lsm(did)%start_day.ne.DD_chk) flag = 99

     write (*,*) "GREENFRAC flag = ", flag
     
     if (flag .eq. 99) then   ! Get new GREENFRAC and ALBEDO12M values only if month changes...
       CALL READVEG_HRLDAS(inflnm,trim(nlst_lsm(did)%wrfsi_static_flnm), IX, JX, &
           nlst_lsm(did)%OLDDATE, VEGTYP, FPAR, LAI,  &
         nlst_lsm(did)%FORC_TYP,GREENFRAC, ALBEDO12M,MM,DD)
       DD_chk = nlst_lsm(did)%start_day
     end if
#endif

      write(6,*) "finish call readveg_hrldas"
      call flush(6)



  acrain = 0   

! Read the forcing data.

! For HRLDAS, we're assuming (for now) that each time period is in a 
! separate file.  So we can open a new one right now.

      write(6,*) "ix, jx, nsoil ", ix,jx, nsoil
    

      write(6,*) "before call read_seq_forcing"
      call flush(6)


#ifndef COUPLED
#ifdef MPP_LAND
   call mpp_read_forcing( &
#else
   call read_seq_forcing( &
#endif
        nlst_lsm(did)%indir,nlst_lsm(did)%range,nlst_lsm(did)%olddate, &
        nlst_lsm(did)%startdate,nlst_lsm(did)%hgrid,nlst_lsm(did)%DOY, &
        ix,jx,nlst_lsm(did)%forc_typ,nlst_lsm(did)%snow_assim,  &
        T2,q2x,u,v,pres,xlong,short,prcp1,&
        weasd,snodep,nlst_lsm(did)%dt,kt, & 
#ifdef MPP_LAND
        global_nx,global_ny, &
#endif
        igrid,acrain )

#endif
      write(6,*) "finish call read_seq_forcing"

      call flush(6)

!!!DJG Adjust incoming solar radiation based on sloping terrain and aspect...
     IF (TERADJ_SOLAR.EQ.1) THEN   ! Perform terrain adjustment of incoming solar
       write(6,*) "Calling TER_ADJ_SOL..."
       call flush(6)
       call TER_ADJ_SOL(IX,JX,SO8LD_D,SO8LD_Vmax,SHORT,LATITUDE,LONGITUDE, &
        nlst_lsm(did)%olddate,nlst_lsm(did)%DT)
      END IF



!------------------------------------------------------------------------
! Spatial Loop to Convert Gridded data to single point for 1-D SFLX call
!------------------------------------------------------------------------

!DJG debug
     write(6,*) "Beginning space loop..."

     call flush(6)
    

     JLOOP : DO J=1,JX ! max(JX-1,1)
        ILOOP : DO I=1, IX ! max(IX-1,1)
!          IF (BASIN_MSK(I,J).EQ.-9999) GOTO 967 !Generic check for basin msk...
           IF((VEGTYP(I,J).GT.0).AND.(VEGTYP(I,J).NE.16)) THEN
              VEGTYPX=VEGTYP(I,J)
              SOILTYP=SOLTYP(I,J)
              IF((SOILTYP.LE.0).OR.(SOILTYP.GE.19).OR.(SOILTYP.EQ.14)) THEN
                print *, i,j,SOILTYP,VEGTYPX
                STOP "PROBLEM with SOILS"
              ENDIF

              SFCTMP=T2(I,J)
              SFCSPD=SQRT(U(I,J)*U(I,J)+  &
                  V(I,J)*V(I,J))
              SFCPRS= PRES(I,J)
              Q2=Q2X(I,J)
              PRCP=PRCP1(I,J)
!DJG

              if (prcp < 0.0) prcp = 0.0 ! Just in case
              SOLDN=SHORT(I,J)
              LWDN=XLONG(I,J)

             !
             ! The following module variables are set in subroutine REDPRM.
             ! If you want other than the default values based on land/soil
             ! categories, reset them after the call to REDPRM.
             !
             !    -- SOIL PARAMETERS:
             !
             !           CSOIL
             !           BEXP
             !           DKSAT (DJG-  'VERTKSAT' in dsitrNoah driver)
             !           DWSAT
             !           F1
             !           PSISAT
             !           QUARTZ
             !           SMCDRY
             !           SMCMAX
             !           SMCREF
             !           SMCWLT
             !
             !    -- "UNIVERSAL" PARAMETERS:
             !
             !           ZBOT
             !           SALP
             !           SBETA
             !           REFDK
             !           FRZK
             !           FXEXP
             !           REFKDT
             !           PTU
             !           KDT
             !           CZIL
             !           REFMANN  !mannings coefficient  scaling factor
             !           SLOPE
             !           FRZFACT
             !           MA
             ! 
             !    -- VEGETATION PARAMETERS:
             !
             !           TOPT
             !           RGL
             !           RSMAX
             !           RSMIN
             !           HS
             !           XLAI
             !           CMCMAX
             !           CFACTR
             !           NROOT
             !           SNUP
             !           ALB
             !           Z0BRD
             !           SHDFAC
             !           SNOALB_NOAH
             !           EMISSI
             !           RTDIS
             !

!DJG         Temp hardwire specification of LKSAT
             CALL REDPRM (VEGTYPX,SOILTYP,SLOPETYP,SLDPTH,NSOIL,VERTKSAT)

!DJG_DES   Overwrite REDPRM pixel values of GREENFRAC and ALBEDO from time-varying grids input above
             SHDFAC = GREENFRAC(I,J)
             FPAR(I,J) = GREENFRAC(I,J)  !Should only be relevant for output....
             ALBBRD = ALBEDO12M(I,J)


!DJG Specify Lateral Sat. Hyd. Conductivity (Note: LKSAT gets scaled upon disaggregation...)
!experiment 10.1.2010             LKSAT(I,J) = VERTKSAT 
                LKSAT(I,J) = VERTKSAT * 1000.0    ! where 1000.0 is an 'anisotropy factor'


!              XLAI =LAI(I,J)
!DJG_DES              SHDFAC = FPAR(I,J)

!   scaling MAXI LAI by Veg. fraction

              XLAI = MAX(XLAI*SHDFAC, 0.01) 

!   snow albedo
              alb = albbrd !?
              albedo = alb !?
              SNOFAC=MIN(SNODEP(I,J)*5.0, 1.0)
              ALBED=ALBEDO+(1.0-FPAR(I,J))*SNOFAC*(0.55-ALBEDO)
!   Net downward radiation
!              F= SRAD*(1.0-ALBED)+LWDN
!   
              CH = 0.1
              CM = 0.1
!-- modify mannings to be adjustable by a global scalar (to help calibration of channel)
! 12/23/2010              MannN = MannN * REFMANN


#ifdef COUPLED
              Z0 = z0_wrf(i,j)
              ZLVL = ZLVL_WRF(i,j)
#else
              Z0 = Z0BRD
#endif
!     Calculate a saturation mix ratio
              CALL CALTMP(T1(I,J), SFCTMP, SFCPRS, ZLVL, Q2, TH2, T1V, TH2V, RHO)
              CALL CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)

!     Calculate the surface exchange coefficients CM, CH
              ! Intent (IN) :: ZLVL, Z0, T1V, TH2V, SFCSPD, CZIL
              ! Intent (INOUT) :: CM
              CALL SFCDIF_off (ZLVL,Z0,T1V,TH2V,SFCSPD,CZIL,CM,CH)                  

              IF (Q2 .LT. 0.0) THEN
!                print*,'Q2<0','I=',I, 'J=',J
                 Q2=.1E-5
              ENDIF
              IF (Q2 .GT. Q2SAT) THEN
                 ! print*, 'Q2 .GT. Q2SAT', 'I=',I, 'J=',J
                 ! print*,'Q2=',Q2,'Q2SAT=',Q2SAT
                 Q2=Q2SAT*0.99
              ENDIF


              CHKFF = CH * CPHEAT * RHO           


!DJG debug Insert check to insure that SMC <= 0.95*SMCMAX at first time step only...
! DJG then print output from initial cond...
!             if (K.eq.1) THEN
!               if (SMC(I,J).ge.SMCMAX) SMC(I,J)=0.95*SMCMAX
!             end if

#ifdef COUPLED
              TBOT = TBOT_WRF(i,j)
#endif
              STC1(1:NSOIL)=STC(I,J,1:NSOIL)
              SMC1(1:NSOIL)=SMC(I,J,1:NSOIL)
              SH2O(1:NSOIL)=SH2OX(I,J,1:NSOIL)
!             ZSOILX(I,J,1:NSOIL)=ZSOIL(1:NSOIL)
! *** diagnostics 
              SFCSPDX(I,J)=SFCSPD


!--- Other conversions 'History (State) Variables

              CMCX=CMC(I,J)
              T1X=T1(I,J)
              SNOWH=SNODEP(I,J)

!#ifdef    COUPLED
!              SNEQV = WEASD(I,J) * 0.001
!#else
!              SNEQV = WEASD(I,J)
!#endif
               SNEQV = WEASD(I,J)
!yw added for possible bug
              if(SNEQV .ne. 0 .and. SNOWH .eq. 0)then
                 write(6,*) "yyyywww warning: SNEQV=",SNEQV, &
			"SNOWH=",SNOWH
                 SNEQV = 0   
              endif
!yw
              shdmin = 0.0

              esnow = 0.0

!DJG   Convert two gridded routing vars to single pts
              SFHEAD1RT = SFCHEADRT(I,J)
              INFXS1RT =0.0


              CALL SFLX_RT (ICE,nlst_lsm(did)%DT,ZLVL,NSOIL,SLDPTH,                  &
                   LWDN,SOLDN,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,         &    !F
                   COSZ,PRCPRAIN, SOLARDIRECT,                      &    !F
                   TH2,Q2SAT,DQSDT2,                                &    !I
                   VEGTYPX,SHDMIN,SHDMAX,SFHEAD1RT,                 &    !I  
                   SHDFAC,ALBBRD,                                   &    !I DJG_DES time-var params
                   TBOT,Z0,                                         &    !S   
!DJG_DES possible error in ALBEDO...     CMCX,T1X,STC1,SMC1,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,&    !H  
                   CMCX,T1X,STC1,SMC1,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,&    !H  
!! ----------------------------------------------------------------------         
!! OUTPUTS, DIAGNOSTICS, PARAMETERS BELOW GENERALLY NOT NECESSARY WHEN            
!! COUPLED WITH E.G. A NWP MODEL (SUCH AS THE NOAA/NWS/NCEP MESOSCALE ETA         
!! MODEL).  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.            
!! ----------------------------------------------------------------------         
                   ETA,SHEAT, ETA_KINEMATIC,FDOWN,                  &    !O  
                   EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                   &    !O  
                   BETA,ETP,SSOIL,                                  &    !O  
                   FLX1,FLX2,FLX3,                                  &    !O  
                   SNOMLT,SNCOVR,                                   &    !O  
                   RUNOFF1,RUNOFF2,RUNOFF3,                         &    !O  
                   RC,PC,RCS,RCT,RCQ,RCSOIL,                        &    !O  
                   SOILW,SOILM,Q1,ETPND1,INFXS1RT)

!               write(6,*) "end SFLX_RT"
              call flush(6)

!---------------------------------------------------------------------
! Begin Converting Data back to grid from 1-d and make units conversions
!---------------------------------------------------------------------


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CONVERT ACTUAL EVAPOTRANSPIRATION VALUE FROM  KG M-2 S-1  TO
!     W M-2  (BY MULTIPLYING BY 2.501E+6) FOR USE IN ENERGY BALANCE.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              T14 = T1X * T1X * T1X * T1X
              AET = ETA
              FUP = 5.67E-8 * T14
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALCULATE RESIDUAL OF ALL SURFACE ENERGY BALANCE EQN TERMS.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              S = -S
!FC              RESX(I,J) = F - H - S - AET - FUP - FLX1 - FLX2 - FLX3
!KWM              SNOFLX=FLX3
!  Convert ETA and ETP from W M-2 to KG M-2 S-1

#ifdef COUPLED
! added by yw
       LH(i,j) = ETA
!      SOILW_yw(I,J)=SOILW
!      SOILM_yw(I,J)=SOILM*1000.
!end  yw
#endif

!KWM              ETA = ETA/2.501E+6

              ETP = ETP/2.501E+6  


!KWM Overwrite ETA for now
              ETA = ((EDIR+EC+ETT+ETPND1)/2.501E+6) + (ESNOW/2.83E6)

! Fill output variable arrays and doing output

              STC(I,J,1:NSOIL)=STC1(1:NSOIL)    ! Updated Soil Temperature
              SMC(I,J,1:NSOIL)=SMC1(1:NSOIL)    ! Updated Soil Moisture
              SH2OX(I,J,1:NSOIL)=SH2O(1:NSOIL)  ! Updated Soil Liquid Water
              SNODEP(I,J)=SNOWH                 ! Updated snow depth !KWM
              WEASD(I,J)=SNEQV      ! ( m of water )
              ETPX(I,J)=ETPX(I,J)+ETP * nlst_lsm(did)%dt  ! ( kg m{-2} s{-2} to mm)
              ETAKIN(I,J)=ETA_KINEMATIC * nlst_lsm(did)%dt
#ifdef COUPLED
              ZNT(I,J)=Z0
              SNOWC(I,J)=SNCOVR
              ETPX(I,J)=ETPX(I,J)+ETP  ! ( kg m{-2} s{-2} to mm)
              ETAKIN(I,J)=ETA_KINEMATIC
              QSFC(I,J)=Q1   ! added by yw for coupling with wrf
#endif
!             FX(I,J)=FDOWN
              T1(I,J)=T1X
              CMC(I,J)=CMCX
              SOILHX(I,J)=SSOIL
!             CHX(I,J)=CH

   lsm_diag(did)%PRCP_OUT(I,J)=PRCP*nlst_lsm(did)%dt      ! (mm/s to mm)
   lsm_diag(did)%ETPNDX(I,J)=lsm_diag(did)%ETPNDX(I,J)+(ETPND1/2.501E6)*nlst_lsm(did)%dt ! (W m{-2} to kg m{-2} s{-2} to mm)
   lsm_diag(did)%QFX(I,J) = (EDIR+EC+ETT+ETPND1) + ESNOW ! in W m{-2}
   lsm_diag(did)%fpar(i,j) = fpar(i,j)
   lsm_diag(did)%ESNOW2D(I,J)=lsm_diag(did)%ESNOW2D(I,J)+(ESNOW/2.83E6)*nlst_lsm(did)%dt
   lsm_diag(did)%DRIP2D(I,J)=lsm_diag(did)%DRIP2D(I,J)+DRIP*1.E3   ! convert m to mm
   lsm_diag(did)%DEWFALL(I,J)=lsm_diag(did)%DEWFALL(I,J)+DEW*1.E3  ! convert m to mm

   lsm_diag(did)%ACSNOM(I,J)=lsm_diag(did)%ACSNOM(I,J)+snomlt*1.E3
   lsm_diag(did)%ETAX(I,J)=lsm_diag(did)%ETAX(I,J)+ETA* nlst_lsm(did)%dt ! kg m{-2} s{-1} to mm liquid
   lsm_diag(did)%RUNOFF1X(I,J)=lsm_diag(did)%RUNOFF1X(I,J)+RUNOFF1* nlst_lsm(did)%dt*1.E3     ! ( mm/s to mm)
   lsm_diag(did)%RUNOFF2X(I,J)=lsm_diag(did)%RUNOFF2X(I,J)+RUNOFF2* nlst_lsm(did)%dt*1.E3     ! ( mm/s to mm)
   lsm_diag(did)%RUNOFF3X(I,J)=lsm_diag(did)%RUNOFF3X(I,J)+RUNOFF3* nlst_lsm(did)%dt*1.E3     ! ( mm/s to mm)
   lsm_diag(did)%ECX(I,J)=lsm_diag(did)%ECX(I,J)+(EC/2.501E6)*nlst_lsm(did)%dt
   lsm_diag(did)%EDIRX(I,J)=lsm_diag(did)%EDIRX(I,J)+(EDIR/2.501E6)* nlst_lsm(did)%dt ! (W m{-2} to kg m{-2} s{-2} to mm)
   lsm_diag(did)%ETTX(I,J)=lsm_diag(did)%ETTX(I,J)+(ETT/2.501E6)* nlst_lsm(did)%dt    ! (W m{-2} to kg m{-2} s{-2} to mm)
              HX(I,J)=SHEAT
               SMCMAX1(I,J)=SMCMAX
               SMCREF1(I,J)=SMCREF
               SMCWLT1(I,J)=SMCWLT
              ALBEDX(I,J)=ALBEDO
  lsm_diag(did)%ACRAIN(I,J)=lsm_diag(did)%ACRAIN(I,J)+PRCP* nlst_lsm(did)%dt      ! (mm/s to mm)
  lsm_diag(did)%SOILMX = -1.0 * SMC (:,:,1)* ZSOIL (1)
  DO K1 = 2,NSOIL
     lsm_diag(did)%SOILMX = lsm_diag(did)%SOILMX + SMC(:,:,K1) * (ZSOIL (K1-1) - ZSOIL (K1))
  ENDDO
  lsm_diag(did)%SOILMX = lsm_diag(did)%SOILMX * 1.E3 ! Convert from m to mm


!DJG Convert from point to grid (units of SFHEAD & INFXS (mm))
              INFXSRT(I,J)=INFXS1RT     ! Use this as default
              SOLDRAIN(I,J)=RUNOFF2* nlst_lsm(did)%dt*1.E3

! ***  endif of the land-point
           ENDIF

967     CONTINUE   ! (For COWS watershed mask)
        ENDDO ILOOP
     ENDDO JLOOP

#ifdef MPP_LAND
             call mpp_land_bcast_real1(REFMANN)
#endif
             REFMANN_out = REFMANN

!DJG debug

      write(6,*) "end space loop"
      call flush(6) 

    end  subroutine lsm_exe 


#ifdef MPP_LAND
      subroutine mpp_rtland_ini(ntime,did)

      use module_rtland
      use module_mpp_land
      use module_LSM_WRF_data
      use module_namelist, only: nlst_lsm

      implicit none
      integer ntime, did

      NSOIL = nlst_lsm(did)%NSOIL

  call mpp_land_bcast_int1(NSOIL)
 
!yw  call decompose_data_real(etpx,lsm(did)%etpx)

  if(my_id .ne. IO_id) then
     allocate(SMCRTCHK (NSOIL))
     allocate(SMCAGGRT (NSOIL))
     allocate(STCAGGRT (NSOIL))
     allocate(SH2OAGGRT(NSOIL))
     allocate(SLDPTH   (NSOIL))
  endif


  call mpp_land_bcast_rd(NSOIL,SMCRTCHK(1:NSOIL))
  call mpp_land_bcast_real(NSOIL,SMCAGGRT(1:NSOIL))
  call mpp_land_bcast_real(NSOIL,STCAGGRT(1:NSOIL))
  call mpp_land_bcast_real(NSOIL,SH2OAGGRT(1:NSOIL))

  call mpp_land_bcast_real(NSOIL,ZSOIL(1:NSOIL))
  call mpp_land_bcast_real(NSOIL,SLDPTH(1:NSOIL))

  call mpp_land_bcast_int1(ntime)

    call mpp_land_bcast_real1(alb)
    call mpp_land_bcast_real1(xlai)
    call mpp_land_bcast_real1(s)

    call mpp_land_bcast_real1(SMCMAX)
    call mpp_land_bcast_real1(smcwlt)
  call mpp_land_bcast_real1(SMCREF)

  write(6,*) "step 4"
  call flush(6)
!add temporary
    call mpp_land_bcast_int1(LUCATS)
    call mpp_land_bcast_real(NLUS,OV_ROUGH)
    call mpp_land_bcast_real(NLUS,ALBTBL)
    call mpp_land_bcast_real(NLUS,Z0TBL)
    call mpp_land_bcast_real(NLUS,SHDTBL)
    call mpp_land_bcast_int(NLUS,NROTBL)
    call mpp_land_bcast_real(NLUS,RSTBL)
    call mpp_land_bcast_real(NLUS,RGLTBL)
    call mpp_land_bcast_real(NLUS,HSTBL)
    call mpp_land_bcast_real(NLUS,SNUPTBL)
    call mpp_land_bcast_real(NLUS,LAITBL)
    call mpp_land_bcast_real(NLUS,MAXALB)
    call mpp_land_bcast_real(NLUS,EMITBL)
    call mpp_land_bcast_real1(TOPT_DATA)
    call mpp_land_bcast_real1(CMCMAX_DATA)
    call mpp_land_bcast_real1(CFACTR_DATA)
    call mpp_land_bcast_real1(RSMAX_DATA)
    call mpp_land_bcast_int1(BARE)
    call mpp_land_bcast_int1(SLCATS)
    call mpp_land_bcast_int1(SLPCATS)
    call mpp_land_bcast_int1(NSLTYPE)
    call mpp_land_bcast_real(NSLTYPE,BB)
    call mpp_land_bcast_real(NSLTYPE,DRYSMC)
    call mpp_land_bcast_real(NSLTYPE,F11)
    call mpp_land_bcast_real(NSLTYPE,MAXSMC)
    call mpp_land_bcast_real(NSLTYPE,REFSMC)
    call mpp_land_bcast_real(NSLTYPE,SATPSI)
    call mpp_land_bcast_real(NSLTYPE,SATDK)
    call mpp_land_bcast_real(NSLTYPE,SATDW)
    call mpp_land_bcast_real(NSLTYPE,WLTSMC)
    call mpp_land_bcast_real(NSLTYPE,QTZ)
    call mpp_land_bcast_real(NSLOPE,SLOPE_DATA)
    call mpp_land_bcast_real1(SBETA_DATA)
    call mpp_land_bcast_real1(FXEXP_DATA)
    call mpp_land_bcast_real1(CSOIL_DATA)
    call mpp_land_bcast_real1(SALP_DATA)
    call mpp_land_bcast_real1(REFDK_DATA)
    call mpp_land_bcast_real1(REFKDT_DATA)
    call mpp_land_bcast_real1(REFMANN_DATA)
    call mpp_land_bcast_real1(FRZK_DATA)
    call mpp_land_bcast_real1(ZBOT_DATA)
    call mpp_land_bcast_real1(CZIL_DATA)
    call mpp_land_bcast_real1(SMLOW_DATA)
    call mpp_land_bcast_real1(SMHIGH_DATA)
 end subroutine mpp_rtland_ini
#endif

    subroutine nlst_2LAND (did)
!!!  pass the namelist to the rtland model
       use module_rtland
       use module_namelist, only: nlst_lsm
       implicit none
       integer did

       INDIR = nlst_lsm(did)%INDIR 
       FORC_TYP = nlst_lsm(did)%FORC_TYP 
       NSOIL = nlst_lsm(did)%NSOIL 
       ZSOIL8 = nlst_lsm(did)%ZSOIL8 
       DT = nlst_lsm(did)%DT 
       TBOT = nlst_lsm(did)%TBOT 
       START_YEAR = nlst_lsm(did)%START_YEAR 
       START_MONTH = nlst_lsm(did)%START_MONTH 
       START_DAY = nlst_lsm(did)%START_DAY 
       START_HOUR = nlst_lsm(did)%START_HOUR 
       START_MIN = nlst_lsm(did)%START_MIN 
       DOY = nlst_lsm(did)%DOY 
       RESTART_HDATE = nlst_lsm(did)%RESTART_HDATE 
       HISTORY_OUTPUT = nlst_lsm(did)%HISTORY_OUTPUT 
       SPLIT_OUTPUT_COUNT = nlst_lsm(did)%SPLIT_OUTPUT_COUNT 
       IGRID = nlst_lsm(did)%IGRID 
       RANGE = nlst_lsm(did)%RANGE 
       KHOUR = nlst_lsm(did)%KHOUR 
       KDAY = nlst_lsm(did)%KDAY
       ZLVL = nlst_lsm(did)%ZLVL 
       Z = nlst_lsm(did)%Z 
       wrfsi_static_flnm = nlst_lsm(did)%wrfsi_static_flnm 
       out_dt = nlst_lsm(did)%out_dt 
       rst_dt = nlst_lsm(did)%rst_dt 
       SLOPETYP = nlst_lsm(did)%SLOPETYP 
       SNOW_ASSIM = nlst_lsm(did)%SNOW_ASSIM 
       HIRES_OUT = nlst_lsm(did)%HIRES_OUT 
       DEEPGWSPIN = nlst_lsm(did)%DEEPGWSPIN 
       SOLVEG_INITSWC = nlst_lsm(did)%SOLVEG_INITSWC 

        
    end subroutine nlst_2LAND


     subroutine read_land_par(did,ix,jx)
       use module_rt_data, only: rt_domain
       use module_namelist, only: nlst_rt, nlst_lsm
       use module_LSM_wrf_data, only: lsm
       use module_rtland,only: readveg_hrldas, redprm
       use module_rtland, only: REFMANN ,SMCMAX , SMCREF, SMCWLT
#ifdef MPP_LAND
       use module_mpp_land
#endif  


        implicit none
        integer ix,jx
        integer i, j, did, flag, MM, DD, DD_chk
        real vertksat
        character(len=256) :: inflnm
        real,    dimension(ix,jx) :: fpar, lai
        save DD_chk 


!DJG_DES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG_DES Insert additional call to READVEG to update GREENFRAC and ALBEDO12M
!DJG_DES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef MPP_LAND
   flag = -1
   if(my_id.eq.IO_id) then
      read(nlst_lsm(did)%olddate(6:7),"(I2.2)") MM ! Day of the month
      read(nlst_lsm(did)%olddate(9:10),"(I2.2)") DD ! Day of the month
      if (DD.ne.DD_chk) flag = 99
   endif 
   call mpp_land_bcast_int1(flag) 


     if (flag .eq. 99) then   ! Get new GREENFRAC and ALBEDO12M values only if month changes...
       CALL MPP_READVEG_HRLDAS(inflnm,trim(nlst_lsm(did)%wrfsi_static_flnm),  &
               ix,jx, nlst_lsm(did)%OLDDATE, &
         RT_DOMAIN(did)%VEGTYP, FPAR, LAI,nlst_lsm(did)%FORC_TYP,&
               lsm(did)%GREENFRAC, lsm(did)%ALBEDO12M,MM,DD,  &
         global_nx,global_ny)

       DD_chk =DD 
     end if

#else
     flag = -1
     read(nlst_lsm(did)%olddate(6:7),"(I2.2)") MM ! Day of the month
     read(nlst_lsm(did)%olddate(9:10),"(I2.2)") DD ! Day of the month
     if (DD.ne.DD_chk) flag = 99

     write (*,*) "GREENFRAC flag = ", flag
     
     if (flag .eq. 99) then   ! Get new GREENFRAC and ALBEDO12M values only if month changes...
       CALL READVEG_HRLDAS(inflnm,trim(nlst_lsm(did)%wrfsi_static_flnm), IX, JX, &
           nlst_lsm(did)%OLDDATE, RT_DOMAIN(did)%VEGTYP, FPAR, LAI,  &
         nlst_lsm(did)%FORC_TYP,lsm(did)%GREENFRAC, lsm(did)%ALBEDO12M,MM,DD)
       DD_chk = DD
     end if
#endif
!              rt_domain(did)%SMCMAX1=SMCMAX
!              rt_domain(did)%SMCREF1=SMCREF
!              rt_domain(did)%SMCWLT1=SMCWLT

        do j = 1, jx
           do i = 1, ix
             IF((RT_DOMAIN(did)%VEGTYP(I,J).GT.0).AND.(RT_DOMAIN(did)%VEGTYP(I,J).NE.16)) THEN
                CALL REDPRM (RT_DOMAIN(did)%VEGTYP(i,j),lsm(did)%SOLTYP(i,j),nlst_lsm(did)%SLOPETYP,&
                  rt_domain(did)%SLDPTH,nlst_lsm(did)%NSOIL,VERTKSAT )
                  rt_domain(did)%LKSAT(I,J) = VERTKSAT * 1000.0    ! where 1000.0 is an 'anisotropy factor'

               IF(RT_DOMAIN(did)%VEGTYP(i,j)==1)THEN
                  SMCMAX = 0.45
                  SMCREF = 0.42
                  SMCWLT = 0.40
               ENDIF
               rt_domain(did)%SMCMAX1(i,j)=SMCMAX
               rt_domain(did)%SMCREF1(i,j)=SMCREF
               rt_domain(did)%SMCWLT1(i,j)=SMCWLT
             endif
           end do
        end do
#ifdef MPP_LAND
        call mpp_land_bcast_real1(REFMANN)
#endif
        rt_domain(did)%REFMANN = REFMANN

     end subroutine read_land_par
  

#ifdef MPP_LAND
       subroutine MPP_READVEG_HRLDAS(flnm,flnm2, ix, jx, OLDDATE, vegtyp, fpar, &
              lai,forctyp,greenfrac, albedo12m,mm,dd,global_nx,global_ny)

          USE module_sf_Noahlsm_wrf_input_rt, only:READVEG_HRLDAS
          use module_mpp_land
       implicit none
#include <netcdf.inc>
       integer, intent(in)  :: global_nx,global_ny
       character(len=*),                intent(in)  :: flnm,flnm2,OLDDATE
       integer,                         intent(in)  :: ix
       integer,                         intent(in)  :: jx
       integer,                         intent(in)  :: forctyp
       integer,                         intent(in)  :: mm
       integer,                         intent(in)  :: dd 
       integer, dimension(ix,jx),       intent(in)  :: vegtyp
       real,    dimension(ix,jx),       intent(out) :: fpar
       real,    dimension(ix,jx),       intent(out) :: lai
       real,    dimension(ix,jx),       intent(out) :: greenfrac
       real,    dimension(ix,jx),       intent(out) :: albedo12m

       integer, dimension(global_nx,global_ny) :: g_vegtyp
       real,    dimension(global_nx,global_ny) :: g_fpar,g_lai
       real,    dimension(global_nx,global_ny) :: g_greenfrac,g_albedo12m
   
       integer :: ierr_fpar
       integer :: ierr_lai
       integer :: ierr_greenfrac
       integer :: ierr_albedo12m

       call write_IO_int(VEGTYP,g_VEGTYP)
       call write_IO_real(FPAR,g_FPAR)
       call write_IO_real(LAI,g_LAI)
       call write_IO_real(GREENFRAC,g_GREENFRAC)
       call write_IO_real(ALBEDO12M,g_ALBEDO12M)

       if(my_id.eq.IO_id) then
         CALL READVEG_HRLDAS(trim(flnm),trim(flnm2),global_nx,global_ny, OLDDATE, g_VEGTYP, &
              g_FPAR, g_LAI,FORCTYP,g_GREENFRAC,g_ALBEDO12M,mm,dd)
       endif

       call decompose_data_real(g_FPAR,FPAR)
       call decompose_data_real(g_LAI,LAI)
       call decompose_data_real(g_GREENFRAC,GREENFRAC)
       call decompose_data_real(g_ALBEDO12M,ALBEDO12M)


       return 
       end SUBROUTINE MPP_READVEG_HRLDAS
#endif

      subroutine land_2rt_ini(did)
        use module_rtland, only: SMC,STC,SH2OX,SFCHEADRT, &
                INFXSRT,SMCMAX1,SMCREF1,LKSAT,SMCWLT1,& 
                SOLDRAIN, DSMC, VEGTYP, basin_msk, &
                OV_ROUGH,SMCRTCHK,SMCAGGRT,STCAGGRT,SH2OAGGRT,&
                ZSOIL
        use module_rtland, only: &
            SO8LD_D, SO8LD_Vmax, latitude, longitude, sldpth, &
            cmc,snodep,weasd,t1,ALBEDX, GREENFRAC,ALBEDO12M, &
            soltyp, REFMANN, TERRAIN

        use module_rt_data, only: rt_domain
        use module_LSM_wrf_data , only: lsm 
        use module_namelist, only: nlst_rt, nlst_lsm
#ifdef MPP_LAND    
        USE module_mpp_land
#endif
        implicit none
        integer :: did, i,j, nsoil, k
        real:: l3moist, l4moist,l3temp,l4temp

        nsoil = nlst_lsm(did)%nsoil

#ifdef MPP_LAND
        call decompose_data_real(GREENFRAC,lsm(did)%GREENFRAC)
        call decompose_data_real(ALBEDO12M,lsm(did)%ALBEDO12M)


        call decompose_data_int(soltyp,lsm(did)%soltyp)
        call decompose_data_real(TERRAIN,RT_DOMAIN(did)%TERRAIN)


        call decompose_data_real(latitude,rt_domain(did)%lat_lsm)
        call decompose_data_real(longitude,rt_domain(did)%lon_lsm)


        call decompose_data_real(LKSAT,RT_DOMAIN(did)%LKSAT)


        call decompose_data_real(SOLDRAIN,RT_DOMAIN(did)%SOLDRAIN)


        call decompose_data_int(VEGTYP,RT_DOMAIN(did)%VEGTYP)
        call decompose_data_int(BASIN_MSK,lsm(did)%BASIN_MSK)

#else
        lsm(did)%GREENFRAC = GREENFRAC
        lsm(did)%ALBEDO12M = ALBEDO12M
        lsm(did)%soltyp = soltyp
        lsm(did)%BASIN_MSK = BASIN_MSK
        RT_DOMAIN(did)%TERRAIN = TERRAIN
        rt_domain(did)%lat_lsm = latitude
        rt_domain(did)%lon_lsm = longitude
        RT_DOMAIN(did)%VEGTYP = VEGTYP
        RT_DOMAIN(did)%LKSAT = LKSAT
        RT_DOMAIN(did)%SOLDRAIN = SOLDRAIN
#endif



!!! for one dimentional array
       RT_DOMAIN(did)%sldpth  = sldpth
       RT_DOMAIN(did)%OV_ROUGH = OV_ROUGH
       RT_DOMAIN(did)%SMCRTCHK = SMCRTCHK
       RT_DOMAIN(did)%SMCAGGRT = SMCAGGRT
       RT_DOMAIN(did)%STCAGGRT = STCAGGRT
       RT_DOMAIN(did)%SH2OAGGRT = SH2OAGGRT
       RT_DOMAIN(did)%ZSOIL(1:NSOIL) = ZSOIL(1:NSOIL)
       SLDPTH(1:1) = -ZSOIL(1:1)
       do i = 2, nsoil
          RT_DOMAIN(did)%sldpth(i) = RT_DOMAIN(did)%zsoil(i-1)-RT_DOMAIN(did)%zsoil(i)
       enddo
     
       RT_DOMAIN(did)%SOLDEPRT = -1.0*RT_DOMAIN(did)%ZSOIL(NSOIL)


! initialize some data
#ifndef COUPLED
        DO J=1,RT_DOMAIN(did)%JX
          DO I=1,RT_DOMAIN(did)%IX
            l3moist=RT_DOMAIN(did)%SMC(I,J,3)
            l4moist=RT_DOMAIN(did)%SMC(I,J,4)

            do k = 5, nsoil
               RT_DOMAIN(did)%SMC(I,J,k)=l4moist
            end do

            RT_DOMAIN(did)%SMC(I,J,4)=0.5*(l3moist+l4moist)
            RT_DOMAIN(did)%SMC(I,J,3)=0.5*(l3moist+RT_DOMAIN(did)%SMC(I,J,2))
            l3moist=RT_DOMAIN(did)%SH2OX(I,J,3)
            l4moist=RT_DOMAIN(did)%SH2OX(I,J,4)

            do k = 5, nsoil
              RT_DOMAIN(did)%SH2OX(I,J,k)=l4moist
            end do

            RT_DOMAIN(did)%SH2OX(I,J,4)=0.5*(l3moist+l4moist)
            RT_DOMAIN(did)%SH2OX(I,J,3)=0.5*(l3moist+RT_DOMAIN(did)%SH2OX(I,J,2))
            l3temp=RT_DOMAIN(did)%STC(I,J,3)
            l4temp=RT_DOMAIN(did)%STC(I,J,4)

            RT_DOMAIN(did)%STC(I,J,nsoil)=0.5*(nlst_lsm(did)%TBOT+l4temp)
            do k = 1, nsoil-1
            RT_DOMAIN(did)%STC(I,J,k)=l4temp
            end do

            RT_DOMAIN(did)%STC(I,J,4)=0.5*(l3temp+l4temp)
            RT_DOMAIN(did)%STC(I,J,3)=0.5*(l3temp+RT_DOMAIN(did)%STC(I,J,2))
          ENDDO
        ENDDO
#endif
      end subroutine land_2rt_ini

    subroutine drive_noahLSF(did,kt)

       use module_routing
       use module_rt_data, only: rt_domain
       use module_LSM_wrf_data, only: lsm
       use module_namelist, only: nlst_rt, nlst_lsm
       use module_diag_field, only: lsm_diag

      implicit none
      integer did , kt
    
! note: 
! SFCHEADRT : input variable only ?
! t_soisno -> stc : (soil temperature)
! dz -> SLDPTH : thickness of each layer
! watsat ->  smc  : volumetric soil water at saturation (porosity)
! h2osoi_vol -> sh2ox  : Unfrozen soil moisture content
! INFXSRT : output from SFLX
! SMCMAX1  : output not used. (1-d)
! SMCREF1  : output not used.  (1-d)
! LKSAT    : from REDPRM
! SOLDRAIN : output from RUNOFF2
! SMCWLT1  : output not used
!  

      call lsm_exe( did, &
          kt, &
          rt_domain(did)%lat_lsm,rt_domain(did)%lon_lsm,&
          RT_DOMAIN(did)%SO8LD_D,RT_DOMAIN(did)%SO8LD_Vmax,&

          RT_DOMAIN(did)%SLDPTH,  &
          RT_DOMAIN(did)%SFCHEADRT, &
          RT_DOMAIN(did)%stc,RT_DOMAIN(did)%smc,      &
          RT_DOMAIN(did)%sh2ox,   &
          RT_DOMAIN(did)%INFXSRT, RT_DOMAIN(did)%SMCMAX1, &
          RT_DOMAIN(did)%SMCREF1, RT_DOMAIN(did)%LKSAT, &
          RT_DOMAIN(did)%SOLDRAIN,RT_DOMAIN(did)%SMCWLT1, &

          RT_DOMAIN(did)%VEGTYP , nlst_rt(did)%TERADJ_SOLAR,&
          RT_DOMAIN(did)%zsoil, RT_DOMAIN(did)%REFMANN,&
           
          lsm(did)%ix,lsm(did)%jx,nlst_lsm(did)%NSOIL, &
          lsm(did)%T2,lsm(did)%q2x,lsm(did)%u,lsm(did)%v,&
          lsm(did)%pres,lsm(did)%xlong,& 
          lsm(did)%SHORT, lsm(did)%prcp1, lsm(did)%weasd, &
          lsm(did)%snodep,lsm(did)%t1,lsm(did)%cmc,& 
          lsm(did)%znt, lsm(did)%snowc,lsm(did)%LH,       &
          lsm(did)%HX,lsm(did)%EtAKIN,&
          lsm(did)%QSFC,lsm(did)%SOILHX,lsm(did)%ALBEDX, &
          lsm(did)%soltyp,lsm(did)%GREENFRAC,lsm(did)%ALBEDO12M, &
          lsm(did)%ETPX,  &

          lsm_diag(did)%ACRAIN &

#ifdef COUPLED
          ,lsm(did)%tbot_wrf,lsm(did)%zlvl_wrf,lsm(did)%z0_wrf &
#endif
          )

    end subroutine drive_noahLSF

