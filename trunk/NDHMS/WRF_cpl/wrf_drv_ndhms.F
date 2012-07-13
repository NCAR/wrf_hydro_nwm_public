!2345678
       subroutine wrf_drv_ndhms(grid,its,ite,jts,jte)
          use module_wrf_ndhms, only: wrf_cpl_ndhms
          USE module_domain, ONLY : domain 
          implicit none
           integer:: its,ite,jts,jte
          TYPE ( domain ), INTENT(INOUT) :: grid
!         return

          if(grid%num_nests .lt. 1) then
             call wrf_cpl_ndhms(grid,its,ite,jts,jte)  ! do not run the off-line lsm
          endif
       end subroutine wrf_drv_ndhms

!       run the off-line lsm
     subroutine wrf_drv_lsm_ndhms(wrf_ix,wrf_jx,cpl_land_dt, & 
           wrf_T2, &
           wrf_Q2X, &
           wrf_U, &
           wrf_V, &
           wrf_PRES, &
           wrf_XLONG, &
           wrf_SHORT, &
           wrf_PRCP1, &
           wrf_WEASD, &
           wrf_SNODEP, &
           wrf_T1, &     ! begin output flux
           wrf_HX, &
           wrf_ETAKIN,&
           wrf_QSFC,&
           wrf_SOILHX,&
           wrf_SNOWC,&
           wrf_CMC,&
           wrf_ZNT,  &
           wrf_z0,  &
           wrf_emiss, &
           wrf_albedo,  &
!added
            LH,CHS2,CQS2,XICE,XLAND, &
            TSLB,SMOIS,wrf_SH2O,      &
            wrf_ZLVL,wrf_tmn, &
            its,ite,jts,jte )

       use module_rt_data, only:  rt_domain
       use module_lsm_wrf_data, only:  lsm 
       use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
       use module_mpp_land
       use module_namelist, only: nlst_lsm
       use module_wrf_ndhms, only: to4layer, to8layer

       USE module_domain, ONLY : domain_clock_get

        implicit none
 
        integer ::  did, its,ite,jts,jte
!        real t1max,t1min

        integer ntime, wrf_ix,wrf_jx
        real cpl_land_dt
        real,dimension(wrf_ix,wrf_jx):: &
           wrf_T2, &
           wrf_Q2X, &
           wrf_U, &
           wrf_V, &
           wrf_PRES, &
           wrf_XLONG, &
           wrf_SHORT, &
           wrf_PRCP1, &
           wrf_WEASD, &
           wrf_SNODEP, &
           wrf_emiss, &
           wrf_albedo, wrf_tmn

!output flux and state variable
        real,dimension(wrf_ix,wrf_jx):: &
           wrf_T1, &     
           wrf_HX, &
           wrf_ETAKIN,&
           wrf_QSFC,&
           wrf_SOILHX,&
           wrf_SNOWC,&
           wrf_CMC,&
           wrf_ZNT, wrf_ZLVL, wrf_z0

        real,dimension(wrf_ix,wrf_jx):: &
            LH,CHS2,CQS2,XICE,XLAND
        real,dimension(wrf_ix,4,wrf_jx):: &
            TSLB,SMOIS,wrf_SH2O
        real time1, time2

        integer :: i,j, kstart

        kstart = 2     
        did = 1

        if(.not. RT_DOMAIN(did)%initialized) then

            write(6,*) "yyyywww step 4 before NDHMS_ini= "
            kstart = 1
! kstart = 1 for cold start. It will be 2 if we read the restart file from NDHMS_exe.
            call CPL_LAND_INIT(its,ite,jts,jte)

            call NDHMS_ini(ntime,did, kstart)

            nlst_lsm(did)%dt = cpl_land_dt

            cpl_outdate(1:19) = nlst_lsm(did)%startdate(1:19) 
            write(6,*) "yyyywww step 4 finish NDHMS_ini= "
            call flush(6)

            call to8layer(SMOIS,RT_DOMAIN(did)%SMC,wrf_ix,wrf_jx)
            call to8layer(TSLB,RT_DOMAIN(did)%STC,wrf_ix,wrf_jx)
            call to8layer(wrf_SH2O,RT_DOMAIN(did)%SH2OX,wrf_ix,wrf_jx)

            lsm(did)%CMC = WRF_CMC
            lsm(did)%T1  = wrf_T1
            lsm(did)%WEASD = wrf_WEASD * 0.001
            lsm(did)%SNODEP = wrf_SNODEP

            RT_DOMAIN(did)%initialized = .true.
        endif





! pass forcing
      do i = 1, wrf_ix
        do j = 1, wrf_jx
           IF (XICE(I,J) .GT. 0.5) THEN
              lsm(did)%tbot_wrf(i,j)=271.16   ! it is ice
            ELSE
              lsm(did)%TBOT_wrf(i,j)=wrf_TMN(I,J)
            ENDIF
        enddo
      enddo

!DJG temp hardwire...
      lsm(did)%TBOT_wrf = 280.0

           lsm(did)%T2 = wrf_T2
           lsm(did)%Q2X = wrf_Q2X
           lsm(did)%U = wrf_U
           lsm(did)%V = wrf_V
           lsm(did)%PRES = wrf_PRES
           lsm(did)%XLONG = wrf_XLONG * wrf_EMISS
           lsm(did)%SHORT = wrf_SHORT /(1.0 - wrf_albedo)
           lsm(did)%PRCP1 = wrf_PRCP1 / cpl_land_dt

           lsm(did)%WEASD = wrf_WEASD * 0.001
!          lsm(did)%WEASD = wrf_WEASD 
           lsm(did)%SNODEP = wrf_SNODEP

           lsm(did)%zlvl_wrf = wrf_zlvl * 0.5 

           lsm(did)%z0_wrf = wrf_z0 
        
        ntime = 1  ! for coupling with wrf
        nlst_lsm(did)%dt = cpl_land_dt


        call NDHMS_exe(did,ntime,kstart)

! pass back flux and state variable



!       t1max = -1
!       t1min = 9999
!       do j = 1, wrf_jx
!          do i = 1, wrf_ix
!             if(t1(i,j) .gt. -9.9999999E+32 ) then
!                t1max = max(t1max,t1(i,j)) 
!                t1min = min(t1min,t1(i,j)) 
!             endif
!          end do
!       enddo

        where(lsm(did)%soltyp .ne. 14) wrf_T1 =  lsm(did)%T1       ! TSK
        where(lsm(did)%soltyp .ne. 14) wrf_HX =  lsm(did)%HX        ! HFX
        where(lsm(did)%soltyp .ne. 14) wrf_ETAKIN = lsm(did)%ETAKIN    ! QFX
        where(lsm(did)%soltyp .ne. 14) wrf_QSFC =  lsm(did)%QSFC   
        where(lsm(did)%soltyp .ne. 14) wrf_SOILHX =  lsm(did)%SOILHX   !GRDFLX
        where(lsm(did)%soltyp .ne. 14) wrf_WEASD =  lsm(did)%WEASD*1000     ! SNOW, & ! WEASD, & ?
!        where(lsm(did)%soltyp .ne. 14) wrf_WEASD =  lsm(did)%WEASD     ! SNOW, & ! WEASD, & ?
        where(lsm(did)%soltyp .ne. 14) wrf_SNOWC = lsm(did)%SNOWC 
        where(lsm(did)%soltyp .ne. 14) wrf_SNODEP =  lsm(did)%SNODEP    ! SNOWH
        where(lsm(did)%soltyp .ne. 14) wrf_CMC =  lsm(did)%CMC       ! CANWAT
        where(lsm(did)%soltyp .ne. 14) wrf_ZNT =  lsm(did)%ZNT 

!#ifdef COUPLED
!       time_step_read_rstart = .false.
!       time_step_write_rstart = .false.
!       time_step_output = .false. 
!#endif

! add for update the wrf state variable.
     ! 3 d variable
    call to4layer(RT_DOMAIN(did)%STC,TSLB,wrf_ix,wrf_jx,lsm(did)%soltyp)
    call to4layer(RT_DOMAIN(did)%SMC,SMOIS,wrf_ix,wrf_jx,lsm(did)%soltyp)
    call to4layer(RT_DOMAIN(did)%SH2OX,wrf_SH2O,wrf_ix,wrf_jx,lsm(did)%soltyp)


!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
     do j = 1, wrf_jx
        do i = 1,wrf_ix 
           IF((XLAND(I,J)-1.5).GE.0.)THEN
          !***   Open Water Case
!              SMSTAV(I,J)=1.0
!              SMSTOT(I,J)=1.0
               SMOIS(I,:,J)=1.0
               TSLB(I,:,J)=273.16                                          !STEMP
           else
             IF(XICE(I,J).EQ.1.)THEN
   !***        SEA-ICE CASE
!                SMSTAV(I,J)=1.0
!                SMSTOT(I,J)=1.0
                 SMOIS(I,:,J)=1.0
              endif
           endif
        enddo
     enddo

    where(lsm(did)%soltyp .ne. 14) LH = lsm(did)%LH
    where(lsm(did)%soltyp .ne. 14) wrf_ALBEDO = lsm(did)%ALBEDX 

!?   CHS2 and CQS2
    CHS2 = CQS2

      
     return 
     end subroutine wrf_drv_lsm_ndhms
