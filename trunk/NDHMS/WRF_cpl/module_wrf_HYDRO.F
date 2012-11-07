module module_WRF_HYDRO

    use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id, mpp_land_bcast_real1, IO_id, &
                mpp_land_bcast_real
    use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe

    use module_rt_data, only:  rt_domain
    use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
    use module_namelist, only: nlst_rt
    USE module_domain, ONLY : domain, domain_clock_get

    implicit none
     




CONTAINS

!wrf_cpl_HYDRO will not call the off-line lsm 
    subroutine wrf_cpl_HYDRO(HYDRO_dt,grid,its,ite,jts,jte)

       implicit none
       TYPE ( domain ), INTENT(INOUT) :: grid
       integer its, ite, jts, jte, ij
       real :: HYDRO_dt


        integer k, ix,jx, mm, nn

        integer ::  did

        integer ntime

        integer :: i,j

!output flux and state variable

        did = 1
        ix = ite - its + 1
        jx = jte - jts + 1


        ntime = 1

!        do ij = 1, grid%num_tiles
!            its = grid%i_start(ij)
!            ite = min(grid%i_end(ij), ide-1)
!            jts =  grid%j_start(ij)
!            jte = min(grid%j_end(ij), jde)
!            write(6,*) "its,ite,jts,jte ",its,ite,jts,jte
!         end do
    
            nlst_rt(did)%dt = HYDRO_dt

  
        if(.not. RT_DOMAIN(did)%initialized) then

            write(6,*) "yyyywww step 4 before HYDRO_ini= "


            CALL domain_clock_get( grid, current_timestr=cpl_outdate)
            nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
            nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

            call CPL_LAND_INIT(its,ite,jts,jte)
            call HYDRO_ini(ntime,did,ix0=ix,jx0=jx,vegtyp=grid%IVGTYP(its:ite,jts:jte),soltyp=grid%isltyp(its:ite,jts:jte))

!           call lsm_wrf_input(did,grid%IVGTYP(its:ite,jts:jte),grid%isltyp(its:ite,jts:jte),ite-its+1,jte-jts+1)

            nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
            nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

        endif
            nlst_rt(did)%dt = HYDRO_dt
            write(6,*) "HYDRO_dt = ", HYDRO_dt
            mm = HYDRO_dt/nlst_rt(did)%dtrt
            if(mm*nlst_rt(did)%dtrt .lt. HYDRO_dt) nlst_rt(did)%dtrt = HYDRO_dt/mm

        if(nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .and. nlst_rt(did)%OVRTSWCRT .eq. 0 .and. nlst_rt(did)%GWBASESWCRT .eq. 0) return


!            CALL domain_clock_get( grid, current_timestr=cpl_outdate)
!            nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

        write(6,*) "yyww cpl_outdate = ", cpl_outdate(1:19)
        write(6,*) "yyww nlst_rt(did)%olddate(1:19)=", nlst_rt(did)%olddate(1:19)

        nn = nlst_rt(did)%nsoil

        ! get the data from WRF 

        if(RT_DOMAIN(did)%initialized .or. (nlst_rt(did)%rst_typ .eq. 0) ) then
!           call wrf2lsm (grid%zs(1:4),grid%TSLB(its:ite,1:4,jts:jte),4,abs(rt_domain(did)%ZSOIL(1:nn)),RT_DOMAIN(did)%STC,ix,jx,nn,RT_DOMAIN(did)%vegtyp)
!           call wrf2lsm (grid%zs(1:4),grid%smois(its:ite,1:4,jts:jte),4,abs(rt_domain(did)%ZSOIL(1:nn)),RT_DOMAIN(did)%SMC,ix,jx,nn,RT_DOMAIN(did)%vegtyp)
!           call wrf2lsm (grid%zs(1:4),grid%sh2o(its:ite,1:4,jts:jte),4,abs(rt_domain(did)%ZSOIL(1:nn)),RT_DOMAIN(did)%SH2OX,ix,jx,nn,RT_DOMAIN(did)%vegtyp)
            do k = 1, nlst_rt(did)%nsoil
                RT_DOMAIN(did)%STC(:,:,k) = grid%TSLB(its:ite,k,jts:jte) 
                RT_DOMAIN(did)%smc(:,:,k) = grid%smois(its:ite,k,jts:jte) 
                RT_DOMAIN(did)%sh2ox(:,:,k) = grid%sh2o(its:ite,k,jts:jte) 
            end do 
            rt_domain(did)%infxsrt = grid%infxsrt(its:ite,jts:jte)
            rt_domain(did)%soldrain = grid%soldrain(its:ite,jts:jte)
        endif  

! output variable for routing model.
!           where(RT_DOMAIN(did)%vegtyp .ne. 16)  rt_domain(did)%infxsrt = grid%infxsrt(its:ite,jts:jte)
!           where(RT_DOMAIN(did)%vegtyp .ne. 16) rt_domain(did)%soldrain = grid%soldrain(its:ite,jts:jte)

             

            write(6,*) "before call HYDRO_exe" 
            call flush(6)     


            call HYDRO_exe(did)

! add for update the WRF state variable.
!   call to4layer(RT_DOMAIN(did)%STC,grid%TSLB(its:ite,1:4,jts:jte),ix,jx,grid%ISLTYP(its:ite,jts:jte))
!   call to4layer(RT_DOMAIN(did)%SMC,grid%SMOIS(its:ite,1:4,jts:jte),ix,jx,grid%ISLTYP(its:ite,jts:jte))
!   call to4layer(RT_DOMAIN(did)%SH2OX,grid%SH2O(its:ite,1:4,jts:jte),ix,jx,grid%ISLTYP(its:ite,jts:jte))
!           call lsm2wrf (abs(rt_domain(did)%ZSOIL(1:nn)),RT_DOMAIN(did)%STC,nn,grid%zs(1:4),grid%TSLB(its:ite,1:4,jts:jte),ix,jx,4, RT_DOMAIN(did)%vegtyp)
!           call lsm2wrf (abs(rt_domain(did)%ZSOIL(1:nn)),RT_DOMAIN(did)%SMC,nn,grid%zs(1:4),grid%smois(its:ite,1:4,jts:jte),ix,jx,4, RT_DOMAIN(did)%vegtyp)
!           call lsm2wrf (abs(rt_domain(did)%ZSOIL(1:nn)),RT_DOMAIN(did)%SH2OX,nn,grid%zs(1:4),grid%sh2o(its:ite,1:4,jts:jte),ix,jx,4, RT_DOMAIN(did)%vegtyp)

            do k = 1, nlst_rt(did)%nsoil
                ! grid%TSLB(its:ite,k,jts:jte) = RT_DOMAIN(did)%STC(:,:,k)
                grid%smois(its:ite,k,jts:jte) = RT_DOMAIN(did)%smc(:,:,k)
                grid%sh2o(its:ite,k,jts:jte) = RT_DOMAIN(did)%sh2ox(:,:,k)
            end do 

! update WRF variable after running routing model.
!            where(RT_DOMAIN(did)%vegtyp .ne. 16) grid%sfcheadrt(its:ite,jts:jte) = rt_domain(did)%sfcheadrt
            grid%sfcheadrt(its:ite,jts:jte) = rt_domain(did)%sfcheadrt

!? not sure for the following
!           grid%xice(its:ite,jts:jte) = rt_domain(did)%sice

            RT_DOMAIN(did)%initialized = .true.
     return 
     end subroutine wrf_cpl_HYDRO



     subroutine to4layer(in,out,ix,jx,soltyp)
! ZSOIL8(1) = -0.10
! ZSOIL8(2) = -0.40
! ZSOIL8(3) = -0.70
! ZSOIL8(4) = -1.00
! ZSOIL8(5) = -1.30
! ZSOIL8(6) = -1.60
! ZSOIL8(7) = -1.90
! ZSOIL8(8) = -2.00

! ZSOIL(1) = -0.10
! ZSOIL(2) = -0.40
! ZSOIL(3) = -1.00
! ZSOIL(4) = -2.00

     integer ix,jx
     real in(ix,jx,8),out(ix,4,jx) 
     integer i,j,k, soltyp(ix,jx)
     where(soltyp .ne. 14) out(:,1,:) = in(:,:,1)
     where(soltyp .ne. 14) out(:,2,:) = in(:,:,2)
     where(soltyp .ne. 14) out(:,3,:) = in(:,:,3)*0.25 + in(:,:,5)*0.25 + in(:,:,4)*0.5
     where(soltyp .ne. 14) out(:,4,:) = in(:,:,6)*0.1 + in(:,:,7)*0.4 + in(:,:,8)*0.5
     return
     end subroutine to4layer

     subroutine to8layer(in,out,ix,jx)
! ZSOIL8(1) = -0.10
! ZSOIL8(2) = -0.40
! ZSOIL8(3) = -0.70
! ZSOIL8(4) = -1.00
! ZSOIL8(5) = -1.30
! ZSOIL8(6) = -1.60
! ZSOIL8(7) = -1.90
! ZSOIL8(8) = -2.00

! ZSOIL(1) = -0.10
! ZSOIL(2) = -0.40
! ZSOIL(3) = -1.00
! ZSOIL(4) = -2.00

     integer ix,jx
     real out(ix,jx,8),in(ix,4,jx) 
     integer i,j,k, soltyp(ix,jx)
     out(:,:,1) = in(:,1,:)
     out(:,:,2) = in(:,2,:)
     out(:,:,3) = in(:,2,:)*0.5 + in(:,3,:)*0.5
     out(:,:,4) = in(:,3,:)
     out(:,:,5) = in(:,3,:)*0.7+ in(:,4,:)*0.3
     out(:,:,6) = in(:,3,:)*0.4+ in(:,4,:)*0.6
     out(:,:,7) = in(:,3,:)*0.1+ in(:,4,:)*0.9
     out(:,:,8) = in(:,4,:)
     return
     end subroutine to8layer

!program drive rtland
! this program is called by wrf model for coupling with the RTLAND
!
!    call drive_rtland(ite-its+1,jte-jts+1, dtbl& 
!          t_phy(its:ite,1,jts:jte), &
!          qv_curr(its:ite,1,jts:jte),         &
!          u_phy(its:ite,1,jts:jte),            &
!          v_phy(its:ite,1,jts:jte),            &
!          p8w(its:ite,1,jts:jte),         &
!          GLW(its:ite,jts:jte)*EMISS(its:ite,jts:jte), & !  XLONG
!          GSW(its:ite,jts:jte)/(1.0-albedo(its:ite,jts:jte)), & ! SHORT
!          rainbl(its:ite,jts:jte)/dtbl               , &    ! PRCP1
!          SNOW(its:ite,jts:jte)*0.001 ,         & ! WEASD
!          SNOWH(its:ite,jts:jte),               & ! SNODEP
!          tsk(its:ite,jts:jte), &
!          hfx(its:ite,jts:jte), &
!          qfx(its:ite,jts:jte), &
!          qsfc(its:ite,jts:jte), &
!          GRDFLX(its:ite,jts:jte), &
!          snowc(its:ite,jts:jte), &
!          canwat(its:ite,jts:jte), &
!          znt(its:ite,jts:jte) &
!added
!          LH(its:ite,jts:jte),&
!          CHS2(its:ite,jts:jte),&
!          CQS2(its:ite,jts:jte),&
!          XICE(its:ite,jts:jte),&
!          XLAND(its:ite,jts:jte), &
!          TSLB(its:ite,:,jts:jte), &
!          SMOIS(its:ite,:,jts:jte),&
!          SH2O(its:ite,:,jts:jte) &
!       )

      subroutine wrf2lsm (z1,v1,kk1,z,vout,ix,jx,kk,vegtyp)
!  input: z1,v1,kk1,z,ix,jx,kk
!  output: vout
!  interpolate based on soil layer: z1 and z 
!  z :  soil layer of output variable.
!  z1: array of soil layers of input variable.
         implicit none
         integer:: i,j,k
         integer:: kk1, ix,jx,kk, vegtyp(ix,jx)
         real :: z1(kk1), z(kk), v1(ix,kk1,jx),vout(ix,jx,kk)

       
         do j = 1, jx
            do i = 1, ix
!             if(vegtyp(i,j) .ne. 16)  then
                do k = 1, kk
                  call interpLayer(Z1,v1(i,1:kk1,j),kk1,Z(k),vout(i,j,k)) 
                end do
!             endif 
            end do
         end do
      end subroutine wrf2lsm

      subroutine lsm2wrf (z1,v1,kk1,z,vout,ix,jx,kk,vegtyp)
!  input: z1,v1,kk1,z,ix,jx,kk
!  output: vout
!  interpolate based on soil layer: z1 and z 
!  z :  soil layer of output variable.
!  z1: array of soil layers of input variable.
         implicit none
         integer:: i,j,k
         integer:: kk1, ix,jx,kk, vegtyp(ix,jx)
         real :: z1(kk1), z(kk), v1(ix,jx,kk1),vout(ix,kk,jx)

       
         do j = 1, jx
            do i = 1, ix
!             if(vegtyp(i,j) .ne. 16) then
                 do k = 1, kk
                    call interpLayer(Z1,v1(i,j,1:kk1),kk1,Z(k),vout(i,k,j)) 
                 end do
!             endif
            end do
         end do
      end subroutine lsm2wrf

      subroutine interpLayer(inZ,inV,inK,outZ,outV)
         implicit none
         integer:: k, k1, k2
         integer :: inK
         real:: inV(inK),inZ(inK)
         real:: outV, outZ, w1, w2

         if(outZ .le. inZ(1)) then
             if(inZ(2) .eq. inZ(1)) then 
                write(6,*) "Error: inZ(2)=inZ(1) ", inZ(2),inZ(1)
                call flush(6)
                stop 99
             end if
             w1 = (inZ(2)-outZ)/(inZ(2)-inZ(1))
             w2 = (inZ(1)-outZ)/(inZ(2)-inZ(1))
             outV = inV(1)*w1-inV(2)*w2
             return
         elseif(outZ .ge. inZ(inK)) then
             if(inZ(inK-1) .eq. inZ(inK)) then 
                write(6,*) "Error: inZ(inK-1)=inZ(inK) ", inZ(inK-1),inZ(inK)
                call flush(6)
                stop 99
             end if
             w1 = (outZ-inZ(inK-1))/(inZ(inK)-inZ(inK-1)) 
             w2 = (outZ-inZ(inK))  /(inZ(inK)-inZ(inK-1))
             outV = inV(inK)*w1 -inV(inK-1)* w2
             return
         else  
            do k = 2, inK
             if((inZ(k) .ge. outZ).and.(inZ(k-1) .le. outZ) ) then
                k1  = k-1
                k2 = k
             if(inZ(k1) .eq. inZ(k2)) then 
                write(6,*) "Error: inZ(k1)=inZ(k2) ", inZ(k1),inZ(k2)
                call flush(6)
                stop 99
             end if
                w1 = (outZ-inZ(k1))/(inZ(k2)-inZ(k1))
                w2 = (inZ(k2)-outZ)/(inZ(k2)-inZ(k1))
                outV = inV(k2)*w1 + inV(k1)*w2
                return 
             end if 
            end do
         endif
      end subroutine interpLayer

      subroutine lsm_wrf_input(did,vegtyp,soltyp,ix,jx)
         implicit none
         integer did, leng
         parameter(leng=100)
         integer :: i,j, nn, ix,jx
         integer, dimension(ix,jx) :: soltyp, vegtyp
         real, dimension(leng) :: xdum1, MAXSMC,refsmc,wltsmc


         where(soltyp == 14) VEGTYP = 16
         where(VEGTYP == 16 ) soltyp = 14

         RT_DOMAIN(did)%VEGTYP = vegtyp

!      input OV_ROUGH from OVROUGH.TBL
#ifdef MPP_LAND
       if(my_id .eq. IO_id) then
#endif

       open(71,file="HYDRO.TBL", form="formatted")
!read OV_ROUGH first
          read(71,*) nn
          read(71,*)
          do i = 1, nn
             read(71,*) RT_DOMAIN(did)%OV_ROUGH(i)
          end do
!read parameter for LKSAT
          read(71,*) nn
          read(71,*)
          do i = 1, nn
             read(71,*) xdum1(i), MAXSMC(i),refsmc(i),wltsmc(i)
          end do
       close(71)

#ifdef MPP_LAND
       endif
       call mpp_land_bcast_real(leng,RT_DOMAIN(did)%OV_ROUGH)
       call mpp_land_bcast_real(leng,xdum1)
       call mpp_land_bcast_real(leng,MAXSMC)
       call mpp_land_bcast_real(leng,refsmc)
       call mpp_land_bcast_real(leng,wltsmc)
#endif

       rt_domain(did)%lksat = 0.0
       do j = 1, RT_DOMAIN(did)%jx
             do i = 1, RT_DOMAIN(did)%ix
                rt_domain(did)%lksat(i,j) = xdum1(soltyp(i,j) ) * 1000.0
                IF(rt_domain(did)%VEGTYP(i,j) == 1 ) THEN   ! urban
                    rt_domain(did)%SMCMAX1(i,j) = 0.45
                    rt_domain(did)%SMCREF1(i,j) = 0.42
                    rt_domain(did)%SMCWLT1(i,j) = 0.40
                else
                    rt_domain(did)%SMCMAX1(i,j) = MAXSMC(soltyp(I,J))
                    rt_domain(did)%SMCREF1(i,j) = refsmc(soltyp(I,J))
                    rt_domain(did)%SMCWLT1(i,j) = wltsmc(soltyp(I,J))
                ENDIF
             end do
       end do


      end subroutine lsm_wrf_input

end module module_wrf_HYDRO
