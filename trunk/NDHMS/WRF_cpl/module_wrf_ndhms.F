module module_WRF_NDHMS

! NDHMS  module   
    use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id

    implicit none
    integer begg, endg
    integer :: numg, numl, numc, nump
    INTEGER, PARAMETER :: double=8
    real(kind=double), pointer :: r2p(:,:) , r1p(:)
    integer :: kstart
     
    integer ::  begl, endl, begc, endc, begp, endp

    real, allocatable, dimension(:,:) :: vg_test



CONTAINS

!wrf_cpl_ndhms will not call the off-line lsm 
    subroutine wrf_cpl_ndhms(grid,its,ite,jts,jte)

       use module_rt_data, only:  rt_domain
       use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
       use module_mpp_land
       use module_namelist, only: nlst_lsm, nlst_rt

       USE module_domain, ONLY : domain, domain_clock_get
       implicit none
       TYPE ( domain ), INTENT(INOUT) :: grid
       integer its, ite, jts, jte, ij


        integer k, ix,jx, nn

        integer ::  did

        integer ntime
        real cpl_land_dt

        integer :: i,j
        integer clm_lev

!output flux and state variable

        did = 1
        cpl_land_dt = grid%dt

        write(6,*) "cpl_land_dt = ",cpl_land_dt

! kstart = 2 for continue run.
        kstart = 2
        ntime = 1

!        do ij = 1, grid%num_tiles
!            its = grid%i_start(ij)
!            ite = min(grid%i_end(ij), ide-1)
!            jts =  grid%j_start(ij)
!            jte = min(grid%j_end(ij), jde)
             write(6,*) "its,ite,jts,jte ",its,ite,jts,jte
!         end do
    


        if(.not. RT_DOMAIN(did)%initialized) then

            write(6,*) "yyyywww step 4 before NDHMS_ini= "
            kstart = 1
! kstart = 1 for cold start. It will be 2 if we read the restart file from NDHMS_exe.
            call CPL_LAND_INIT(its,ite,jts,jte)

            call NDHMS_ini(ntime,did, kstart)

            RT_DOMAIN(did)%initialized = .true.
            nlst_lsm(did)%dt = cpl_land_dt

            CALL domain_clock_get( grid, current_timestr=cpl_outdate)
            nlst_lsm(did)%startdate(1:19) = cpl_outdate(1:19)
            nlst_lsm(did)%olddate(1:19) = cpl_outdate(1:19)
            write(6,*) "yyyywww step 4 finish NDHMS_ini= "
            call flush(6)
        endif


        ix = rt_domain(did)%ix
        jx = rt_domain(did)%jx

        ! get the data from WRF 
! need input for RT_DOMAIN(did)%SOLDRAIN(ix,jx)             
            call to8layer(grid%TSLB(its:ite,1:4,jts:jte),RT_DOMAIN(did)%STC,ix,jx)
            call to8layer(grid%smois(its:ite,1:4,jts:jte),RT_DOMAIN(did)%SMC,ix,jx)
            call to8layer(grid%sh2o(its:ite,1:4,jts:jte),RT_DOMAIN(did)%SH2OX,ix,jx)

! output variable for routing model.
            rt_domain(did)%infxsrt = grid%infxsrt(its:ite,jts:jte)
            rt_domain(did)%sfcheadrt = grid%sfcheadrt(its:ite,jts:jte)


            write(6,*) "before call NDHMS_exe" 
            call flush(6)     


            call NDHMS_exe(did,ntime,kstart)

! add for update the WRF state variable.
    call to4layer(RT_DOMAIN(did)%STC,grid%TSLB(its:ite,1:4,jts:jte),ix,jx,grid%ISLTYP(its:ite,jts:jte))
    call to4layer(RT_DOMAIN(did)%SMC,grid%SMOIS(its:ite,1:4,jts:jte),ix,jx,grid%ISLTYP(its:ite,jts:jte))
    call to4layer(RT_DOMAIN(did)%SH2OX,grid%SH2O(its:ite,1:4,jts:jte),ix,jx,grid%ISLTYP(its:ite,jts:jte))

! update WRF variable after running routing model.
            grid%sfcheadrt(its:ite,jts:jte) = rt_domain(did)%sfcheadrt
            grid%infxsrt(its:ite,jts:jte) = rt_domain(did)%infxsrt

     return 
     end subroutine wrf_cpl_ndhms

!wrf_lsm_ndhms will call the off-line lsm model


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


end module module_wrf_ndhms
