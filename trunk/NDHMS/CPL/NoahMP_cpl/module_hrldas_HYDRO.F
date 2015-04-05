module module_HRLDAS_HYDRO

! NDHMS  module   
#ifdef MPP_LAND
    use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id, mpp_land_bcast_real1, IO_id, &
                 mpp_land_bcast_int1
#endif
    use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe, HYDRO_rst_out
    use module_rt_data, only:  rt_domain
    use module_namelist, only: nlst_rt
    use module_gw_gw2d_data, only: gw2d

    implicit none
    integer begg, endg
    integer :: numg, numl, numc, nump
    INTEGER, PARAMETER :: double=8
    real(kind=double), pointer :: r2p(:,:) , r1p(:)
     
    integer ::  begl, endl, begc, endc, begp, endp

    real, allocatable, dimension(:,:) :: vg_test
    integer :: nn
    integer :: open_unit_status



CONTAINS

    subroutine hrldas_cpl_HYDRO(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)


       implicit none

       integer ii,jj,kk
       integer k, gwsoilcpl
       real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
       real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain, qsgw
!lpr add 2014-06-24
#ifdef WRF_HYDRO_RAPID
#ifdef MPP_LAND
        real, dimension(ii,jj) :: g_runoff
#endif
        real, dimension(ii,jj) :: runoff
#endif
!lpr add end
        integer ::  did

        integer ntime

        integer :: i,j

        real*8 :: t1, t2, dact
        save dact


!output flux and state variable

        did = 1

!--------LPR add 2014-06-24---------------------------
!    it is rapid model
#ifdef WRF_HYDRO_RAPID
        if(nlst_rt(did)%channel_option .eq. 4) then
                runoff  = infxsrt + soldrain
                call hrldas_RAPID_ini(ntime)    
#ifdef MPP_LAND
                call write_io_real(runoff,g_runoff)
                if(my_id.eq.io_id) &
                call hrldas_RAPID_exe(runoff,g_global_nx,global_ny)     
#else
                call hrldas_RAPID_exe(runoff,ii,jj)     
#endif
                sfcheadrt = 0.0
                return
        endif
#endif
!--------LPR add end----------------------------------




!        write(6,*) "nlst_rt(did)%CHANRTSWCRT nlst_rt(did)%SUBRTSWCRT  nlst_rt(did)%OVRTSWCRT =", &
!                 nlst_rt(did)%CHANRTSWCRT, nlst_rt(did)%SUBRTSWCRT,  nlst_rt(did)%OVRTSWCRT

        IF (nlst_rt(did)%GWBASESWCRT .eq. 0 &
               .and. nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .and. nlst_rt(did)%OVRTSWCRT .eq. 0 ) return



! decompose the hrldas 1-d data into routing domain
        RT_DOMAIN(did)%STC = STC
        RT_DOMAIN(did)%SMC = SMC
        RT_DOMAIN(did)%SH2OX = SH2OX
        RT_DOMAIN(did)%infxsrt = infxsrt
        RT_DOMAIN(did)%soldrain = soldrain
        if(nlst_rt(did)%GWBASESWCRT == 3) gw2d(did)%qsgw = qsgw
  
#ifdef MPP_LAND
        if(my_id .eq. IO_id) then
           call  time_seconds(t1)
        endif
#endif

        ntime = 1

        call HYDRO_exe(did)

#ifdef MPP_LAND
        if(my_id .eq. IO_id) then
           call  time_seconds(t2)
           dact = dact + t2 - t1
           write(6,*) "accumulated time (s): ",dact
        endif
#endif

! add for update the HRLDAS state variable.
        STC = rt_domain(did)%STC
        SMC = rt_domain(did)%SMC
        SH2OX = rt_domain(did)%SH2OX
        sfcheadrt = rt_domain(did)%sfcheadrt
        if(nlst_rt(did)%GWBASESWCRT == 3)  qsgw = gw2d(did)%qsgw

!? not sure for the following
!           grid%xice(its:ite,jts:jte) = rt_domain(did)%sice


     end subroutine hrldas_cpl_HYDRO

    subroutine hrldas_cpl_HYDRO_ini(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt,dt, olddate,zsoil)


       implicit none

       integer ii,jj,kk
       integer k, kt
       real :: dt
       real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
       real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain
       real, dimension(kk) :: zsoil
       character(len = *) :: olddate

        integer ::  did

        integer ntime

        integer :: i,j

!output flux and state variable

        did = 1


        if(.not. RT_DOMAIN(did)%initialized) then  
           nlst_rt(did)%dt = dt
           nlst_rt(did)%olddate(1:19) = olddate(1:19)
           nlst_rt(did)%startdate(1:19) = olddate(1:19)

           nlst_rt(did)%nsoil = kk
#ifdef MPP_LAND
           call mpp_land_bcast_int1(nlst_rt(did)%nsoil)
#endif
           allocate(nlst_rt(did)%zsoil8(nlst_rt(did)%nsoil))
           nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil) = zsoil(1:nlst_rt(did)%nsoil)
           
           
           call HYDRO_ini(ntime,did,ix0=1,jx0=1)

            if(nlst_rt(did)%sys_cpl .ne. 1) then
               write(6,*) "Error: sys_cpl should be 1."
               call hydro_stop()
            endif

           RT_DOMAIN(did)%initialized = .true.

#ifdef WRF_HYDRO_RAPID
!--------LPR add 2014-06-24---------------------------
         if(nlst_rt(did)%channel_option .eq. 4) then
!  it  is rapid model.
             return
         endif
!--------LPR add 2014-06-24---------------------------
#endif
   
           IF (nlst_rt(did)%GWBASESWCRT .eq. 0 &
               .and. nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .and. nlst_rt(did)%OVRTSWCRT .eq. 0 ) return

#ifdef MPP_LAND
           call mpp_land_bcast_real1(nlst_rt(did)%dt)
#endif
           sfcheadrt = rt_domain(did)%sfcheadrt
           infxsrt = rt_domain(did)%infxsrt
           if(nlst_rt(did)%rst_typ .eq. 1) then
              STC = rt_domain(did)%STC
              SMC = rt_domain(did)%SMC
              SH2OX = rt_domain(did)%SH2OX
           else
              if(nlst_rt(did)%sys_cpl .eq. 1) then
                 where( abs(STC) .gt. 500) stc = 282
                 where( abs(SMC) .gt. 500) SMC = 0.25
                 where( abs(SH2OX) .gt. 500) SH2OX = 0.25
              endif
           endif
        endif

     end subroutine hrldas_cpl_HYDRO_ini

     subroutine open_print_mpp(iunit)
       implicit none
       integer iunit
       character(len=16) fileout

       if(open_unit_status == 999) return
       open_unit_status = 999 

#ifdef MPP_LAND  
       write(fileout,'(a11,i0.5)') "diag_hydro.",my_id    
#else
       write(fileout,'(a11,i0.5)') "diag_hydro.",0    
#endif
       open(iunit,file=fileout,form="formatted")
     endsubroutine open_print_mpp
end module module_HRLDAS_HYDRO
