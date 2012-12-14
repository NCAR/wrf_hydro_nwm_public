module module_HRLDAS_HYDRO

! NDHMS  module   
#ifdef MPP_LAND
    use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id, mpp_land_bcast_real1, IO_id
#endif
    use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe, HYDRO_rst_out
    use module_rt_data, only:  rt_domain
    use module_namelist, only: nlst_rt

    implicit none
    integer begg, endg
    integer :: numg, numl, numc, nump
    INTEGER, PARAMETER :: double=8
    real(kind=double), pointer :: r2p(:,:) , r1p(:)
     
    integer ::  begl, endl, begc, endc, begp, endp

    real, allocatable, dimension(:,:) :: vg_test
    integer :: nn



CONTAINS

    subroutine hrldas_cpl_HYDRO(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)


       implicit none

       integer ii,jj,kk
       integer k
       real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
       real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain

        integer ::  did

        integer ntime

        integer :: i,j


!output flux and state variable

        did = 1



!        write(6,*) "nlst_rt(did)%CHANRTSWCRT nlst_rt(did)%SUBRTSWCRT  nlst_rt(did)%OVRTSWCRT =", &
!                 nlst_rt(did)%CHANRTSWCRT, nlst_rt(did)%SUBRTSWCRT,  nlst_rt(did)%OVRTSWCRT
!       call flush(6) 

        IF (nlst_rt(did)%GWBASESWCRT .eq. 0 &
               .and. nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .and. nlst_rt(did)%OVRTSWCRT .eq. 0 ) return



! decompose the hrldas 1-d data into routing domain
#ifdef MPP_LAND
        do k = 1, kk
           call decompose_data_real(STC(:,:,k),RT_DOMAIN(did)%STC(:,:,k))
           call decompose_data_real(SMC(:,:,k),RT_DOMAIN(did)%SMC(:,:,k))
           call decompose_data_real(SH2OX(:,:,k),RT_DOMAIN(did)%SH2OX(:,:,k))
        end do
        call decompose_data_real(infxsrt,RT_DOMAIN(did)%infxsrt)
        call decompose_data_real(soldrain,RT_DOMAIN(did)%soldrain)
#else
        RT_DOMAIN(did)%STC = STC
        RT_DOMAIN(did)%SMC = SMC
        RT_DOMAIN(did)%SH2OX = SH2OX
        RT_DOMAIN(did)%infxsrt = infxsrt
        RT_DOMAIN(did)%soldrain = soldrain
#endif
  

        ntime = 1
        call HYDRO_exe(did)

! add for update the HRLDAS state variable.
#ifdef MPP_LAND
        do k = 1, kk
           call write_io_real(rt_domain(did)%STC(:,:,k),STC(:,:,k))
           call write_io_real(rt_domain(did)%SMC(:,:,k),SMC(:,:,k))
           call write_io_real(rt_domain(did)%SH2OX(:,:,k),SH2OX(:,:,k))
        end do
         call write_io_real(rt_domain(did)%sfcheadrt,sfcheadrt)
#else
        STC = rt_domain(did)%STC
        SMC = rt_domain(did)%SMC
        SH2OX = rt_domain(did)%SH2OX
        sfcheadrt = rt_domain(did)%sfcheadrt
#endif

!? not sure for the following
!           grid%xice(its:ite,jts:jte) = rt_domain(did)%sice


     return 
     end subroutine hrldas_cpl_HYDRO

    subroutine hrldas_cpl_HYDRO_ini(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt,dt, olddate)


       implicit none

       integer ii,jj,kk
       integer k, kt
       real :: dt
       real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
       real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain
       character(len = *) :: olddate

        integer ::  did

        integer ntime

        integer :: i,j

!output flux and state variable

        did = 1

        if(my_id .eq. IO_id) write(6,*) "yyww before HYDRO ini"

        if(.not. RT_DOMAIN(did)%initialized) then  
           nlst_rt(did)%dt = dt
           nlst_rt(did)%olddate(1:19) = olddate(1:19)
           nlst_rt(did)%startdate(1:19) = olddate(1:19)

!           write(6,*) "yyyywww step 4 before HYDRO_ini= "
           
           call HYDRO_ini(ntime,did,ix0=1,jx0=1)

            if(nlst_rt(did)%sys_cpl .ne. 1) then
               write(6,*) "Error: sys_cpl should be 1."
               call hydro_stop()
            endif

           RT_DOMAIN(did)%initialized = .true.
   
           IF (nlst_rt(did)%GWBASESWCRT .eq. 0 &
               .and. nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .and. nlst_rt(did)%OVRTSWCRT .eq. 0 ) return

#ifdef MPP_LAND
           call mpp_land_bcast_real1(nlst_rt(did)%dt)
           call write_io_real(rt_domain(did)%sfcheadrt,sfcheadrt)
           call write_io_real(rt_domain(did)%infxsrt, infxsrt)
!          call write_io_real(rt_domain(did)%soldrain,soldrain)
           if(nlst_rt(did)%rst_typ .eq. 1) then
              do k = 1, kk
                 call write_io_real(rt_domain(did)%STC(:,:,k),STC(:,:,k))
                 call write_io_real(rt_domain(did)%SMC(:,:,k),SMC(:,:,k))
                 call write_io_real(rt_domain(did)%SH2OX(:,:,k),SH2OX(:,:,k))
              end do
           else
              if(nlst_rt(did)%sys_cpl .eq. 1) then
                 where( abs(STC) .gt. 500) stc = 282
                 where( abs(SMC) .gt. 500) SMC = 0.25
                 where( abs(SH2OX) .gt. 500) SH2OX = 0.25
              endif
           endif 
#else
           sfcheadrt = rt_domain(did)%sfcheadrt
           infxsrt = rt_domain(did)%infxsrt
!          soldrain = rt_domain(did)%soldrain
           if(nlst_rt(did)%rst_typ .eq. 1) then
              STC = rt_domain(did)%STC
              SMC = rt_domain(did)%SMC
              SH2OX = rt_domain(did)%SH2OX
           endif
#endif
        endif


        if(my_id .eq. IO_id) write(6,*) "yyww end HYDRO ini"
     return 
     end subroutine hrldas_cpl_HYDRO_ini
end module module_HRLDAS_HYDRO
