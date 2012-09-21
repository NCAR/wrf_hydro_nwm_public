module module_HRLDAS_NDHMS

! NDHMS  module   
#ifdef MPP_LAND
    use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id
#endif
    use module_ndhms_rt_drv, only: ndhms_ini, ndhms_exe, ndhms_rst_out
    use module_rt_data, only:  rt_domain
    use module_namelist, only: nlst_lsm, nlst_rt

    implicit none
    integer begg, endg
    integer :: numg, numl, numc, nump
    INTEGER, PARAMETER :: double=8
    real(kind=double), pointer :: r2p(:,:) , r1p(:)
     
    integer ::  begl, endl, begc, endc, begp, endp

    real, allocatable, dimension(:,:) :: vg_test
    integer :: nn



CONTAINS

    subroutine hrldas_cpl_ndhms(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)


       implicit none

       integer ii,jj,kk
       integer k
       real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
       real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain

        integer ::  did

        integer ntime

        integer :: i,j
        integer clm_lev

!output flux and state variable

        did = 1

        write(6,*) "nlst_rt(did)%CHANRTSWCRT nlst_rt(did)%SUBRTSWCRT  nlst_rt(did)%OVRTSWCRT =", &
                  nlst_rt(did)%CHANRTSWCRT, nlst_rt(did)%SUBRTSWCRT,  nlst_rt(did)%OVRTSWCRT
        call flush(6) 

        IF (nlst_rt(did)%CHANRTSWCRT.eq.0   &
               .or. nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .or. nlst_rt(did)%OVRTSWCRT .eq. 0 ) return


!       if(.not. RT_DOMAIN(did)%initialized) then
!           write(6,*) "yyyywww step 4 before NDHMS_ini= "
!           call NDHMS_ini(ntime,did)
!           RT_DOMAIN(did)%initialized = .true.
!       endif

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
        call NDHMS_exe(did,ntime)

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
     end subroutine hrldas_cpl_ndhms

    subroutine hrldas_cpl_ndhms_ini(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt)


       implicit none

       integer ii,jj,kk
       integer k, kt
       real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
       real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain

        integer ::  did

        integer ntime

        integer :: i,j
        integer clm_lev

!output flux and state variable

        did = 1

        if(.not. RT_DOMAIN(did)%initialized) then  

           write(6,*) "yyyywww step 4 before NDHMS_ini= "
           call NDHMS_ini(ntime,did)
           RT_DOMAIN(did)%initialized = .true.
   
           IF (nlst_rt(did)%CHANRTSWCRT.eq.0   &
               .or. nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .or. nlst_rt(did)%OVRTSWCRT .eq. 0 ) return

#ifdef MPP_LAND
           call write_io_real(rt_domain(did)%sfcheadrt,sfcheadrt)
           call write_io_real(rt_domain(did)%infxsrt, infxsrt)
           call write_io_real(rt_domain(did)%soldrain,soldrain)
           do k = 1, kk
              call write_io_real(rt_domain(did)%STC(:,:,k),STC(:,:,k))
              call write_io_real(rt_domain(did)%SMC(:,:,k),SMC(:,:,k))
              call write_io_real(rt_domain(did)%SH2OX(:,:,k),SH2OX(:,:,k))
           end do
#else
           sfcheadrt = rt_domain(did)%sfcheadrt
           infxsrt = rt_domain(did)%infxsrt
           soldrain = rt_domain(did)%soldrain
           STC = rt_domain(did)%STC
           SMC = rt_domain(did)%SMC
           SH2OX = rt_domain(did)%SH2OX
#endif
        endif

        call NDHMS_rst_out(did,kt)

     return 
     end subroutine hrldas_cpl_ndhms_ini
end module module_HRLDAS_ndhms
