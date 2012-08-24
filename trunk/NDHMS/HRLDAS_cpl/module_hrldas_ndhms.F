module module_HRLDAS_NDHMS

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
    integer :: nn



CONTAINS

    subroutine hrldas_cpl_ndhms(STC,SMC,SH2OX,infxsrt,sfcheadrt,ii,jj,kk)

       use module_rt_data, only:  rt_domain
       use module_mpp_land
       use module_namelist, only: nlst_lsm, nlst_rt

       implicit none

       integer ii,jj,kk
       integer k
       real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
       real,dimension(ii,jj) ::infxsrt,sfcheadrt

        integer ::  did

        integer ntime

        integer :: i,j
        integer clm_lev

!output flux and state variable

        did = 1


! kstart = 2 for continue run.
        kstart = 2

        if(.not. RT_DOMAIN(did)%initialized) then
            write(6,*) "yyyywww step 4 before NDHMS_ini= "
            kstart = 1
            call NDHMS_ini(ntime,did, kstart)
            RT_DOMAIN(did)%initialized = .true.
        endif


! decompose the hrldas 1-d data into routing domain
        do k = 1, kk
           call decompose_data_real(STC(:,:,k),RT_DOMAIN(did)%STC(:,:,k))
           call decompose_data_real(SMC(:,:,k),RT_DOMAIN(did)%SMC(:,:,k))
           call decompose_data_real(SH2OX(:,:,k),RT_DOMAIN(did)%SH2OX(:,:,k))
        end do
        call decompose_data_real(infxsrt,RT_DOMAIN(did)%infxsrt)

        ntime = 1
        call NDHMS_exe(did,ntime,kstart)

! add for update the HRLDAS state variable.
        do k = 1, kk
           call write_io_real(rt_domain(did)%STC(:,:,k),STC(:,:,k))
           call write_io_real(rt_domain(did)%SMC(:,:,k),SMC(:,:,k))
           call write_io_real(rt_domain(did)%SH2OX(:,:,k),SH2OX(:,:,k))
        end do
           call write_io_real(rt_domain(did)%sfcheadrt,sfcheadrt)

!? not sure for the following
!           grid%xice(its:ite,jts:jte) = rt_domain(did)%sice

     return 
     end subroutine hrldas_cpl_ndhms
end module module_HRLDAS_ndhms
