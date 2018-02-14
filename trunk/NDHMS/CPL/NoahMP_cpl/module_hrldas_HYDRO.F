!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!  <brief list of changes to this source file>
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

module module_HRLDAS_HYDRO

! NDHMS  module   
#ifdef MPP_LAND
    use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id, mpp_land_bcast_real1, IO_id, &
                 mpp_land_bcast_int1, mpp_land_sync
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

#ifdef WRF_HYDRO_RAPID
     real :: timeAcc1 = 0
     real :: timeAcc2 = 0
     integer :: clock_count_1 = 0
     integer :: clock_count_2 = 0
     integer :: clock_count_3 = 0
     integer :: clock_rate    = 0
#endif




CONTAINS

    subroutine hrldas_cpl_HYDRO(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)

!---lpr added 2015-07-30---------
#ifdef WRF_HYDRO_RAPID
    use hrldas_RAPID_wrapper, only: hrldas_RAPID_ini,hrldas_RAPID_exe
#endif
!---lpr add end-----------------


       implicit none

       integer ii,jj,kk
       integer k, gwsoilcpl
       real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
       real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain, qsgw

!lpr add 2014-06-24
#ifdef WRF_HYDRO_RAPID
#ifdef MPP_LAND
        real, dimension(global_nx,global_ny) :: g_runoff
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


        if(nlst_rt(did)%rtFlag .eq. 0) return
!--------LPR add 2014-06-24---------------------------
!    it is rapid model
#ifdef WRF_HYDRO_RAPID

        if(nlst_rt(did)%channel_option .eq. 4) then
                !write(83,*) infxsrt
                runoff  = infxsrt + soldrain

		!---MPI debug information-------
                !write(60+my_id,*) "before hrldas_RAPID_ini step 1"
                !call flush(60+my_id)

                call hrldas_RAPID_ini(ntime)    

                call system_clock(count=clock_count_1, count_rate=clock_rate)

                !write(60+my_id,*) "after hrldas_RAPID_ini step 2"
                !call flush(60+my_id)
#ifdef MPP_LAND
                call write_io_real(runoff,g_runoff)

                !write(60+my_id,*) "before hrldas_RAPID_exe step 3"
                !call flush(60+my_id)

                call mpp_land_sync()
                call system_clock(count=clock_count_2, count_rate=clock_rate)
                call hrldas_RAPID_exe(g_runoff,global_nx,global_ny)     
                call mpp_land_sync()
                !write(60+my_id,*) "after hrldas_RAPID_exe step 4"
                !call flush(60+my_id)

#else
                call hrldas_RAPID_exe(runoff,ii,jj)     
#endif

                call system_clock(count=clock_count_3, count_rate=clock_rate)
                timeAcc2 = timeAcc2+ float(clock_count_3-clock_count_2)/float(clock_rate)
                timeAcc1 = timeAcc1+ float(clock_count_3-clock_count_1)/float(clock_rate)
                write(6,*) "Timing (accumulated time for Rapid) :",timeAcc1, timeAcc2


                sfcheadrt = 0.0


                return
        endif
#endif
!--------LPR add end----------------------------------




!        write(6,*) "nlst_rt(did)%CHANRTSWCRT nlst_rt(did)%SUBRTSWCRT  nlst_rt(did)%OVRTSWCRT =", &
!                 nlst_rt(did)%CHANRTSWCRT, nlst_rt(did)%SUBRTSWCRT,  nlst_rt(did)%OVRTSWCRT

        IF (nlst_rt(did)%GWBASESWCRT                 .eq. 0 &
               .and. nlst_rt(did)%SUBRTSWCRT         .eq. 0 &
               .and. nlst_rt(did)%OVRTSWCRT          .eq. 0 &
               .and. nlst_rt(did)%channel_only       .eq. 0 &
               .and. nlst_rt(did)%channelBucket_only .eq. 0  ) return


        if(nlst_rt(did)%channel_only       .eq. 0 .and. &
           nlst_rt(did)%channelBucket_only .eq. 0        ) then
           ! decompose the hrldas 1-d data into routing domain
           RT_DOMAIN(did)%STC = STC
           RT_DOMAIN(did)%SMC = SMC
           RT_DOMAIN(did)%SH2OX = SH2OX
           RT_DOMAIN(did)%infxsrt = infxsrt
           RT_DOMAIN(did)%soldrain = soldrain
        end if
           
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
#ifdef HYDRO_D
           write(6,*) "accumulated time (s): ",dact
#endif
        endif
#endif

        if(nlst_rt(did)%channel_only       .eq. 0 .and. &
           nlst_rt(did)%channelBucket_only .eq. 0        ) then
           ! add for update the HRLDAS state variable.
           STC = rt_domain(did)%STC
           SMC = rt_domain(did)%SMC
           SH2OX = rt_domain(did)%SH2OX
           sfcheadrt = rt_domain(did)%overland%control%surface_water_head_lsm
        end if
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
              call hydro_stop("In module_hrldas_HYDRO.F hrldas_cpl_HYDRO_ini()"// &
                   " - sys_cpl should be 1. Check hydro.namelist file.")
           endif

           RT_DOMAIN(did)%initialized = .true.

#ifdef WRF_HYDRO_RAPID
!--------LPR add 2014-06-24---------------------------
           if(nlst_rt(did)%rtFlag .eq. 0) return
!  it  is rapid model.
           return
        endif ! if(.not. RT_DOMAIN(did)%initialized) then  
!--------LPR add 2014-06-24---------------------------
#endif
   
          if (nlst_rt(did)%GWBASESWCRT .eq. 0 &
               .and. nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .and. nlst_rt(did)%OVRTSWCRT .eq. 0 ) return

#ifdef MPP_LAND
          call mpp_land_bcast_real1(nlst_rt(did)%dt)
#endif
          sfcheadrt = rt_domain(did)%overland%control%surface_water_head_lsm
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
       endif  ! if(.not. RT_DOMAIN(did)%initialized) then  

     end subroutine hrldas_cpl_HYDRO_ini

     subroutine open_print_mpp(iunit)
       implicit none
       integer iunit
       character(len=48) fileout
!#ifdef NCEP_WCOSS
       character(len=32) diag_prefix
       integer len, status 
!#endif

       if(open_unit_status == 999) return
       open_unit_status = 999 

#ifdef NCEP_WCOSS
       CALL GET_ENVIRONMENT_VARIABLE('FORT78',diag_prefix, len, status, .true.)
       if (status .ge. 2) then
          write (*,*) 'get_environment_variable failed: status = ', status
          call hydro_stop("In module_hrldas_HYDRO.F open_print_mpp() - "// &
                          "GET_ENVIRONMENT_VARIABLE(FORT78) Failed.")
       end if
       if (status .eq. 1) then
          write (*,*) 'env var does not exist'
          call hydro_stop("In module_hrldas_HYDRO.F open_print_mpp() - "// &
                          "FORT78 environment variable does not exist.")

       end if
       if (status .eq. -1) then
          write (*,*) 'env var length = ', len, ' truncated to 32'
          len = 32
       end if
       if (len .eq. 0) then
         write (*,*) 'env var exists  but has no value'
         call hydro_stop("In module_hrldas_HYDRO.F open_print_mpp() - "// &
                         "FORT78 environment variable exists but has no value.")
       end if
#else
       diag_prefix = 'diag_hydro.'
#endif

#ifdef MPP_LAND  
       write(fileout,'(a11,i0.5)') TRIM(diag_prefix),my_id    
#else
       write(fileout,'(a11,i0.5)') TRIM(diag_prefix),0    
#endif
       open(iunit,file=fileout,form="formatted")
     endsubroutine open_print_mpp
end module module_HRLDAS_HYDRO
