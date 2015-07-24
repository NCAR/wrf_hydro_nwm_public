Module module_namelist

#ifdef MPP_LAND
          USE module_mpp_land
#endif

    IMPLICIT NONE
    INTEGER, PARAMETER :: max_domain=5

#include "namelist.inc"
    TYPE(namelist_rt_field) , dimension(max_domain) :: nlst_rt
    save nlst_rt 

CONTAINS 

    subroutine read_rt_nlst(nlst)     
          implicit none

          TYPE(namelist_rt_field) nlst

          integer ierr
          integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
                    GWBASESWCRT,  GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
                    sys_cpl, rst_typ, rst_bi_in, rst_bi_out, &
                    gwChanCondSw, GwPreCycles, GwSpinCycles, GwPreDiagInterval, gwsoilcpl
          real:: DTRT,dxrt, gwChanCondConstIn, gwChanCondConstOut, gwIhShift
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: geo_finegrid_flnm =""
       integer :: SOLVEG_INITSWC
       real out_dt, rst_dt
       character(len=256)  :: RESTART_FILE = ""
       logical            :: GwPreDiag, GwSpinUp
       integer            :: split_output_count, order_to_write
       integer :: igrid
       character(len=256) :: geo_static_flnm = ""
       integer  :: DEEPGWSPIN

       integer :: i

       
          integer ::CHRTOUT_DOMAIN           ! Netcdf point timeseries output at all channel points
          integer ::CHRTOUT_GRID                ! Netcdf grid of channel streamflow values
          integer ::LSMOUT_DOMAN              ! Netcdf grid of variables passed between LSM and routing components
          integer ::RTOUT_DOMAIN                ! Netcdf grid of terrain routing variables on routing grid
          integer  :: output_gw
          integer  :: outlake


!!! add the following two dummy variables 
       integer  :: NSOIL
       real :: ZSOIL8(8)

       namelist /HYDRO_nlist/ NSOIL, ZSOIL8,&
            RESTART_FILE,SPLIT_OUTPUT_COUNT,IGRID,&
            geo_static_flnm, &
            out_dt, rst_dt, &
            DEEPGWSPIN, SOLVEG_INITSWC, &
            RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, dtrt,dxrt,&
                    GwSpinCycles, GwPreCycles, GwSpinUp, GwPreDiag, GwPreDiagInterval, gwIhShift, &
                    GWBASESWCRT, gwChanCondSw, gwChanCondConstIn, gwChanCondConstOut , &
                    route_topo_f,route_chan_f,route_link_f,route_lake_f, &
                    route_direction_f,route_order_f,gwbasmskfil, geo_finegrid_flnm,&
                    gwstrmfil,GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, sys_cpl, &
                    order_to_write , rst_typ, rst_bi_in, rst_bi_out, gwsoilcpl, &
                    CHRTOUT_DOMAIN,CHRTOUT_GRID,LSMOUT_DOMAN,RTOUT_DOMAIN, output_gw, outlake
                    
#ifdef MPP_LAND
       if(IO_id .eq. my_id) then
#endif

#ifndef NCEP_WCOSS
          open(30, file="hydro.namelist", form="FORMATTED")
          read(30, HYDRO_nlist, iostat=ierr)
          close(30)
#else
          open(12, form="FORMATTED")
          read(12, HYDRO_nlist, iostat=ierr)
          close(12)
#endif

#ifdef MPP_LAND
       endif
#endif

   rst_bi_in = 0
   rst_bi_out = 0

#ifdef MPP_LAND
!  call mpp_land_bcast_real1(DT)
  call mpp_land_bcast_int1(SPLIT_OUTPUT_COUNT)
  call mpp_land_bcast_int1(IGRID)
  call mpp_land_bcast_real1(out_dt)
  call mpp_land_bcast_real1(rst_dt)
  call mpp_land_bcast_int1(DEEPGWSPIN)
  call mpp_land_bcast_int1(SOLVEG_INITSWC)
#endif


#ifdef MPP_LAND
      call mpp_land_bcast_int1(nlst%NSOIL)
      do i = 1, nlst%NSOIL
        call mpp_land_bcast_real1(nlst%ZSOIL8(i))
      end do
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
      write(6,*) "nlst%NSOIL = ", nlst%NSOIL
#else
      write(78,*) "nlst%NSOIL = ", nlst%NSOIL
#endif
#ifndef NCEP_WCOSS
      write(6,*) "nlst%ZSOIL8 = ",nlst%ZSOIL8
#else
      write(78,*) "nlst%ZSOIL8 = ",nlst%ZSOIL8
#endif
#endif
#endif

!  nlst%DT = DT
  nlst%RESTART_FILE = RESTART_FILE
  nlst%SPLIT_OUTPUT_COUNT = SPLIT_OUTPUT_COUNT
  nlst%IGRID = IGRID
  nlst%geo_static_flnm = geo_static_flnm
  nlst%out_dt = out_dt
  nlst%rst_dt = rst_dt
  nlst%DEEPGWSPIN = DEEPGWSPIN
  nlst%SOLVEG_INITSWC = SOLVEG_INITSWC

#ifdef MPP_LAND
  call mpp_land_bcast_char(256,nlst%RESTART_FILE)
#endif

  write(nlst%hgrid,'(I1)') igrid


  if(RESTART_FILE .eq. "") rst_typ = 0

#ifdef MPP_LAND
  !bcast namelist variable.
  call mpp_land_bcast_int1(rt_option)
  call mpp_land_bcast_int1(CHANRTSWCRT)
  call mpp_land_bcast_int1(channel_option)
  call mpp_land_bcast_int1(SUBRTSWCRT)
  call mpp_land_bcast_int1(OVRTSWCRT)
  call mpp_land_bcast_int1(AGGFACTRT)
  call mpp_land_bcast_real1(DTRT)
  call mpp_land_bcast_real1(DXRT)
  call mpp_land_bcast_real1(gwChanCondConstIn)
  call mpp_land_bcast_real1(gwChanCondConstOut)
  call mpp_land_bcast_real1(gwIhShift)
  call mpp_land_bcast_int1(GWBASESWCRT)
  call mpp_land_bcast_int1(GWSOILCPL)
  call mpp_land_bcast_int1(gwChanCondSw)
  call mpp_land_bcast_int1(GwSpinCycles)
  call mpp_land_bcast_int1(GwPreCycles)
  call mpp_land_bcast_log1(GwPreDiag)
  call mpp_land_bcast_log1(GwSpinUp)
  call mpp_land_bcast_int1(GwPreDiagInterval)
  call mpp_land_bcast_int1(GW_RESTART)
  call mpp_land_bcast_int1(RSTRT_SWC  )
  call mpp_land_bcast_int1(TERADJ_SOLAR)
  call mpp_land_bcast_int1(sys_cpl)
  call mpp_land_bcast_int1(rst_typ)
  call mpp_land_bcast_int1(rst_bi_in)
  call mpp_land_bcast_int1(rst_bi_out)
  call mpp_land_bcast_int1(order_to_write)
  call mpp_land_bcast_int1(CHRTOUT_DOMAIN)
  call mpp_land_bcast_int1(output_gw)
  call mpp_land_bcast_int1(outlake)
  call mpp_land_bcast_int1(CHRTOUT_GRID)
  call mpp_land_bcast_int1(LSMOUT_DOMAN)
  call mpp_land_bcast_int1(RTOUT_DOMAIN)
#endif
    nlst%CHRTOUT_DOMAIN = CHRTOUT_DOMAIN
    nlst%output_gw      = output_gw
    nlst%outlake      = outlake
    nlst%CHRTOUT_GRID = CHRTOUT_GRID
    nlst%LSMOUT_DOMAN = LSMOUT_DOMAN
    nlst%RTOUT_DOMAIN = RTOUT_DOMAIN
    nlst%RT_OPTION = RT_OPTION
    nlst%CHANRTSWCRT = CHANRTSWCRT
    nlst%GW_RESTART  = GW_RESTART 
    nlst%RSTRT_SWC   = RSTRT_SWC  
    nlst%channel_option = channel_option
    nlst%DTRT   = DTRT
    nlst%DTCT   = DTRT
    nlst%SUBRTSWCRT = SUBRTSWCRT
    nlst%OVRTSWCRT = OVRTSWCRT
    nlst%dxrt0 = dxrt
    nlst%AGGFACTRT = AGGFACTRT
    nlst%GWBASESWCRT = GWBASESWCRT
    nlst%GWSOILCPL= GWSOILCPL
    nlst%gwChanCondSw = gwChanCondSw
    nlst%gwChanCondConstIn = gwChanCondConstIn
    nlst%gwChanCondConstOut = gwChanCondConstOut
    nlst%gwIhShift = gwIhShift
    nlst%GwSpinCycles = GwSpinCycles
    nlst%GwPreCycles = GwPreCycles
    nlst%GwPreDiag = GwPreDiag
    nlst%GwSpinUp = GwSpinUp
    nlst%GwPreDiagInterval = GwPreDiagInterval
    nlst%TERADJ_SOLAR = TERADJ_SOLAR
    nlst%sys_cpl = sys_cpl
    nlst%rst_typ = rst_typ
    nlst%rst_bi_in = rst_bi_in
    nlst%rst_bi_out = rst_bi_out
    nlst%order_to_write = order_to_write
! files
    nlst%route_topo_f   =  route_topo_f
    nlst%route_chan_f = route_chan_f 
    nlst%route_link_f = route_link_f
    nlst%route_lake_f =route_lake_f
    nlst%route_direction_f =  route_direction_f
    nlst%route_order_f =  route_order_f
    nlst%gwbasmskfil =  gwbasmskfil
    nlst%gwstrmfil =  gwstrmfil
    nlst%geo_finegrid_flnm =  geo_finegrid_flnm

#ifdef MPP_LAND
  if(my_id .eq. IO_id) then
#endif
#ifdef HYDRO_D
#ifndef NCEP_WCOSS
     write(6,*) "output of the namelist file "
#else
     write(78,*) "output of the namelist file "
#endif

#ifndef NCEP_WCOSS
    write(6,*) " nlst%RT_OPTION ", RT_OPTION
#else
    write(78,*) " nlst%RT_OPTION ", RT_OPTION
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%CHANRTSWCRT ", CHANRTSWCRT
#else
    write(78,*) " nlst%CHANRTSWCRT ", CHANRTSWCRT
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%GW_RESTART  ", GW_RESTART 
#else
    write(78,*) " nlst%GW_RESTART  ", GW_RESTART 
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%RSTRT_SWC   ", RSTRT_SWC  
#else
    write(78,*) " nlst%RSTRT_SWC   ", RSTRT_SWC  
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%channel_option ", channel_option
#else
    write(78,*) " nlst%channel_option ", channel_option
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%DTRT   ", DTRT
#else
    write(78,*) " nlst%DTRT   ", DTRT
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%SUBRTSWCRT ", SUBRTSWCRT
#else
    write(78,*) " nlst%SUBRTSWCRT ", SUBRTSWCRT
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%OVRTSWCRT ", OVRTSWCRT
#else
    write(78,*) " nlst%OVRTSWCRT ", OVRTSWCRT
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%dxrt0 ", dxrt
#else
    write(78,*) " nlst%dxrt0 ", dxrt
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%AGGFACTRT ", AGGFACTRT
#else
    write(78,*) " nlst%AGGFACTRT ", AGGFACTRT
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%GWBASESWCRT ", GWBASESWCRT
#else
    write(78,*) " nlst%GWBASESWCRT ", GWBASESWCRT
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%GWSOILCPL ", GWSOILCPL
#else
    write(78,*) " nlst%GWSOILCPL ", GWSOILCPL
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%gwChanCondSw ", gwChanCondSw
#else
    write(78,*) " nlst%gwChanCondSw ", gwChanCondSw
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%gwChanCondConstIn ", gwChanCondConstIn
#else
    write(78,*) " nlst%gwChanCondConstIn ", gwChanCondConstIn
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%gwChanCondConstOut ", gwChanCondConstOut
#else
    write(78,*) " nlst%gwChanCondConstOut ", gwChanCondConstOut
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%gwIhShift ", gwIhShift
#else
    write(78,*) " nlst%gwIhShift ", gwIhShift
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%GwSpinCycles ", GwSpinCycles
#else
    write(78,*) " nlst%GwSpinCycles ", GwSpinCycles
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%GwPreDiag ", GwPreDiag
#else
    write(78,*) " nlst%GwPreDiag ", GwPreDiag
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%GwPreDiagInterval ", GwPreDiagInterval
#else
    write(78,*) " nlst%GwPreDiagInterval ", GwPreDiagInterval
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%TERADJ_SOLAR ", TERADJ_SOLAR
#else
    write(78,*) " nlst%TERADJ_SOLAR ", TERADJ_SOLAR
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%sys_cpl ", sys_cpl
#else
    write(78,*) " nlst%sys_cpl ", sys_cpl
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%rst_typ ", rst_typ
#else
    write(78,*) " nlst%rst_typ ", rst_typ
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%order_to_write ", order_to_write
#else
    write(78,*) " nlst%order_to_write ", order_to_write
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%route_topo_f   ",  route_topo_f
#else
    write(78,*) " nlst%route_topo_f   ",  route_topo_f
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%route_chan_f ", route_chan_f 
#else
    write(78,*) " nlst%route_chan_f ", route_chan_f 
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%route_link_f ", route_link_f
#else
    write(78,*) " nlst%route_link_f ", route_link_f
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%route_lake_f ",route_lake_f
#else
    write(78,*) " nlst%route_lake_f ",route_lake_f
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%route_direction_f ",  route_direction_f
#else
    write(78,*) " nlst%route_direction_f ",  route_direction_f
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%route_order_f ",  route_order_f
#else
    write(78,*) " nlst%route_order_f ",  route_order_f
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%gwbasmskfil ",  gwbasmskfil
#else
    write(78,*) " nlst%gwbasmskfil ",  gwbasmskfil
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%gwstrmfil ",  gwstrmfil
#else
    write(78,*) " nlst%gwstrmfil ",  gwstrmfil
#endif
#ifndef NCEP_WCOSS
    write(6,*) " nlst%geo_finegrid_flnm ",  geo_finegrid_flnm
#else
    write(78,*) " nlst%geo_finegrid_flnm ",  geo_finegrid_flnm
#endif
#endif
#ifdef MPP_LAND
  endif
#endif

#ifdef MPP_LAND
  !bcast other  variable.
      call mpp_land_bcast_real1(nlst%dt)
#endif
      return
    end subroutine read_rt_nlst


end module module_namelist
