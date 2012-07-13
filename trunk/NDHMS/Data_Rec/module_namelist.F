Module module_namelist

#ifdef MPP_LAND
          USE module_mpp_land
#endif

    IMPLICIT NONE
    INTEGER, PARAMETER :: max_domain=5

#include "namelist.inc"
    TYPE(namelist_lsm_field) , dimension(max_domain) :: nlst_lsm
    TYPE(namelist_rt_field) , dimension(max_domain) :: nlst_rt
    save nlst_lsm, nlst_rt 

CONTAINS 
    subroutine read_lsm(nlst)     
       implicit none
       TYPE(namelist_lsm_field) nlst
              CHARACTER(len=256) :: indir
       integer :: forc_typ
       integer :: nsoil, slopetyp, SNOW_ASSIM, SOLVEG_INITSWC
       real ZSOIL8(8)
       real out_dt, rst_dt, dt, tbot
       integer :: START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN
       CHARACTER(len=3)         :: DOY   !COWS Julian day for filename...
       character(len=19)  :: restart_hdate = ""
       logical            :: history_output
       integer            :: split_output_count
       integer :: igrid
       character(len=256) :: range = ""
       integer            :: KHOUR, KDAY
       REAL    :: ZLVL, Z
       character(len=256) :: wrfsi_static_flnm = ""
       integer  :: DEEPGWSPIN
       integer :: HIRES_OUT
       integer :: ierr, i, rst_diag

       namelist /NOAHLSM_OFFLINE/INDIR, FORC_TYP, NSOIL, ZSOIL8, DT, TBOT, &
            START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN, DOY, &
            RESTART_HDATE,HISTORY_OUTPUT,SPLIT_OUTPUT_COUNT,IGRID,RANGE, &
            KHOUR, KDAY, ZLVL, Z, wrfsi_static_flnm, &
            out_dt, rst_dt, rst_diag,&
            SLOPETYP,SNOW_ASSIM, &
            HIRES_OUT, & 
            DEEPGWSPIN, SOLVEG_INITSWC

#ifdef MPP_LAND
       if(IO_id .eq. my_id) then
#endif
          open(30, file="noah_wrfcode.namelist", form="FORMATTED")
          read(30, NOAHLSM_OFFLINE, iostat=ierr)
          close(30)
#ifdef MPP_LAND
       endif
#endif

#ifdef MPP_LAND  
  call mpp_land_bcast_int1(FORC_TYP)
  call mpp_land_bcast_int1(NSOIL)
  do i = 1, NSOIL
    call mpp_land_bcast_real1(ZSOIL8(i))
  end do
  call mpp_land_bcast_int1(rst_diag)
  call mpp_land_bcast_real1(DT)
  call mpp_land_bcast_real1(TBOT)
  call mpp_land_bcast_int1(SPLIT_OUTPUT_COUNT)
  call mpp_land_bcast_int1(IGRID)
  call mpp_land_bcast_int1(KHOUR)
  call mpp_land_bcast_int1(KDAY )
  call mpp_land_bcast_real1(Z)
  call mpp_land_bcast_real1(ZLVL)
  call mpp_land_bcast_real1(out_dt)
  call mpp_land_bcast_real1(rst_dt)
  call mpp_land_bcast_int1(SLOPETYP)
  call mpp_land_bcast_int1(SNOW_ASSIM)
  call mpp_land_bcast_int1(HIRES_OUT)
  call mpp_land_bcast_int1(DEEPGWSPIN)
  call mpp_land_bcast_int1(SOLVEG_INITSWC)
#endif
  nlst%rst_diag = rst_diag
  nlst%INDIR = INDIR
  nlst%FORC_TYP = FORC_TYP
  nlst%NSOIL = NSOIL
  nlst%ZSOIL8 = ZSOIL8 
  nlst%DT = DT 
  nlst%TBOT = TBOT
  nlst%START_YEAR = START_YEAR 
  nlst%START_MONTH = START_MONTH 
  nlst%START_DAY = START_DAY 
  nlst%START_HOUR = START_HOUR 
  nlst%START_MIN = START_MIN 
  nlst%DOY = DOY
  write(RESTART_HDATE(17:19),'(A3)') ":00"
  nlst%RESTART_HDATE = RESTART_HDATE 
  nlst%HISTORY_OUTPUT = HISTORY_OUTPUT 
  nlst%SPLIT_OUTPUT_COUNT = SPLIT_OUTPUT_COUNT 
  nlst%IGRID = IGRID 
  nlst%RANGE = RANGE
  nlst%KHOUR = KHOUR 
  nlst%KDAY = KDAY 
  nlst%ZLVL = ZLVL 
  nlst%Z = Z
  nlst%wrfsi_static_flnm = wrfsi_static_flnm
  nlst%out_dt = out_dt 
  nlst%rst_dt = rst_dt
  nlst%SLOPETYP = SLOPETYP 
  nlst%SNOW_ASSIM = SNOW_ASSIM
  nlst%HIRES_OUT = HIRES_OUT
  nlst%DEEPGWSPIN = DEEPGWSPIN 
  nlst%SOLVEG_INITSWC = SOLVEG_INITSWC

  write(nlst%hgrid,'(I1)') igrid
  write(nlst%olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
       start_year, start_month, start_day, start_hour, start_min, 0
  nlst%startdate = nlst%olddate

  write(6,*) "output of the namelist file "
!  write(6,*) " nlst%INDIR ", trim(nlst%INDIR)
  write(6,*) " nlst%FORC_TYP ", FORC_TYP
  write(6,*) " nlst%NSOIL ", NSOIL
  write(6,*) " nlst%ZSOIL8 ", ZSOIL8 
  write(6,*) " nlst%DT ", DT 
  write(6,*) " nlst%TBOT ", TBOT
  write(6,*) " nlst%START_YEAR ", START_YEAR 
  write(6,*) " nlst%START_MONTH ", START_MONTH 
  write(6,*) " nlst%START_DAY ", START_DAY 
  write(6,*) " nlst%START_HOUR ", START_HOUR 
  write(6,*) " nlst%START_MIN ", START_MIN 
  write(6,*) " nlst%DOY ", DOY
  write(6,*) " nlst%RESTART_HDATE ", RESTART_HDATE 
  write(6,*) " nlst%HISTORY_OUTPUT ", HISTORY_OUTPUT 
  write(6,*) "  nlst%SPLIT_OUTPUT_COUNT ", SPLIT_OUTPUT_COUNT 
  write(6,*) " nlst%IGRID ", IGRID 
  write(6,*) " nlst%RANGE ", RANGE
  write(6,*) " nlst%KHOUR ", KHOUR 
  write(6,*) " nlst%KDAY ", KDAY 
  write(6,*) " nlst%ZLVL ", ZLVL 
  write(6,*) " nlst%Z ", Z
  write(6,*) "  nlst%wrfsi_static_flnm ", wrfsi_static_flnm
  write(6,*) " nlst%out_dt ", out_dt 
  write(6,*) " nlst%rst_dt ", rst_dt
  write(6,*) " nlst%SLOPETYP ", SLOPETYP 
  write(6,*) " nlst%SNOW_ASSIM ", SNOW_ASSIM
  write(6,*) " nlst%HIRES_OUT ", HIRES_OUT
  write(6,*) " nlst%DEEPGWSPIN ", DEEPGWSPIN 
  write(6,*) " nlst%SOLVEG_INITSWC ", SOLVEG_INITSWC
    end subroutine read_lsm


    subroutine read_rt(nlst)     
          implicit none

          TYPE(namelist_rt_field) nlst

          integer ierr
          integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
                    GWBASESWCRT, GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
                    sys_cpl
          real:: DTRT,dxrt
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: wrfsi_finegrid_flnm =""

 
          namelist /NDHMS_nlist/ RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, dtrt,dxrt,&
                    GWBASESWCRT,route_topo_f,route_chan_f,route_link_f,route_lake_f, &
                    route_direction_f,route_order_f,gwbasmskfil, wrfsi_finegrid_flnm,&
                    gwstrmfil,GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, sys_cpl
#ifdef MPP_LAND
       if(IO_id .eq. my_id) then
#endif
          open(30, file="noah_wrfcode.namelist", form="FORMATTED")
          read(30, NDHMS_nlist, iostat=ierr)
          close(30)
#ifdef MPP_LAND
       endif
#endif


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
  call mpp_land_bcast_int1(GWBASESWCRT)
  call mpp_land_bcast_int1(GW_RESTART)
  call mpp_land_bcast_int1(RSTRT_SWC  )
  call mpp_land_bcast_int1(TERADJ_SOLAR)
  call mpp_land_bcast_int1(sys_cpl)
#endif
    nlst%RT_OPTION = RT_OPTION
    nlst%CHANRTSWCRT = CHANRTSWCRT
    nlst%GW_RESTART  = GW_RESTART 
    nlst%RSTRT_SWC   = RSTRT_SWC  
    nlst%channel_option = channel_option
    nlst%DTRT   = DTRT
    nlst%SUBRTSWCRT = SUBRTSWCRT
    nlst%OVRTSWCRT = OVRTSWCRT
    nlst%dxrt0 = dxrt
    nlst%AGGFACTRT = AGGFACTRT
    nlst%GWBASESWCRT = GWBASESWCRT
    nlst%TERADJ_SOLAR = TERADJ_SOLAR
    nlst%sys_cpl = sys_cpl
! files
    nlst%route_topo_f   =  route_topo_f
    nlst%route_chan_f = route_chan_f 
    nlst%route_link_f = route_link_f
    nlst%route_lake_f =route_lake_f
    nlst%route_direction_f =  route_direction_f
    nlst%route_order_f =  route_order_f
    nlst%gwbasmskfil =  gwbasmskfil
    nlst%gwstrmfil =  gwstrmfil
    nlst%wrfsi_finegrid_flnm =  wrfsi_finegrid_flnm

    write(6,*) " nlst%RT_OPTION ", RT_OPTION
    write(6,*) " nlst%CHANRTSWCRT ", CHANRTSWCRT
    write(6,*) " nlst%GW_RESTART  ", GW_RESTART 
    write(6,*) " nlst%RSTRT_SWC   ", RSTRT_SWC  
    write(6,*) " nlst%channel_option ", channel_option
    write(6,*) " nlst%DTRT   ", DTRT
    write(6,*) " nlst%SUBRTSWCRT ", SUBRTSWCRT
    write(6,*) " nlst%OVRTSWCRT ", OVRTSWCRT
    write(6,*) " nlst%dxrt0 ", dxrt
    write(6,*) " nlst%AGGFACTRT ", AGGFACTRT
    write(6,*) " nlst%GWBASESWCRT ", GWBASESWCRT
    write(6,*) " nlst%TERADJ_SOLAR ", TERADJ_SOLAR
    write(6,*) " nlst%sys_cpl ", sys_cpl
    write(6,*) " nlst%route_topo_f   ",  route_topo_f
    write(6,*) " nlst%route_chan_f ", route_chan_f 
    write(6,*) " nlst%route_link_f ", route_link_f
    write(6,*) " nlst%route_lake_f ",route_lake_f
    write(6,*) " nlst%route_direction_f ",  route_direction_f
    write(6,*) " nlst%route_order_f ",  route_order_f
    write(6,*) " nlst%gwbasmskfil ",  gwbasmskfil
    write(6,*) " nlst%gwstrmfil ",  gwstrmfil
    write(6,*) " nlst%wrfsi_finegrid_flnm ",  wrfsi_finegrid_flnm

      return
    end subroutine read_rt


end module module_namelist
