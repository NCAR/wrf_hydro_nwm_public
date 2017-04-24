!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
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
                    gwChanCondSw, GwPreCycles, GwSpinCycles, GwPreDiagInterval, gwsoilcpl, &
                    UDMP_OPT
          real:: DTRT_TER,DTRT_CH,dxrt, gwChanCondConstIn, gwChanCondConstOut, gwIhShift
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: geo_finegrid_flnm =""
          character(len=256) :: udmap_file =""
          character(len=256) :: GWBUCKPARM_file = ""
       integer :: SOLVEG_INITSWC
       real*8 :: out_dt, rst_dt
       character(len=256)  :: RESTART_FILE = ""
       character(len=256)  :: hydrotbl_f   = ""
       logical            :: GwPreDiag, GwSpinUp
       integer            :: split_output_count, order_to_write
       integer :: igrid, iocflag, t0OutputFlag, output_channelBucket_influx
       character(len=256) :: geo_static_flnm = ""
       integer  :: DEEPGWSPIN

       integer :: i

       
          integer ::CHRTOUT_DOMAIN           ! Netcdf point timeseries output at all channel points
          integer ::CHRTOUT_GRID                ! Netcdf grid of channel streamflow values
          integer ::LSMOUT_DOMAIN              ! Netcdf grid of variables passed between LSM and routing components
          integer ::RTOUT_DOMAIN                ! Netcdf grid of terrain routing variables on routing grid
          integer  :: output_gw
          integer  :: outlake


!!! add the following two dummy variables 
       integer  :: NSOIL
       real :: ZSOIL8(8)

       logical            :: dir_e
#ifdef WRF_HYDRO_NUDGING
       character(len=256) :: nudgingParamFile
       character(len=256) :: netwkReExFile
       logical            :: readTimesliceParallel
       logical            :: temporalPersistence
       logical            :: persistBias
       logical            :: biasWindowBeforeT0
       character(len=256) :: nudgingLastObsFile
       character(len=256) :: timeSlicePath
       integer            :: nLastObs
       integer            :: minNumPairsBiasPersist
       integer            :: maxAgePairsBiasPersist
       logical            :: invDistTimeWeightBias
       logical            :: correlationReduceBias
#endif 

       namelist /HYDRO_nlist/ NSOIL, ZSOIL8,&
            RESTART_FILE,SPLIT_OUTPUT_COUNT,IGRID,&
            geo_static_flnm, &
            out_dt, rst_dt, &
            DEEPGWSPIN, SOLVEG_INITSWC, &
            RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, dtrt_ter,dtrt_ch,dxrt,&
                    GwSpinCycles, GwPreCycles, GwSpinUp, GwPreDiag, GwPreDiagInterval, gwIhShift, &
                    GWBASESWCRT, gwChanCondSw, gwChanCondConstIn, gwChanCondConstOut , &
                    route_topo_f,route_chan_f,route_link_f,route_lake_f, &
                    route_direction_f,route_order_f,gwbasmskfil, geo_finegrid_flnm,&
                    gwstrmfil,GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, sys_cpl, &
                    order_to_write , rst_typ, rst_bi_in, rst_bi_out, gwsoilcpl, &
                    CHRTOUT_DOMAIN,CHRTOUT_GRID,LSMOUT_DOMAIN,RTOUT_DOMAIN, output_gw, outlake, udmap_file, &
                    UDMP_OPT, GWBUCKPARM_file, iocflag, hydrotbl_f, t0OutputFlag, output_channelBucket_influx

   UDMP_OPT = 0
   rst_bi_in = 0
   rst_bi_out = 0
   iocflag = 0
   t0OutputFlag = 1
   output_channelBucket_influx = 0

#ifdef WRF_HYDRO_NUDGING
   namelist /NUDGING_nlist/ nudgingParamFile,       netwkReExFile,          &
                            readTimesliceParallel,  temporalPersistence,    &
                            persistBias,            nudgingLastObsFile,     &
                            timeSlicePath,          nLastObs,               &
                            minNumPairsBiasPersist, maxAgePairsBiasPersist, &
                            biasWindowBeforeT0,     invDistTimeWeightBias,  &
                            correlationReduceBias
   ! Default values for NUDGING_nlist
   nudgingParamFile = "DOMAIN/nudgingParams.nc"
   netwkReExFile    = "DOMAIN/netwkReExFile.nc"
   readTimesliceParallel  = .true.
   temporalPersistence    = .true.
   persistBias            = .false.
   biasWindowBeforeT0     = .false.
   nudgingLastObsFile     = ""
   timeSlicePath          = "./nudgingTimeSliceObs/"
   nLastObs               = 960
   minNumPairsBiasPersist = 8
   maxAgePairsBiasPersist = -99999
   invDistTimeWeightBias  = .false.
   correlationReduceBias  = .false.
#endif 

   !! default  values for HYDRO_nlist
   UDMP_OPT = 0
   rst_bi_in = 0
   rst_bi_out = 0
   iocflag = 0
                    
#ifdef MPP_LAND
       if(IO_id .eq. my_id) then
#endif
#ifndef NCEP_WCOSS
          open(12, file="hydro.namelist", form="FORMATTED")
#else
          open(12, form="FORMATTED")
#endif
          read(12, HYDRO_nlist, iostat=ierr)
          if(ierr .ne. 0) call hydro_stop("HYDRO_nlst namelist error in read_rt_nlst")

#ifdef WRF_HYDRO_NUDGING          
          read(12, NUDGING_nlist, iostat=ierr)
          if(ierr .ne. 0) call hydro_stop("NUDGING_nlst namelist error in read_rt_nlst")
          !! Conditional default values for nuding_nlist
          if(maxAgePairsBiasPersist .eq. -99999) maxAgePairsBiasPersist = -1*nLastObs
#endif
          close(12)

#ifdef MPP_LAND
       endif
#endif

#ifdef HYDRO_REALTIME
   if ( iocflag .eq. 4 ) RTOUT_DOMAIN = 0
   if ( (iocflag .gt. 0) .and. (CHRTOUT_DOMAIN .eq.1) .and. (channel_option .ne. 3) ) CHRTOUT_DOMAIN = 2
#endif

   if(output_channelBucket_influx .ne. 0) then
      if(nlst%dt .ne. out_dt*60) &
           call hydro_stop("read_rt_nlst:: output_channelBucket_influx =! 0 inconsistent with out_dt and NOAH_TIMESTEP choices.")
      if(output_channelBucket_influx .eq. 2 .and. GWBASESWCRT .ne. 1) &
           call hydro_stop("read_rt_nlst:: output_channelBucket_influx = 2 but GWBASESWCRT != 1.")
   end if

#ifdef MPP_LAND
!  call mpp_land_bcast_real1(DT)
  call mpp_land_bcast_int1(SPLIT_OUTPUT_COUNT)
  call mpp_land_bcast_int1(IGRID)
  call mpp_land_bcast_int1(iocflag)
  call mpp_land_bcast_int1(t0OutputFlag)
  call mpp_land_bcast_int1(output_channelBucket_influx)
  call mpp_land_bcast_real1_double(out_dt)
  call mpp_land_bcast_real1_double(rst_dt)
  call mpp_land_bcast_int1(DEEPGWSPIN)
  call mpp_land_bcast_int1(SOLVEG_INITSWC)
#endif


#ifdef MPP_LAND
      call mpp_land_bcast_int1(nlst%NSOIL)
      do i = 1, nlst%NSOIL
        call mpp_land_bcast_real1(nlst%ZSOIL8(i))
      end do
#ifdef HYDRO_D
      write(6,*) "nlst%NSOIL = ", nlst%NSOIL
      write(6,*) "nlst%ZSOIL8 = ",nlst%ZSOIL8
#endif
#endif

  !  nlst%DT = DT !!JLM: Note that %dt is set in the Land/WRF initialization, 
  !               !!JLM: e.g. CPL/NoahMP_cpl/module_hrldas_HYDRO.F:hrldas_cpl_HYDRO_ini
  nlst%RESTART_FILE = RESTART_FILE
  nlst%hydrotbl_f = trim(hydrotbl_f) 
  nlst%SPLIT_OUTPUT_COUNT = SPLIT_OUTPUT_COUNT
  nlst%IGRID = IGRID
  nlst%iocflag = iocflag
  nlst%t0OutputFlag = t0OutputFlag
  nlst%output_channelBucket_influx = output_channelBucket_influx
  nlst%geo_static_flnm = geo_static_flnm
  nlst%out_dt = out_dt
  nlst%rst_dt = rst_dt
  nlst%DEEPGWSPIN = DEEPGWSPIN
  nlst%SOLVEG_INITSWC = SOLVEG_INITSWC

#ifdef MPP_LAND
  call mpp_land_bcast_char(256,nlst%RESTART_FILE)
  call mpp_land_bcast_char(256,nlst%hydrotbl_f)
#endif

  write(nlst%hgrid,'(I1)') igrid


  if(RESTART_FILE .eq. "") rst_typ = 0

  if(rst_bi_out .eq. 1) then
!     inquire(directory='restart', exist=dir_e)
     inquire(file='restart/.', exist=dir_e)
     if(.not. dir_e) then
        call system('mkdir restart')
     endif
  endif


#ifdef MPP_LAND
  !bcast namelist variable.
  call mpp_land_bcast_int1(rt_option)
  call mpp_land_bcast_int1(CHANRTSWCRT)
  call mpp_land_bcast_int1(channel_option)
  call mpp_land_bcast_int1(SUBRTSWCRT)
  call mpp_land_bcast_int1(OVRTSWCRT)
  call mpp_land_bcast_int1(AGGFACTRT)
  call mpp_land_bcast_real1(DTRT_TER)
  call mpp_land_bcast_real1(DTRT_CH)
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
  call mpp_land_bcast_int1(LSMOUT_DOMAIN)
  call mpp_land_bcast_int1(RTOUT_DOMAIN)
  call mpp_land_bcast_int1(UDMP_OPT)
#ifdef WRF_HYDRO_NUDGING
  call mpp_land_bcast_char(256, nudgingParamFile  )
  call mpp_land_bcast_char(256, netwkReExFile     )
  call mpp_land_bcast_char(256, nudgingLastObsFile)
  call mpp_land_bcast_log1(readTimesliceParallel)
  call mpp_land_bcast_log1(temporalPersistence)
  call mpp_land_bcast_log1(persistBias)
  call mpp_land_bcast_log1(biasWindowBeforeT0)
  call mpp_land_bcast_char(256, timeSlicePath)
  call mpp_land_bcast_int1(nLastObs)
  call mpp_land_bcast_int1(minNumPairsBiasPersist)
  call mpp_land_bcast_int1(maxAgePairsBiasPersist)
  call mpp_land_bcast_log1(invDistTimeWeightBias)
  call mpp_land_bcast_log1(correlationReduceBias)
#endif 
#endif /* MPP_LAND */
    nlst%CHRTOUT_DOMAIN = CHRTOUT_DOMAIN
    nlst%output_gw      = output_gw
    nlst%outlake      = outlake
    nlst%CHRTOUT_GRID = CHRTOUT_GRID
    nlst%LSMOUT_DOMAIN = LSMOUT_DOMAIN
    nlst%RTOUT_DOMAIN = RTOUT_DOMAIN
    nlst%RT_OPTION = RT_OPTION
    nlst%CHANRTSWCRT = CHANRTSWCRT
    nlst%GW_RESTART  = GW_RESTART 
    nlst%RSTRT_SWC   = RSTRT_SWC  
    nlst%channel_option = channel_option
    nlst%DTRT_TER   = DTRT_TER
    nlst%DTRT_CH   = DTRT_CH
    nlst%DTCT      = DTRT_CH   ! small time step for grid based channel routing

    if(nlst%DT .lt. DTRT_CH) then 
          print*, "nlst%DT,  DTRT_CH = ",nlst%DT,  DTRT_CH
          print*, "reset DTRT_CH=nlst%DT "
          DTRT_CH=nlst%DT
    endif
    if(nlst%DT .lt. DTRT_TER) then 
          print*, "nlst%DT,  DTRT_TER = ",nlst%DT,  DTRT_TER
          print*, "reset DTRT_TER=nlst%DT "
          DTRT_TER=nlst%DT
    endif
    if(nlst%DT/DTRT_TER .ne. real(int(nlst%DT) / int(DTRT_TER)) ) then 
         print*, "nlst%DT,  DTRT_TER = ",nlst%DT,  DTRT_TER
         call hydro_stop("module_namelist: DT not a multiple of DTRT_TER")
    endif
    if(nlst%DT/DTRT_CH .ne. real(int(nlst%DT) / int(DTRT_CH)) ) then 
         print*, "nlst%DT,  DTRT_CH = ",nlst%DT,  DTRT_CH
         call hydro_stop("module_namelist: DT not a multiple of DTRT_CH")
    endif

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
    nlst%udmap_file =  udmap_file
    nlst%UDMP_OPT = UDMP_OPT
    nlst%GWBUCKPARM_file =  GWBUCKPARM_file
#ifdef WRF_HYDRO_NUDGING
    nlst%nudgingParamFile       = nudgingParamFile
    nlst%netWkReExFile          = netWkReExFile
    nlst%readTimesliceParallel  = readTimesliceParallel
    nlst%temporalPersistence    = temporalPersistence
    nlst%persistBias            = persistBias
    nlst%biasWindowBeforeT0     = biasWindowBeforeT0
    nlst%nudgingLastObsFile     = nudgingLastObsFile
    nlst%timeSlicePath          = timeSlicePath
    nlst%nLastObs               = nLastObs
    nlst%minNumPairsBiasPersist = minNumPairsBiasPersist
    nlst%maxAgePairsBiasPersist = maxAgePairsBiasPersist
    nlst%invDistTimeWeightBias  = invDistTimeWeightBias
    nlst%correlationReduceBias  = correlationReduceBias
#endif

#ifdef MPP_LAND
  if(my_id .eq. IO_id) then
#endif
#ifdef HYDRO_D
     write(6,*) "output of the namelist file "
    write(6,*) "nlst%udmap_file ", trim(nlst%udmap_file)
    write(6,*) "nlst%UDMP_OPT ", nlst%UDMP_OPT
    write(6,*) " nlst%RT_OPTION ", RT_OPTION
    write(6,*) " nlst%CHANRTSWCRT ", CHANRTSWCRT
    write(6,*) " nlst%GW_RESTART  ", GW_RESTART 
    write(6,*) " nlst%RSTRT_SWC   ", RSTRT_SWC  
    write(6,*) " nlst%channel_option ", channel_option
    write(6,*) " nlst%DTRT_TER   ", DTRT_TER
    write(6,*) " nlst%DTRT_CH   ", DTRT_CH
    write(6,*) " nlst%SUBRTSWCRT ", SUBRTSWCRT
    write(6,*) " nlst%OVRTSWCRT ", OVRTSWCRT
    write(6,*) " nlst%dxrt0 ", dxrt
    write(6,*) " nlst%AGGFACTRT ", AGGFACTRT
    write(6,*) " nlst%GWBASESWCRT ", GWBASESWCRT
    write(6,*) " nlst%GWSOILCPL ", GWSOILCPL
    write(6,*) " nlst%gwChanCondSw ", gwChanCondSw
    write(6,*) " nlst%gwChanCondConstIn ", gwChanCondConstIn
    write(6,*) " nlst%gwChanCondConstOut ", gwChanCondConstOut
    write(6,*) " nlst%gwIhShift ", gwIhShift
    write(6,*) " nlst%GwSpinCycles ", GwSpinCycles
    write(6,*) " nlst%GwPreDiag ", GwPreDiag
    write(6,*) " nlst%GwPreDiagInterval ", GwPreDiagInterval
    write(6,*) " nlst%TERADJ_SOLAR ", TERADJ_SOLAR
    write(6,*) " nlst%sys_cpl ", sys_cpl
    write(6,*) " nlst%rst_typ ", rst_typ
    write(6,*) " nlst%order_to_write ", order_to_write
    write(6,*) " nlst%route_topo_f   ",  route_topo_f
    write(6,*) " nlst%route_chan_f ", route_chan_f 
    write(6,*) " nlst%route_link_f ", route_link_f
    write(6,*) " nlst%route_lake_f ",route_lake_f
    write(6,*) " nlst%route_direction_f ",  route_direction_f
    write(6,*) " nlst%route_order_f ",  route_order_f
    write(6,*) " nlst%gwbasmskfil ",  gwbasmskfil
    write(6,*) " nlst%gwstrmfil ",  gwstrmfil
    write(6,*) " nlst%geo_finegrid_flnm ",  geo_finegrid_flnm
#ifdef WRF_HYDRO_NUDGING
    write(6,*) " nlst%nudgingParamFile      ",  trim(nudgingParamFile)
    write(6,*) " nlst%netWkReExFile         ",  trim(netWkReExFile)
    write(6,*) " nlst%readTimesliceParallel ",  readTimesliceParallel
    write(6,*) " nlst%temporalPersistence   ",  temporalPersistence
    write(6,*) " nlst%persistBias           ",  persistBias
    write(6,*) " nlst%biasWindowBeforeT0    ",  biasWindowBeforeT0
    write(6,*) " nlst%nudgingLastObsFile    ",  trim(nudgingLastObsFile)
    write(6,*) " timeSlicePath              ",  trim(timeSlicePath)
    write(6,*) " nLastObs                   ",  nLastObs
    write(6,*) " minNumPairsBiasPersist     ",  minNumPairsBiasPersist
    write(6,*) " maxAgePairsBiasPersist     ",  maxAgePairsBiasPersist
    write(6,*) " invDistTimeWeightBias      ",  invDistTimeWeightBias
    write(6,*) " correlationReduceBias      ",  correlationReduceBias
#endif
#endif /* HYDRO_D */
#ifdef MPP_LAND
  endif
#endif

#ifdef MPP_LAND
  !bcast other  variable.
      call mpp_land_bcast_real1(nlst%dt)
#endif

! derive rtFlag
      nlst%rtFlag = 1
      if(channel_option .eq. 4) nlst%rtFlag = 0
!      if(CHANRTSWCRT .eq. 0 .and.  SUBRTSWCRT .eq. 0 .and. OVRTSWCRT .eq. 0 .and. GWBASESWCRT .eq. 0) nlst%rtFlag = 0
      if(SUBRTSWCRT .eq. 0 .and. OVRTSWCRT .eq. 0 .and. GWBASESWCRT .eq. 0) nlst%rtFlag = 0
      return
    end subroutine read_rt_nlst


end module module_namelist
