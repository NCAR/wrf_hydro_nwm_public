   TYPE namelist_rt_field  
      
       integer :: nsoil, SOLVEG_INITSWC
       real,allocatable,dimension(:) :: ZSOIL8
       real out_dt, rst_dt, dt
       integer :: START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN
       character(len=256)  :: restart_file = ""
       integer            :: split_output_count
       integer :: igrid
       integer :: rst_bi_in   ! used for parallel io with large restart file.
       integer :: rst_bi_out   ! used for parallel io with large restart file.
                           ! each process will output the restart tile.
       character(len=256) :: geo_static_flnm = ""
       integer  :: DEEPGWSPIN
       integer ::  order_to_write, rst_typ
       
!      additional character
       character :: hgrid
       character(len=19) :: olddate="123456"
       character(len=19) :: startdate="123456"
       character(len=19) :: sincedate="123456"



          integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
                  SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
                  GWBASESWCRT,  GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
                  sys_cpl, gwChanCondSw, GwPreCycles, GwSpinCycles, GwPreDiagInterval, &
                  gwsoilcpl
          logical:: GwPreDiag, GwSpinUp
          real:: DTRT,dxrt0, DTCT, gwChanCondConstIn, gwChanCondConstOut, gwIhShift
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: geo_finegrid_flnm =""

          integer ::frxst_pts_out            ! ASCII point timeseries output at user specified points
          integer ::CHRTOUT_DOMAIN           ! Netcdf point timeseries output at all channel points
          integer ::CHRTOUT_GRID                ! Netcdf grid of channel streamflow values
          integer ::LSMOUT_DOMAN              ! Netcdf grid of variables passed between LSM and routing components
          integer ::RTOUT_DOMAIN                ! Netcdf grid of terrain routing variables on routing grid
          integer ::output_gw                   ! Netcdf grid of GW
          integer ::outlake                   ! Netcdf grid of lake

   END TYPE namelist_rt_field 

