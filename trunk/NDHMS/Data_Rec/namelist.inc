   TYPE namelist_lsm_field
      
       CHARACTER(len=256) :: indir= ''
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
       integer :: rst_diag  ! 0: no output for restart. 1: output diag for restart. 
       
!      additional character
       character :: hgrid
       character(len=19) :: olddate, startdate

   END TYPE namelist_lsm_field



   TYPE namelist_rt_field  
          integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
                  SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
                  GWBASESWCRT, GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
                  sys_cpl
          real:: DTRT,dxrt0
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: wrfsi_finegrid_flnm =""

   END TYPE namelist_rt_field 

