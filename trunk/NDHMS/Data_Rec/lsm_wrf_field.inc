   TYPE LSM_WRF
       integer:: ix,jx


       real, allocatable, dimension(:,:) :: CMC, T1, WEASD, &
           SNODEP, T2, Q2X, U, V, PRES, XLONG, SHORT, PRCP1

       real, allocatable, dimension(:,:) :: HX,EtAKIN, QSFC,SOILHX, &
             snowc, znt
#ifdef COUPLED
       real,allocatable, dimension(:,:) :: tbot_wrf,zlvl_wrf, z0_wrf
#endif
!! added
       real, allocatable, dimension(:,:) :: LH, ALBEDX,GREENFRAC, ALBEDO12M
       integer, allocatable, dimension(:,:) :: soltyp, BASIN_MSK
!! tmp array
       real, allocatable, dimension(:,:) :: ETPX
!      real, allocatable, dimension(:,:) :: latitude,longitude
!      real, allocatable, dimension(:,:,:) :: dist

    END TYPE LSM_WRF

