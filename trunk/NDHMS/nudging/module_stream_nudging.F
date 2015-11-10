module module_stream_nudging

#ifdef MPP_LAND
     use module_mpp_land
     use module_mpp_reachls,  only: ReachLS_write_io
#endif

implicit none
!===================================================================================================
! Module Variables

!========================
! obs and obsTime data structures. Each entry in obsTime holds a timeslice file, with the 
! individual obs in the obsStr.
type obsStructure
   character(len=15) :: usgsId        ! the 15 char USGS identifier.                             
   character(len=19) :: obsTime       ! observation at gage dims: nGages                         
   real              :: obsDischarge  ! observation at gage dims: nGages
   real              :: obsQC         ! quality control factpr [0,1]
   integer           :: obsStaticInd  ! the index to the obsStaticStr where static info is kept
   real              :: innov         ! obs-modeled
end type obsStructure

type obsTimeStructure
   character(len=19) :: time 
   character(len=19) :: updateTime
   integer, allocatable, dimension(:) :: allCellInds  ! cell indices affected at this time
   integer, allocatable, dimension(:) :: nGageCell    ! number of gages for each affected cell ind
   type(obsStructure),   allocatable, dimension(:) :: obsStr ! the obs at this file time / timeslice
end type obsTimeStructure

! The top level structure used to solve the nudges
type(obsTimeStructure), allocatable, dimension(:) :: obsTimeStr ! size=nObsTimes

!========================
! This holds static information for a given gage for a given cycle (when R, G, and tau) do not 
! change.
type obsStaticStructure
   character(len=15) :: usgsId        ! the 15 char USGS identifier.                             
   integer           :: obsCellInd    ! index of obs on model channel network, for distance calc 
   real    :: R, G, tau               ! the nudging parameters at this gage.                     
   integer, allocatable, dimension(:) ::  cellsAffected  ! indices of cells affected             
   real,    allocatable, dimension(:) ::  dist           ! optional: dist to affected cells, optional
   real,    allocatable, dimension(:) ::  ws             ! spatial cressman weights at affected cells
end type obsStaticStructure

! The static obs/gage information - store here to perform calculations 
! only once per cycle.
! Currently the dimensions of this are fixed to 10000 which should 
! work for the forseeable future. May want to consider some routine for 
! augmenting this size if necessary. 
integer, parameter :: maxGageDim=18226
type(obsStaticStructure), dimension(maxGageDim) :: obsStaticStr 

!========================
! Node and gage collocation - For reach based routing, corresponds to the "gage" column
! of RouteLink.nc.
type nodeGageStructure
   integer,           allocatable, dimension(:) :: nodeId
   character(len=15), allocatable, dimension(:) :: usgsId
end type nodeGageStructure
type(nodeGageStructure) :: nodeGageTmp, nodeGageStr
integer :: nGagesDomain   ! the number of gages specified

!========================
! Nudging parameters structure, corresponding to NudgeParams.nc file.
type nudgingParamStructure
   character(len=15), allocatable, dimension(:) :: usgsId
   real,              allocatable, dimension(:) :: R
   real,              allocatable, dimension(:) :: G
   real,              allocatable, dimension(:) :: tau
end type nudgingParamStructure
type(nudgingParamStructure) :: nudgingParamsTmp, nudgingParamsStr

!========================
! Network reExpression structure, corresponding to netwkReExFile.nc
type netwkReExpStructure
   integer*4, allocatable, dimension(:) :: go
   integer*4, allocatable, dimension(:) :: start
   integer*4, allocatable, dimension(:) :: end
end type netwkReExpStructure
type(netwkReExpStructure) :: downNetwkStr, upNetwkStr

!========================
! Track gages from NWIS not in our param file and/or NHD+
integer, parameter :: maxNwisNotNhdCount=5000
character(len=15), dimension(maxNwisNotNhdCount) :: nwisNotNhd
integer :: nwisNotNhdCount

!========================
! Control book keeping
logical :: nudgeThisLsmTimeStep

!! JLM: these files are hardcoded/hardwired for now, evnt go in namelist
character(len=256), parameter :: nudgingParamFile = "nudgingParams.nc"
character(len=256), parameter :: netwkReExFile    = "netwkReExFile.nc"
integer,            parameter :: obsResolutionInt = 15 ! minutes
real,               parameter :: obsCheckFreq     = 90 ! minutes 
logical,            parameter :: filterObsToDom   = .TRUE.
character(len=15),  parameter :: missingGage = '               '
logical,            parameter :: nudgeWAdvance = .FALSE.
character(len=4),   parameter :: nudgingClockType = 'wall'

character(len=2) :: obsResolution    

!========================
! Time book keeping
real    :: maxTau
integer :: nObsTimes
#ifdef MPP_LAND
character(len=19) :: lsmTimeMinusDt
integer :: lsmDt
integer, parameter :: did=1

!========================
! Parallel book keeping
real, allocatable, dimension(:) :: chanlen_image0    !A global version kept on image 0
#endif 

contains


!===================================================================================================
! Program Names: 
!   init_stream_nudging_clock
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   One-time initialization of diagnostic stream nuding clock.
! History Log: 
!   10/08/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files: 
! Output Files: None.
! Condition codes: 
! User controllable options: None. 
! Notes:

subroutine init_stream_nudging_clock
use module_nudging_utils, only: totalNudgeTime,    &
                           sysClockCountRate, &
                           sysClockCountMax,  &
                           clockType
totalNudgeTime = 0. ! Nudging time accumulation init.
call system_clock(count_rate=sysClockCountRate, count_max=sysClockCountMax)
clockType = trim(nudgingClockType)
#ifdef HYDRO_D
print*,'Ndg: totalNudgeTime: ', totalNudgeTime
print*,'Ndg: sysClockCountRate: ', sysClockCountRate
print*,'Ndg: sysClockCountMax: ', sysClockCountMax
print*,'Ndg: clockType: ', trim(nudgingClockType)
#endif HYDRO_D
end subroutine init_stream_nudging_clock


!===================================================================================================
! Program Names: 
!   init_stream_nudging
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   One-time initialization of certain stream nuding information. Some of this infomation 
!   may be updated later in the run, but probably not frequently.
! History Log: 
!   7/23/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files: currently hardwired module variables
!   nudgingParamFile = "NudgeParams.nc"
!   gageGageDistFile = "GageGageDistances.nc"
!   netwkReExFile   = "netwkReExFile.nc"
! Output Files: None.
! Condition codes: 
! User controllable options: None. 
! Notes:

subroutine init_stream_nudging

use module_namelist, only: nlst_rt
use module_RT_data,  only: rt_domain
use module_nudging_utils, only: whichInLoop,        &
                                totalNudgeTime,     &
                                sysClockCountRate,  &
                                sysClockCountMax,   &
                                clockType          
use module_nudging_io,    only: get_netcdf_dim,                      & 
                                read_gridded_nudging_frxst_gage_csv, &
                                read_reach_gage_collocation,         &
                                read_nudging_param_file,             &
                                read_network_reexpression

implicit none
integer                    :: nLinks, nLinksL
!integer, dimension(nlinks) :: strmFrxstPts
integer :: did=1
integer :: ii, kk, ll, count, toSize, fromSize
integer :: nParamGages, nGgDists, nGgDistsKeep
integer :: nWhGageDists1, nWhGageDists2, nGagesWParamsDom
integer, allocatable, dimension(:) :: whGageDists1, whGageDists2, whParamsDom
integer :: downSize, upSize, baseSize

#ifdef HYDRO_D
#ifdef MPP_LAND
print*,'Ndg: JLM:: PARALLEL NUDGING!'
#endif
print*, "Ndg: Start init_stream_nudging"
#endif

nLinks       = RT_DOMAIN(did)%NLINKS   ! For gridded channel routing
#ifdef MPP_LAND
nLinksL      = RT_DOMAIN(did)%gNLINKSL  ! For reach-based routing in parallel, no decomp for nudging
#else 
nLinksL      = RT_DOMAIN(did)%NLINKSL   ! For reach-based routing                       
#endif

!Variable init
nwisNotNhd = missingGage
nwisNotNhdCount=0

!=================================================
! 0. This routine is called with condition that chanrtswcrt.ne.0 in module_HYDRO_drv

!=================================================
! 1. Gage link/node collocation.
! which gages are actually in the domain?
! Musk routines: use routelink csv! New column for associated gage Id for each link.
! Grid channel : frxst_pts layer in Fulldom, and a new Nudge_frxst_gage.csv file, which 
!                simply contains frxst_pts index and the associated gage ID, *** may be blank?***
!========================
!!$! Gridded channel routing is option 3
!!$if (nlst_rt(did)%channel_option .eq. 3) then
!!$   strmfrxstpts = RT_DOMAIN(did)%STRMFRXSTPTS
!!$   ! For gridded channel routing, setup the relationship between frxst_pts and gage IDs via
!!$   ! the Nudging_frxst_gage.csv.
!!$   ! For now this is a csv, but it should be netcdf in the long run.
!!$#ifdef HYDRO_D
!!$   print*, 'Ndg: Start initializing Nudging_frxst_gage.csv'
!!$#endif
!!$   !! allocate the maximum number of gages, say 8000.
!!$   allocate(nodeGageTmp%nodeId(maxGages), nodeGageTmp%usgsId(maxGages))
!!$   ! This actually returns frxst point from the file...
!!$   call read_gridded_nudging_frxst_gage_csv(nodeGageTmp%nodeId, nodeGageTmp%usgsId, nGagesDomain)
!!$   ! ... we'll convert this to index on the stream network.
!!$   allocate(nodeGageStr%nodeId(nGagesDomain), nodeGageStr%usgsId(nGagesDomain))
!!$   
!!$   !! JLM: need to handle desired frxst points which are not gages.
!!$   !! Need to make sure these are a complete set 1:nGages.
!!$   do ll=1,nLinks
!!$      if(strmFrxstPts(ll) .ne. -9999) then 
!!$         nodeGageStr%nodeId(strmFrxstPts(ll)) = ll
!!$         nodeGageStr%usgsId(strmFrxstPts(ll)) = adjustr(nodeGageTmp%usgsId(strmFrxstPts(ll)))
!!$      end if
!!$   end do
!!$
!!$   deallocate(nodeGageTmp%nodeId, nodeGageTmp%usgsId)
!!$
!!$#ifdef HYDRO_D
!!$   print*,nGagesDomain
!!$   print*,nodeGageStr%nodeId
!!$   print*,nodeGageStr%usgsId
!!$   print*, 'Ndg: Finish initializing Nudging_frxst_gage.csv'
!!$#endif
!!$end if ! gridded channel models

!========================
! Muskingum routines are channel_options 1 and 2
if (nlst_rt(did)%channel_option .eq. 1 .or. &
    nlst_rt(did)%channel_option .eq. 2) then

   ! For reach-based/muskingum routing methods, we are currently requiring 
   ! the netcdf file for input of gages.

#ifdef MPP_LAND
   if(my_id .eq. io_id) then
#endif

#ifdef HYDRO_D
      print*, 'Ndg: Start initializing reach gages (netcdf)'
      print*,'Ndg: maxGageDim:',maxGageDim
#endif

      allocate(nodeGageTmp%usgsId(nLinksL))
      call read_reach_gage_collocation(nodeGageTmp%usgsId)

      nGagesDomain=0
      !check: nLinksL .eq. size(nodeGageTmp%usgsId)
      !do ll=1,nLinksL
      do ll=1,size(nodeGageTmp%usgsId)
         if(nodeGageTmp%usgsId(ll) .ne. missingGage) nGagesDomain=nGagesDomain+1
      end do

      if(nGagesDomain .gt. 0) then  
         if(nGagesDomain .gt. maxGageDim) call &
              hydro_stop('init_stream_nudging: nGagesDomain > hardcoded maxGageDim, increase it!')

         allocate(nodeGageStr%nodeId(nGagesDomain), nodeGageStr%usgsId(nGagesDomain))
         nodeGageStr%usgsId = pack(nodeGageTmp%usgsId, mask=nodeGageTmp%usgsId .ne. missingGage)
         ! This just index, we are NOT using comIds in nudging.
         ! nodeGageStr%nodeId = pack(rt_domain(did)%linkId, mask=nodeGageTmp%usgsId .ne. '')
         nodeGageStr%nodeId = pack((/(ii, ii=1,nLinksL)/), mask=nodeGageTmp%usgsId .ne. missingGage)
      end if
      deallocate(nodeGageTmp%usgsId)

#ifdef HYDRO_D
      print*,'Ndg: nGagesDomain:',nGagesDomain
      print*,'Ndg: nLinksL', nLinksL
      print*,"Ndg: size(nodeGageStr%nodeId):", size(nodeGageStr%nodeId)
      print*,&
     'Ndg: nodeGageStr%usgsId((size(nodeGageStr%nodeId)-nGagesDomain+1):(size(nodeGageStr%nodeId))):',&
           nodeGageStr%usgsId((size(nodeGageStr%nodeId)-nGagesDomain+1):(size(nodeGageStr%nodeId)))
      print*, 'Ndg: Finish initializing reach gages (netcdf)'
#endif

#ifdef MPP_LAND
   end if ! my_id .eq. io_id
   !! Broadcast
   call mpp_land_bcast_int1(nGagesDomain)
   if(my_id .ne. io_id)                            &
        allocate(nodeGageStr%nodeId(nGagesDomain), &
                 nodeGageStr%usgsId(nGagesDomain)  )
   call mpp_land_bcast_char1d(nodeGageStr%usgsId)
   call mpp_land_bcast_int1d(nodeGageStr%nodeId)
#endif 

!print*,'Ndg: JLM: my_id & nGagesDomain:', my_id, nGagesDomain
!print*,'Ndg: JLM: my_id & nodeGageStr%usgsId:', my_id, nodeGageStr%usgsId
!print*,'Ndg: JLM: my_id & nodeGageStr%nodeId:', my_id, nodeGageStr%nodeId

end if ! muskingum channel models

!=================================================
! 3. Read nudging parameter files and reduce to the gages in the domain (nGagesDomain, etc above).
#ifdef MPP_LAND
if(my_id .eq. IO_id) then
#endif
   nParamGages = get_netcdf_dim(nudgingParamFile, 'stationIdInd', 'init_stream_nudging') 

#ifdef HYDRO_D
   print*,'Ndg: nParamGages: ',nParamGages
#endif

   allocate(nudgingParamsTmp%usgsId(nParamGages),  &
            nudgingParamsTmp%R(nParamGages),       &
            nudgingParamsTmp%G(nParamGages),       &
            nudgingParamsTmp%tau(nParamGages)      )

   call read_nudging_param_file(nudgingParamFile,        &
                                nudgingParamsTmp%usgsId, &
                                nudgingParamsTmp%R,      &
                                nudgingParamsTmp%G,      &
                                nudgingParamsTmp%tau     )

#ifdef HYDRO_D
!   print*,'Ndg: nudgingParamsTmp%usgsId(nParamGages)', nudgingParamsTmp%usgsId(nParamGages)
!   print*,'Ndg: nudgingParamsTmp%R(nParamGages)',      nudgingParamsTmp%R(nParamGages)
!   print*,'Ndg: nudgingParamsTmp%G(nParamGages)',      nudgingParamsTmp%G(nParamGages)
!   print*,'Ndg: nudgingParamsTmp%tau(nParamGages)',    nudgingParamsTmp%tau(nParamGages)
#endif

   ! Reduce to just the gages in the domain
   allocate(whParamsDom(nParamGages))

   call whichInLoop(nudgingParamsTmp%usgsId, nodeGageStr%usgsId, whParamsDom, nGagesWParamsDom)
#ifdef HYDRO_D
   if(nGagesWParamsDom .ne. nGagesDomain) then
      print*,'Ndg: Gages are apparently missing from the nudgingParams.nc file'
      print*,'Ndg: nGagesWParamsDom: ', nGagesWParamsDom
      print*,'Ndg: nGagesDomain: ', nGagesDomain
   end if
#endif

   allocate(nudgingParamsStr%usgsId(nGagesWParamsDom),  &
            nudgingParamsStr%R(nGagesWParamsDom),       &
            nudgingParamsStr%G(nGagesWParamsDom),       &
            nudgingParamsStr%tau(nGagesWParamsDom)      )
   count=1
   do kk=1,nParamGages
      if(whParamsDom(kk) .gt. 0) then
         nudgingParamsStr%usgsId(count) = nudgingParamsTmp%usgsId(kk)
         nudgingParamsStr%R(count)      = nudgingParamsTmp%R(kk)
         nudgingParamsStr%G(count)      = nudgingParamsTmp%G(kk)
         nudgingParamsStr%tau(count)    = nudgingParamsTmp%tau(kk)
         count=count+1
      end if
   end do

   deallocate(nudgingParamsTmp%usgsId,  nudgingParamsTmp%R,   &
              nudgingParamsTmp%G,       nudgingParamsTmp%tau  )

#ifdef HYDRO_D
   print*,'Ndg: nudgingParamsStr%usgsId', nudgingParamsStr%usgsId(size(nudgingParamsStr%usgsId))
   print*,'Ndg: nudgingParamsStr%R',      nudgingParamsStr%R(size(nudgingParamsStr%R))
   print*,'Ndg: nudgingParamsStr%G',      nudgingParamsStr%G(size(nudgingParamsStr%G))
   print*,'Ndg: nudgingParamsStr%tau',    nudgingParamsStr%tau(size(nudgingParamsStr%tau))
#endif

#ifdef MPP_LAND
endif ! my_id .eq. io_id
!! Broadcast
call mpp_land_bcast_int1(nGagesWParamsDom)
if(my_id .ne. io_id)                                      &
     allocate(nudgingParamsStr%usgsId(nGagesWParamsDom),  &
              nudgingParamsStr%R(nGagesWParamsDom),       &
              nudgingParamsStr%G(nGagesWParamsDom),       &
              nudgingParamsStr%tau(nGagesWParamsDom)      )
call mpp_land_bcast_char1d(nudgingParamsStr%usgsId)
call mpp_land_bcast_real_1d(nudgingParamsStr%R)
call mpp_land_bcast_real_1d(nudgingParamsStr%G)
call mpp_land_bcast_real_1d(nudgingParamsStr%tau)
#endif

!print*,'Ndg: JLM: my_id & nudgingParamsStr%usgsId:', my_id, nudgingParamsStr%usgsId
!print*,'Ndg: JLM: my_id & nudgingParamsStr%R:', my_id, nudgingParamsStr%R
!print*,'Ndg: JLM: my_id & nudgingParamsStr%G:', my_id, nudgingParamsStr%G
!print*,'Ndg: JLM: my_id & nudgingParamsStr%tau:', my_id, nudgingParamsStr%tau

!=================================================
! 4. Sort out which gages are actually in the domain.
! JLM: there can be an inconsistency between the gages with parameters
! JLM: and the gages with distances. 
! JLM: How to handle locations without parameters?
! JLM: How to determine the functional list of gages in the domain?

!=================================================
! 5. Bring in the network re-expression for indexed traversal 
!    of the stream network.
#ifdef MPP_LAND
if(my_id .eq. IO_id) then
#endif
   downSize = get_netcdf_dim(netwkReExFile, 'downDim', 'init_stream_nudging')
   upSize   = get_netcdf_dim(netwkReExFile, 'upDim',   'init_stream_nudging')
   baseSize = get_netcdf_dim(netwkReExFile, 'baseDim', 'init_stream_nudging')
   !if(baseSize .ne. nLinksL) call hydro_stop('init_stream_nudging: baseSize .ne. nLinksL')

   allocate(upNetwkStr%go(upSize),       &
            upNetwkStr%start(nLinksL),   &
            upNetwkStr%end(nLinksL),     &
            downNetwkStr%go(downSize),   &
            downNetwkStr%start(nLinksL), &
            downNetwkStr%end(nLinksL)    ) 

   call read_network_reexpression( &
        netwkReExFile,      & ! file with dims of the stream netwk
        upNetwkStr%go,      & ! where each ind came from, upstream
        upNetwkStr%start,   & ! where each ind's upstream links start in upGo
        upNetwkStr%end,     & ! where each ind's upstream links end   in upGo
        downNetwkStr%go,    & ! where each ind goes, downstream
        downNetwkStr%start, & ! where each ind's downstream links start in downGo
        downNetwkStr%end    ) ! where each ind's downstream links end   in downGo

#ifdef MPP_LAND
endif ! my_id .eq. io_id
! Broadcast
call mpp_land_bcast_int1(upSize)
call mpp_land_bcast_int1(downSize)
if(my_id .ne. io_id)                       &
     allocate(upNetwkStr%go(upSize),       &
              upNetwkStr%start(nLinksL),   &
              upNetwkStr%end(nLinksL),     &
              downNetwkStr%go(downSize),   &
              downNetwkStr%start(nLinksL), &
              downNetwkStr%end(nLinksL)    ) 
call mpp_land_bcast_int1d(upNetwkStr%go)
call mpp_land_bcast_int1d(upNetwkStr%start)
call mpp_land_bcast_int1d(upNetwkStr%end)
call mpp_land_bcast_int1d(downNetwkStr%go)
call mpp_land_bcast_int1d(downNetwkStr%start)
call mpp_land_bcast_int1d(downNetwkStr%end)
#endif

!=================================================
! Could do the broadcasts here, having used different images for the IO. This would allow
! some level of parallel IO and asychronous computation... 

!=================================================
! 6. Set up the global time parameters
#ifdef MPP_LAND
! MPP need to broadcast the initial time and setup the dt
if(my_id .eq. io_id) then
   lsmDt = nlst_rt(did)%dt
endif 
call mpp_land_bcast_int1(lsmDt)
#else 
lsmDt = nlst_rt(did)%dt
#endif

!S      - -+- -       !
!E      !       - -+- -
!  |- - - -L* * * *|- - - -|- - - -|
!       t t w w w w t t
! - time chunks of obsResolution
! | separators denoting lsmDt chunks
! * obsRresolution points in the current lsmDt/hydro time advance
! + is the center of the assim window size = tau*2, at that the hydro time
! ! are the bounds of all assim windows
! L is LSM time
! S-E The start and end times for hydro advance
! t denotes times needed by size of tau
! w denotes times needed by size of lsmDt
! Regardless of tau, need window of size (2*tau+lsmDt)
! which will have (2*tau+lsmDt)/obsResolution+1 observation times in it
! the +1 is for the zeroth time.

maxTau = maxval(nudgingParamsStr%tau) ! This is fixed as tau does not change for a cycle. 
write(obsResolution, '(i0.2)') obsResolutionInt
nObsTimes  = 2*ceiling(maxTau/obsResolutionInt) + ceiling(real(lsmDt)/(60.*obsResolutionInt)) + 1

#ifdef HYDRO_D
print*,'Ndg: obsResolution:',obsResolution
print*,'Ndg: maxTau: ', maxTau
print*,'Ndg: nObsTimes: ', nObsTimes
#endif 

allocate(obsTimeStr(nObsTimes))
!! initialize these as blanks and 'none'
obsTimeStr(:)%time     = ''
obsTimeStr(:)%updateTime = 'none'

! Init obsStaticStr for it's allocated variables
obsStaticStr(:)%usgsId = missingGage
obsStaticStr(:)%obsCellInd = 0
obsStaticStr(:)%R = -9999.
obsStaticStr(:)%G = -9999.
obsStaticStr(:)%tau = -9999.


#ifdef MPP_LAND
!=================================================
! 7. MPP: Going to keep a copy of the full chanlen on image 0 
! May broadcast to all images if we want to do parallel IO
if(my_id .eq. 0) allocate(chanlen_image0(nLinksL))
call ReachLS_write_io(rt_domain(did)%chanlen, chanlen_image0)
#endif

!=================================================
#ifdef HYDRO_D
print*, "Ndg: Finish init_stream_nudging"
call flush(6)
#endif

end subroutine init_stream_nudging

!===================================================================================================
! Program Names: 
!   subroutine setup_stream_nudging
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Setup the nudging for the current hydroTime, only establishes the 
!   shared obsTimeStr above.
! History Log: 
!   6/04/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files: 
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes:

subroutine setup_stream_nudging(hydroDT)

use module_namelist,      only: nlst_rt
use module_RT_data,       only: rt_domain
use module_nudging_utils,      only: whichLoop,               &
                                whichInLoop,             &
                                whUniLoop,               &
                                accum_nudging_time,      &
                                nudging_timer

use module_date_utils_nudging, only: geth_newdate,            &
                                round_resolution_minute, &
                                geth_idts
implicit none

integer,           intent(in) :: hydroDT ! the number of seconds of hydro advance from lsmTime

integer :: ff, ii, tt
character(len=19) :: hydroTime, obsHydroTime ! hydro model time and corresponding observation 
character(len=19), dimension(nObsTimes) :: obsTimes ! obs times in the current window
integer :: oldDiff, nShiftLeft

logical, allocatable, dimension(:) :: theMask
integer, allocatable, dimension(:) :: whObsMiss

integer :: nObsMiss

integer, allocatable, dimension(:) :: streamCellObsCount
character(len=256), dimension(60)  :: timeSliceFiles
integer :: did=1  !! jlm: assuming did=1

integer :: nWhParamsNow, ww, count, gg, rr, jj, gg0, nWhObsMiss
integer, allocatable, dimension(:) :: regionInds, whParamsNow, usgsId
real,    allocatable, dimension(:) :: R, G, tau
integer :: whNodeGage, whNudgeParams,  allRegSize

real,    allocatable, dimension(:,:) :: distsInR
integer, allocatable, dimension(:,:) :: indsInR
integer, allocatable, dimension(:) :: nDistsInR
real, dimension(10000) :: dumDistsInR
integer, dimension(10000) :: dumIndsInR
integer :: dumNDistsInR
real :: rReg
logical :: obsTimeStrAllocated

#ifdef MPP_LAND 
integer :: nGages, nCellsInR, cc, nLinkAff, nStatic
#endif

#ifdef HYDRO_D
real :: startCodeTime, endCodeTime
#endif HYDROD

#ifdef HYDRO_D
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif
   print*,'Ndg: start setup_stream_nudging'
#endif

!#ifdef MPP_LAND - just do this section on all images. There is no IO.

#ifdef HYDRO_D
if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D

! The hydro model time as a string.
#ifdef MPP_LAND
!update and broadcast the lsm time
if(my_id .eq. io_id) lsmTimeMinusDt = nlst_rt(did)%olddate
!! JLM - why broadcast this? is it not the same on all processors?
call mpp_land_bcast_char(19, lsmTimeMinusDt)
#endif


#ifdef HYDRO_D
if(my_id .eq. io_id) then
   call nudging_timer(endCodeTime)
   call accum_nudging_time(startCodeTime, endCodeTime, &
                           'bcast lsm time', .false.   )
endif
if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D


!! unintuitively, olddate is 2 lsm timesteps behind when the hydro model is run.
!call geth_newdate(hydroTime, nlst_rt(1)%olddate, hydroDT+nint(nlst_rt(1)%dt))
call geth_newdate(hydroTime, lsmTimeMinusDt, hydroDT+lsmDt)
! Calculate the closest multiple of obsResolution to the hydroTime
obsHydroTime = round_resolution_minute(hydroTime, obsResolutionInt)

! Now solve all of the observation times in the current nudging window.
! nObsTimes is set at init from maxTau and obsResolution. 
! These times correspond to the timestamps on the observation files, which 
! mark the center of the time period they represent with width obsResolution.
do tt=1,nObsTimes
   print*,"Ndg: ", (tt-1-ceiling(maxTau/obsResolutionInt))*60*obsResolutionInt
   call geth_newdate(obsTimes(tt), obsHydroTime,  &
                     !obsResolutionInt*(tt - (nObsTimes+1)/2 )*60)
                     (tt-1-ceiling(maxTau/obsResolutionInt))*60*obsResolutionInt )
end do


! If this is the first setup, these are all blank. This is their init value.
if(all(obsTimeStr(:)%time .eq. '')) obsTimeStr(:)%time = obsTimes

!print*,'Ndg: hydroTime: ', hydroTime
!print*,'Ndg: obsHydroTime: ', obsHydroTime
!print*,'Ndg: obsTimes: ', obsTimes
!print*,'Ndg: obsTimeStr: before obsTimeStr(:)%time: ', obsTimeStr(:)%time


! If there are existing observations older than the first obsTime for this 
! nudging window, shift them left. This should just shift one position left 
! with each advance in time resolution.
call geth_idts(obsTimeStr(1)%time, obsTimes(1), oldDiff)
print*,"Ndg: olddiff:", oldDiff
if(oldDiff .eq. 0) then 
   nShiftLeft=0
else 
   ! nShiftLeft should not exceed nObsTimes
   nShiftLeft = min( abs(oldDiff/obsResolutionInt/60), nObsTimes )
end if
print*,"Ndg: nShiftLeft:", nShiftLeft

if(nShiftLeft .gt. 0 .and. nShiftLeft .lt. nObsTimes) then 
   do tt=1,nObsTimes-nShiftLeft
      obsTimeStr(tt)%time = obsTimeStr(tt+nShiftLeft)%time
      obsTimeStr(tt)%updateTime = obsTimeStr(tt+nShiftLeft)%updateTime
      if(allocated(obsTimeStr(tt)%allCellInds)) deallocate(obsTimeStr(tt)%allCellInds)
      if(allocated(obsTimeStr(tt)%nGageCell))   deallocate(obsTimeStr(tt)%nGageCell)
      if(allocated(obsTimeStr(tt)%obsStr))      deallocate(obsTimeStr(tt)%obsStr)
      allocate(obsTimeStr(tt)%allCellInds(size(obsTimeStr(tt+nShiftLeft)%allCellInds)), &
           obsTimeStr(tt)%nGageCell(size(obsTimeStr(tt+nShiftLeft)%nGageCell)),     &
           obsTimeStr(tt)%obsStr(size(obsTimeStr(tt+nShiftLeft)%obsStr))            )
      obsTimeStr(tt)%allCellInds = obsTimeStr(tt+nShiftLeft)%allCellInds
      obsTimeStr(tt)%nGageCell = obsTimeStr(tt+nShiftLeft)%nGageCell
      obsTimeStr(tt)%obsStr = obsTimeStr(tt+nShiftLeft)%obsStr
   end do
endif
if(nShiftLeft .gt. 0) then 
   ! here tt= nObsTimes
   do tt=nObsTimes-nShiftLeft+1,nObsTimes
      obsTimeStr(tt)%time = ''
      obsTimeStr(tt)%updateTime = 'none'
      if(allocated(obsTimeStr(tt)%allCellInds)) deallocate(obsTimeStr(tt)%allCellInds)
      if(allocated(obsTimeStr(tt)%nGageCell))   deallocate(obsTimeStr(tt)%nGageCell)
      if(allocated(obsTimeStr(tt)%obsStr))      deallocate(obsTimeStr(tt)%obsStr)
   end do
end if

#ifdef HYDRO_D
if(my_id .eq. io_id) then
   call nudging_timer(endCodeTime)
   call accum_nudging_time(startCodeTime, endCodeTime, &
                           'io_id shift left', .false.)
endif
if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D

  
!if(.NOT. all(obsTimeStr(:)%time .EQ. obsTimes)) &
!     call hydro_stop("obsTimeStr(:)%times not what they should be. Please investigate.")

! Updates obs already in memory if flag is set.
! Not going to be used for IOC. This will happen later. 
! Should the frequency be in model time or in real time? Probably real time
! and will require a clock time of 'obsLast' checked in obsTimeStr?
! call check_for_new_obs()

! Read in obs at times not already checked, eg. new obs this window
! Obs not already checked have updateTime='none'
! Obs already checked for with: 
!  : a missing file have updateTime='no file'
!  : no observations in the domain have updateTime='no obs'.
#ifdef MPP_LAND
if(my_id .eq. io_id) then 
#endif
   nWhObsMiss = size(obsTimeStr(:)%updateTime)
   allocate(theMask(nWhObsMiss),  &
            whObsMiss(nWhObsMiss) )
   do ii=1,size(obsTimeStr(:)%updateTime) 
      theMask(ii) = trim(obsTimeStr(ii)%updateTime) .eq. 'none'
   end do
   call whichLoop(theMask, whObsMiss, nObsMiss)
   deallocate(theMask)
#ifdef MPP_LAND
end if

#ifdef HYDRO_D
if(my_id .eq. io_id) then
   call nudging_timer(endCodeTime)
   call accum_nudging_time(startCodeTime, endCodeTime, &
                           'io_id basic info', .false.)
endif
if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D


! Broadcast the basic info above.
call mpp_land_bcast_int1(nWhObsMiss)
if(my_id .ne. io_id) then
   if(allocated(whObsMiss)) deallocate(whObsMiss)
   allocate(whObsMiss(nWhObsMiss))
endif
call mpp_land_bcast_int1d(whObsMiss)
call mpp_land_bcast_int1(nObsMiss)
#endif

#ifdef HYDRO_D
if(my_id .eq. io_id) then
   call nudging_timer(endCodeTime)
   call accum_nudging_time(startCodeTime, endCodeTime, &
        'bcast basic info', .false.)
endif
if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D


!bring in timeslice files
do ii=1,nObsMiss ! if nObsMiss is zero, this loop is skipped?

#ifdef MPP_LAND
   if(my_id .eq. io_id) then 
   !if(my_id .eq. mod(ii,numprocs)) then  !! this would give parallel IO
#endif
      tt = whObsMiss(ii)
      ! set/reset this obsTime
      obsTimeStr(tt)%time = obsTimes(tt)
      obsTimeStr(tt)%updateTime = 'none'
      !! This might be paranoid/overkill... 
      if(allocated(obsTimeStr(tt)%allCellInds)) deallocate(obsTimeStr(tt)%allCellInds)
      if(allocated(obsTimeStr(tt)%nGageCell))   deallocate(obsTimeStr(tt)%nGageCell)
      if(allocated(obsTimeStr(tt)%obsStr))      deallocate(obsTimeStr(tt)%obsStr)
      call timeslice_file_to_struct(tt) ! uses obsTimeStr%time to get file
      obsTimeStrAllocated = allocated(obsTimeStr(tt)%obsStr)
#ifdef MPP_LAND
   end if ! my_id .eq. io_id  -OR-  my_id .eq. mod(ii,numprocs)

#ifdef HYDRO_D
   print*,'Ndg: obsTimeStr(tt)%time: ',obsTimeStr(tt)%time
   print*,'Ndg: obstTimeStrAllocated: ', obsTimeStrAllocated
   call flush(6)
   if(my_id .eq. io_id) then
      call nudging_timer(endCodeTime)
      call accum_nudging_time(startCodeTime, endCodeTime, &
           'timeslice files io_id', .false.)
   endif
   if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D

   ! broadcast
   ! problem broadcasting here cant take adv of my_id .eq. mod(ii,numprocs)
   ! because it creates a block in the loop.
   ! Writing it efficiently outside the loop requires using something
   ! like mpi_get to get dimensions on a certain image.
   ! Here have to expose some of the guts of timeslice_file_to_struct
   ! Note nGages depends on ii/tt
   call mpp_land_bcast_int1(tt)
   call mpp_land_bcast_logical(obsTimeStrAllocated)
   if(obsTimeStrAllocated) then 
      if(my_id .eq. io_id) nGages=size(obsTimeStr(tt)%obsStr)
      call mpp_land_bcast_int1(nGages)
      if(my_id .ne. io_id) then
         if(allocated(obsTimeStr(tt)%obsStr)) deallocate(obsTimeStr(tt)%obsStr)
         allocate(obsTimeStr(tt)%obsStr(nGages))
      endif
      ! Variables in order assigned in the call in timeslice_file_to_struct
      call mpp_land_bcast_char(19,obsTimeStr(tt)%time)
      call mpp_land_bcast_char(19,obsTimeStr(tt)%updateTime)
      call mpp_land_bcast_char1d(obsTimeStr(tt)%obsStr(:)%usgsId)
      call mpp_land_bcast_char1d(obsTimeStr(tt)%obsStr(:)%obsTime)
      call mpp_land_bcast_real_1d(obsTimeStr(tt)%obsStr(:)%obsQC)
      call mpp_land_bcast_real_1d(obsTimeStr(tt)%obsStr(:)%obsDischarge)
   end if

print*,'Ndg: obsTimeStr: After obsTimeStr(:)%time: ', obsTimeStr(:)%time

#ifdef HYDRO_D
   if(my_id .eq. io_id) then
      call nudging_timer(endCodeTime)
      call accum_nudging_time(startCodeTime, endCodeTime, &
           'Bcast timeslice files', .false.)
   endif
   if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D

   if(my_id .eq. io_id) then 
   !if(my_id .eq. mod(ii,numprocs)) then
#endif
      if(obsTimeStrAllocated) then 
         call obs_static_to_struct(tt)      

#ifdef HYDRO_D
         if(my_id .eq. io_id) then
            call nudging_timer(endCodeTime)
            call accum_nudging_time(startCodeTime, endCodeTime, &
                 'obs_static_to_struct io_id', .false.)
         endif
         if(my_id .eq. io_id) call nudging_timer(startCodeTime)
         call flush(6)
#endif HYDRO_D

         call tally_affected_links(tt)
#ifdef HYDRO_D         
         !print*,'Ndg: naff0', my_id, size(obsTimeStr(tt)%allCellInds)
#endif
      endif

#ifdef HYDRO_D
      if(my_id .eq. io_id) then
         call nudging_timer(endCodeTime)
         call accum_nudging_time(startCodeTime, endCodeTime, &
              'Tally affected links io_id', .false.)
      endif
      if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D

#ifdef MPP_LAND
   end if ! my_id .eq. io_id  -OR-  my_id .eq. mod(ii,numprocs)
   ! broadcast
   ! problem broadcasting here cant take adv of my_id .eq. mod(ii,numprocs)
   ! because it creates a block in the loop.
   if(obsTimeStrAllocated) then
      ! Treat obs_static_to_strutct
      call mpp_land_bcast_int1d(obsTimeStr(tt)%obsStr(:)%obsStaticInd)
      call mpp_land_bcast_char1d(obsStaticStr(:)%usgsId)
      call mpp_land_bcast_int1d(obsStaticStr(:)%obsCellInd)
      call mpp_land_bcast_real_1d(obsStaticStr(:)%R)
      call mpp_land_bcast_real_1d(obsStaticStr(:)%G)
      call mpp_land_bcast_real_1d(obsStaticStr(:)%tau)

      nStatic = sum( (/ (1, cc=1,size(obsStaticStr(:)%obsCellInd)) /), &
                     mask=obsStaticStr(:)%obsCellInd .ne. 0            )
      do cc=1,nStatic
         if(my_id .eq. io_id) nCellsInR = size(obsStaticStr(cc)%cellsAffected)
         call mpp_land_bcast_int1(nCellsInR)
         if(my_id .ne. io_id) then
            if(allocated(obsStaticStr(cc)%cellsAffected)) deallocate(obsStaticStr(cc)%cellsAffected)
            if(allocated(obsStaticStr(cc)%dist))          deallocate(obsStaticStr(cc)%dist)
            if(allocated(obsStaticStr(cc)%ws))            deallocate(obsStaticStr(cc)%ws)
            allocate(obsStaticStr(cc)%cellsAffected(nCellsInR), &
                     obsStaticStr(cc)%dist(nCellsInR),          &
                     obsStaticStr(cc)%ws(nCellsInR)             )
         end if
         call mpp_land_bcast_int1d(obsStaticStr(cc)%cellsAffected)
         call mpp_land_bcast_real_1d(obsStaticStr(cc)%dist)
         call mpp_land_bcast_real_1d(obsStaticStr(cc)%ws)
      end do

      ! Treat tally_affected_links
      if(my_id .eq. io_id) nLinkAff = size(obsTimeStr(tt)%allCellInds)
      call mpp_land_bcast_int1(nLinkAff)
      call mpp_land_bcast_int1(nLinkAff)

      if(my_id .ne. io_id) then
         if(allocated(obsTimeStr(tt)%allCellInds)) deallocate(obsTimeStr(tt)%allCellInds)
         if(allocated(obsTimeStr(tt)%nGageCell))   deallocate(obsTimeStr(tt)%nGageCell)
         allocate(obsTimeStr(tt)%allCellInds(nLinkAff), &
                  obsTimeStr(tt)%nGageCell(nLinkAff)    )
      end if
      call mpp_land_bcast(obsTimeStr(tt)%allCellInds)
      call mpp_land_bcast(obsTimeStr(tt)%nGageCell)
#endif

#ifdef HYDRO_D
#ifdef MPP_LAND
      !if(my_id .eq. io_id) then 
      if(my_id .eq. -5) then 
#endif          
        print*,'Ndg: '
         print*,'Ndg: !-------------------------------------------------'
         print*,'Ndg: obsTimeStr(tt=',tt,')'
         print*,'Ndg: obsTimeStr(tt)%time:',        obsTimeStr(tt)%time
         print*,'Ndg: obsTimeStr(tt)%updateTime:',  obsTimeStr(tt)%updateTime
         print*,'Ndg: obsTimeStr(tt)%allCellInds:', obsTimeStr(tt)%allCellInds
         print*,'Ndg: obsTimeStr(tt)%nGageCell:',   obsTimeStr(tt)%nGageCell
         if(trim(obsTimeStr(tt)%updateTime(1:7)) .ne. 'no file') then
            print*,'Ndg: nobs:', size(obsTimeStr(tt)%obsStr)
            print*,'Ndg: obsTimeStr(tt)%obsStr(1)%usgsId:',       obsTimeStr(tt)%obsStr(1)%usgsId
            print*,'Ndg: obsTimeStr(tt)%obsStr(1)%obsTime:',      obsTimeStr(tt)%obsStr(1)%obsTime
            print*,'Ndg: obsTimeStr(tt)%obsStr(1)%obsDischarge:', obsTimeStr(tt)%obsStr(1)%obsDischarge
            print*,'Ndg: obsTimeStr(tt)%obsStr(1)%obsQC:',        obsTimeStr(tt)%obsStr(1)%obsQC
            print*,'Ndg: obsTimeStr(tt)%obsStr(1)%obsStaticInd:', obsTimeStr(tt)%obsStr(1)%obsStaticInd
         end if
         print*,'Ndg: !-------------------------------------------------'
         print*,'Ndg: '
#ifdef MPP_LAND
      end if ! (my_id .eq. io_id)
   endif ! (obsTimeStrAllocated)
#endif
#endif !HYDRO_D
end do ! ii 

#ifdef HYDRO_D
            if(my_id .eq. io_id) then
               call nudging_timer(endCodeTime)
               call accum_nudging_time(startCodeTime, endCodeTime, &
                                       'Bcast tally affected links io_id', .false.)
            endif
#endif HYDRO_D


#ifdef HYDRO_D
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif 
print*,'Ndg: finish setup_stream_nudging'
#endif

end subroutine setup_stream_nudging

!===================================================================================================
! Program Names: 
!   timeslice_file_to_struct
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Read a timeslice file, subset to observations in the domain, put into the 
!   obsTimeStr(timeIndex)%obsStr where obsTimeStr(timeIndex)%time impiles the
!   file being read in and timeIndex is the only argument to this subroutine.
! History Log: 
!   7/23/15 -Created, JLM.
! Usage:
! Parameters: 
!  timeIndex: the index corresponding to time in the obsTimeStr
! Input Files:  Specified argument. 
! Output Files: None.
! Condition codes: 
! User controllable options: 
!   Namelist option, logical "filterObsToDom". This removes observations which are not in the 
!   domain. May be useful for running small domains using large data files. 
! Notes:

subroutine timeslice_file_to_struct(structIndex)
use module_nudging_utils, only: whichInLoop,         &
                                accum_nudging_time,  &
                                nudging_timer

use module_nudging_io,    only: find_timeslice_file, &
                                read_timeslice_file, &
                                get_netcdf_dim

implicit none
integer, intent(in) :: structIndex

character(len=19)  :: thisTime  !! the of this time/slice of the structure
character(len=256) :: fileName  !! the corresponding obs file.

integer :: nGages
integer, allocatable, dimension(:) :: whSliceInDom
integer :: nWhSliceInDom, count, ww

character(len=19)  :: timeIn, updateTimeIn  !! the time of file/slice and when it was updated
character(len=2)   :: sliceResoIn           !! the temporal resolution of the slice
character(len=19), allocatable, dimension(:) :: gageTimeIn      !! 
real,              allocatable, dimension(:) :: gageQCIn        !! 
character(len=15), allocatable, dimension(:) :: usgsIdIn        !! USGS ID
real,              allocatable, dimension(:) :: gageDischargeIn !! m3/s

#ifdef HYDRO_D
real :: startCodeTime, endCodeTime
#endif HYDROD

#ifdef HYDRO_D
print*,'Ndg: start timeslice_file_to_struct'
#endif 

#ifdef HYDRO_D
      if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D

! Is there a timeslice file?
thisTime = obsTimeStr(structIndex)%time
fileName = find_timeslice_file(thisTime, obsResolution)

! If no file, note in updateTime and get out!
if(fileName .eq. '') then
   obsTimeStr(structIndex)%updateTime='no file'
#ifdef HYDRO_D
   print*,'Ndg: no timeSliceFile at this time: ', thisTime
#endif
   return 
end if

#ifdef HYDRO_D
print*,'Ndg: Found file:', fileName
print*,'Ndg: timeSlice: ',thisTime
print*,'Ndg: timeSliceFile:',trim(fileName)
call flush(6)
#endif

nGages=get_netcdf_dim(fileName, 'stationIdInd', 'timeslice_file_to_struct')

!! Reduce to just the observations in the domain or not?
if(filterObsToDom) then 
   
   ! Bring in the full file to intermediate local variables.
   !! JLM:: it would probably be more efficient to do this when readingin the netcdf files. 
   !! This capability is probably not needed for IOC, so I'm not doing it now.
   allocate(usgsIdIn(nGages),       & 
            gageTimeIn(nGages),     & 
            gageQCIn(nGages),       & 
            gageDischargeIn(nGages) )

#ifdef HYDRO_D
      if(my_id .eq. io_id) then
         call nudging_timer(endCodeTime)
         print*
         call accum_nudging_time(startCodeTime, endCodeTime, &
                                 'ts_file_to_struct above read in', .false.)
      endif
      if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D
   
   call read_timeslice_file(fileName,            &
                            timeIn,              &
                            updateTimeIn,        &
                            sliceResoIn,         &
                            usgsIdIn,            & 
                            gageTimeIn,          & 
                            gageQCIn,            & 
                            gageDischargeIn      )

#ifdef HYDRO_D
      if(my_id .eq. io_id) then
         call nudging_timer(endCodeTime)
         call accum_nudging_time(startCodeTime, endCodeTime, &
                                 'ts_file_to_struct below read in', .false.)
      endif
      if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D


   if(timeIn .NE. obsTimeStr(structIndex)%time) &
        call hydro_stop('timeslice_file_to_struct: file time does not match structure')
   if(obsResolution .NE. sliceResoIn) &
        call hydro_stop('timeslice_file_to_struct: model and file timeslice resolution do not match.')
   !save when the file was updated
   obsTimeStr(structIndex)%updateTime = updateTimeIn

   allocate(whSliceInDom(size(usgsIdIn)))
   !! parallelize this? !!
   call whichInLoop(usgsIdIn, nodeGageStr%usgsId, &
                    whSliceInDom, nWhSliceInDom)

#ifdef HYDRO_D
!   print*,'Ndg: usgsIdIn: ',         usgsIdIn
!   print*,'Ndg: nodeGageStr%usgsId', nodeGageStr%usgsId
   print*,'Ndg: nWhSliceInDom:',      nWhSliceInDom
#endif
   allocate(obsTimeStr(structIndex)%obsStr(nWhSliceInDom))
   !! because these dont get set here, give them default values.
   obsTimeStr(structIndex)%obsStr%obsStaticInd = 0
   obsTimeStr(structIndex)%obsStr%innov = -9999

   if(nWhSliceInDom .gt. 0) then 
      count=1
      do ww=1,size(whSliceInDom)
         if(whSliceInDom(ww) .gt. 0) then
            obsTimeStr(structIndex)%obsStr(count)%usgsId       = usgsIdIn(ww)
            obsTimeStr(structIndex)%obsStr(count)%obsTime      = gageTimeIn(ww)
            obsTimeStr(structIndex)%obsStr(count)%obsQC        = gageQCIn(ww)
            obsTimeStr(structIndex)%obsStr(count)%obsDischarge = gageDischargeIn(ww)
#ifdef HYDRO_D
            print*,obsTimeStr(structIndex)%obsStr(count)%usgsId
            print*,obsTimeStr(structIndex)%obsStr(count)%obsTime
            print*,obsTimeStr(structIndex)%obsStr(count)%obsQC
            print*,obsTimeStr(structIndex)%obsStr(count)%obsDischarge
#endif
            count=count+1
         else 
            !! the gage is not in the domain/parameter file, record that this 
            !! gage is available but unable to be assimilated.
            call accumulate_nwis_not_in_nhd(usgsIdIn(ww))
         end if
      end do
   end if
      
   deallocate(whSliceInDom)
   deallocate(usgsIdIn, gageTimeIn, gageQCIn, gageDischargeIn)

else ! dont filterObsToDom
   
   ! not reducing the obs file to the domain, can
   ! bring in the full file directly to the structure.
   allocate(obsTimeStr(structIndex)%obsStr(nGages))
   obsTimeStr(structIndex)%obsStr%obsStaticInd = 0
   obsTimeStr(structIndex)%obsStr%innov = -9999

   call read_timeslice_file(fileName,                                       &
                            timeIn,                                         &
                            obsTimeStr(structIndex)%updateTime,             &
                            sliceResoIn,                                    &
                            obsTimeStr(structIndex)%obsStr(:)%usgsId,       &
                            obsTimeStr(structIndex)%obsStr(:)%obsTime,      &
                            obsTimeStr(structIndex)%obsStr(:)%obsQC,        &
                            obsTimeStr(structIndex)%obsStr(:)%obsDischarge  ) 

   if(timeIn .NE. obsTimeStr(structIndex)%time) &
        call hydro_stop('timeslice_file_to_struct: file time does not match that in obsTimeStr')
   if(obsResolution .NE. sliceResoIn) &
        call hydro_stop('timeslice_file_to_struct: model and file timeslice resolution do not match.')

endif !filterObsToDom


#ifdef HYDRO_D
      if(my_id .eq. io_id) then
         call nudging_timer(endCodeTime)
         call accum_nudging_time(startCodeTime, endCodeTime, &
              'ts_file_to_struct bottom of routine', .false.)
      endif
#endif HYDRO_D


#ifdef HYDRO_D
print*,'Ndg: finish timeslice_file_to_struct'
#endif 

end subroutine timeslice_file_to_struct

!===================================================================================================
! Program Name: 
!   obs_static_to_struct
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   For new obs read in from file, determine the associate static data:
!   link index, parameters, cellsAffected, distances, and weights.
! History Log: 
! 8/5/15 -Created, JLM.
! Usage:
! Parameters: 
! Input Files:  Specified argument. 
! Output Files: None.
! Condition codes: 
! User controllable options: 
! Notes:

subroutine obs_static_to_struct(timeIndex)
use module_nudging_utils, only: whUniLoop &
#ifdef HYDRO_D
                        , accum_nudging_time, nudging_timer
#endif HYDRO_D

implicit none
integer, intent(in) :: timeIndex
integer :: nObs, nObsStaticPrev, count, oo, cc, whParams, ii
integer :: staticInd, nCellsInR
logical :: newStaticCount
logical, allocatable, dimension(:) :: whCalcStatic, theMask
integer, allocatable, dimension(:) :: whStaticInd, whObsInd
integer, dimension(maxGageDim) :: upAllInds,  downAllInds   ! the collected inds/links
real,    dimension(maxGageDim) :: upAllDists, downAllDists  ! distance at each collected ind
integer                        :: upLastInd,  downLastInd   ! number of inds/links collected so far

#ifdef HYDRO_D
real :: startCodeTime, endCodeTime

if(my_id .eq. io_id) call nudging_timer(startCodeTime)
print*,'Ndg: start obs_static_to_struct'
#endif HYDRO_D

! number of obs at this time
nObs = size(obsTimeStr(timeIndex)%obsStr)

! which obs need their static information created/calculated and
! what is their index in obsStaticStr?
allocate(whCalcStatic(nObs), whStaticInd(nObs), whObsInd(nObs))
whCalcStatic = .FALSE.
whStaticInd = 0
whObsInd    = 0

! how many existing entries in obsStatic?
allocate(theMask(size(obsStaticStr%usgsId)))
theMask = obsStaticStr%usgsId .ne. missingGage
nObsStaticPrev = sum((/ (1, ii=1,size(obsStaticStr%usgsId)) /), mask=theMask)
deallocate(theMask)

#ifdef HYDRO_D
print*,'Ndg: nObsStaticPrev', nObsStaticPrev
#endif

! Solve which observations need their static data generated. 
! (Do this in a serial loop up-front so that the actual calculations 
!  can be done in parallel.)
count=0
do oo=1,nObs
   
   ! If the gage is not in the parameter file, skip it
   if(.not. any(nudgingParamsStr%usgsId .eq. obsTimeStr(timeIndex)%obsStr(oo)%usgsId)) then
      !! If you ended up here, then filterObsToDom should not be on... 
      if(filterObsToDom) call hydro_stop('obs_static_to_struct: logical clash with filterObsToDom')
      call accumulate_nwis_not_in_nhd(obsTimeStr(timeIndex)%obsStr(oo)%usgsId)
      cycle
   endif

   ! Has the static information already been calculated for this usgsID?
   if(any(obsStaticStr%usgsId .eq. obsTimeStr(timeIndex)%obsStr(oo)%usgsId)) then
      allocate(theMask(size(obsStaticStr%usgsId))) 
      theMask = obsStaticStr%usgsId .eq. obsTimeStr(timeIndex)%obsStr(oo)%usgsId
      obsTimeStr(timeIndex)%obsStr(oo)%obsStaticInd = whUniLoop(theMask)
      deallocate(theMask)
#ifdef HYDRO_D
      print*,'Ndg: static prev solved:', obsTimeStr(timeIndex)%obsStr(oo)%usgsId
      print*,'Ndg: static prev above Ind:', obsTimeStr(timeIndex)%obsStr(oo)%obsStaticInd
#endif HYDRO_D
   else 
      whCalcStatic(oo) = .TRUE.
      count=count+1
      whStaticInd(oo) = nObsStaticPrev + count
      whObsInd(count) = oo
#ifdef HYDRO_D
      print*,'Ndg: static solving:', obsTimeStr(timeIndex)%obsStr(oo)%usgsId
      print*,'Ndg: static solving above Ind:', whStaticInd(oo)
#endif HYDRO_D
   end if

end do

#ifdef HYDRO_D
      if(my_id .eq. io_id) then
         call nudging_timer(endCodeTime)
         call accum_nudging_time(startCodeTime, endCodeTime, &
                                 'after: already solved static 4 gage?', .false.)
      endif
      if(my_id .eq. io_id) call nudging_timer(startCodeTime)
#endif HYDRO_D

!|||parallelize this|||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Create new entries in  obsStaticStr.      
do cc=1,count  ! not executed if count is zero
   oo = whObsInd(cc)
   staticInd = whStaticInd(oo)

   if(staticInd .gt. maxGageDim) &
        call hydro_stop('obs_static_to_struct: staticInd exceeds hardcoded maxGageDim, increase it!')

   !---------------------
   ! Assign the previously solved index in the obsStatic structure
   obsTimeStr(timeIndex)%obsStr(oo)%obsStaticInd = staticInd

   !---------------------
   ! Find and get the location/link information
   allocate(theMask(size(nodeGageStr%usgsId)))
   theMask = nodeGageStr%usgsId .eq. obsTimeStr(timeIndex)%obsStr(oo)%usgsId
   if(any(theMask)) then
      obsStaticStr(staticInd)%obsCellInd = nodeGageStr%nodeId(whUniLoop(theMask))
      obsStaticStr(staticInd)%usgsId     = obsTimeStr(timeIndex)%obsStr(oo)%usgsId
      deallocate(theMask)
   else 
      deallocate(theMask)
      cycle
   end if

   !---------------------
   ! Find and get the parameters
   allocate(theMask(size(nudgingParamsStr%usgsId)))
   theMask = nudgingParamsStr%usgsId .eq. obsTimeStr(timeIndex)%obsStr(oo)%usgsId
   whParams = whUniLoop(theMask)
   deallocate(theMask)
   obsStaticStr(staticInd)%R   = nudgingParamsStr%R(whParams)
   obsStaticStr(staticInd)%G   = nudgingParamsStr%G(whParams)
   obsStaticStr(staticInd)%tau = nudgingParamsStr%tau(whParams)

   !---------------------
   ! Calculate cells affected by this gage
   ! upstream to R keep distances
   upLastInd = 0
   call distance_along_channel(                &
        upNetwkStr,                            & ! traversal structure in up/down direction
        obsStaticStr(staticInd)%obsCellInd,    & ! the starting link 
        0.0000000000,                          & ! distance (m) at the starting node (this iter.)
        obsStaticStr(staticInd)%R,             & ! to traverse, in meters.
        upAllInds,                             & ! collected links/inds
        upAllDists,                            & ! distance to collected links
        upLastInd                              ) ! index of last collected link in above 2 arrays.

   ! downstream to R keep distances
   downLastInd = 0
   call distance_along_channel(                &
        downNetwkStr,                          & ! traversal structure in up/down direction
        obsStaticStr(staticInd)%obsCellInd,    & ! the starting link 
        0.0000000000,                          & ! distance (m) at the starting node (this iter.)
        obsStaticStr(staticInd)%R,             & ! to traverse, in meters.
        downAllInds,                           & ! collected links/inds
        downAllDists,                          & ! distance to collected links
        downLastInd                            ) ! index of last collected link in above 2 arrays.

   ! Collect the up and down stream cells
   nCellsInR = 1 + upLastInd + downLastInd
   allocate(obsStaticStr(staticInd)%cellsAffected(nCellsInR), &
            obsStaticStr(staticInd)%dist(nCellsInR),          &
            obsStaticStr(staticInd)%ws(nCellsInR)             )

   obsStaticStr(staticInd)%cellsAffected(1)                       = obsStaticStr(staticInd)%obsCellInd
   obsStaticStr(staticInd)%cellsAffected(2:(upLastInd+1))         = upAllInds(1:upLastInd)
   obsStaticStr(staticInd)%cellsAffected((upLastInd+2):nCellsInR) = downAllInds(1:downLastInd)

   ! Distance is optional in obsStaticStr. Right now keeping it for diagnostic potential.
   ! May replace with a local variable later when we dont want to keep it.
   obsStaticStr(staticInd)%dist(1)                       = 0
   obsStaticStr(staticInd)%dist(2:(upLastInd+1))         = -1. * upAllDists(1:upLastInd)
   obsStaticStr(staticInd)%dist((upLastInd+2):nCellsInR) = downAllDists(1:downLastInd)

   ! Calculate the cressman spatial weights
   obsStaticStr(staticInd)%ws = &
        ( obsStaticStr(staticInd)%R**2 - obsStaticStr(staticInd)%dist**2 ) / &
        ( obsStaticStr(staticInd)%R**2 + obsStaticStr(staticInd)%dist**2 ) 

#ifdef HYDRO_D
!if (my_id .eq. -5) then
if( any( (/ 981, 2242, 2845, 2946, 3014, 3066, 3068, 3072, 3158, 3228, 3325, 3328, 3330, 3353, &
           3374, 3398, 3416, 3445, 3487, 3536, 3621, 3667, 13554, 13651, 15062, 15311, 17450, 17394 /) &
           .eq. obsStaticStr(staticInd)%obsCellInd ) )then
   print*,'Ndg: '
   print*,'Ndg: !----------------------------------'
   print*,'Ndg: ! obsStaticStr(',staticInd,'):'
   print*,'Ndg: obsStaticStr(staticInd)%usgsId: ',        obsStaticStr(staticInd)%usgsId
   print*,'Ndg: obsStaticStr(staticInd)%obsCellInd: ',    obsStaticStr(staticInd)%obsCellInd
   print*,'Ndg: obsStaticStr(staticInd)%R: ',             obsStaticStr(staticInd)%R
   print*,'Ndg: obsStaticStr(staticInd)%G: ',             obsStaticStr(staticInd)%G
   print*,'Ndg: obsStaticStr(staticInd)%tau: ',           obsStaticStr(staticInd)%tau
   print*,'Ndg: obsStaticStr(staticInd)%cellsAffected: ', obsStaticStr(staticInd)%cellsAffected
   print*,'Ndg: obsStaticStr(staticInd)%dist: ',          obsStaticStr(staticInd)%dist
   print*,'Ndg: obsStaticStr(staticInd)%ws: ',            obsStaticStr(staticInd)%ws
   print*,'Ndg: !----------------------------------'
   print*,'Ndg: '
endif
#endif

end do

deallocate(whCalcStatic, whStaticInd, whObsInd)

#ifdef HYDRO_D
if(my_id .eq. io_id) then
   call nudging_timer(endCodeTime)
   call accum_nudging_time(startCodeTime, endCodeTime, &
        'after solved static 4 gage', .false.)
endif
print*,'Ndg: finish obs_static_to_struct'
call flush(6)
#endif HYDRO_D

end subroutine obs_static_to_struct


!===================================================================================================
! Program Name: 
!   accumulate_nwis_not_in_nhd
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Keep a running, non-redundant log of gages seen in the forecast cycle 
!   (from nwis) which were not collocated by us and/or by NHD. 
! History Log: 
!   11/4/15 - Created, JLM.
! Usage:
! Parameters: 
! Input Files: 
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine accumulate_nwis_not_in_nhd( gageId )
character(len=15) :: gageId
if(.not. any(nwisNotNhd .eq. gageId)) then
   nwisNotNhdCount=nwisNotNhdCount+1
   if(nwisNotNhdCount .gt. maxNwisNotNhdCount) &
        call hydro_stop('accumulate_nwis_not_in_nhd: gages from NWIS not found ' // &
                        'in NHD exceeding the hardcoded limit, maxNwisNotNhdCount')
   nwisNotNhd(nwisNotNhdCount)=gageId
end if
end subroutine accumulate_nwis_not_in_nhd


!===================================================================================================
! Program Name: 
!   distance_along_channel
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Traverse the gridded channel network up or down stream, return the cumulative distance
!   and indices of points visited. Going from one link to the next counts half the length 
!   of each as the distance. Conceptually that's midpoint to midpoint though there's really 
!   no midpoint. If half the length to the first midpoint exceeds R, no index is returned. 
!   That is, lastInd is zero.

! History Log: 
!   6/4/15 - Created, JLM.
!   8/7/15 - Heavily refactored to remove searching, JLM.
! Usage:
! Parameters: 
!   See formal arguments and their declarations
! Input Files:  Specified argument. 
! Output Files: None.
! Condition codes: 
! User controllable options: 
! Notes: 
!   The total number of links gathered is hardwired to 10,000 which is for both directions. In NHD+
!   the shortest link is 1m. If you managed to traverse just the shortest 10,000 links then you only
!   go [R language: > sum(head(sort(reInd$length),10000))/1000 = 60.0745] 60km. This is the minimum
!   upper bound on R implied by the choice of 10,000. If lastInd ever becomes 10,001 a fatal error
!   is issued.

recursive subroutine distance_along_channel(direction, & ! traversal structure in up/down direction
                                            startInd,  & ! the starting link 
                                            startDist, & ! distance at the starting node (this iter.)
                                            radius,    & ! to traverse, in meters.
                                            allInds,   & ! collected links/inds
                                            allDists,  & ! distance to collected links
                                            lastInd    ) ! index of last collected link in above.
use module_RT_data,  only: rt_domain

implicit none
type(netwkReExpStructure), intent(in) :: direction ! up/down NtwkStr with tarversal inds, pass by ref!
integer, intent(in)    :: startInd  ! the starting link
real,    intent(in)    :: startDist ! distance at the starting node (for this iteration)
real,    intent(in)    :: radius    ! in meteres
integer, intent(inout), dimension(10000) :: allInds   ! the collected inds/links
real,    intent(inout), dimension(10000) :: allDists  ! the distance at each collected ind
integer, intent(inout) :: lastInd   ! the number of inds/links collected so far.

! whGo is only > 1 upstream with a current max of 17 in NHD+
integer :: go, nGo 
integer :: gg
real    :: newDist
integer, parameter :: did=1

#ifdef HYDRO_D
!print*,'Ndg: start distance_along_channel'
#endif

if(direction%end(startInd) .eq. 0) return ! a pour point (downstream) or a 1st order link (upstream)

nGo = direction%end(startInd) - direction%start(startInd)
nGo = nGo + 1  ! end-start+1 if end-start > 0
do gg=0,nGo-1
   go = direction%go( direction%start(startInd) + gg )  
   allInds(lastInd+1) = go
#ifdef MPP_LAND
   newDist = ( chanlen_image0(startInd) + chanlen_image0(go) ) / 2.
#else 
   newDist = ( rt_domain(did)%chanlen(startInd) + rt_domain(did)%chanlen(go) ) / 2.
#endif 
   if(startDist + newDist .gt. radius) return  ! strictly greater than.
   allDists(lastInd+1) = startDist + newDist
   lastInd = lastInd+1
   if(lastInd .ge. 10001) &
        call hydro_stop('distance_along_channel: hardwired 10000 variable size exceeded. FIX.')
   call distance_along_channel( &
        direction,              &  ! the traversal structure, pass by reference.
        go,                     &  ! the new startInd is where we go from the old startInd 
        startDist+newDist,      &  ! a little bit further is where we start the next call.
        radius,                 &  ! static
        allInds,                &  ! collected inds
        allDists,               &  ! collected dists
        lastInd                 )  ! the number collected so far
end do

#ifdef HYDRO_D
print*,'Ndg: end distance_along_channel'
call flush(6)
#endif

end subroutine distance_along_channel

!===================================================================================================
! Program Name: 
!   tally_affected_links
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
! History Log: 
!   8/11/15
! Usage: call tally_affected_links(tt)
! Parameters: 
! Input Files:  None.
! Output Files: None.
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine tally_affected_links(timeIndex)
use module_namelist, only: nlst_rt
use module_RT_data,  only: rt_domain

implicit none
integer, intent(in) :: timeIndex
integer, allocatable, dimension(:) :: affectedInds, nGageAffect
integer :: nlinks, ii, oo, cc, nLinkAff, indAff, staticInd
integer, parameter :: did=1

if (nlst_rt(did)%channel_option .eq. 3) nLinks = RT_DOMAIN(did)%NLINKS ! For gridded channel routing
if (nlst_rt(did)%channel_option .eq. 1 .or.   &
    nlst_rt(did)%channel_option .eq. 2      ) &
    nLinks = &
#ifdef MPP_LAND
             RT_DOMAIN(did)%gNLINKSL  ! For reach-based routing in parallel
#else 
             RT_DOMAIN(did)%NLINKSL   ! For reach-based routing                       
#endif

allocate(affectedInds(nLinks), nGageAffect(nLinks))

print*,'Ndg: JLM: nlinks: ', nlinks
call flush(6)

affectedInds = 0
nGageAffect  = 0

! parallelize this ||||||||||||||||||||||||||||||||||||||||
do oo=1,size(obsTimeStr(timeIndex)%obsStr)
   staticInd = obsTimeStr(timeIndex)%obsStr(oo)%obsStaticInd
   if(staticInd .eq. 0) cycle
   nLinkAff = size(obsStaticStr(staticInd)%cellsAffected)
   do cc=1, nLinkAff
      indAff = obsStaticStr(staticInd)%cellsAffected(cc)
      affectedInds(indAff) = indAff
      nGageAffect(indAff) = nGageAffect(indAff) + 1
   end do
end do

! how many total cells affected?
nLinkAff = sum((/(1, ii=1,nLinks)/), mask=affectedInds .ne. 0)

#ifdef HYDRO_D
print*,'Ndg: naff-1',nLinkAff
call flush(6)
#endif

allocate( obsTimeStr(timeIndex)%allCellInds(nLinkAff), &
          obsTimeStr(timeIndex)%nGageCell(nLinkAff)    )

obsTimeStr(timeIndex)%allCellInds = pack(affectedInds, mask=affectedInds .ne. 0)
obsTimeStr(timeIndex)%nGageCell   = pack(nGageAffect,  mask=nGageAffect .ne. 0)

deallocate(affectedInds, nGageAffect)

#ifdef HYDRO_D
print*,'Ndg: end tally_affected_links'
call flush(6)
#endif

end subroutine tally_affected_links


!===================================================================================================
! program name: 
!   x
! author(s)/contact(s): 
!   y
! abstract: 
!   compute the temporal weight factor for an observation
! history log: 
!   6/4/15 - created,
! usage:
! parameters: 
!   q
! input files:  
! output files: 
! condition codes: 
! user controllable options: 
! notes: 

real function time_wt(modelTime, tau, obsTime)
use module_date_utils_nudging, only: geth_idts
implicit none
character(len=19), intent(in)  :: modelTime   ! model time string
!Do we need to subtract this from modelTime? Model should be advanced to model time
!before called.
!real,              intent(in)  :: dtMin       ! model timestep  (seconds)
real,              intent(in)  :: tau         ! tau half window (minutes)
character(len=19), intent(in)  :: obsTime     ! observation time string
! local variables
integer :: timeDiff
!!$real :: fdtim            ! reference time (minutes)
!!$real :: tw1              ! half of twindo, scaled, in minutes
!!$real :: tw2              ! twindo, scaled, in minutes
!!$real :: tconst           ! reciprical of tw1
!!$real :: ttim             ! obtime in minutes
!!$real :: dift             ! | fdtim-ttim |
!!$!real :: time_wt           ! returned weight
!! returned timeDiff is in seconds
call geth_idts(obsTime, modelTime, timeDiff) 
timeDiff = timeDiff / 60.
time_wt = 0. ! = if(abs(timeDiff) .gt. tau)
if(abs(timeDiff) .lt. tau/2.)       time_wt = 1.
if(abs(timeDiff) .ge. tau/2. .and. & 
   abs(timeDiff) .le. tau      ) time_wt = (tau - abs(timeDiff)) / (tau/2.)
!!$! determine the time-weight factor for n
!!$fdtim=xtime-dtmin  !model time in minutes
!!$tw1=twindo*30.     ! twindo is in minutes
!!$tw2=twindo*60.
!!$tconst=float(1)/tw1
!!$time_wt=float(0)
!!$ttim=obtime*float(60) !obs in minutes
!!$
!!$dift=abs(fdtim-ttim)
!!$if(dift.le.tw1)time_wt=1.
!!$if(dift.gt.tw1.and.dift.le.tw2) then
!!$   if(fdtim.lt.ttim)time_wt=(fdtim-(ttim-tw2))*tconst
!!$   if(fdtim.gt.ttim)time_wt=((ttim+tw2)-fdtim)*tconst
!!$endif
end function time_wt

!!$!===================================================================================================
!!$! Program Name: 
!!$!   x
!!$! Author(s)/Contact(s): 
!!$!   y
!!$! Abstract: 
!!$!   z
!!$! History Log: 
!!$!   6/4/15 - Created,
!!$! Usage:
!!$! Parameters: 
!!$!   q
!!$! Input Files:  
!!$! Output Files: 
!!$! Condition codes: 
!!$! User controllable options: 
!!$! Notes: 
!!$real function cress(rr,rrmax)
!!$implicit none
!!$real,intent(in):: rr,rrmax
!!$if(rr .ge. rrmax)then
!!$   cress = 0.
!!$else
!!$   cress = (rrmax - rr)/(rrmax + rr) 
!!$endif
!!$end function cress


!===================================================================================================
! Program Name: 
!   x
! Author(s)/Contact(s): 
!   Wu YH
!   James McCreight >jamesmcc<>at<>ucar<>dot<>edu<
! Abstract: 
!   Calculate the innovations of obs.
! History Log: 
!   2015.07.15 - Created.
!   2015.09.19 
! Usage:
! Parameters: 
!   q
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine nudge_innov(discharge)
implicit none
real, dimension(:,:), intent(in) :: discharge !! modeled discharge (m3/s)
integer :: tt, oo, nObsTt, linkInd, staticInd
if(.not. allocated(obsTimeStr)) return
do tt=1,size(obsTimeStr)
   if(.not. allocated(obsTimeStr(tt)%obsStr)) cycle
   nObsTt = size(obsTimeStr(tt)%obsStr)  
   !! parallelize this?
   do oo=1,nObsTt
      staticInd = obsTimeStr(tt)%obsStr(oo)%obsStaticInd
      linkInd = obsStaticStr(staticInd)%obsCellInd
      obsTimeStr(tt)%obsStr(oo)%innov =                                      &
           ( obsTimeStr(tt)%obsStr(oo)%obsDischarge - discharge(linkInd,2) ) &
           *obsTimeStr(tt)%obsStr(oo)%obsQC
   enddo
end do
end subroutine nudge_innov

!!$
!===================================================================================================
! Program Name: 
!   x
! Author(s)/Contact(s): 
!   Wu YH
! Abstract: 
!   Calculate the nuding term for one 
! History Log: 
!   2015.07.21 - Created,
! Usage:
! Parameters: 
!   q
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine nudge_term_all(discharge, global_discharge, hydroAdv)
use module_RT_data,  only: rt_domain
#ifdef MPP_LAND
use MODULE_mpp_ReachLS,   only: linkls_s, linkls_e
#endif

implicit none
real, dimension(:,:), intent(inout) :: discharge !! modeled discharge (m3/s)
real, dimension(:,:), intent(inout) :: global_discharge !! modeled discharge (m3/s)
integer,              intent(in)    :: hydroAdv  !! number of seconds the channel model has advanced 

integer :: ll, startInd, endInd, checkInd
real :: theNudge

!! need to transform passed index to appropriate index for image
#ifdef MPP_LAND
startInd=linkls_s(my_id+1)
endInd  =linkls_e(my_id+1)
#else 
startInd=1
endInd  =RT_DOMAIN(did)%NLINKSL
#endif

#ifdef DO_CONCURRENT
do concurrent (ll=startInd:endInd)
#else
do ll=startInd, endInd  ! ll is in the index of the global Route_Link
#endif

#ifdef HYDRO_D
   checkInd = -3589
   if(ll .eq. checkInd) then
      print*,"Ndg: checkInd: checkInd: ",checkInd
      print*,'Ndg: checkInd: discharge(ll,2) before:',discharge(ll-startInd+1,2)
   end if
#endif 
   theNudge = nudge_term_link(ll-startInd+1, hydroAdv, global_discharge)
   discharge(ll-startInd+1,2) = discharge(ll-startInd+1,2) + theNudge
   !+ nudge_term_link(ll-startInd+1, hydroAdv, global_discharge)
#ifdef HYDRO_D
   if(ll .eq. checkInd) then
      print*,'Ndg: checkInd: theNudge:',theNudge
      print*,'Ndg: checkInd: discharge(ll,2) after:',discharge(ll-startInd+1,2)
   endif
#endif
end do

end subroutine nudge_term_all


!===================================================================================================
! Program Name: 
!   nudge_term_link
! Author(s)/Contact(s): 
!   Wu YH, 
!   James L McCreight jamesmcc><at><ucar><dot><edu
! Abstract: 
!   Calculate the nuding term for one model cell 
! History Log: 
!   2015.07.21 - Created,
! Usage:
! Parameters: 
!   q
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

#ifdef DO_CONCURRENT
pure function nudge_term_link(linkIndIn, hydroAdv, discharge)
#else
function nudge_term_link(linkIndIn, hydroAdv, discharge)
#endif
use module_nudging_utils,      only: whUniLoop
use module_namelist,      only: nlst_rt
use module_date_utils_nudging, only: geth_newdate
#ifdef MPP_LAND
use MODULE_mpp_ReachLS,   only: linkls_s, linkls_e
#endif

implicit none
integer,              intent(in) :: linkIndIn   !! stream cell index
integer,              intent(in) :: hydroAdv  !! number of seconds the channel model has advanced 
real, dimension(:,:), intent(in) :: discharge !! modeled discharge (m3/s)
real :: nudge_term_link

integer :: linkInd
character(len=19)  :: hydroTime !! actual time of the routine model
real :: theInnov, weighting 
integer :: tt, oo, ll, jj, nObsTt, staticInd, obsInd
logical, allocatable, dimension(:) :: theMask
#ifdef MPP_LAND
integer :: whImage, checkInd

!! need to transform passed index to appropriate index for image
linkInd = linkIndIn + linkls_s(my_id+1) - 1
#else
linkInd = linkIndIn
#endif
checkInd=-3589  ! see above

!! horrible, olddate is 2 lsm timesteps behind when the hydro model is run.
!call geth_newdate(hydroTime, nlst_rt(1)%olddate, hydroAdv+nint(nlst_rt(1)%dt))
call geth_newdate(hydroTime, lsmTimeMinusDt, hydroAdv+lsmDt)
!call geth_newdate(hydroTime, nlst_rt(did)%olddate, hydroAdv)

nudge_term_link = 0.
weighting = 0.

if(linkInd .eq. checkInd) print*,'Ndg: checkInd: hydroTime: ',hydroTime
print*,'Ndg: checkInd: -*-*-*-*-*-*-*-*-*-*-*-*'
print*,'Ndg: checkInd: hydroTime: ',hydroTime      
print*,'Ndg: linkInd', linkInd

do tt=1,size(obsTimeStr)

   if(.not. allocated(obsTimeStr(tt)%allCellInds)) cycle
   if(.not. any(obsTimeStr(tt)%allCellInds .eq. linkInd)) cycle
   if(.not. allocated(obsTimeStr(tt)%obsStr)) cycle

   do oo=1,size(obsTimeStr(tt)%obsStr)      

      staticInd = obsTimeStr(tt)%obsStr(oo)%obsStaticInd
      if(.not. any(obsStaticStr(staticInd)%cellsAffected .eq. linkInd)) cycle

      allocate(theMask(size(obsStaticStr(staticInd)%cellsAffected)))
      theMask = obsStaticStr(staticInd)%cellsAffected .eq. linkInd
      ll = whUniLoop(theMask)
      deallocate(theMask)

      obsInd = obsStaticStr(staticInd)%obsCellInd
! because obsInd could be on a different image and getting the data here might 
! slow down computation, especially if there is sync.
! note that we also dont know if the innov is based on the current time solution 
! at the link or the previous time solution at the link... it depends on if the 
! current link comes after or before then observed link, respectively.
! given that, I elect to just go with using the solution at the previous hydro 
! timestep. This means the streamflow is sync'd to image 0 with each iteration.
! then the innovations are broadcast to all the nodes. 
! conclusion: see module_channel_routing.F:drive_CHANNEL where the muskingum 
! loops on nsteps to fine the call to solve the innovations using a separate 
! subroutine.
! all of this makes vectorizing the nuding more attractive.... 
!#ifdef MPP_LAND
!      !! obsInd could be on a different image! 
!      allocate(theMask(size(linkls_s)))
!      theMask = linkInd .ge. linkls_s .and. linkInd .le. linkls_e
!      whImage = whUniLoop(theMask)-1
!      deallocate(theMask)
!      print*,'Ndg: whImage: ',whImage
!#endif

      theInnov =                                                            & 
           ( obsTimeStr(tt)%obsStr(oo)%obsDischarge - discharge(obsInd,2) ) &
           *obsTimeStr(tt)%obsStr(oo)%obsQC

      nudge_term_link = nudge_term_link                 &
           !+obsTimeStr(tt)%obsStr(oo)%innov             &
           +theInnov                                    &
           !*obsStaticStr(staticInd)%G                   &   !!JLM??
           *time_wt(hydroTime,                          &
                    obsStaticStr(staticInd)%tau,        &
                    obsTimeStr(tt)%obsStr(oo)%obsTime)  &
           *time_wt(hydroTime,                          &
                    obsStaticStr(staticInd)%tau,        &
                    obsTimeStr(tt)%obsStr(oo)%obsTime)  &
           *obsStaticStr(staticInd)%ws(ll)              &
           *obsStaticStr(staticInd)%ws(ll)
      
      weighting = weighting                             &
           +time_wt(hydroTime,                          & 
                    obsStaticStr(staticInd)%tau,        &
                    obsTimeStr(tt)%obsStr(oo)%obsTime)  &
           *obsStaticStr(staticInd)%ws(ll)     

#ifdef HYDRO_D
!if(linkInd .eq. checkInd) then 
!if( (obsTimeStr(tt)%time .eq. obsTimeStr(tt)%obsStr(oo)%obsTime) .and. &
!    ( abs(discharge(obsInd,2)+theInnov - obsTimeStr(tt)%obsStr(oo)%obsDischarge) .gt. .01) ) then
      print*,'Ndg: checkInd: -*-*-*-*-*-*-*-*-*-*-*-*'
      print*,'Ndg: checkInd: linkInd: ', linkInd
      print*,'Ndg: checkInd: tt: ', tt
      print*,'Ndg: checkInd: oo: ', oo
      print*,'Ndg: checkInd: obsCellInd: ', obsInd

      print*,'Ndg: checkInd: discharge: ',    discharge(obsInd,2)
      print*,'Ndg: checkInd: theInnov: ',theInnov

      print*,'Ndg: checkInd: obsDischarge: ', obsTimeStr(tt)%obsStr(oo)%obsDischarge 

      print*,'Ndg: checkInd: obsQC: ',        obsTimeStr(tt)%obsStr(oo)%obsQC
      print*,'Ndg: checkInd: usgsId: ',       obsTimeStr(tt)%obsStr(oo)%usgsId

      print*,'Ndg: checkInd: time_wt: ', time_wt(hydroTime,obsStaticStr(staticInd)%tau, &
                                  obsTimeStr(tt)%obsStr(oo)%obsTime      )
      print*,'Ndg: checkInd: time: ', obsTimeStr(tt)%time
      print*,'Ndg: checkInd: obsTime: ', obsTimeStr(tt)%obsStr(oo)%obsTime
      print*,'Ndg: checkInd: ws: ',      obsStaticStr(staticInd)%ws(ll)
      print*,'Ndg: checkInd: weighting: ', weighting
!endif
#endif

   enddo

enddo

if(abs(weighting) .gt. 1.E-8) then
#ifdef HYDRO_D
!!$   if(linkInd .eq. 4) then 
!!$      print*,'Ndg: nudge_term_link: ', nudge_term_link
!!$      print*,'Ndg: weighting: ', weighting
!!$      print*,'Ndg: nudge_term_link/weighting: ',nudge_term_link/weighting
!!$   end if
#endif
   nudge_term_link = nudge_term_link/weighting
else
   nudge_term_link = 0.
endif

end function nudge_term_link

!===================================================================================================
! Program Name: 
!   finish_stream_nudging
! Author(s)/Contact(s): 
!   James L McCreight, <jamesmcc><ucar><edu>
! Abstract: 
!   Finish off the nudging routines, memory and timing.
! History Log: 
!   8/20/15 - Created, JLM
! Usage:
! Parameters: 
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine finish_stream_nudging
use module_nudging_utils, only: totalNudgeTime
use module_nudging_io,    only: write_nwis_not_in_nhd
implicit none
if(nwisNotNhdCount .gt. 0) call write_nwis_not_in_nhd(nwisNotNhd, nwisNotNhdCount)
!! deallocate module variables here and time this
print*,'Ndg: *nudge timing* TOTAL NUDGING TIME (seconds): ', totalNudgeTime
end subroutine finish_stream_nudging

!===================================================================================================
! Program Name: 
!   x
! Author(s)/Contact(s): 
!   y
! Abstract: 
!   z
! History Log: 
!   6/4/15 - Created,
! Usage:
! Parameters: 
!   q
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

!!$subroutine get_up_down_reach (kcu,kup,kdn,kindex)
!!$!! find the up and downsteam of a given reach
!!$!! Author:: Wu YH
!!$!! Date  :: 2015.07.30 
!!$!!!===================================================================================================
!!$implicit none
!!$integer :: kcu   ! current reach
!!$integer :: kup   ! upstream number
!!$integer :: kdn   ! downstream number
!!$integer :: korder ! order of the channel
!!$integer,dimension(-kup:kdn)::kindex
!!$
!!$integer :: m,mcu,mup,mdn
!!$
!!$kindex = -999  ! default value
!!$kindex(kcu) = kcu
!!$korder = ORDER(kcu)
!!$
!!$mup = 0
!!$mcu = kcu
!!$#ifdef MPP_LAND
!!$101 continue
!!$mloop:   do m = 1, nToNodeInd
!!$   if(ORDER(m) .eq. korder) then    !--do the orders sequentially
!!$      if(gtoNODE(m) .eq. mcu) then
!!$         mup = mup - 1
!!$         kindex(mup) = m
!!$         mcu = m
!!$         go to 201
!!$      endif
!!$   endif
!!$enddo mloop ! loop over m
!!$go to 301
!!$201 continue 
!!$if (-mup .lt. kup) go to 101
!!$301 continue
!!$#else
!!$102 continue
!!$mloop:  do m = 1, NLINKSL
!!$   if(ORDER(m) .eq. korder) then    !--do the orders sequentially
!!$      if (TO_NODE(m) .eq. mcu ) then
!!$         mup = mup - 1
!!$         kindex(mup) = m
!!$         mcu = m
!!$         go to 202
!!$      endif
!!$   endif
!!$enddo mloop ! loop over m
!!$go to 302
!!$202 continue 
!!$if (-mup .lt. kup) go to 102
!!$302 continue
!!$#endif
!!$
!!$mdn = 0
!!$#ifdef MPP_LAND
!!$mcu = gtoNODE(kcu)     
!!$401 continue
!!$mloop:   do m = 1, nToNodeInd
!!$   if(ORDER(m) .eq. korder) then    !--do the orders sequentially
!!$      if(m .eq. mcu) then
!!$         mdn = mnd + 1
!!$         kindex(mdn) = m
!!$         mcu =gtoNODE(m)
!!$         go to 501
!!$      endif
!!$   endif
!!$enddo mloop ! loop over m
!!$go to 601
!!$501 continue 
!!$if ( mdn .lt. kdn) go to 401
!!$601 continue
!!$#else
!!$mcu = TO_NODE(kcu)
!!$402 continue
!!$mloop:  do m = 1, NLINKSL
!!$   if(ORDER(m) .eq. korder) then    !--do the orders sequentially
!!$      if (m .eq. mcu ) then
!!$         mdn = mdn + 1
!!$         kindex(mdn) = m
!!$         mcu = TO_NODE(m)
!!$         go to 502
!!$      endif
!!$   endif
!!$enddo mloop ! loop over m
!!$go to 602
!!$502 continue
!!$if (-mdn .lt. kdn) go to 402
!!$602 continue
!!$#endif
!!$
!!$return        
!!$end subroutine get_up_down_reach


!===================================================================================================
end module module_stream_nudging
