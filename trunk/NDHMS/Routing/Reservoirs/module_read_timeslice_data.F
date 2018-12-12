module module_read_timeslice_data

    !use module_namelist,      only: nlst_rt




    implicit none


!========================
! lastObs structure, corresponding to nudgingLastObs.YYYY-mm-dd_HH:MM:ss.nc
! How observations from the past are carried forward.
! This type is extended in module_stream_nudging
type lastObsStructure
   character(len=15)                            :: usgsId
   real,              allocatable, dimension(:) :: lastObsDischarge
   real,              allocatable, dimension(:) :: lastObsModelDischarge
   character(len=19), allocatable, dimension(:) :: lastObsTime
   real,              allocatable, dimension(:) :: lastObsQuality
end type lastObsStructure


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
! lastObs structure, corresponding to nudgingLastObs.YYYY-mm-dd_HH:MM:ss.nc
! How observations from the past are carried forward.
! Type defined in module_nudging_io.F
type(lastObsStructure), allocatable, dimension(:) :: lastObsStr

!========================
! This holds static information for a given gage for a given cycle (when R, G, and tau) do not
! change.
! The "lastObs" variables are not exactly static... they were an afterthought and this was
! by far the best place for them to live.
type, extends(lastObsStructure) :: obsStaticStructure
   !! Inherited components
   !!character(len=15) :: usgsId        ! the 15 char USGS identifier.
   !!real              :: lastObsDischarge(:)       ! last observed discharge
   !!character(len=19) :: lastObsTime(:)            ! time of last obs discharge (.le.hydroTime)
   !!real              :: lastObsQuality(:)         ! quality of the last obs discharge
   !!real              :: lastObsModelDischarge(:)  ! the modeled discharge value at the time of the last obs
   !! New components
   integer :: obsCellInd    ! index of obs on model channel network, for distance calc
   real    :: R, G, tau     ! the nudging parameters at this gage.
   integer, allocatable, dimension(:) ::  cellsAffected  ! indices of cells affected
   real,    allocatable, dimension(:) ::  dist           ! optional: dist to affected cells, optional
   real,    allocatable, dimension(:) ::  ws             ! spatial cressman weights at affected cells discharge.
end type obsStaticStructure

! The static obs/gage information - store here to perform calculations
! only once per cycle.
! Currently the dimensions of this are fixed to 10000 which should
! work for the forseeable future. May want to consider some routine for
! augmenting this size if necessary.
type(obsStaticStructure), allocatable, dimension(:) :: obsStaticStr

!! The number of up/down stream links which can be collected
!! using R to solve which links are "neighboring".
!! This value applies to both directions, it's the number
!! of links per R.
integer, parameter :: maxNeighLinksDim=5000

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
!! for dealloction purposes, might be better to put the dimension onthe derived type.
type nudgingParamStructure
   character(len=15), allocatable, dimension(:)     :: usgsId
   real,              allocatable, dimension(:)     :: R
   real,              allocatable, dimension(:)     :: G
   real,              allocatable, dimension(:)     :: tau
   real,              allocatable, dimension(:,:,:) :: qThresh  !! gage, month, nThresh
   real,              allocatable, dimension(:,:,:) :: expCoeff !! gage, month, nThresh
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
! Track gages from NWIS not in our param file and not in the intersection or
! the Route_Link gages and the gages in the parameter file.
integer, parameter :: maxNwisNotRLAndParamsCount=20000
character(len=15), dimension(maxNwisNotRLAndParamsCount) :: nwisNotRLAndParams
integer :: nwisNotRLAndParamsCount

!========================
! Random data
real, allocatable, dimension(:) :: t0Discharge

!========================
! Control book keeping
logical :: nudgeThisLsmTimeStep

!! JLM: these options are hardcoded/hardwired for now, evnt go in namelist
integer,            parameter :: obsResolutionInt = 15 ! minutes
real,               parameter :: obsCheckFreq     = 90 ! minutes
logical,            parameter :: filterObsToDom   = .TRUE.
character(len=15),  parameter :: missingGage = '               '
logical,            parameter :: nudgeWAdvance = .FALSE.
character(len=4),   parameter :: nudgingClockType = 'wall'
logical,            parameter :: sanityQcDischarge = .true.
logical,            parameter :: readTimesliceErrFatal = .false.
logical,            parameter :: futureLastObsFatal = .true.
logical,            parameter :: futureLastObsZero  = .false.
real,               parameter :: invDistTimeWeightExp = 5.000
real,               parameter :: noConstInterfCoeff   = 1.0 !0.500

! hydro.namelist: &NUDGING_nlist  variables.
character(len=256) :: nudgingParamFile
character(len=256) :: netwkReExFile
character(len=256) :: nudgingLastObsFile    !! passed by namelist
character(len=256) :: nudgingLastObsFileTry !! either the passed or, if not passed, a default
logical            :: readTimesliceParallel
logical            :: temporalPersistence
logical            :: persistBias
logical            :: biasWindowBeforeT0
integer            :: nTimesLastObs   !! aka nlst_rt(did)%nLastObs
integer            :: minNumPairsBiasPersist
integer            :: maxAgePairsBiasPersist
logical            :: invDistTimeWeightBias
logical            :: noConstInterfBias

!========================
! Space book keeping
logical :: nudgeSpatial = .true.  !! is spatial interpolation of nudging active?
integer,   parameter :: did=1     !! 1 for WRF-uncoupled runs...

!========================
! Time book keeping
real    :: maxTau
integer :: nObsTimes
character(len=19) :: lsmTime, initTime
integer :: lsmDt                                   !1234567890123456789
character(len=19), parameter :: missingLastObsTime='9999999999999999999'
character(len=2) :: obsResolution
logical :: gotT0Discharge

#ifdef HYDRO_D
integer, parameter :: flushUnit=6
logical, parameter :: flushAll=.true.
#endif

!========================
! Parallel book keeping
real, allocatable, dimension(:) :: chanlen_image0    !A global version kept on image 0












    type :: read_timslice_data_container
        class (read_timslice_data_struct), pointer :: ptr
    end type


    type :: read_timslice_data_struct

        character(len=19) :: current_date
        character(len=19) :: start_date
        character(len=256) :: timeslice_path

    contains

        procedure :: init => read_timslice_data_struct_init
        procedure :: destroy => read_timslice_data_struct_destroy
        procedure :: setup_read => setup_read_timeslice


    end type read_timslice_data_struct




contains


        subroutine read_timslice_data_struct_init(this, start_date, timeslice_path)

            implicit none
            class(read_timslice_data_struct), intent(inout) :: this ! object being initialized

            character(len=19), intent(in) :: start_date

            character(len=256), intent(in) :: timeslice_path


            print *, 'in timeslice object init'

            this%start_date = start_date

            this%current_date = start_date

            this%timeslice_path = timeslice_path



        end subroutine read_timslice_data_struct_init


        subroutine read_timslice_data_struct_destroy(this)

            implicit none
            class(read_timslice_data_struct), intent(inout) :: this ! object being destroyed

        end subroutine read_timslice_data_struct_destroy




subroutine setup_read_timeslice(this, hydroDT)

    implicit none

    class(read_timslice_data_struct), intent(inout) :: this



    integer,           intent(in) :: hydroDT ! the number of seconds of hydro advance from lsmTime
    !integer :: ff
    integer :: ii, tt, oo
    character(len=19) :: hydroTime, obsHydroTime ! hydro model time and corresponding observation
    character(len=19), dimension(nObsTimes) :: obsTimes ! obs times in the current window
    integer :: oldDiff, nShiftLeft

    logical, allocatable, dimension(:) :: theMask
    integer, allocatable, dimension(:) :: whObsMiss
    logical, allocatable, dimension(:) :: obsTimeStrAllocated

    integer :: nObsMiss
    integer :: did=1  !! jlm: assuming did=1
    integer :: nWhObsMiss

    integer :: nGages, nCellsInR, cc, nLinkAff, nStatic, iiImage


    character(len=19) :: lsmTime, initTime    ! set global to belong to structure in nudging


    character(len=256) :: timeslice_file_name  !! the corresponding obs file.


    integer :: obsResolutionInt

!!!!!!!!!!!
    lsmTime = this%current_date
    initTime = this%start_date
!!!!!!!!!!



    !call geth_newdate(hydroTime, lsmTime, hydroDT)


    ! Calculate the closest multiple of obsResolution to the hydroTime
    !obsHydroTime = round_resolution_minute(hydroTime, obsResolutionInt)


    !INCLUDE??????
    !if(flushAll) flush(flushUnit)


    obsResolutionInt = 15


    timeslice_file_name = find_timeslice_file(initTime, obsResolution, this%timeslice_path)



end subroutine setup_read_timeslice


function find_timeslice_file(date, obsResolution, timeslice_path)
    implicit none
    character(len=256) :: find_timeslice_file      ! Output
    character(len=19), intent(in) :: date          ! Input
    character(len=2),  intent(in) :: obsResolution ! Input

    character(len=256), intent(in) :: timeslice_path
    !Internals
    character(len=256) :: tmpTimeSlice
    logical :: fileExists

!#ifdef HYDRO_D
!print*,'Ndg: start find_timeslice_file'
!call flush(6)
!#endif

    find_timeslice_file=''
    ! is there a file with this name?
    ! note files are not resolved below minutes.
    tmpTimeSlice = trim(timeslice_path) // date // "." // &
                        obsResolution // 'min.usgsTimeSlice.ncdf'
    inquire(FILE=tmpTimeSlice, EXIST=fileExists)
    if (fileExists) find_timeslice_file = trim(tmpTimeSlice)

    !#ifdef HYDRO_D
    print*,'Reservoirs: timeSlice file: ', tmpTimeSlice
    print*,'Reservoirs: file found: ', fileExists
    print*,'Reservoirs: finish find_timeslice_file'
    call flush(6)
    !#endif

end function find_timeslice_file




subroutine geth_newdate (ndate, odate, idt)
implicit none

!  From old date ("YYYY-MM-DD HH:MM:SS.ffff" or "YYYYMMDDHHMMSSffff") and
!  delta-time, compute the new date.

!  on entry     -  odate  -  the old hdate.
!                  idt    -  the change in time

!  on exit      -  ndate  -  the new hdate.

integer, intent(in)           :: idt
character (len=*), intent(out) :: ndate
character (len=*), intent(in)  :: odate

!  Local Variables

!  yrold    -  indicates the year associated with "odate"
!  moold    -  indicates the month associated with "odate"
!  dyold    -  indicates the day associated with "odate"
!  hrold    -  indicates the hour associated with "odate"
!  miold    -  indicates the minute associated with "odate"
!  scold    -  indicates the second associated with "odate"

!  yrnew    -  indicates the year associated with "ndate"
!  monew    -  indicates the month associated with "ndate"
!  dynew    -  indicates the day associated with "ndate"
!  hrnew    -  indicates the hour associated with "ndate"
!  minew    -  indicates the minute associated with "ndate"
!  scnew    -  indicates the second associated with "ndate"

!  mday     -  a list assigning the number of days in each month

!  i        -  loop counter
!  nday     -  the integer number of days represented by "idt"
!  nhour    -  the integer number of hours in "idt" after taking out
!              all the whole days
!  nmin     -  the integer number of minutes in "idt" after taking out
!              all the whole days and whole hours.
!  nsec     -  the integer number of minutes in "idt" after taking out
!              all the whole days, whole hours, and whole minutes.

integer :: newlen, oldlen
integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
integer :: yrold, moold, dyold, hrold, miold, scold, frold
integer :: nday, nhour, nmin, nsec, nfrac, i, ifrc
logical :: opass
character (len=10) :: hfrc
character (len=1) :: sp
logical :: punct
integer :: yrstart, yrend, mostart, moend, dystart, dyend
integer :: hrstart, hrend, mistart, miend, scstart, scend, frstart
integer :: units
integer, dimension(12) :: mday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

! Determine if odate is "YYYY-MM-DD_HH ... " or "YYYYMMDDHH...."
if (odate(5:5) == "-") then
   punct = .TRUE.
else
   punct = .FALSE.
endif

!  Break down old hdate into parts

hrold = 0
miold = 0
scold = 0
frold = 0
oldlen = LEN(odate)
if (punct) then
   yrstart = 1
   yrend = 4
   mostart = 6
   moend = 7
   dystart = 9
   dyend = 10
   hrstart = 12
   hrend = 13
   mistart = 15
   miend = 16
   scstart = 18
   scend = 19
   frstart = 21
   select case (oldlen)
   case (10)
      ! Days
      units = 1
   case (13)
      ! Hours
      units = 2
   case (16)
      ! Minutes
      units = 3
   case (19)
      ! Seconds
      units = 4
   case (21)
      ! Tenths
      units = 5
   case (22)
      ! Hundredths
      units = 6
   case (23)
      ! Thousandths
      units = 7
   case (24)
      ! Ten thousandths
      units = 8
   case default

      write(*,*) 'FATAL ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
      call hydro_stop("geth_newdate")

   end select

   if (oldlen.ge.11) then
      sp = odate(11:11)
   else
      sp = ' '
   end if

else

   yrstart = 1
   yrend = 4
   mostart = 5
   moend = 6
   dystart = 7
   dyend = 8
   hrstart = 9
   hrend = 10
   mistart = 11
   miend = 12
   scstart = 13
   scend = 14
   frstart = 15

   select case (oldlen)
   case (8)
      ! Days
      units = 1
   case (10)
      ! Hours
      units = 2
   case (12)
      ! Minutes
      units = 3
   case (14)
      ! Seconds
      units = 4
   case (15)
      ! Tenths
      units = 5
   case (16)
      ! Hundredths
      units = 6
   case (17)
      ! Thousandths
      units = 7
   case (18)
      ! Ten thousandths
      units = 8
   case default

      write(*,*) 'FATAL ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
      call hydro_stop("geth_newdate")

   end select
endif

!  Use internal READ statements to convert the CHARACTER string
!  date into INTEGER components.

read(odate(yrstart:yrend),  '(i4)') yrold
read(odate(mostart:moend),  '(i2)') moold
read(odate(dystart:dyend), '(i2)') dyold
if (units.ge.2) then
   read(odate(hrstart:hrend),'(i2)') hrold
   if (units.ge.3) then
      read(odate(mistart:miend),'(i2)') miold
      if (units.ge.4) then
         read(odate(scstart:scend),'(i2)') scold
         if (units.ge.5) then
            read(odate(frstart:oldlen),*) frold
         end if
      end if
   end if
end if

!  Set the number of days in February for that year.

mday(2) = nfeb(yrold)

!  Check that ODATE makes sense.

opass = .TRUE.

!  Check that the month of ODATE makes sense.

if ((moold.gt.12).or.(moold.lt.1)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
#endif
   opass = .FALSE.
end if

!  Check that the day of ODATE makes sense.

if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
#endif
   opass = .FALSE.
end if

!  Check that the hour of ODATE makes sense.

if ((hrold.gt.23).or.(hrold.lt.0)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
#endif
   opass = .FALSE.
end if

!  Check that the minute of ODATE makes sense.

if ((miold.gt.59).or.(miold.lt.0)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
#endif
   opass = .FALSE.
end if

!  Check that the second of ODATE makes sense.

if ((scold.gt.59).or.(scold.lt.0)) then
#ifdef HYDRO_D
   write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
#endif
   opass = .FALSE.
end if

!  Check that the fractional part  of ODATE makes sense.


if (.not.opass) then

   write(*,*) 'FATAL ERROR: Crazy ODATE: ', odate(1:oldlen), oldlen
   stop

end if

!  Date Checks are completed.  Continue.


!  Compute the number of days, hours, minutes, and seconds in idt

if (units.ge.5) then !idt should be in fractions of seconds
   ifrc = oldlen-(frstart)+1
   ifrc = 10**ifrc
   nday   = abs(idt)/(86400*ifrc)
   nhour  = mod(abs(idt),86400*ifrc)/(3600*ifrc)
   nmin   = mod(abs(idt),3600*ifrc)/(60*ifrc)
   nsec   = mod(abs(idt),60*ifrc)/(ifrc)
   nfrac = mod(abs(idt), ifrc)
else if (units.eq.4) then  !idt should be in seconds
   ifrc = 1
   nday   = abs(idt)/86400 ! integer number of days in delta-time
   nhour  = mod(abs(idt),86400)/3600
   nmin   = mod(abs(idt),3600)/60
   nsec   = mod(abs(idt),60)
   nfrac  = 0
else if (units.eq.3) then !idt should be in minutes
   ifrc = 1
   nday   = abs(idt)/1440 ! integer number of days in delta-time
   nhour  = mod(abs(idt),1440)/60
   nmin   = mod(abs(idt),60)
   nsec   = 0
   nfrac  = 0
else if (units.eq.2) then !idt should be in hours
   ifrc = 1
   nday   = abs(idt)/24 ! integer number of days in delta-time
   nhour  = mod(abs(idt),24)
   nmin   = 0
   nsec   = 0
   nfrac  = 0
else if (units.eq.1) then !idt should be in days
   ifrc = 1
   nday   = abs(idt)    ! integer number of days in delta-time
   nhour  = 0
   nmin   = 0
   nsec   = 0
   nfrac  = 0
else

   write(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
        oldlen
   write(*,*) '#'//odate(1:oldlen)//'#'
   call hydro_stop("geth_newdate")

end if

if (idt.ge.0) then

   frnew = frold + nfrac
   if (frnew.ge.ifrc) then
      frnew = frnew - ifrc
      nsec = nsec + 1
   end if

   scnew = scold + nsec
   if (scnew .ge. 60) then
      scnew = scnew - 60
      nmin  = nmin + 1
   end if

   minew = miold + nmin
   if (minew .ge. 60) then
      minew = minew - 60
      nhour  = nhour + 1
   end if

   hrnew = hrold + nhour
   if (hrnew .ge. 24) then
      hrnew = hrnew - 24
      nday  = nday + 1
   end if

   dynew = dyold
   monew = moold
   yrnew = yrold
   do i = 1, nday
      dynew = dynew + 1
      if (dynew.gt.mday(monew)) then
         dynew = dynew - mday(monew)
         monew = monew + 1
         if (monew .gt. 12) then
            monew = 1
            yrnew = yrnew + 1
            ! If the year changes, recompute the number of days in February
            mday(2) = nfeb(yrnew)
         end if
      end if
   end do

else if (idt.lt.0) then

   frnew = frold - nfrac
   if (frnew .lt. 0) then
      frnew = frnew + ifrc
      nsec = nsec + 1
   end if

   scnew = scold - nsec
   if (scnew .lt. 00) then
      scnew = scnew + 60
      nmin  = nmin + 1
   end if

   minew = miold - nmin
   if (minew .lt. 00) then
      minew = minew + 60
      nhour  = nhour + 1
   end if

   hrnew = hrold - nhour
   if (hrnew .lt. 00) then
      hrnew = hrnew + 24
      nday  = nday + 1
   end if

   dynew = dyold
   monew = moold
   yrnew = yrold
   do i = 1, nday
      dynew = dynew - 1
      if (dynew.eq.0) then
         monew = monew - 1
         if (monew.eq.0) then
            monew = 12
            yrnew = yrnew - 1
            ! If the year changes, recompute the number of days in February
            mday(2) = nfeb(yrnew)
         end if
         dynew = mday(monew)
      end if
   end do
end if

!  Now construct the new mdate

newlen = LEN(ndate)

if (punct) then

   if (newlen.gt.frstart) then
      write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
      write(hfrc,'(i10)') frnew+1000000000
      ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

   else if (newlen.eq.scend) then
      write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
19    format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

   else if (newlen.eq.miend) then
      write(ndate,16) yrnew, monew, dynew, hrnew, minew
16    format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

   else if (newlen.eq.hrend) then
      write(ndate,13) yrnew, monew, dynew, hrnew
13    format(i4,'-',i2.2,'-',i2.2,'_',i2.2)

   else if (newlen.eq.dyend) then
      write(ndate,10) yrnew, monew, dynew
10    format(i4,'-',i2.2,'-',i2.2)

   end if

else

   if (newlen.gt.frstart) then
      write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
      write(hfrc,'(i10)') frnew+1000000000
      ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

   else if (newlen.eq.scend) then
      write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
119   format(i4,i2.2,i2.2,i2.2,i2.2,i2.2)

   else if (newlen.eq.miend) then
      write(ndate,116) yrnew, monew, dynew, hrnew, minew
116   format(i4,i2.2,i2.2,i2.2,i2.2)

   else if (newlen.eq.hrend) then
      write(ndate,113) yrnew, monew, dynew, hrnew
113   format(i4,i2.2,i2.2,i2.2)

   else if (newlen.eq.dyend) then
      write(ndate,110) yrnew, monew, dynew
110   format(i4,i2.2,i2.2)

   end if

endif

if (punct .and. (oldlen.ge.11) .and. (newlen.ge.11)) ndate(11:11) = sp

end subroutine geth_newdate




integer function nfeb(year)
!
! Compute the number of days in February for the given year.
!
implicit none
integer, intent(in) :: year ! Four-digit year

nfeb = 28 ! By default, February has 28 days ...
if (mod(year,4).eq.0) then
   nfeb = 29  ! But every four years, it has 29 days ...
   if (mod(year,100).eq.0) then
      nfeb = 28  ! Except every 100 years, when it has 28 days ...
      if (mod(year,400).eq.0) then
         nfeb = 29  ! Except every 400 years, when it has 29 days ...
         if (mod(year,3600).eq.0) then
            nfeb = 28  ! Except every 3600 years, when it has 28 days.
         endif
      endif
   endif
endif
end function nfeb



function round_resolution_minute(time, resolution)
implicit none
character(len=19)             :: round_resolution_minute ! return value
character(len=19), intent(in) :: time         ! time to round
integer                       :: resolution   ! the time resolution in minutes

integer :: diffMin
real    :: resolutionInv, theMinute, theSecond, resMin
character(len=19) :: roundTime, timeCopy

! nearest defines a time resolution
resolutionInv = 1./resolution
!1234567890123456789
!YYYY-MM-DD_HH:mm:ss
read(time(15:16),*) theMinute
read(time(18:19),*) theSecond
theMinute = theMinute + (theSecond/60.)
! What fraction of the time resolution is the current time?
resMin = (theMinute * resolutionInv)
! Round to the time resolution.
! nint doesnt document its rounding rule on gfortran! boo.
if(resMin - floor(resMin) .ge. .5) resMin=ceiling(resMin)
if(resMin - floor(resMin) .lt. .5) resMin=floor(resMin)
! Convert back to regular minutes.
resMin = resMin / resolutionInv
! To get the exact right time, some floor/ceil operations are helpful.
timeCopy = time
timeCopy(18:19) = '00' ! Floor of theMinute in string form
! nint is needed b/c resMin is real and often slightly off integer,
! and this is exacerbated by *60
diffMin = (nint(resMin) - floor(theMinute))*60
!print*, nint(resMin), floor(theMinute), timeCopy, diffMin
call geth_newdate(round_resolution_minute, timeCopy, diffMin)

end function round_resolution_minute




end module module_read_timeslice_data




