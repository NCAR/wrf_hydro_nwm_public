module read_timslice_data

    !use module_namelist,      only: nlst_rt




    implicit none



contains








subroutine setup_read_timeslice()

    implicit none



    !integer,           intent(in) :: hydroDT ! the number of seconds of hydro advance from lsmTime
    !integer :: ff
    integer :: ii, tt, oo
    character(len=19) :: hydroTime, obsHydroTime ! hydro model time and corresponding observation
    !character(len=19), dimension(nObsTimes) :: obsTimes ! obs times in the current window
    integer :: oldDiff, nShiftLeft

    logical, allocatable, dimension(:) :: theMask
    integer, allocatable, dimension(:) :: whObsMiss
    logical, allocatable, dimension(:) :: obsTimeStrAllocated

    integer :: nObsMiss
    integer :: did=1  !! jlm: assuming did=1
    integer :: nWhObsMiss

    integer :: nGages, nCellsInR, cc, nLinkAff, nStatic, iiImage


    !lsmTime = nlst_rt(did)%olddate
    !initTime = nlst_rt(did)%startdate




end subroutine setup_read_timeslice




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



end module read_timslice_data




