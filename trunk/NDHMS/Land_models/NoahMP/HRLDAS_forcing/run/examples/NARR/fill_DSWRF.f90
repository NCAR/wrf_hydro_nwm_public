program fill_DSWRF
implicit none
integer,parameter :: maxpts = 96673, lugbin = 10, lugi = 0
integer,parameter :: lugbout = 20
integer,dimension(200)      :: jpds,jgds,kpds,kgds
logical*1,dimension(maxpts) :: lb
real,dimension(maxpts)      :: data_in, lat, lon, sum_cza, czaxta
real,dimension(maxpts,3)    :: data_out, cza
integer                     :: i, iret, j, kf, k, yr(3), mo(3), dy(3), hr(3)
real                        :: jday(3), hr_mid(3)
character*100               :: infile,outfile(3)
real, parameter    :: pi = 3.14159265
integer, parameter :: testpt = 76401
logical, parameter :: debug = .false.

open(20, file='fill_DSWRF.input', form ='formatted')
  read(20,'(a100)') infile
 do i=1,3
  read(20,'(a100)') outfile(i)
  read(20,*) yr(i), mo(i), dy(i), hr(i), jday(i), hr_mid(i)
 end do
close(20)

open(30, file='NARR_lat', form ='formatted')
do i = 0,276
  read(30,'(349f7.2)') lat( i*349+1 : i*349+349 )
end do
close(30)
open(30, file='NARR_lon', form ='formatted')
do i = 0,276
  read(30,'(349f8.2)') lon( i*349+1 : i*349+349 )
end do
close(30)

if (debug) print *, lat(testpt),lon(testpt)
lat = lat * pi / 180.0
lon = lon * pi / 180.0

! GET COSINE ZENITH ANGLE ARRAY FOR EACH HOUR

do i = 1,3
  call get_cza(jday(i),hr_mid(i),lat,lon,cza(:,i))
end do
  call get_cza(jday(3),hr_mid(3)+1,lat,lon,czaxta)

sum_cza = cza(:,1) + 2.0*cza(:,2) + 2.0*cza(:,3) + czaxta  ! the sum of all zenith angles

if (debug) print *, lat(testpt),lon(testpt),cza(testpt,:),sum_cza(testpt)

! READ IN VARIABLE

  call baopenr(lugbin,infile,iret)
  if(iret/=0) stop 1

! Set GRIB1 field identification values to search for

  j=0      ! search from beginning
  jgds    =  -1
  jpds    =  -1

  call getgb(lugbin,lugi,maxpts,j,jpds,jgds,kf,k,kpds,kgds,lb,data_in,iret)
  if(iret/=0) stop 2

  call baclose(lugbin,iret)
  if(iret/=0) stop 3

! START MESSING WITH THE INPUT DATA

where(data_in < 0) data_in = 0.0

data_out = 0.0
do j = 1, maxpts
 if(sum_cza(j) > 0.0) then
   do i = 1,3
     data_out(j,i) = cza(j,i)/sum_cza(j) * data_in(j) * 6.0
   end do
 end if
end do

! If there is input solar, but sun is not up at three mid-hours, check if up at hour 4 and put in 3rd hour

!where(sum_cza == 0.0 .and. czaxta > 0.0 .and. data_in > 5.0) data_out(:,3) = data_in * 3.0

! If there is input solar > 5 W/m2, and all 4 hours sun is not up, let me know

do i = 1, maxpts
  if(sum_cza(i) == 0.0 .and. data_in(i) > 5.0) &
    print *, "Seems to be sun with no sun up", sum_cza(i),czaxta(i),data_in(i)
end do

! WRITE OUT TO THE 3 NEW FILES

do i = 1,3

if (debug) print *, cza(testpt,i), data_out(testpt,i), data_in(testpt)     ! 17820 = lat(50),lon(180)

  kpds(8)  = yr(i)
  if(yr(i) == 00) then
    kpds(8)  = 100
    kpds(21) = 20
  end if
  kpds(9)  = mo(i)
  kpds(10) = dy(i)
  kpds(11) = hr(i)
  kpds(15) = 1            ! change number of hours in average to 1
  kpds(22) = 2            ! preserve 2 decimal points

  call baopenw(lugbout+i,outfile(i),iret)
  if(iret/=0) stop 4

  call putgb(lugbout+i,maxpts,kpds,kgds,lb,data_out(:,i),iret)
  if(iret/=0) stop 5

  call baclose(lugbout+i,iret)
  if(iret/=0) stop 6

end do 

contains

subroutine get_cza(jday, gmthour, latrad, lonrad, cza)
  implicit none
  integer,parameter :: maxpts = 96673
  real, intent(in) :: jday,gmthour
  real, intent(in) :: latrad(maxpts)
  real, intent(in) :: lonrad(maxpts)
  real, intent(out) :: cza(maxpts)

  real :: gg, tc, declin, sha(maxpts)
  real, parameter :: pi = 3.14159265
  
  ! Fractional day of the year, in radians
  gg = (360./365.25) * (JDAY+gmthour/24.) * pi/180.

  ! Solar declination angle, in radians.
  DECLIN = 0.006918 - 0.399912*cos(gg) + 0.070257*sin(gg) - 0.006758*cos(2.0*gg) + &
       0.000907*sin(2.0*gg) - 0.002697*cos(3.0*gg) + 0.00148*sin(3.0*gg)

  ! Time Correction for solar angle, in radians.  Whatever.
  TC = 0.000075 + 0.001868*cos(gg) - 0.032077*sin(gg) - 0.014615*cos(2.0*gg) - &
       0.040849*sin(2.0*gg)
!  TC = 0.0000

  ! Solar Hour Angle, in radians
  SHA = ((gmthour-12.0)*15.0)*(pi/180.) + lonrad + TC
  ! SHA = (gmthour*15.0)*(pi/180.) + lonrad + TC

  ! ! Solar Zenith Angle, in radians
  CZA = sin(latrad)*sin(DECLIN)+cos(latrad)*cos(DECLIN)*cos(SHA)
  where(cza.lt.0) cza = 0.0

end subroutine get_cza


end program fill_DSWRF
