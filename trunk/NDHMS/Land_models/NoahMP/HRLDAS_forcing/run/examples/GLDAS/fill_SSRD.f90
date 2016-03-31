program fill_SSRD
implicit none
integer,parameter :: maxpts = 864000, lugbin1 = 10, lugbin2 = 11, lugi = 0
integer,parameter :: lugbout = 20
integer,dimension(200)      :: jpds,jgds,kpds,kgds
logical*1,dimension(maxpts) :: lb
real,dimension(maxpts)      :: data_in1, data_in2, lat, lon, cosdiff,datadiff
real,dimension(maxpts,4)    :: data_out, cza
integer                     :: i, iret, j, kf, k, yr(4), mo(4), dy(4), hr(4)
real                        :: jday(4), hr_mid(4),critnrg,critcos
character*100               :: infile1,infile2,outfile(4)
logical                     :: check1,check2
real, parameter   :: pi = 3.14159265
integer,parameter :: testpt = 518136
logical,parameter :: debug = .true.

open(20, file='fill_SSRD.input', form ='formatted')
  read(20,'(a100)') infile1
  read(20,'(a100)') infile2
 do i=1,4
  read(20,'(a100)') outfile(i)
  read(20,*) yr(i), mo(i), dy(i), hr(i), jday(i), hr_mid(i)
 end do
close(20)

do i = 0,599
  lat( i*1440+1 : i*1440+1440 ) = -59.875 + i*0.25
end do
do i = 0,1439
  lon( i+1 : : 1440 ) = -179.875 + i*0.25
end do

if(debug) print *, lat(testpt),lon(testpt)

lat = lat * pi / 180.0
lon = lon * pi / 180.0

! GET COSINE ZENITH ANGLE ARRAY FOR EACH HOUR

do i = 1,4
  call get_cza(jday(i),hr_mid(i),lat,lon,cza(:,i))
end do

! READ IN VARIABLE

data_in1 = 0.0
data_in2 = 0.0

inquire(file=infile1,exist=check1)
inquire(file=infile2,exist=check2)

if(check1) then

  call baopenr(lugbin1,infile1,iret)
  if(iret/=0) stop 11

! Set GRIB1 field identification values to search for

  j=0      ! search from beginning
  jgds    =  -1
  jpds    =  -1

  call getgb(lugbin1,lugi,maxpts,j,jpds,jgds,kf,k,kpds,kgds,lb,data_in1,iret)
  if(iret/=0) stop 21

  call baclose(lugbin1,iret)
  if(iret/=0) stop 31

end if

if(check2) then 

  call baopenr(lugbin2,infile2,iret)
  if(iret/=0) stop 12

! Set GRIB1 field identification values to search for

  j=0      ! search from beginning
  jgds    =  -1
  jpds    =  -1

  call getgb(lugbin2,lugi,maxpts,j,jpds,jgds,kf,k,kpds,kgds,lb,data_in2,iret)
  if(iret/=0) stop 22

  call baclose(lugbin2,iret)
  if(iret/=0) stop 32

end if
if(debug) print *, cza(testpt,1), data_in1(testpt)
if(debug) print *, cza(testpt,4), data_in2(testpt)
if(debug) print *, count(cza(:,1)<0.and.data_in1>0)
if(debug) print *, count(cza(:,4)<0.and.data_in2>0)

! START MESSING WITH THE INPUT DATA

where(data_in1 <= 0 .or. cza(:,1) <= 0.0) 
  data_in1 = 0.0
  cza(:,1) = 0.0
end where
where(data_in2 <= 0 .or. cza(:,4) <= 0.0)
  data_in2 = 0.0
  cza(:,4) = 0.0
end where

critnrg = 50.0
critcos = 0.05
data_out(:,1) = data_in1
data_out(:,4) = data_in2
data_out(:,2) = (2.0*data_in1 + data_in2)/3.0  ! default to linear interp
data_out(:,3) = (data_in1 + 2.0*data_in2)/3.0

where(data_in1 > critnrg .and. data_in2 <= critnrg .and. cza(:,1) > critcos) 
  data_out(:,2) = cza(:,2)/cza(:,1) * data_in1   ! if sun sufficently high
  data_out(:,3) = cza(:,3)/cza(:,1) * data_in1   ! around sunset, use left value
end where

where(data_in1 <= critnrg .and. data_in2 > critnrg .and. cza(:,4) > critcos) 
  data_out(:,2) = cza(:,2)/cza(:,4) * data_in2   ! if sun sufficently high
  data_out(:,3) = cza(:,3)/cza(:,4) * data_in2   ! around sunrise, use right value
end where

where(data_in1 > critnrg .and. data_in2 > critnrg .and. cza(:,1) > critcos .and. cza(:,4) > critcos)
  data_out(:,3) = cza(:,3)/cza(:,4) * data_in2  ! mid-day with sufficient sun 
  data_out(:,2) = cza(:,2)/cza(:,1) * data_in1  !   so use zenith angle
end where

! WRITE OUT NEW FILES

do i = 1,4
  
  where(data_out(:,i) <= 0 .or. cza(:,i) <= 0.0) data_out(:,i) = 0.0

  if(debug) print *, cza(testpt,i), data_out(testpt,i), data_in1(testpt), data_in2(testpt)

  kpds(8)  = yr(i)
  kpds(9)  = mo(i)
  kpds(10) = dy(i)
  kpds(11) = hr(i)
  kpds(15) = 0            ! change number of hours in average to 1
  kpds(22) = 2            ! preserve 2 decimal points

  call baopenw(lugbout+i,outfile(i),iret)
  if(iret/=0) stop 4

  call putgb(lugbout+i,maxpts,kpds,kgds,lb,data_out(:,i),iret)
  if(iret/=0) stop 5

  call baclose(lugbout+i,iret)
  if(iret/=0) stop 6

end do 

if(debug) print *

contains

subroutine get_cza(jday, gmthour, latrad, lonrad, cza)
  implicit none
  integer,parameter :: maxpts = 864000
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


end program fill_SSRD
