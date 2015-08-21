implicit none
integer,parameter           :: maxpts = 864000, lugbin = 10, lugbout1 = 11, lugbout2 = 12, lugi = 0
integer,dimension(200)      :: jpds,jgds,kpds,kgds
logical*1,dimension(maxpts) :: lb
real,dimension(maxpts)      :: data_in, data_out
integer                     :: iret, j, kf, k
character*100               :: infile,outfile1,outfile2

call getarg(1,infile)
call getarg(2,outfile1)
call getarg(3,outfile2)

! READ IN DUMMY VARIABLE

call baopenr(lugbin,infile,iret)
if(iret/=0) stop 1

! Set GRIB1 field identification values to search for

j=0      ! search from beginning
jgds    =  -1
jpds    =  -1
jpds(5) = 32

call getgb(lugbin,lugi,maxpts,j,jpds,jgds,kf,k,kpds,kgds,lb,data_in,iret)
if(iret/=0) stop 2

call baclose(lugbin,iret)
if(iret/=0) stop 3

! Set all wind to U

kpds(5)  = 33

data_out = data_in

call baopenw(lugbout1,outfile1,iret)
if(iret/=0) stop 4

call putgb(lugbout1,maxpts,kpds,kgds,lb,data_out,iret)
if(iret/=0) stop 5

call baclose(lugbout1,iret)
if(iret/=0) stop 6
 

! CREATE FAKE V AND SET TO ZERO

kpds(5)  = 34

data_out = 0

call baopenw(lugbout2,outfile2,iret)
if(iret/=0) stop 7

call putgb(lugbout2,maxpts,kpds,kgds,lb,data_out,iret)
if(iret/=0) stop 8

call baclose(lugbout2,iret)
if(iret/=0) stop 9
 

end
