implicit none
integer,parameter           :: maxpts = 864000, lugbin1 = 11, lugbin2 = 12, lugbout = 10, lugi = 0
integer,dimension(200)      :: jpds,jgds,kpds,kgds
logical*1,dimension(maxpts) :: lb
real,dimension(maxpts)      :: data_in1, data_in2, data_out
integer                     :: iret, j, kf, k
character*100               :: infile1,infile2,outfile

call getarg(1,infile1)
call getarg(2,infile2)
call getarg(3,outfile)

! READ IN SNOW

call baopenr(lugbin2,infile2,iret)
if(iret/=0) stop 4

! Set GRIB1 field identification values to search for

j=0      ! search from beginning
jgds    =  -1
jpds    =  -1
jpds(5) = 131

call getgb(lugbin2,lugi,maxpts,j,jpds,jgds,kf,k,kpds,kgds,lb,data_in2,iret)
if(iret/=0) stop 5

call baclose(lugbin2,iret)
if(iret/=0) stop 6

! READ IN RAIN

call baopenr(lugbin1,infile1,iret)
if(iret/=0) stop 1

! Set GRIB1 field identification values to search for

j=0      ! search from beginning
jgds    =  -1
jpds    =  -1
jpds(5) = 132

call getgb(lugbin1,lugi,maxpts,j,jpds,jgds,kf,k,kpds,kgds,lb,data_in1,iret)
if(iret/=0) stop 2

call baclose(lugbin1,iret)
if(iret/=0) stop 3

! COMBINE AND PUT INTO TOTAL PRECIP

kpds(5)  = 59
kpds(22)  = 8

data_out = data_in1 + data_in2
where (data_out<0) data_out = 0

call baopenw(lugbout,outfile,iret)
if(iret/=0) stop 10

call putgb(lugbout,maxpts,kpds,kgds,lb,data_out,iret)
if(iret/=0) stop 11

call baclose(lugbout,iret)
if(iret/=0) stop 12
 

end
