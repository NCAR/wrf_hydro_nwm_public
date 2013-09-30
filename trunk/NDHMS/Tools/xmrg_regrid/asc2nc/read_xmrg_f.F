!234567
!pgf90 -o asc2nc.exe read_xmrg_f.F -I $HOME/netcdf/include -L $HOME/netcdf/lib -lnetcdf -lnetcdff
! this is the main Fortran program to read the xmrg file and write into netcdf file.
      program read_xmrg
          implicit none
          integer :: idim, jdim, status
          real :: rlat, rlon, tmpR
          character(len=256) :: inFile
          real, allocatable, dimension(:,:):: array
          integer read_data_c
          integer :: i,j,tmpI 
          real :: hrap_x,hrap_y

!          real, allocatable, dimension(:,:):: array
!         real :: array(612,690)
          ! real, dimension(:,:):: array
          real, allocatable, dimension(:,:):: xlat, xlon 
          inFile = "tmp_in"
        
           jdim = 690 
           idim = 612 
           allocate(array(jdim,idim))
           allocate(xlat(idim,jdim))
           allocate(xlon(idim,jdim))

          ! status =  read_data_c(idim,jdim, array)
            call read_data_f(trim(inFile),idim,jdim, rlat, rlon, array)

!    !     do j = 1, jdim
!    !        xlat(j) = rlat + (j-1)*1
!    !     end do
!    !     do i = 1, idim
!    !        xlon(i) = rlon + (i-1)*1
!    !     end do
!    !     
!         !call hrap_to_latlon(hrap_x,hrap_y,xlon(i,j),xlat(i,j))


      open(16,file="hrap_lat_lon.txt",form="formatted")
!     do j = 1, jdim
      do j = jdim, 1, -1
        do i=1,idim
          read(16,*) tmpI,tmpR,xlon(i,j),xlat(i,j)
        end do
      end do
      close(16)


          write(6,*) "finish reading, idim , jdim", idim, jdim
          write(6,*) "rlon, rlat", rlon, rlat
          call flush(6)
!         write(6,*) "array =", array

          call output_nc2(xlat,xlon,array,idim,jdim) 

          deallocate(array)
          deallocate(xlat)
          deallocate(xlon)
      stop
      end

     
      subroutine output_nc2(xlat, xlon, array,idim,jdim)
      implicit none
#include <netcdf.inc>
          integer idim,jdim
          real array(idim,jdim)
          real xlat(idim,jdim)
          real xlon(idim,jdim)
          integer dim(2)
          character*120 file_name,var_name
          integer   iret,ncid,varid,idim_id,jdim_id
          integer i,j
          file_name = "out.nc"
          var_name = "pcp"

          iret = nf_create(trim(file_name), 0, ncid)
          iret = nf_def_dim(ncid, "idim", idim,idim_id)
          iret = nf_def_dim(ncid, "jdim", jdim,jdim_id)
          dim(1)=idim_id
          dim(2)=jdim_id
          iret = nf_put_att_real(ncid,NF_GLOBAL,"missing_value",
     &       NF_FLOAT,1,-1.E33)

          iret = nf_def_var(ncid,var_name,NF_FLOAT,2,dim,varid)
          iret = nf_def_var(ncid,"xlat",NF_FLOAT,2,dim,varid)
          iret = nf_def_var(ncid,"xlon",NF_FLOAT,2,dim,varid)
          iret = nf_enddef(ncid)

!   output
          iret = nf_inq_varid(ncid,var_name,varid)
          iret = nf_put_var_real(ncid,varid,array)

          iret = nf_inq_varid(ncid,"xlat",varid)
          iret = nf_put_var_real(ncid,varid,xlat)

          iret = nf_inq_varid(ncid,"xlon",varid)
          iret = nf_put_var_real(ncid,varid,xlon)

          iret=nf_close(ncid)
      return
      end


c**********************************************************************
      subroutine latlon_to_hrap(rlon,rlat,hrap_x,hrap_y)
c**********************************************************************
c
c subroutine converts lat-lon to HRAP
c
c input variables
c
c rlon   - longitude in fractional degrees
c rlat   - lattitude in fractional degrees
c
c output variables
c
c hrap_x - global HRAP x-coordinate
c hrap_y - global HRAP y-coordinate
c
      pi=3.141592654
      d2rad=pi/180.
      earthr=6371.2
      ref_lat=60.
      ref_lon=105.
      rmesh=4.7625
      tlat=ref_lat*d2rad
      re=(earthr*(1.+sin(tlat)))/rmesh
      flat=rlat*d2rad
      flon=(rlon+180.-ref_lon)*d2rad
      r=re*cos(flat)/(1.+sin(flat))
      x=r*sin(flon)
      y=r*cos(flon)
      hrap_x=x+401.
      hrap_y=y+1601.
      return
      end
c**********************************************************************
      subroutine hrap_to_latlon(hrap_x,hrap_y,rlon,rlat)
c**********************************************************************
c
c subroutine converts HRAP to lat-lon
c
c input variables
c
c hrap_x - global HRAP x-coordinate
c hrap_y - global HRAP y-coordinate
c
c output variables
c
c rlon   - longitude in fractional degrees
c rlat   - lattitude in fractional degrees
c
      earthr=6371.2
      stlon=105.
      pi=3.141592654
      raddeg=180./pi
      xmesh=4.7625
      tlat=60./raddeg
      x=hrap_x-401.
      y=hrap_y-1601.
      rr=x*x+y*y
      gi=((earthr*(1.+sin(tlat)))/xmesh)
      gi=gi*gi
      rlat=asin((gi-rr)/(gi+rr))*raddeg
      ang=atan2(y,x)*raddeg
      if(ang.lt.0.) ang=ang+360.
      rlon=270.+stlon-ang
      if(rlon.lt.0.) rlon=rlon+360.
      if(rlon.gt.360.) rlon=rlon-360.
      return
      end


      subroutine read_data_f(inFile,idim,jdim, rlat, rlon, array)
         implicit none
         integer :: idim, jdim, iunit
         real :: array(idim,jdim)
         character*20 tmpStr
         character (len=*) inFile
         real :: rlat,rlon, csize, missing
         integer :: i, j
         iunit = 20
         open(iunit, file=inFile,form="formatted")
         read(iunit,*) tmpStr,idim
         read(iunit,*) tmpStr,jdim
         read(iunit,*) tmpStr,rlon
         read(iunit,*) tmpStr,rlat
         read(iunit,*) tmpStr,csize
         read(iunit,*) tmpStr,missing
         print*, "idim,jdim,rlat,rlon,csize=",idim,jdim,rlat,rlon,csize
!yw         do j = 1,jdim
         do j = jdim, 1, -1
            read(iunit,*) (array(i,j),i=1,idim)
         end do
         close(iunit)
      end subroutine read_data_f
