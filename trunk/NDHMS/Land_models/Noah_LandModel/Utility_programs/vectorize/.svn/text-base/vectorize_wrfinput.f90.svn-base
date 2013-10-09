program vectorize_wrfinput
  use netcdf
  implicit none

  integer :: ncid_in, ncid_out
  integer :: iret,iloc,ilat,ilon
  integer :: dim_date, dim_we, dim_sn, dim_time, ndims, nvars, natts
  integer :: varid_in(6), varid_out(6)
  character (len = 80) :: attname

  integer, parameter :: west_east = 1563016
  integer, parameter :: south_north = 1
  real :: lu_index(1795,2632,1)
  integer, dimension(1795,2632,1) :: ivgtyp_in,   &
                                     isltyp_in
     real, dimension(1795,2632,1) :: hgt_in,      &
				     xlat_in,     &
				     xlong_in,    &
                                     tmn_in
  integer, dimension(west_east,south_north,1) :: ivgtyp_out,   &
                                                 isltyp_out
     real, dimension(west_east,south_north,1) :: hgt_out,      &
					         xlat_out,     &
					         xlong_out,    &
                                                 tmn_out

!!!!!
! READ GEO_EM FILE TO GET THE VEGTYPE
!!!!!

  iret = nf90_open("/d1/model/WPS/geo_em.d01.nc", NF90_NOWRITE, ncid_in)
   if(iret/=0) print*, "Problem opening file"

  iret = nf90_inq_varid(ncid_in,"LU_INDEX",varid_in(1))
   if(iret/=0) print*, "Problem getting varid"

  iret = nf90_get_var(ncid_in, varid_in(1), lu_index)
   if(iret/=0) print*, "Problem getting variable"

  iret = nf90_close(ncid_in)
   if(iret/=0) print*, "Problem closing file"

!!!!!
! CREATE A NEW FILE IN VECTOR FORMAT
!!!!!

  iret = nf90_create("/d1/data/vector/WRF/wrfinput_d01", NF90_CLOBBER, ncid_out)
   if(iret/=0) print*, "Problem creating file"

  iret = nf90_def_dim(ncid_out,"Time",NF90_UNLIMITED,dim_time)
  iret = nf90_def_dim(ncid_out,"DateStrLen",19,dim_date)
  iret = nf90_def_dim(ncid_out,"west_east",west_east,dim_we)
  iret = nf90_def_dim(ncid_out,"south_north",south_north,dim_sn)

  iret = nf90_open("/d1/model/WRFV3/run/wrfinput_d01", NF90_NOWRITE, ncid_in)
   if(iret/=0) print*, "Problem opening file"

  iret = nf90_inquire(ncid_in,ndims, nvars, natts)
   if(iret/=0) print*, "Problem getting number attributes"
  
  do iloc = 1, natts
    iret = nf90_inq_attname(ncid_in,NF90_GLOBAL, iloc, attname)
     if(iret/=0) print*, "Problem getting attribute name"
    iret = nf90_copy_att(ncid_in, NF90_GLOBAL, trim(attname), ncid_out, NF90_GLOBAL)
  end do

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"WEST-EAST_GRID_DIMENSION", west_east+1)
  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"SOUTH-NORTH_GRID_DIMENSION", south_north+1)
  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"WEST-EAST_PATCH_END_UNSTAG", west_east)
  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"WEST-EAST_PATCH_END_STAG", west_east+1)
  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"SOUTH-NORTH_PATCH_END_UNSTAG", south_north)
  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"SOUTH-NORTH_PATCH_END_STAG", south_north+1)

!!!!!
! READ IN VARIABLES ONE AT A TIME, EXTRACT AND WRITE TO NEW FILE
!!!!!

!!!!!
! IVGTYP
!!!!!

  iret = nf90_inq_varid(ncid_in,"IVGTYP",varid_in(1))
   if(iret/=0) print*, "Problem getting varid"

  iret = nf90_get_var(ncid_in, varid_in(1), ivgtyp_in)
   if(iret/=0) print*, "Problem getting variable"
  
!!!!!
! ISLTYP
!!!!!

  iret = nf90_inq_varid(ncid_in,"ISLTYP",varid_in(2))
   if(iret/=0) print*, "Problem getting varid"

  iret = nf90_get_var(ncid_in, varid_in(2), isltyp_in)
   if(iret/=0) print*, "Problem getting variable"
  
!!!!!
! HGT
!!!!!

  iret = nf90_inq_varid(ncid_in,"HGT",varid_in(3))
   if(iret/=0) print*, "Problem getting varid"

  iret = nf90_get_var(ncid_in, varid_in(3), hgt_in)
   if(iret/=0) print*, "Problem getting variable"
  
!!!!!
! XLAT
!!!!!

  iret = nf90_inq_varid(ncid_in,"XLAT",varid_in(4))
   if(iret/=0) print*, "Problem getting varid"

  iret = nf90_get_var(ncid_in, varid_in(4), xlat_in)
   if(iret/=0) print*, "Problem getting variable"
  
!!!!!
! XLONG
!!!!!

  iret = nf90_inq_varid(ncid_in,"XLONG",varid_in(5))
   if(iret/=0) print*, "Problem getting varid"

  iret = nf90_get_var(ncid_in, varid_in(5), xlong_in)
   if(iret/=0) print*, "Problem getting variable"
  
!!!!!
! TMN
!!!!!

  iret = nf90_inq_varid(ncid_in,"TMN",varid_in(6))
   if(iret/=0) print*, "Problem getting varid"

  iret = nf90_get_var(ncid_in, varid_in(6), tmn_in)
   if(iret/=0) print*, "Problem getting variable"
    
  iloc = 0
  do ilat = 1,2632
  do ilon = 1,1795
   if(lu_index(ilon,ilat,1)>=1 .and. lu_index(ilon,ilat,1)<=5) then
     iloc = iloc + 1
     ivgtyp_out(iloc,1,1) = ivgtyp_in(ilon,ilat,1)
     isltyp_out(iloc,1,1) = isltyp_in(ilon,ilat,1)
        hgt_out(iloc,1,1) = hgt_in(ilon,ilat,1)
       xlat_out(iloc,1,1) = xlat_in(ilon,ilat,1)
      xlong_out(iloc,1,1) = xlong_in(ilon,ilat,1)
        tmn_out(iloc,1,1) = tmn_in(ilon,ilat,1)
   end if
  end do
  end do

!!!!!
! IVGTYP
!!!!!

  iret = nf90_def_var(ncid_out,"IVGTYP",NF90_INT,(/dim_we,dim_sn,dim_time/),varid_out(1))
   if(iret/=0) print*, "Problem defining variable"
    iret = nf90_put_att(ncid_out,varid_out(1),"FieldType",106)
    iret = nf90_put_att(ncid_out,varid_out(1),"MemoryOrder","XY ")
    iret = nf90_put_att(ncid_out,varid_out(1),"description","DOMINANT VEGETATION CATEGORY")
    iret = nf90_put_att(ncid_out,varid_out(1),"units","")
    iret = nf90_put_att(ncid_out,varid_out(1),"stagger","")
    iret = nf90_put_att(ncid_out,varid_out(1),"coordinates","XLONG XLAT")

!!!!!
! ISLTYP
!!!!!

  iret = nf90_def_var(ncid_out,"ISLTYP",NF90_INT,(/dim_we,dim_sn,dim_time/),varid_out(2))
   if(iret/=0) print*, "Problem defining variable"
    iret = nf90_put_att(ncid_out,varid_out(2),"FieldType",106)
    iret = nf90_put_att(ncid_out,varid_out(2),"MemoryOrder","XY ")
    iret = nf90_put_att(ncid_out,varid_out(2),"description","DOMINANT SOIL CATEGORY")
    iret = nf90_put_att(ncid_out,varid_out(2),"units","")
    iret = nf90_put_att(ncid_out,varid_out(2),"stagger","")
    iret = nf90_put_att(ncid_out,varid_out(2),"coordinates","XLONG XLAT")

!!!!!
! HGT
!!!!!

  iret = nf90_def_var(ncid_out,"HGT",NF90_FLOAT,(/dim_we,dim_sn,dim_time/),varid_out(3))
   if(iret/=0) print*, "Problem defining variable"
    iret = nf90_put_att(ncid_out,varid_out(3),"FieldType",104)
    iret = nf90_put_att(ncid_out,varid_out(3),"MemoryOrder","XY ")
    iret = nf90_put_att(ncid_out,varid_out(3),"description","Terrain Height")
    iret = nf90_put_att(ncid_out,varid_out(3),"units","m")
    iret = nf90_put_att(ncid_out,varid_out(3),"stagger","")
    iret = nf90_put_att(ncid_out,varid_out(3),"coordinates","XLONG XLAT")

!!!!!
! XLAT
!!!!!

  iret = nf90_def_var(ncid_out,"XLAT",NF90_FLOAT,(/dim_we,dim_sn,dim_time/),varid_out(4))
   if(iret/=0) print*, "Problem defining variable"
    iret = nf90_put_att(ncid_out,varid_out(4),"FieldType",104)
    iret = nf90_put_att(ncid_out,varid_out(4),"MemoryOrder","XY ")
    iret = nf90_put_att(ncid_out,varid_out(4),"description","LATITUDE, SOUTH IS NEGATIVE")
    iret = nf90_put_att(ncid_out,varid_out(4),"units","degree_north")
    iret = nf90_put_att(ncid_out,varid_out(4),"stagger","")

!!!!!
! XLONG
!!!!!

  iret = nf90_def_var(ncid_out,"XLONG",NF90_FLOAT,(/dim_we,dim_sn,dim_time/),varid_out(5))
   if(iret/=0) print*, "Problem defining variable"
    iret = nf90_put_att(ncid_out,varid_out(5),"FieldType",104)
    iret = nf90_put_att(ncid_out,varid_out(5),"MemoryOrder","XY ")
    iret = nf90_put_att(ncid_out,varid_out(5),"description","LONGITUDE, WEST IS NEGATIVE")
    iret = nf90_put_att(ncid_out,varid_out(5),"units","degree_east")
    iret = nf90_put_att(ncid_out,varid_out(5),"stagger","")

!!!!!
! TMN
!!!!!

  iret = nf90_def_var(ncid_out,"TMN",NF90_FLOAT,(/dim_we,dim_sn,dim_time/),varid_out(6))
   if(iret/=0) print*, "Problem defining variable"
    iret = nf90_put_att(ncid_out,varid_out(6),"FieldType",104)
    iret = nf90_put_att(ncid_out,varid_out(6),"MemoryOrder","XY ")
    iret = nf90_put_att(ncid_out,varid_out(6),"description","SOIL TEMPERATURE AT LOWER BOUNDARY")
    iret = nf90_put_att(ncid_out,varid_out(6),"units","K")
    iret = nf90_put_att(ncid_out,varid_out(6),"stagger","")
    iret = nf90_put_att(ncid_out,varid_out(6),"coordinates","XLONG XLAT")

  iret = nf90_enddef(ncid_out)


!!!!!
! WRITE VARIABLES
!!!!!

  iret = nf90_put_var(ncid_out,varid_out(1),ivgtyp_out)
   if(iret/=0) print*, "Problem writing variable", iret

  iret = nf90_put_var(ncid_out,varid_out(2),isltyp_out)
   if(iret/=0) print*, "Problem writing variable", iret

  iret = nf90_put_var(ncid_out,varid_out(3),hgt_out)
   if(iret/=0) print*, "Problem writing variable", iret

  iret = nf90_put_var(ncid_out,varid_out(4),xlat_out)
   if(iret/=0) print*, "Problem writing variable", iret

  iret = nf90_put_var(ncid_out,varid_out(5),xlong_out)
   if(iret/=0) print*, "Problem writing variable", iret

  iret = nf90_put_var(ncid_out,varid_out(6),tmn_out)
   if(iret/=0) print*, "Problem writing variable", iret


  iret = nf90_close(ncid_out)
   if(iret/=0) print*, "Problem closing output file"
  
  iret = nf90_close(ncid_in)
   if(iret/=0) print*, "Problem closing input file"
  
    
end program
