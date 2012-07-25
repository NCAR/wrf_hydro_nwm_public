
        real function domain_sum_rt(var,ix,jx)
#ifdef MPP_LAND
           use module_mpp_land, only: write_IO_rt_real, global_rt_nx, global_rt_ny, &
                   mpp_land_bcast_real1
#endif
           implicit none
            real, dimension(ix,jx) :: var
            integer ix,jx
#ifdef MPP_LAND
            real, dimension(global_rt_nx,global_rt_ny):: outVar
            call write_IO_rt_real(var,outVar)
            domain_sum_rt = sum(outVar) 
            call mpp_land_bcast_real1(domain_sum_rt)
#else
            domain_sum_rt = sum(var) 
#endif
           
        end function domain_sum_rt

        real function get2d_real(var_name,out_buff,ix,jx,fileName)
          implicit none
#include <netcdf.inc>
          integer :: ivar, iret,varid,ncid,ix,jx
          real out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            out_buff = -9999.
            return
!            stop 9999
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_real(ncid, varid, out_buff)
          iret = nf_close(ncid)
          get2d_real =  ivar
       ! return
      end function get2d_real

  subroutine read_hrldas_hdrinfo(wrfsi_static_flnm, ix, jx, land_cat, soil_cat)
    ! Simply return the dimensions of the grid.
    implicit none
#include <netcdf.inc>
    character(len=*),          intent(in)  :: wrfsi_static_flnm
    integer, intent(out) :: ix, jx, land_cat, soil_cat ! dimensions

    integer :: iret, ncid, dimid

    ! Open the NetCDF file.
    write(*,'("wrfsi_static_flnm: ''", A, "''")') trim(wrfsi_static_flnm)
    iret = nf_open(wrfsi_static_flnm, NF_NOWRITE, ncid)
    if (iret /= 0) then
       write(*,'("Problem opening wrfsi_static file: ''", A, "''")') &
            trim(wrfsi_static_flnm)
       stop
    endif

    iret = nf_inq_dimid(ncid, "west_east", dimid)

    if (iret /= 0) then
       stop "nf_inq_dimid:  west_east"
    endif

    iret = nf_inq_dimlen(ncid, dimid, ix)
    if (iret /= 0) then
       stop "nf_inq_dimlen:  west_east"
    endif

    iret = nf_inq_dimid(ncid, "south_north", dimid)
    if (iret /= 0) then
       stop "nf_inq_dimid:  south_north"
    endif

    iret = nf_inq_dimlen(ncid, dimid, jx)
    if (iret /= 0) then
       stop "nf_inq_dimlen:  south_north"
    endif

    iret = nf_inq_dimid(ncid, "land_cat", dimid)
    if (iret /= 0) then
       stop "nf_inq_dimid:  land_cat"
    endif

    iret = nf_inq_dimlen(ncid, dimid, land_cat)
    if (iret /= 0) then
       stop "nf_inq_dimlen:  land_cat"
    endif

    iret = nf_inq_dimid(ncid, "soil_cat", dimid)
    if (iret /= 0) then
       stop "nf_inq_dimid:  soil_cat"
    endif

    iret = nf_inq_dimlen(ncid, dimid, soil_cat)
    if (iret /= 0) then
       stop "nf_inq_dimlen:  soil_cat"
    endif

    iret = nf_close(ncid)

    print*, 'ix, jx = ', ix, jx
  end subroutine read_hrldas_hdrinfo



  subroutine readland_hrldas(wrfsi_static_flnm,ix,jx,land_cat,soil_cat,vegtyp,soltyp, &
                  terrain,latitude,longitude,SOLVEG_INITSWC)
    implicit none
#include <netcdf.inc>
    character(len=*),          intent(in)  :: wrfsi_static_flnm
    integer,                   intent(in)  :: ix, jx, land_cat, soil_cat,SOLVEG_INITSWC
    integer, dimension(ix,jx), intent(out) :: vegtyp, soltyp
    real,    dimension(ix,jx), intent(out) :: terrain, latitude, longitude

    character(len=256) :: units
    integer :: ierr,i,j,jj
    integer :: ncid,varid
    real, dimension(ix,jx) :: xdum
    integer, dimension(ix,jx) :: vegtyp_inv, soiltyp_inv,xdum_int
    integer flag ! flag = 1 from wrfsi, flag =2 from WPS.
    CHARACTER(len=256)       :: var_name


    ! Open the NetCDF file.
    write(*,'("wrfsi_static_flnm: ''", A, "''")') trim(wrfsi_static_flnm)
    ierr = nf_open(wrfsi_static_flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("Problem opening wrfsi_static file: ''", A, "''")') trim(wrfsi_static_flnm)
       stop
    endif

    flag = -99 
    ierr = nf_inq_varid(ncid,"XLAT", varid)
    flag = 1
    if(ierr .ne. 0) then
        ierr = nf_inq_varid(ncid,"XLAT_M", varid)
        if(ierr .ne. 0) then
            write(6,*) "XLAT not found from wrfstatic file. "
            call land_finish()
        endif
        flag = 2
    endif
    print *, "Flag=", flag

    ! Get Latitude (lat)
    if(flag .eq. 1) then
       call get_2d_netcdf("XLAT", ncid, latitude,  units, ix, jx, .TRUE., ierr)
    else
      call get_2d_netcdf("XLAT_M", ncid, latitude,  units, ix, jx, .TRUE., ierr)
    endif
    print*, 'latitude(1,1) = ', latitude(1,1)

    ! Get Longitude (lon)
    if(flag .eq. 1) then 
        call get_2d_netcdf("XLONG", ncid, longitude, units, ix, jx, .TRUE., ierr)
    else
        call get_2d_netcdf("XLONG_M", ncid, longitude, units, ix, jx, .TRUE., ierr)
    endif
    print*, 'longitude(1,1) = ', longitude(1,1)

    ! Get Terrain (avg)
    if(flag .eq. 1) then
       call get_2d_netcdf("HGT", ncid, terrain,   units, ix, jx, .TRUE., ierr)
    else
        call get_2d_netcdf("HGT_M", ncid, terrain,   units, ix, jx, .TRUE., ierr)
    endif
    print*, 'terrain(1,1) = ', terrain(1,1)


    if (SOLVEG_INITSWC.eq.0) then
!      ! Get Dominant Land Use categories (use)
!      call get_landuse_netcdf(ncid, xdum ,   units, ix, jx, land_cat)
!      vegtyp = nint(xdum)

     var_name = "LU_INDEX"
         call get2d_int(var_name,xdum_int,ix,jx,&
               trim(wrfsi_static_flnm))
         vegtyp = xdum_int

      ! Get Dominant Soil Type categories in the top layer (stl)
      call get_soilcat_netcdf(ncid, xdum ,   units, ix, jx, soil_cat)
      soltyp = nint(xdum)

    else if (SOLVEG_INITSWC.eq.1) then
       var_name = "VEGTYP"
       call get2d_int(var_name,VEGTYP_inv,ix,jx,&
              trim(wrfsi_static_flnm))

       var_name = "SOILTYP"
       call get2d_int(var_name,SOILTYP_inv,ix,jx,&
              trim(wrfsi_static_flnm))
       do i=1,ix
         jj=jx
         do j=1,jx
           VEGTYP(i,j)=VEGTYP_inv(i,jj)
           SOLTYP(i,j)=SOILTYP_inv(i,jj)
           jj=jx-j
         end do
       end do

    endif

      print*, 'vegtyp(1,1) = ', vegtyp(1,1)
      print*, 'soltyp(1,1) = ', soltyp(1,1)


    ! Close the NetCDF file
    ierr = nf_close(ncid)
    if (ierr /= 0) stop "MODULE_NOAHLSM_HRLDAS_INPUT:  READLAND_HRLDAS:  NF_CLOSE"

    ! Make sure vegtyp and soltyp are consistent when it comes to water points,
    ! by setting soil category to water when vegetation category is water, and
    ! vice-versa.
    where (vegtyp == 16) soltyp = 14
    where (soltyp == 14) vegtyp = 16

!DJG test for deep gw function...
!    where (soltyp <> 14) soltyp = 1

  end subroutine readland_hrldas



  subroutine get_landuse_netcdf(ncid, array, units, idim, jdim, ldim)
    implicit none
#include <netcdf.inc>
    integer, intent(in) :: ncid
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j, l
    character(len=24), parameter :: name = "LANDUSEF"

    units = ""

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_landuse_netcdf:  nf_inq_varid"
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_landuse_netcdf:  nf_get_var_real"
    endif

    do i = 1, idim
       do j = 1, jdim
          mp = maxloc(xtmp(i,j,:))
          array(i,j) = mp(1)
          do l = 1,ldim
            if(xtmp(i,j,l).lt.0) array(i,j) = -9999.0
          enddo
       enddo
    enddo

  end subroutine get_landuse_netcdf


  subroutine get_soilcat_netcdf(ncid, array, units, idim, jdim, ldim)
    implicit none
#include <netcdf.inc>

    integer, intent(in) :: ncid
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j
    character(len=24), parameter :: name = "SOILCTOP"

    units = ""

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_soilcat_netcdf:  nf_inq_varid"
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_soilcat_netcdf:  nf_get_var_real"
    endif

    do i = 1, idim
       do j = 1, jdim
          mp = maxloc(xtmp(i,j,:))
          array(i,j) = mp(1)
       enddo
    enddo

    where (array == 14) array = 1   ! DJG remove all 'water' soils...

  end subroutine get_soilcat_netcdf


subroutine get_greenfrac_netcdf(ncid, array3, units, idim, jdim, ldim,mm,dd)
    implicit none
#include <netcdf.inc>
    integer, intent(in) :: ncid,mm,dd
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim) :: array
    real, dimension(idim,jdim) :: array2
    real, dimension(idim,jdim) :: diff
    real, dimension(idim,jdim), intent(out) :: array3
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j, mm2,daytot
    real :: ddfrac
    character(len=24), parameter :: name = "GREENFRAC"

    units = "fraction"

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_greenfrac_netcdf:  nf_inq_varid"
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_greenfrac_netcdf:  nf_get_var_real"
    endif


    if (mm.lt.12) then 
      mm2 = mm+1
    else
      mm2 = 1
    end if

!DJG_DES Set up dates for daily interpolation...
          if (mm.eq.1.OR.mm.eq.3.OR.mm.eq.5.OR.mm.eq.7.OR.mm.eq.8.OR.mm.eq.10.OR.mm.eq.12) then
             daytot = 31
          else if (mm.eq.4.OR.mm.eq.6.OR.mm.eq.9.OR.mm.eq.11) then 
             daytot = 30
          else if (mm.eq.2) then
             daytot = 28
          end if
          ddfrac = float(dd)/float(daytot)
          if (ddfrac.gt.1.0) ddfrac = 1.0   ! Assumes Feb. 29th change is same as Feb 28th

    print *,"DJG_DES Made it past netcdf read...month = ",mm,mm2,dd,daytot,ddfrac

    do i = 1, idim
       do j = 1, jdim
          array(i,j) = xtmp(i,j,mm)   !GREENFRAC in geogrid in units of fraction from month 1
          array2(i,j) = xtmp(i,j,mm2)   !GREENFRAC in geogrid in units of fraction from month 1
          diff(i,j) = array2(i,j) - array(i,j)
          array3(i,j) = array(i,j) + ddfrac * diff(i,j) 
       enddo
    enddo

end subroutine get_greenfrac_netcdf



subroutine get_albedo12m_netcdf(ncid, array3, units, idim, jdim, ldim,mm,dd)
    implicit none
#include <netcdf.inc>
    integer, intent(in) :: ncid,mm,dd
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim) :: array
    real, dimension(idim,jdim) :: array2
    real, dimension(idim,jdim) :: diff
    real, dimension(idim,jdim), intent(out) :: array3
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j, mm2,daytot
    real :: ddfrac
    character(len=24), parameter :: name = "ALBEDO12M"


    units = "fraction"

    iret = nf_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_albedo12m_netcdf:  nf_inq_varid"
    endif

    iret = nf_get_var_real(ncid, varid, xtmp)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_albedo12m_netcdf:  nf_get_var_real"
    endif

    if (mm.lt.12) then 
      mm2 = mm+1
    else
      mm2 = 1
    end if

!DJG_DES Set up dates for daily interpolation...
          if (mm.eq.1.OR.mm.eq.3.OR.mm.eq.5.OR.mm.eq.7.OR.mm.eq.8.OR.mm.eq.10.OR.mm.eq.12) then
             daytot = 31
          else if (mm.eq.4.OR.mm.eq.6.OR.mm.eq.9.OR.mm.eq.11) then 
             daytot = 30
          else if (mm.eq.2) then
             daytot = 28
          end if
          ddfrac = float(dd)/float(daytot)
          if (ddfrac.gt.1.0) ddfrac = 1.0   ! Assumes Feb. 29th change is same as Feb 28th

    print *,"DJG_DES Made it past netcdf read...month = ",mm,mm2,dd,daytot,ddfrac

    do i = 1, idim
       do j = 1, jdim
          array(i,j) = xtmp(i,j,mm) / 100.0   !Convert ALBEDO12M from % to fraction...month 1
          array2(i,j) = xtmp(i,j,mm2) / 100.0   !Convert ALBEDO12M from % to fraction... month 2
          diff(i,j) = array2(i,j) - array(i,j)
          array3(i,j) = array(i,j) + ddfrac * diff(i,j) 
       enddo
    enddo

end subroutine get_albedo12m_netcdf



  subroutine get_2d_netcdf(name, ncid, array, units, idim, jdim, &
       fatal_if_error, ierr)
    implicit none
#include <netcdf.inc>
    character(len=*), intent(in) :: name
    integer, intent(in) :: ncid
    integer, intent(in) :: idim, jdim
    real, dimension(idim,jdim), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    ! .TRUE._IF_ERROR:  an input code value:
    !      .TRUE. if an error in reading the data should stop the program.
    !      Otherwise the, IERR error flag is set, but the program continues.
    logical, intent(in) :: fatal_if_error 
    integer, intent(out) :: ierr

    units = ""

    iret = nf_inq_varid(ncid,  name,  varid)

    if (iret /= 0) then
       if (fatal_IF_ERROR) then
          print*, 'name = "', trim(name)//'"'
          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf_inq_varid"
       else
          ierr = iret
          return
       endif
    endif

!DJG    iret = nf_get_att_text(ncid, varid, "units", units)
!DJG   if (iret /= 0) then
!DJG      if (.TRUE._IF_ERROR) then
!DJG          print*, 'name = "', trim(name)//'"'
!DJG          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf_get_att_text"
!DJG       else
!DJG          ierr = iret
!DJG          return
!DJG       endif
!DJG    endif

    iret = nf_get_var_real(ncid, varid, array)
    if (iret /= 0) then
       if (fatal_IF_ERROR) then
          print*, 'name = "', trim(name)//'"'
          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf_get_var_real"
       else
          ierr = iret
          return
       endif
    endif
!   print *, name//' = ', idim,jdim
!   print*, name//' = ', array(1,1), array(1,jdim), array(idim,jdim), array(idim,1)

    ierr = 0;
  end subroutine get_2d_netcdf

  

      subroutine get_2d_netcdf_ruc(var_name,ncid,var, &
            ix,jx,tlevel,fatal_if_error,ierr)
#include <netcdf.inc>
          character(len=*), intent(in) :: var_name
          integer,intent(in) ::  ncid,ix,jx,tlevel
          real, intent(out):: var(ix,jx)
          logical, intent(in) :: fatal_if_error
          integer dims(4), dim_len(4)
          integer ierr,iret
          integer varid
           integer start(4),count(4)
           data count /1,1,1,1/
           data start /1,1,1,1/
          count(1) = ix
          count(2) = jx
          start(4) = tlevel
      iret = nf_inq_varid(ncid,  var_name,  varid)

      if (iret /= 0) then
        if (fatal_IF_ERROR) then
           stop "MODULE_NOAHLSM_HRLDAS_INPUT: get_2d_netcdf_ruc:nf_inq_varid"
        else
          ierr = iret
          return
        endif
      endif

      iret = nf_get_vara_real(ncid, varid, start,count,var)

      return
      end subroutine get_2d_netcdf_ruc



      subroutine get_2d_netcdf_cows(var_name,ncid,var, &
            ix,jx,tlevel,fatal_if_error,ierr)
#include <netcdf.inc>
          character(len=*), intent(in) :: var_name
          integer,intent(in) ::  ncid,ix,jx,tlevel
          real, intent(out):: var(ix,jx)
          logical, intent(in) :: fatal_if_error
          integer ierr, iret
          integer varid
          integer start(4),count(4)
          data count /1,1,1,1/
          data start /1,1,1,1/
          count(1) = ix
          count(2) = jx
          start(4) = tlevel
      iret = nf_inq_varid(ncid,  var_name,  varid)

      if (iret /= 0) then
        if (fatal_IF_ERROR) then
           stop "MODULE_NOAHLSM_HRLDAS_INPUT: get_2d_netcdf_cows:nf_inq_varid"
        else
          ierr = iret
          return
        endif
      endif
      iret = nf_get_vara_real(ncid, varid, start,count,var)

      return
      end subroutine get_2d_netcdf_cows





  subroutine readinit_hrldas(netcdf_flnm, ix, jx, nsoil, target_date, &
       smc, stc, sh2o, cmc, t1, weasd, snodep)
    implicit none
#include <netcdf.inc>
    character(len=*),                intent(in)  :: netcdf_flnm
    integer,                         intent(in)  :: ix
    integer,                         intent(in)  :: jx
    integer,                         intent(in)  :: nsoil
    character(len=*),                intent(in)  :: target_date
    real,    dimension(ix,jx,nsoil), intent(out) :: smc
    real,    dimension(ix,jx,nsoil), intent(out) :: stc
    real,    dimension(ix,jx,nsoil), intent(out) :: sh2o
    real,    dimension(ix,jx),       intent(out) :: cmc
    real,    dimension(ix,jx),       intent(out) :: t1
    real,    dimension(ix,jx),       intent(out) :: weasd
    real,    dimension(ix,jx),       intent(out) :: snodep

    character(len=256) :: units
    character(len=8) :: name
    integer :: ix_read, jx_read,i,j

    integer :: ierr, ncid, ierr_snodep
    integer :: idx

    logical :: found_canwat, found_skintemp, found_weasd, found_stemp, found_smois

    ! Open the NetCDF file.
    write(*,'("netcdf_flnm: ''", A, "''")') trim(netcdf_flnm)
    ierr = nf_open(netcdf_flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READINIT Problem opening netcdf file: ''", A, "''")') &
            trim(netcdf_flnm)
       stop
    endif

    call get_2d_netcdf("CANWAT",     ncid, cmc,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SKINTEMP",   ncid, t1,      units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("WEASD",      ncid, weasd,   units, ix, jx, .TRUE., ierr)

    if (trim(units) == "m") then
       ! No conversion necessary
    else if (trim(units) == "mm") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else
       print*, 'units = "'//trim(units)//'"'
       stop "Unrecognized units on WEASD"
    endif

    call get_2d_netcdf("SNODEP",     ncid, snodep,   units, ix, jx, .FALSE., ierr_snodep)
    call get_2d_netcdf("STEMP_1",    ncid, stc(:,:,1), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("STEMP_2",    ncid, stc(:,:,2), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("STEMP_3",    ncid, stc(:,:,3), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("STEMP_4",    ncid, stc(:,:,4), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SMOIS_1",    ncid, smc(:,:,1), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SMOIS_2",    ncid, smc(:,:,2), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SMOIS_3",    ncid, smc(:,:,3), units,  ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SMOIS_4",    ncid, smc(:,:,4), units,  ix, jx, .TRUE., ierr)


    if (ierr_snodep /= 0) then
       ! Quick assumption regarding snow depth.
       snodep = weasd * 10.
    endif


!DJG check for erroneous neg WEASD or SNOWD due to offline interpolation...
       do i=1,ix
         do j=1,jx
           if (WEASD(i,j).lt.0.) WEASD(i,j)=0.0  !set lower bound to correct bi-lin interp err...
           if (snodep(i,j).lt.0.) snodep(i,j)=0.0  !set lower bound to correct bi-lin interp err...
         end do
       end do


    sh2o = smc

    ierr = nf_close(ncid)
  end subroutine readinit_hrldas




  subroutine READFORC_HRLDAS(flnm,ix,jx,target_date, t,q,u,v,p,lw,sw,pcp,lai,fpar)
    implicit none
#include <netcdf.inc>

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx), intent(out) :: t
    real,             dimension(ix,jx), intent(out) :: q
    real,             dimension(ix,jx), intent(out) :: u
    real,             dimension(ix,jx), intent(out) :: v
    real,             dimension(ix,jx), intent(out) :: p
    real,             dimension(ix,jx), intent(out) :: lw
    real,             dimension(ix,jx), intent(out) :: sw
    real,             dimension(ix,jx), intent(out) :: pcp
    real,             dimension(ix,jx), intent(out) :: lai
    real,             dimension(ix,jx), intent(out) :: fpar

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    ! Open the NetCDF file.
!KWM    write(*,'("flnm: ''", A, "''")') trim(flnm)
    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC Problem opening netcdf file: ''", A, "''")') trim(flnm)
       stop
    endif

    call get_2d_netcdf("T2D",     ncid, t,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("Q2D",     ncid, q,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("U2D",     ncid, u,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("V2D",     ncid, v,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("PSFC",    ncid, p,     units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("LWDOWN",  ncid, lw,    units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("SWDOWN",  ncid, sw,    units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("RAINRATE",ncid, pcp,   units, ix, jx, .TRUE., ierr)
    call get_2d_netcdf("VEGFRA",  ncid, fpar,  units, ix, jx, .FALSE., ierr)
    if (ierr == 0) then
       fpar = fpar * 1.E-2
    endif
    call get_2d_netcdf("LAI",     ncid, lai,   units, ix, jx, .FALSE., ierr)

    ierr = nf_close(ncid)

  end subroutine READFORC_HRLDAS



  subroutine READFORC_DMIP(flnm,ix,jx,var)
    implicit none
#include <netcdf.inc>

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    real,       dimension(ix,jx), intent(out)       :: var
    character(len=13)                               :: head
    integer                          :: ncols, nrows, cellsize
    real                             :: xllc, yllc, no_data
    integer                          :: i,j
    character(len=256)                              ::junk

    open (77,file=trim(flnm),form="formatted",status="old")

!    read(77,732) head,ncols
!    read(77,732) head,nrows
!732        FORMAT(A13,I4)
!    read(77,733) head,xllc
!    read(77,733) head,yllc
!733        FORMAT(A13,F16.9)
!    read(77,732) head,cellsize
!    read(77,732) head,no_data

    read(77,*) junk
    read(77,*) junk
    read(77,*) junk
    read(77,*) junk
    read(77,*) junk
    read(77,*) junk

    do j=jx,1,-1
      read(77,*) (var(I,J),I=1,ix)
    end do
    close(77)

  end subroutine READFORC_DMIP



  subroutine READFORC_MDV(flnm,ix,jx,pcp,mmflag,ierr_flg)
    implicit none
#include <netcdf.inc>

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    integer,                            intent(out)  :: ierr_flg
    integer :: it,jew,zsn
    real,             dimension(ix,jx), intent(out) :: pcp

    character(len=256) :: units
    integer :: ierr,i,j,i2,j2,varid
    integer :: ncid,mmflag
    real, dimension(ix,jx) :: temp

    mmflag = 0   ! flag for units spec. (0=mm, 1=mm/s)


!open NetCDF file...
        ierr_flg = nf_open(flnm, NF_NOWRITE, ncid)
        if (ierr_flg /= 0) then
          write(*,'("READFORC_MDV Problem opening netcdf file: ''",A,"''")') &
                trim(flnm)
!          stop
           return
        end if

        ierr = nf_inq_varid(ncid,  "precip",  varid)
        if (ierr /= 0) then
          ierr = nf_inq_varid(ncid,  "pcp_daily",  varid)   !recheck variable name...
          if (ierr /= 0) then
            write(*,'("READFORC_MDV Problem reading precip netcdf file: ''", A,"''")') &
                 trim(flnm)
!            stop
          end if
          mmflag = 1
        end if
        ierr = nf_get_var_real(ncid, varid, pcp)
        ierr = nf_close(ncid)

        if (ierr /= 0) then
          write(*,'("READFORC_MDV Problem reading netcdf file: ''", A,"''")') trim(flnm)
!          stop
        end if

  end subroutine READFORC_MDV



  subroutine READFORC_NAMPCP(flnm,ix,jx,pcp,k,product)
    implicit none
#include <netcdf.inc>

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    integer,                            intent(in)  :: k
    character(len=*),                   intent(in)  :: product
    integer :: it,jew,zsn
    parameter(it =  496,jew = 449, zsn = 499)   ! domain 1
!    parameter(it =  496,jew = 74, zsn = 109)   ! domain 2
    real,             dimension(it,jew,zsn) :: buf
    real,             dimension(ix,jx), intent(out) :: pcp

    character(len=256) :: units
    integer :: ierr,i,j,i2,j2,varid
    integer :: ncid
    real, dimension(ix,jx) :: temp

!      varname = trim(product)

!open NetCDF file...
      if (k.eq.1.) then
        ierr = nf_open(flnm, NF_NOWRITE, ncid)
        if (ierr /= 0) then
          write(*,'("READFORC_NAMPCP1 Problem opening netcdf file: ''",A, "''")') &
              trim(flnm)
          stop
        end if

        ierr = nf_inq_varid(ncid,  trim(product),  varid)
        ierr = nf_get_var_real(ncid, varid, buf)
        ierr = nf_close(ncid)

        if (ierr /= 0) then
          write(*,'("READFORC_NAMPCP2 Problem reading netcdf file: ''", A,"''")') &
             trim(flnm)
          stop
        end if
      endif

      print *, "Data read in...",it,ix,jx,k

! Extract single time slice from dataset...

      do i=1,ix
        do j=1,jx
          pcp(i,j) = buf(k,i,j)
        end do
      end do

!      call get_2d_netcdf_ruc("trmm",ncid, pcp, jx, ix,k, .true., ierr)

  end subroutine READFORC_NAMPCP




  subroutine READFORC_COWS(flnm,ix,jx,target_date, t,q,u,p,lw,sw,pcp,tlevel)
    implicit none
#include <netcdf.inc>

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx), intent(out) :: t
    real,             dimension(ix,jx), intent(out) :: q
    real,             dimension(ix,jx), intent(out) :: u
    real,             dimension(ix,jx) :: v
    real,             dimension(ix,jx), intent(out) :: p
    real,             dimension(ix,jx), intent(out) :: lw
    real,             dimension(ix,jx), intent(out) :: sw
    real,             dimension(ix,jx), intent(out) :: pcp
    integer   tlevel

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    ! Open the NetCDF file.
!KWM    write(*,'("flnm: ''", A, "''")') trim(flnm)
    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC_COWS Problem opening netcdf file: ''", A, "''")') trim(flnm)
       stop
    endif

    call get_2d_netcdf_cows("TA2",     ncid, t,     ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("QV2",     ncid, q,     ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("WSPD10",  ncid, u,     ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("PRES",    ncid, p,     ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("GLW",     ncid, lw,    ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("RSD",     ncid, sw,    ix, jx,tlevel, .TRUE., ierr)
    call get_2d_netcdf_cows("RAIN",    ncid, pcp,   ix, jx,tlevel, .TRUE., ierr)
!yw   call get_2d_netcdf_cows("V2D",     ncid, v,     ix, jx,tlevel, .TRUE., ierr)

    ierr = nf_close(ncid)

  end subroutine READFORC_COWS




  subroutine READFORC_RUC(flnm,ix,jx,target_date,t,q,u,v,p,lw,sw,pcp)
    
    implicit none
#include <netcdf.inc>

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx) :: t,q,u,v,p,lw,sw,pcp,pcpc
    integer   tlevel

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    tlevel = 1

    ! Open the NetCDF file.
    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC_RUC Problem opening netcdf file: ''", A, "''")') trim(flnm)
       stop
    endif

    call get_2d_netcdf_ruc("T2",     ncid, t,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("Q2",     ncid, q,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("U10",    ncid, u,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("V10",    ncid, v,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("PSFC",   ncid, p,     ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("GLW",    ncid, lw,    ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("SWDOWN", ncid, sw,    ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("RAINC",  ncid, pcpc,  ix, jx,tlevel, .true., ierr)
    call get_2d_netcdf_ruc("RAINNC", ncid, pcp,   ix, jx,tlevel, .true., ierr)

    ierr = nf_close(ncid)
    

!DJG  Add the convective and non-convective rain components (note: conv. comp=0
!for cloud resolving runs...) 
!DJG  Note that for RUC these are accumulated values to be adjusted to rates in
!driver...

    pcp=pcp+pcpc   ! assumes pcpc=0 for resolved convection...
!    print *, pcp

  end subroutine READFORC_RUC




  subroutine READSNOW_HRLDAS(flnm,ix,jx,target_date,weasd,snodep)
    implicit none
#include <netcdf.inc>

    character(len=*),                   intent(in)  :: flnm
    integer,                            intent(in)  :: ix
    integer,                            intent(in)  :: jx
    character(len=*),                   intent(in)  :: target_date
    real,             dimension(ix,jx), intent(out) :: weasd
    real,             dimension(ix,jx), intent(out) :: snodep

    character(len=256) :: units
    integer :: ierr
    integer :: ncid,i,j

    ! Open the NetCDF file.

    ierr = nf_open(flnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READSNOW Problem opening netcdf file: ''", A, "''")') trim(flnm)
       stop
    endif

    call get_2d_netcdf("WEASD",  ncid, weasd,   units, ix, jx, .FALSE., ierr)

    if (ierr /= 0) then
       print *, "!!!!! NO WEASD present in input file...initialize to 0."
       weasd = 0.
       units = "mm"
    endif

    if (trim(units) == "m") then
       ! No conversion necessary
    else if (trim(units) == "mm") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else
       print*, 'units = "'//trim(units)//'"'
       stop "Unrecognized units on WEASD"
    endif

    call get_2d_netcdf("SNODEP",     ncid, snodep,   units, ix, jx, .FALSE., ierr)

    if (ierr /= 0) then
       ! Quick assumption regarding snow depth.
       snodep = weasd * 10.
    endif

!DJG check for erroneous neg WEASD or SNOWD due to offline interpolation...
       do i=1,ix
         do j=1,jx
           if (WEASD(i,j).lt.0.) WEASD(i,j)=0.0  !set lower bound to correct bi-lin interp err...
           if (snodep(i,j).lt.0.) snodep(i,j)=0.0  !set lower bound to correct bi-lin interp err...
         end do
       end do

    ierr = nf_close(ncid)

  end subroutine READSNOW_HRLDAS

!---------------------------------------------------------
!DJG Subroutinesfor inputting routing fields...
!DNY   first reads the files to get the size of the 
!DNY   LINKS arrays
!DJG   - Currently only hi-res topo is read 
!DJG   - At a future time, use this routine to input
!DJG     subgrid land-use classification or routing
!DJG     parameters 'overland roughness' and 'retention
!DJG     depth'
!
!DJG,DNY - Update this subroutine to read in channel and lake
!           parameters if activated       11.20.2005
!---------------------------------------------------------
       SUBROUTINE READ_ROUTEDIM(IXRT,JXRT,route_chan_f,route_link_f, &
            route_direction_f, route_lake_f, NLINKS, NLAKES, &
            CH_NETLNK, channel_option, wrfsi_finegrid_flnm)

         implicit none
#include <netcdf.inc>
        INTEGER                                      :: I,J,channel_option,iret,jj
        INTEGER, INTENT(INOUT)                       :: NLINKS, NLAKES
        INTEGER, INTENT(IN)                          :: IXRT,JXRT
        INTEGER                                      :: CHNID,cnt
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_NETRT   !- binary channel mask
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETLNK  !- each node gets unique id
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION  !- flow direction
        INTEGER, DIMENSION(IXRT,JXRT)                :: LAKE_MSKRT
        REAL, DIMENSION(IXRT,JXRT)                   :: LAT, LON

!!Dummy read in grids for inverted y-axis
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_NETRT_inv   !- binary channel mask
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION_inv  !- flow direction
        INTEGER, DIMENSION(IXRT,JXRT)                :: LAKE_MSKRT_inv
        REAL, DIMENSION(IXRT,JXRT)                   :: LAT_inv, LON_inv


        CHARACTER(len=*)       :: route_chan_f, route_link_f,route_direction_f,route_lake_f
        CHARACTER(len=256)       :: InputLine
        CHARACTER(len=256)       :: wrfsi_finegrid_flnm
        CHARACTER(len=256)       :: var_name
        external get2d_real
        real get2d_real
     
        NLINKS = 0
        NLAKES = 0
        CH_NETRT = -9999
        CH_NETLNK = -9999


        cnt = 0 
       print *, "Channel Option in Routedim is ", channel_option
       write(6,*) "ixrt,jxrt ",ixrt,jxrt

       IF(channel_option.eq.3) then  !get maxnodes and links from grid

         var_name = "CHANNELGRID"
         call get2d_int(var_name,CH_NETRT_inv,ixrt,jxrt,&
                   trim(wrfsi_finegrid_flnm))
         
         var_name = "FLOWDIRECTION"
         call get2d_int(var_name,DIRECTION_inv,ixrt,jxrt,&
                   trim(wrfsi_finegrid_flnm))

         var_name = "LAKEGRID"
         call get2d_int(var_name,LAKE_MSKRT_inv,ixrt,jxrt,&
                   trim(wrfsi_finegrid_flnm))


        var_name = "LATITUDE"
        iret =  get2d_real(var_name,LAT_inv,ixrt,jxrt,&
                     trim(wrfsi_finegrid_flnm))
        var_name = "LONGITUDE"
        iret = get2d_real(var_name,LON_inv,ixrt,jxrt,&
                     trim(wrfsi_finegrid_flnm))

!!!Flip y-dimension of highres grids from exported Arc files...

        write(6,*) "ixrt,jxrt=",ixrt,jxrt

        do i=1,ixrt
        jj=jxrt
         do j=1,jxrt
           CH_NETRT(i,j)=CH_NETRT_inv(i,jj)
           DIRECTION(i,j)=DIRECTION_inv(i,jj)
           LAKE_MSKRT(i,j)=LAKE_MSKRT_inv(i,jj)
           LAT(i,j)=LAT_inv(i,jj)
           LON(i,j)=LON_inv(i,jj)
           jj=jxrt-j
         end do
        end do
          
! temp fix for buggy Arc export...
        do j=1,jxrt
          do i=1,ixrt
            if(DIRECTION(i,j).eq.-128) DIRECTION(i,j)=128
          end do
        end do


!DJG inv         do j=jxrt,1,-1
         do j=1,jxrt
             do i = 1, ixrt
!                print *, CH_NETRT(i,j),i,j
               if (CH_NETRT(i,j) .ge.0.AND.CH_NETRT(i,j).lt.100) then 
                 NLINKS = NLINKS + 1
               endif
            end do 
         end do 
         print *, "NLINKS IS ", NLINKS 


!DJG inv         DO j = JXRT,1,-1  !rows
         DO j = 1,JXRT  !rows
          DO i = 1 ,IXRT   !colsumns
           If (CH_NETRT(i, j) .ge. 0) then !get its direction
            If ((DIRECTION(i, j) .EQ. 64) .AND. (j+1 .LE. JXRT).AND.(CH_NETRT(i,j+1) .ge.0) ) then !North
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .LE. IXRT) &
               .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i+1,j+1) .ge.0)) then !North East
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .LE. IXRT).AND.(CH_NETRT(i+1,j) .ge. 0)) then !East
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt 
            else if ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j - 1 .NE. 0).AND.(CH_NETRT(i+1,j-1).ge.0)) then !south east
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 4).AND.(j - 1 .NE. 0).AND.(CH_NETRT(i,j-1).ge.0)) then !due south
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .GT. 0) &
                    .AND. (j - 1 .NE. 0).AND. (CH_NETRT(i-1,j-1).ge.0) ) then !south west
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .GT. 0).AND.(CH_NETRT(i-1,j).ge.0)) then !West
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt
            else if ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .GT. 0) &
                    .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i-1,j+1).ge.0)) then !North West
             cnt = cnt + 1
             CH_NETLNK(i,j) = cnt 
           else 
             write(*,135) "PrPt/LkIn", CH_NETRT(i,j), DIRECTION(i,j), LON(i,j), LAT(i,j),i,j 
135             FORMAT(A9,1X,I3,1X,I3,1X,F10.5,1X,F9.5,1X,I4,1X,I4)
!             print *, "Pr Pt or Lk Inflow", CH_NETRT(i,j), DIRECTION(i,j), i,j 
             if (DIRECTION(i,j) .eq. 0) then
               print *, "Direction i,j ",i,j," of point ", cnt, "is invalid"
             endif

           End If
         End If !CH_NETRT check for this node
        END DO
       END DO 
       print *, "found type 0 nodes", cnt
      
!Find out if the boundaries are on an edge or flow into a lake
!DJG inv       DO j = JXRT,1,-1
       DO j = 1,JXRT
         DO i = 1 ,IXRT
          If (CH_NETRT(i, j) .ge. 0) then !get its direction

           If ( ((DIRECTION(i, j).EQ. 64) .AND. (j + 1 .GT. JXRT))         & !-- 64's can only flow north
               .OR. ((DIRECTION(i, j) .EQ. 64).and. (j<jxrt) .AND. (CH_NETRT(i,j+1) .lt. 0))) then !North
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
              print *, "Boundary Pour Point N", cnt,CH_NETRT(i,j), i,j
           else if ( ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .GT. IXRT))  & !-- 128's can flow out of the North or East edge
               .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (j + 1 .GT. JXRT))  & !   this is due north edge     
               .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (i<ixrt .and. j<jxrt) .AND.(CH_NETRT(i + 1, j + 1).lt.0))) then !North East
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
              print *, "Boundary Pour Point NE", cnt, CH_NETRT(i,j),i,j
           else if (((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .GT. IXRT))     & !-- 1's can only flow due east
               .OR. ((DIRECTION(i, j) .EQ. 1) .and. (i<ixrt) .AND. (CH_NETRT(i + 1, j) .lt. 0))) then !East
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
              print *, "Boundary Pour Point E", cnt,CH_NETRT(i,j), i,j
           else if ( ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .GT. IXRT))    &      !-- 2's can flow out of east or south edge
                .OR. ((DIRECTION(i, j) .EQ. 2) .AND. (j - 1 .EQ. 0))       &      !-- this is the south edge
                .OR. ((DIRECTION(i, j) .EQ. 2) .and. (i<ixrt .and. j>1) .AND.(CH_NETRT(i + 1, j - 1) .lt.0))) then !south east
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
              print *, "Boundary Pour Point SE", cnt,CH_NETRT(i,j), i,j
           else if ( ((DIRECTION(i, j) .EQ. 4) .AND. (j - 1 .EQ. 0))       &      !-- 4's can only flow due south
                .OR. ((DIRECTION(i, j) .EQ. 4) .and. (j>1) .AND.(CH_NETRT(i, j - 1) .lt. 0))) then !due south
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
              print *, "Boundary Pour Point S", cnt,CH_NETRT(i,j), i,j
           else if ( ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .LE. 0))      &      !-- 8's can flow south or west
               .OR.  ((DIRECTION(i, j) .EQ. 8) .AND. (j - 1 .EQ. 0))      &      !-- this is the south edge
               .OR.  ((DIRECTION(i, j).EQ.8).and. (i>1 .and. j>1) .AND.(CH_NETRT(i - 1, j - 1).lt.0))) then !south west
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
              print *, "Boundary Pour Point SW", cnt,CH_NETRT(i,j), i,j
           else if ( ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .LE. 0))       &      !-- 16's can only flow due west 
               .OR.  ((DIRECTION(i, j).EQ.16) .and. (i>1) .AND.(CH_NETRT(i - 1, j).lt.0))) then !West
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt              
              print *, "Boundary Pour Point W", cnt,CH_NETRT(i,j), i,j
           else if ( ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .LE. 0))      &      !-- 32's can flow either west or north
               .OR.  ((DIRECTION(i, j) .EQ. 32) .AND. (j + 1 .GT. JXRT))   &      !-- this is the north edge
               .OR.  ((DIRECTION(i, j).EQ.32) .and. (i>1 .and. j<jxrt) .AND.(CH_NETRT(i - 1, j + 1).lt.0))) then !North West
              cnt = cnt + 1
              CH_NETLNK(i,j) = cnt
              print *, "Boundary Pour Point NW", cnt,CH_NETRT(i,j), i,j
           endif
          endif !CH_NETRT check for this node
         END DO
       END DO 

       print *, "total number of channel elements", cnt
       print *, "total number of NLINKS          ", NLINKS
       write(*,*) " " 



      !-- get the number of lakes
       if (cnt .ne. NLINKS) then 
         print *, "Apparent error in network topology", cnt, NLINKS
         stop
       endif
!DJG inv       do j=jxrt,1,-1
       do j=1,jxrt
          do i = 1,ixrt
           if (LAKE_MSKRT(i,j) .gt. NLAKES) then 
             NLAKES = LAKE_MSKRT(i,j)
           endif
        end do
       end do
       write(6,*) "finish read_red ..  nlakes = ", nlakes
       call flush(6)

       return

     else  ! get nlinks from the ascii file of links
        open(unit=79,file=trim(route_link_f),          & !link
                   form='formatted',status='old')
                   write(6,*) "read file ",route_link_f
1011    read(79,*,end= 1999) InputLine
        NLINKS = NLINKS + 1
        goto 1011
1999   continue
        NLINKS = NLINKS - 1 !-- first line is a comment 
        close(79)
        print *, "Number of Links on sparse network", NLINKS
        return
     end if

     END SUBROUTINE READ_ROUTEDIM

!---------------------------------------------------------
       SUBROUTINE READ_CHROUTING(IXRT,JXRT,ELRT,CH_NETRT, LAKE_MSKRT, &
            FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
            NLAKES, MUSK, MUSX, QLINK, CHANLEN, MannN, So, ChSSlp, Bw, &
            HRZAREA, LAKEMAXH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
            ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, &
            route_link_f, &
            route_lake_f, route_direction_f, route_order_f, &
            CHANRTSWCRT,dist, ZELEV, LAKENODE, CH_NETLNK, &
            CHANXI, CHANYJ, CHLAT, CHLON,  &
            channel_option,forctyp,LATVAL,LONVAL, &
            STRMFRXSTPTS,wrfsi_finegrid_flnm)
        use module_NDHMS_utils, only: get_dist_ll
#include <netcdf.inc>
        INTEGER, INTENT(IN)                          :: IXRT,JXRT
        INTEGER                                      :: CHANRTSWCRT, NLINKS, NLAKES
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)       :: ELRT
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION
        INTEGER, DIMENSION(IXRT,JXRT)                :: GSTRMFRXSTPTS
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
        INTEGER,                DIMENSION(IXRT,JXRT) :: GORDER  !-- gridded stream orderk
        INTEGER,                DIMENSION(IXRT,JXRT) :: Link_Location !-- gridded stream orderk
        INTEGER                                      :: I,J,channel_option
        INTEGER                                      :: forctyp
        REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)      :: LATVAL, LONVAL
        CHARACTER(len=28)                            :: dir
!Dummy inverted grids from arc
	INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION_inv
        INTEGER, DIMENSION(IXRT,JXRT)                :: GSTRMFRXSTPTS_inv
        INTEGER, DIMENSION(IXRT,JXRT) :: LAKE_MSKRT_inv
        INTEGER,                DIMENSION(IXRT,JXRT) :: GORDER_inv  !-- gridded stream orderk
        REAL, DIMENSION(IXRT,JXRT)      :: LATVAL_inv, LONVAL_inv


!----DJG,DNY New variables for channel and lake routing
        CHARACTER(len=155)	 :: header
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: FROM_NODE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ZELEV
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHLAT,CHLON

        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TYPEL
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TO_NODE,ORDER
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: STRMFRXSTPTS

        INTEGER, INTENT(INOUT)                       :: MAXORDER
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MUSK, MUSX !muskingum
        REAL, INTENT(INOUT),  DIMENSION(NLINKS,2)    :: QLINK  !channel flow
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHANLEN   !channel length
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MannN, So !mannings N
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: LAKENODE  ! identifies which nodes pour into which lakes
        REAL, INTENT(IN)                             :: dist(ixrt,jxrt,9)

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT)    :: CH_NETLNK
        REAL,  DIMENSION(IXRT,JXRT)                  ::  ChSSlpG,BwG,MannNG  !channel properties on Grid


!-- store the location x,y location of the channel element
         INTEGER, INTENT(INOUT), DIMENSION(NLINKS)   :: CHANXI, CHANYJ

!--reservoir/lake attributes
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: HRZAREA
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: LAKEMAXH
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: WEIRC
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: WEIRL
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEC
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEA
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: LATLAKE,LONLAKE,ELEVLAKE
        REAL, INTENT(INOUT), DIMENSION(NLINKS)       :: ChSSlp, Bw

        CHARACTER(len=256)                           :: route_link_f
        CHARACTER(len=256)                           :: route_lake_f
        CHARACTER(len=256)                           :: route_direction_f
        CHARACTER(len=256)                           :: route_order_f
        CHARACTER(len=256)                           :: wrfsi_finegrid_flnm
        CHARACTER(len=256)                           :: var_name

        INTEGER                                      :: tmp, cnt, ncid, iret, jj,ct
        real                                         :: gc,n

!---------------------------------------------------------
! End Declarations
!---------------------------------------------------------
        MAXORDER = -9999
!initialize GSTRM
        GSTRMFRXSTPTS = -9999

!yw initialize the array.
        to_node =   MAXORDER
        from_node = MAXORDER

        print *, "reading routing initialization files..."
        print *, "route direction", route_direction_f
        print *, "route order", route_order_f
        print *, "route linke",route_link_f
        print *, "route lake",route_lake_f

!DJG Edited code here to retrieve data from hires netcdf file....

   IF((CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2).AND.channel_option.eq.3) then

        var_name = "LATITUDE"
        iret = get2d_real(var_name,LATVAL_inv,ixrt,jxrt,&
                     trim(wrfsi_finegrid_flnm))
        var_name = "LONGITUDE"
        iret = get2d_real(var_name,LONVAL_inv,ixrt,jxrt,&
                     trim(wrfsi_finegrid_flnm))

    END IF


       IF(CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2) then
!DJG change filename to LAKEPARM.TBL        open(unit=79,file=trim(route_link_f),          &
        open(unit=79,file='LAKEPARM.TBL',          &
                  form='formatted',status='old')
       END IF


       var_name = "LAKEGRID"
       call get2d_int(var_name,LAKE_MSKRT_inv,ixrt,jxrt,&
               trim(wrfsi_finegrid_flnm))
       var_name = "FLOWDIRECTION"
       call get2d_int(var_name,DIRECTION_inv,ixrt,jxrt,&
               trim(wrfsi_finegrid_flnm))
       var_name = "STREAMORDER"
       call get2d_int(var_name,GORDER_inv,ixrt,jxrt,&
               trim(wrfsi_finegrid_flnm))
       var_name = "frxst_pts"
       call get2d_int(var_name,GSTRMFRXSTPTS_inv,ixrt,jxrt,&
               trim(wrfsi_finegrid_flnm))

!--1/13/2011 real hi res sfc calibrtion parameters (...)
!       var_name = "LAKEGRID"
!       call get2d_int(var_name,LAKE_MSKRT_inv,ixrt,jxrt,&
!               trim(wrfsi_finegrid_flnm))
!       var_name = "LAKEGRID"
!       call get2d_int(var_name,LAKE_MSKRT_inv,ixrt,jxrt,&
!               trim(wrfsi_finegrid_flnm))


!-- real hi res channel properties (not yet implemented...)
!        var_name = "MANNINGS"
!        iret = get2d_real(var_name,MannNG,ixrt,jxrt,&
!                     trim(wrfsi_finegrid_flnm))
!        var_name = "SIDE_SLOPE"
!        iret = get2d_real(var_name,ChSSlpG,ixrt,jxrt,&
!                     trim(wrfsi_finegrid_flnm))
!        var_name = "BOTTOM_WIDTH"
!        iret = get2d_real(var_name,BwG,ixrt,jxrt,&
!                     trim(wrfsi_finegrid_flnm))


!!!Flip y-dimension of highres grids from exported Arc files...


        ct = 0
        do i=1,ixrt
        jj=jxrt
         do j=1,jxrt
           LAKE_MSKRT(i,j)=LAKE_MSKRT_inv(i,jj)
           DIRECTION(i,j)=DIRECTION_inv(i,jj)
           GORDER(i,j)=GORDER_inv(i,jj)
           GSTRMFRXSTPTS(i,j)=GSTRMFRXSTPTS_inv(i,jj)
           if(GSTRMFRXSTPTS(i,j).ne.-9999) ct = ct+1
           LATVAL(i,j)=LATVAL_inv(i,jj)
           LONVAL(i,j)=LONVAL_inv(i,jj)
           jj=jxrt-j
         end do
        end do
       
!        if(dist(1,1,1) .eq. -999) then 
!           call get_dist_ll(dist,latval,lonval,ixrt,jxrt)
!        end if

        

	print *, "Number of frxst pts: ",ct
          
     
! temp fix for buggy Arc export...
        do j=1,jxrt
          do i=1,ixrt
            if(DIRECTION(i,j).eq.-128) DIRECTION(i,j)=128
          end do
        end do

      cnt =0
      if ((CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2).AND.channel_option .ne. 3) then ! not routing on grid, read from file
       read(79,*)  header
       do i=1,NLINKS
          read (79,*) tmp, FROM_NODE(i), TO_NODE(i), TYPEL(i),&
                   ORDER(i), QLINK(i,1), MUSK(i), MUSX(i), CHANLEN(i), &
                   MannN(i), So(i), ChSSlp(i), Bw(i), HRZAREA(i),&
                   LAKEMAXH(i), WEIRC(i), WEIRL(i), ORIFICEC(i), &
                   ORIFICEA(i),ORIFICEE(i)

           !-- hardwire QLINK
          QLINK(i,1) = 1.0
          QLINK(i,2) = QLINK(i,1)

          if (So(i).lt.0.005) So(i) = 0.005  !-- impose a minimum slope requireement
 
          if (ORDER(i) .gt. MAXORDER) then
            MAXORDER = ORDER(i)
          endif

        end do

      elseif ((CHANRTSWCRT.eq.1.or.CHANRTSWCRT.eq.2).AND.channel_option.eq.3) then  !-- handle setting up topology on the grid for diffusion scheme

       read(79,*)  header  !-- read the lake file
       write(*,*) "reading lake file ", header
       write(6,*) "error check read file ",route_link_f


      if (NLAKES.gt.0) then !read in only if there are lakes
       do i=1, NLAKES
        read (79,*) tmp, HRZAREA(i),LAKEMAXH(i), &
          WEIRC(i), WEIRL(i), ORIFICEC(i), ORIFICEA(i), ORIFICEE(i),&
           LATLAKE(i), LONLAKE(i),ELEVLAKE(i)
        write (*,*) tmp, HRZAREA(i),LAKEMAXH(i), LATLAKE(i), LONLAKE(i),ELEVLAKE(i),NLAKES
       enddo
      end if   !end if for NLAKES >0 check

       cnt = 0 
!DJG inv       DO j = JXRT,1,-1  !rows
       DO j = 1,JXRT  !rows
        DO i = 1 ,IXRT   !colsumns
         If (CH_NETRT(i, j) .ge. 0) then !get its direction and assign its elevation and order
          If ((DIRECTION(i, j) .EQ. 64) .AND. (j + 1 .LE. JXRT) .AND. &
               (CH_NETRT(i,j+1).ge.0) ) then !North
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i, j + 1)
             CHANLEN(cnt) = dist(i,j,1)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i+1,j+1).ge.0) ) then !North East
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i + 1, j + 1)
             CHANLEN(cnt) = dist(i,j,2)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .LE. IXRT) &
                    .AND. (CH_NETRT(i+1,j).ge.0) ) then !East
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i + 1, j)
             CHANLEN(cnt) = dist(i,j,3)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .LE. IXRT) &
                    .AND. (j - 1 .NE. 0) .AND. (CH_NETRT(i+1,j-1).ge.0) ) then !south east
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i + 1, j - 1)
             CHANLEN(cnt) = dist(i,j,4)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 4) .AND. (j - 1 .NE. 0).AND.(CH_NETRT(i,j-1).ge.0) ) then !due south
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i, j - 1)
             CHANLEN(cnt) = dist(i,j,5)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .GT. 0) &
               .AND. (j - 1 .NE. 0) .AND. (CH_NETRT(i-1,j-1).ge.0)) then !south west
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i,j)
             TO_NODE(cnt) = CH_NETLNK(i - 1, j - 1)
             CHANLEN(cnt) = dist(i,j,6)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .GT. 0).AND.(CH_NETRT(i-1,j).ge.0) ) then !West
             cnt = cnt + 1
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             TO_NODE(cnt) = CH_NETLNK(i - 1, j)
             CHANLEN(cnt) = dist(i,j,7)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else if ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .GT. 0) &
                    .AND. (j + 1 .LE. JXRT) .AND. (CH_NETRT(i-1,j+1).ge.0) ) then !North West
             cnt = cnt + 1
             ORDER(cnt) = GORDER(i,j)
             STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
             ZELEV(cnt) = ELRT(i,j)
             MannN(cnt) = MannNG(i,j)
             ChSSlp(cnt) = ChSSlpG(i,j)
             Bw(cnt) = BwG(i,j)
             CHLAT(cnt) = LATVAL(i,j)
             CHLON(cnt) = LONVAL(i,j)
             FROM_NODE(cnt) = CH_NETLNK(i, j)
             TO_NODE(cnt) = CH_NETLNK(i - 1, j + 1)
             CHANLEN(cnt) = dist(i,j,8)
             CHANXI(cnt) = i
             CHANYJ(cnt) = j
             Link_Location(i,j) = cnt
          else 
             print *, "NO MATCH", i,j,CH_NETLNK(i,j),DIRECTION(i,j),i + 1,j - 1  !south east
          End If

         End If !CH_NETRT check for this node

        END DO
       END DO 

       print *, "after exiting the channel, this many nodes", cnt
       write(*,*) " " 

!Find out if the boundaries are on an edge
!DJG inv       DO j = JXRT,1,-1
       DO j = 1,JXRT
         DO i = 1 ,IXRT
          If (CH_NETRT(i, j) .ge. 0) then !get its direction
           If (((DIRECTION(i, j).EQ. 64) .AND. (j + 1 .GT. JXRT)) .OR. &        !-- 64's can only flow north
              ((DIRECTION(i, j) .EQ. 64) .and. (j < jxrt) .AND. (CH_NETRT(i,j+1) .lt. 0))) then !North
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)

              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if(j+1 .GT. JXRT) then !-- an edge
               TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i,j+1).gt.0) then 
               TYPEL(cnt) = 2
               LAKENODE(cnt) = LAKE_MSKRT(i,j+1)
              else
               TYPEL(cnt) = 1 
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,1)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
              print *, "Pour Point N", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
           else if ( ((DIRECTION(i, j) .EQ. 128) .AND. (i + 1 .GT. IXRT))  & !-- 128's can flow out of the North or East edge
               .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (j + 1 .GT. JXRT))  & !   this is due north edge
               .OR.  ((DIRECTION(i, j) .EQ. 128) .AND. (i<ixrt .and. j<jxrt) .AND.(CH_NETRT(i + 1, j + 1).lt.0))) then !North East
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)

              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if((i+1 .GT. IXRT) .OR. (j+1 .GT. JXRT))  then ! an edge
                TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i+1,j+1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i+1,j+1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,2)  
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
              print *, "Pour Point NE", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
           else if (((DIRECTION(i, j) .EQ. 1) .AND. (i + 1 .GT. IXRT)) .OR. &    !-- 1's can only flow due east
               ((DIRECTION(i, j) .EQ. 1) .and. (i<ixrt) .AND. (CH_NETRT(i + 1, j) .lt. 0))) then !East
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if(i+1 .GT. IXRT) then  !an edge
                TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i+1,j).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i+1,j)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,3)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
              print *, "Pour Point E", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
           else if ( ((DIRECTION(i, j) .EQ. 2) .AND. (i + 1 .GT. IXRT))    &      !-- 2's can flow out of east or south edge
                .OR. ((DIRECTION(i, j) .EQ. 2) .AND. (j - 1 .EQ. 0))       &      !-- this is the south edge
                .OR. ((DIRECTION(i, j) .EQ. 2) .and. (i<ixrt .and. j>1) .AND.(CH_NETRT(i + 1, j - 1) .lt.0))) then !south east
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if((i+1 .GT. IXRT) .OR. (j-1 .EQ. 0)) then !an edge 
                TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i+1,j-1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i+1,j-1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,4)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
              print *, "Pour Point SE", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
           else if (((DIRECTION(i, j) .EQ. 4) .AND. (j - 1 .EQ. 0)) .OR. &       !-- 4's can only flow due south
               ((DIRECTION(i, j) .EQ. 4) .and. (j>1) .AND.(CH_NETRT(i, j - 1) .lt. 0))) then !due south
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if(j-1 .EQ. 0) then !- an edge
                TYPEL(cnt) =1
              elseif(LAKE_MSKRT(i,j-1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i,j-1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,5)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
              print *, "Pour Point S", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
          else if ( ((DIRECTION(i, j) .EQ. 8) .AND. (i - 1 .LE. 0))      &      !-- 8's can flow south or west
               .OR.  ((DIRECTION(i, j) .EQ. 8) .AND. (j - 1 .EQ. 0))      &      !-- this is the south edge
               .OR.  ((DIRECTION(i, j).EQ.8).and. (i>1 .and. j>1) .AND.(CH_NETRT(i - 1, j - 1).lt.0))) then !south west
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if( (i-1 .EQ. 0) .OR. (j-1 .EQ. 0) ) then !- an edge
               TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i-1,j-1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i-1,j-1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,6) 
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
              print *, "Pour Point SW", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
           else if (((DIRECTION(i, j) .EQ. 16) .AND. (i - 1 .LE.0) ) &                  !16's can only flow due west
               .OR.((DIRECTION(i, j).EQ.16) .and. (i>1) .AND.(CH_NETRT(i - 1, j).lt.0))) then !West
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if(i-1 .EQ. 0) then !-- an edge
                TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i-1,j).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i-1,j)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,7)
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
              print *, "Pour Point W", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
           else if ( ((DIRECTION(i, j) .EQ. 32) .AND. (i - 1 .LE. 0))      &      !-- 32's can flow either west or north
               .OR.  ((DIRECTION(i, j) .EQ. 32) .AND. (j + 1 .GT. JXRT))   &      !-- this is the north edge
               .OR.  ((DIRECTION(i, j).EQ.32) .and. (i>1 .and. j<jxrt) .AND.(CH_NETRT(i - 1, j + 1).lt.0))) then !North West
              cnt = cnt + 1
              ORDER(cnt) = GORDER(i,j)
              STRMFRXSTPTS(cnt) = GSTRMFRXSTPTS(i,j)
              ZELEV(cnt) = ELRT(i,j)
              MannN(cnt) = MannNG(i,j)
              ChSSlp(cnt) = ChSSlpG(i,j)
              Bw(cnt) = BwG(i,j)
              CHLAT(cnt) = LATVAL(i,j)
              CHLON(cnt) = LONVAL(i,j)
              if( (i-1 .EQ. 0) .OR. (j+1 .GT. JXRT)) then !-- an edge
               TYPEL(cnt) = 1
              elseif(LAKE_MSKRT(i-1,j+1).gt.0) then 
                TYPEL(cnt) = 2
                LAKENODE(cnt) = LAKE_MSKRT(i-1,j+1)
              else
                TYPEL(cnt) = 1
              endif
              FROM_NODE(cnt) = CH_NETLNK(i, j)
              CHANLEN(cnt) = dist(i,j,8)   
              CHANXI(cnt) = i
              CHANYJ(cnt) = j
              Link_Location(i,j) = cnt
              print *, "Pour Point NW", TYPEL(cnt), LAKENODE(cnt), CHANLEN(cnt), cnt
           endif
          endif !CH_NETRT check for this node
         END DO
       END DO 
      endif

     close(79)
       if (cnt .ne. NLINKS) then 
         print *, "Apparent error in network topology", cnt, NLINKS
         stop
       endif

     do i=1,NLINKS
      if (STRMFRXSTPTS(i).ne.-9999) print *,"Frxst_pt: ",STRMFRXSTPTS(i)
     end do
     return

!DJG -----------------------------------------------------
   END SUBROUTINE READ_CHROUTING
!DJG -----------------------------------------------------

#ifdef MPP_LAND
!this subroutine mapping the channel network and lakes.
   subroutine MPP_CHROUTING_CONF(g_ixrt,g_jxrt,ixrt,jxrt, NLAKES,NLINKS,&
         lake_mskrt, lake_index,link_location,HRZAREA,LAKEMAXH,WEIRC,WEIRL,&
             ORIFICEC,ORIFICEA,ORIFICEE,LATLAKE,LONLAKE,ELEVLAKE, &
         FROM_NODE,TO_NODE,ZELEV,CHLAT,CHLON,TYPEL, ORDER,CHANLEN, &
         CHANXI,CHANYJ, lakenode,mpp_nlinks, nlinks_index,MAXORDER,yw_mpp_nlinks)
#ifdef MPP_LAND
   USE module_mpp_land
#endif
   implicit none 
#include <netcdf.inc>
   integer :: i,j,ixrt,g_ixrt,jxrt,g_jxrt, nlakes, nlinks
   integer, dimension(ixrt,jxrt)  :: LAKE_MSKRT, lakenode
   integer, INTENT(OUT) ,dimension(ixrt,jxrt):: link_location
   integer, INTENT(OUT) :: mpp_nlinks, yw_mpp_nlinks
   integer, INTENT(OUT),dimension(nlinks) :: nlinks_index, lake_index
   
   INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: FROM_NODE,TO_NODE, &
            TYPEL,CHANXI,CHANYJ,ORDER
   REAL, INTENT(INOUT),  DIMENSION(NLINKS)   ::CHANLEN,  ZELEV
   REAL, INTENT(INOUT),  DIMENSION(NLINKS)   ::CHLAT, CHLON
   !yw REAL,  DIMENSION(NLINKS)   ::CHLAT4, CHLON4
   integer,  DIMENSION(NLINKS)   :: node_table 
   integer , dimension(g_ixrt,g_jxrt):: g_tmp
   real ywtest(nlinks)
   integer  maxorder
  
! Lake information 
   REAL, INTENT(INOUT),  DIMENSION(*) :: HRZAREA,LAKEMAXH,WEIRC,WEIRL,&
             ORIFICEC,ORIFICEA,ORIFICEE,LATLAKE,LONLAKE,ELEVLAKE

      call mpp_land_bcast_int(NLINKS,FROM_NODE)
      call mpp_land_bcast_int(NLINKS,TO_NODE)
      call mpp_land_bcast_int(NLINKS,TYPEL)
      call mpp_land_bcast_int(NLINKS,ORDER)
      call mpp_land_bcast_int(NLINKS,LAKENODE)

      call mpp_land_bcast_real(NLINKS,CHANLEN)
      call mpp_land_bcast_real(NLINKS,ZELEV)
      
      call mpp_land_bcast_real(NLINKS,CHLAT)
      call mpp_land_bcast_real(NLINKS,CHLON)

      call mpp_land_max_int1(MAXORDER)
      if(MAXORDER .eq. 0)  MAXORDER = -9999

       lake_index = -99
       do j = 1, jxrt
          do i = 1, ixrt
            if (LAKE_MSKRT(i,j) .gt. 0) then
               lake_index(LAKE_MSKRT(i,j)) = LAKE_MSKRT(i,j)
            endif
          enddo
       enddo

       link_location = 0
       if(my_id .eq. IO_id) then
          g_tmp = -1
          do i = 1, nlinks
             g_tmp( CHANXI(i),CHANYJ(i) )  = i 
          enddo
       endif
       call decompose_RT_int(g_tmp,link_location,g_ixrt, g_jxrt, ixrt, jxrt)

      CHANXI = 0
      CHANYj = 0
      do j = 1, jxrt
          do i = 1, ixrt
             if(link_location(i,j) .gt. 0) then
               CHANXI(link_location(i,j)) = i
               CHANYJ(link_location(i,j)) = j
             endif
          end do
      end do

      node_table = 0
      do j = 1, jxrt
          do i = 1, ixrt
            if(link_location(i,j) .gt. 0) then 
               if( i.eq.1 .and. left_id > 0) then
                    continue 
               elseif ( i.eq. ixrt .and. right_id >0) then
                    continue
               elseif ( j.eq. 1 .and. down_id >0 ) then
                    continue
               elseif ( j.eq. jxrt .and. up_id >0) then
                    continue
               else
                  node_table(link_location(i,j)) = link_location(i,j) 
               endif
            endif 
          end do
      end do
      mpp_nlinks = 0
      do i = 1, nlinks
          if(node_table(i) > 0 ) then
                mpp_nlinks = mpp_nlinks + 1
                nlinks_index(mpp_nlinks) = i
          endif
      enddo

!     mpp_nlinks = 0
!     do j = 1, jxrt
!         do i = 1, ixrt
!           if(link_location(i,j) .gt. 0) then 
!               mpp_nlinks = mpp_nlinks + 1
!               nlinks_index(mpp_nlinks) = link_location(i,j)
!           endif
!         enddo
!     enddo
 
! add the boundary links
      yw_mpp_nlinks = mpp_nlinks  
      do j = 1, jxrt
          do i = 1, ixrt
            if(link_location(i,j) .gt. 0) then 
               if( i.eq.1 .and. left_id > 0) then
                    yw_mpp_nlinks = yw_mpp_nlinks + 1
                    nlinks_index(yw_mpp_nlinks) = link_location(i,j)
               elseif ( i.eq. ixrt .and. right_id >0) then
                    yw_mpp_nlinks = yw_mpp_nlinks + 1
                    nlinks_index(yw_mpp_nlinks) = link_location(i,j)
               elseif ( j.eq. 1 .and. down_id >0 ) then
                    yw_mpp_nlinks = yw_mpp_nlinks + 1
                    nlinks_index(yw_mpp_nlinks) = link_location(i,j)
               elseif ( j.eq. jxrt .and. up_id >0) then
                    yw_mpp_nlinks = yw_mpp_nlinks + 1
                    nlinks_index(yw_mpp_nlinks) = link_location(i,j)
               else
                  continue
               endif
            endif 
          end do
      end do


      call mpp_land_bcast_real(NLAKES,HRZAREA)
      call mpp_land_bcast_real(NLAKES,LAKEMAXH)
      call mpp_land_bcast_real(NLAKES,WEIRC)
      call mpp_land_bcast_real(NLAKES,WEIRL)
      call mpp_land_bcast_real(NLAKES,ORIFICEC)
      call mpp_land_bcast_real(NLAKES,ORIFICEA)
      call mpp_land_bcast_real(NLAKES,ORIFICEE)
      call mpp_land_bcast_real(NLAKES,LATLAKE)
      call mpp_land_bcast_real(NLAKES,LONLAKE)
      call mpp_land_bcast_real(NLAKES,ELEVLAKE)

!yw this is for test  the function
     
!     ywtest = node_table 
!     call write_chanel_real(ywtest,nlinks_index,mpp_nlinks,nlinks)   
!     if(my_id .eq. IO_id) then
!         do i = 1, nlinks
!           write(91,*) ywtest(i), i
!        end do
!        call flush(91)
!     endif
!YW 

   end subroutine MPP_CHROUTING_CONF
#endif

#ifdef MPP_LAND
  subroutine MPP_READ_SIMP_GW(IX,JX,IXRT,JXRT,GWSUBBASMSK,gwbasmskfil,&
          gw_strm_msk,numbasns,ch_netrt,AGGFACTRT)
#ifdef MPP_LAND
   USE module_mpp_land
#endif
    
    integer, intent(in)                     :: IX,JX,IXRT,JXRT,AGGFACTRT
    integer, intent(out)                    :: numbasns
    integer, intent(out), dimension(IX,JX)  :: GWSUBBASMSK
    integer, intent(out), dimension(IXRT,JXRT)  :: gw_strm_msk
    integer, intent(in), dimension(IXRT,JXRT)  :: ch_netrt
    character(len=256)                      :: gwbasmskfil
    integer,dimension(global_nX,global_ny) ::  g_GWSUBBASMSK
    integer,dimension(global_rt_nx, global_rt_ny) ::  g_gw_strm_msk,g_ch_netrt


     call write_IO_rt_int(ch_netrt,g_ch_netrt)

     if(my_id .eq. IO_id) then
       call READ_SIMP_GW(global_nX,global_ny,global_rt_nx,global_rt_ny,&
             g_GWSUBBASMSK,gwbasmskfil,g_gw_strm_msk,numbasns,&
             g_ch_netrt,AGGFACTRT) 
     endif
     call decompose_data_int(g_GWSUBBASMSK,GWSUBBASMSK)
     call decompose_RT_int(g_gw_strm_msk,gw_strm_msk,  &
          global_rt_nx, global_rt_ny,ixrt,jxrt)
     call mpp_land_bcast_int1(numbasns)
  return
  end subroutine MPP_READ_SIMP_GW
#endif

!DJG -----------------------------------------------------
!   SUBROUTINE READ_SIMP_GW
!DJG -----------------------------------------------------

  subroutine READ_SIMP_GW(IX,JX,IXRT,JXRT,GWSUBBASMSK,gwbasmskfil,&
          gw_strm_msk,numbasns,ch_netrt,AGGFACTRT)
    implicit none
#include <netcdf.inc>

    integer, intent(in)                     :: IX,JX,IXRT,JXRT,AGGFACTRT
    integer, intent(in), dimension(IXRT,JXRT)  :: ch_netrt
    integer, intent(out)                    :: numbasns
    integer, intent(out), dimension(IX,JX)  :: GWSUBBASMSK
    integer, intent(out), dimension(IXRT,JXRT)  :: gw_strm_msk
    character(len=256)                      :: gwbasmskfil
    integer                                 :: i,j,aggfacxrt,aggfacyrt,ixxrt,jyyrt

    numbasns = 0
    gw_strm_msk = -9999

!Open files...
    open(unit=91,file=trim(gwbasmskfil),          &
            form='formatted',status='old')

!Read in sub-basin mask...
    do j=jx,1,-1
          read (91,*) (GWSUBBASMSK(i,j),i=1,ix)
    end do
    close(91)


!Loop through to count number of basins and assign basin indices to chan grid
     do J=1,JX
       do I=1,IX

!Determine max number of basins...(assumes basins are numbered
!   sequentially from 1 to max number of basins...)
        if (GWSUBBASMSK(i,j).gt.numbasns) then
          numbasns = GWSUBBASMSK(i,j)   ! get count of basins...
        end if

!Assign gw basin index values to channel grid...
        do AGGFACYRT=AGGFACTRT-1,0,-1
          do AGGFACXRT=AGGFACTRT-1,0,-1

             IXXRT=I*AGGFACTRT-AGGFACXRT
             JYYRT=J*AGGFACTRT-AGGFACYRT
             IF(ch_netrt(IXXRT,JYYRT).ge.0) then  !If channel grid cell
               gw_strm_msk(IXXRT,JYYRT) = GWSUBBASMSK(i,j)  ! assign coarse grid basn indx to chan grid
             END IF

           end do !AGGFACXRT
         end do !AGGFACYRT

      end do   !I-ix
    end do    !J-jx

    return

!DJG -----------------------------------------------------
   END SUBROUTINE READ_SIMP_GW
!DJG -----------------------------------------------------

  ! BF read the static input fields needed for the 2D GW scheme
  subroutine readGW2d(ix, jx, hc, ihead, botelv, por, ltype)
#ifdef MPP_LAND
  use module_mpp_land
#endif
  implicit none
#include <netcdf.inc>
  integer, intent(in) :: ix, jx
  integer, dimension(ix,jx), intent(inout)::   ltype
  real, dimension(ix,jx), intent(inout)   ::   hc, ihead, botelv, por

#ifdef MPP_LAND
  integer, dimension(:,:), allocatable ::  gLtype
  real, dimension(:,:), allocatable    ::  gHC, gIHEAD, gBOTELV, gPOR
#endif
  integer :: i, get2d_real
  
#ifdef MPP_LAND
  allocate(gHC(global_rt_nx, global_rt_ny))
  allocate(gIHEAD(global_rt_nx, global_rt_ny))
  allocate(gBOTELV(global_rt_nx, global_rt_ny))
  allocate(gPOR(global_rt_nx, global_rt_ny))
  allocate(gLtype(global_rt_nx, global_rt_ny))
  
  if(my_id .eq. IO_id) then
  print*, "2D GW-Scheme selected, retrieving files from gwhires.nc ..."
#endif


        ! hydraulic conductivity
        i = get2d_real("HC", &
#ifdef MPP_LAND
                       gHC, global_nx, global_ny,  &
#else
                       hc, ix, jx,  &
#endif
                       trim("./gwhires.nc"))

        ! initial head
        i = get2d_real("IHEAD", &
#ifdef MPP_LAND
                       gIHEAD, global_nx, global_ny, &
#else
                       ihead,  ix, jx, &
#endif
                       trim("./gwhires.nc"))
                       
        ! aquifer bottom elevation                
        i = get2d_real("BOTELV", &
#ifdef MPP_LAND
                       gBOTELV, global_nx, global_ny, &
#else
                       botelv, ix, jx,  &
#endif
                       trim("./gwhires.nc"))
                       
	! aquifer porosity
        i = get2d_real("POR", &
#ifdef MPP_LAND
                       gPOR, global_nx, global_ny, &
#else
                       por, ix, jx,  &
#endif
                       trim("./gwhires.nc"))

! bftodo: develop proper landtype mask
 
#ifdef MPP_LAND
       gLtype=1
       gLtype(1,:) = 2
       gLtype(:,1) = 2
       gLtype(global_rt_nx,:) = 2
       gLtype(:,global_rt_ny) = 2 
#else
       ltype=1
       ltype(1,:) =2
       ltype(:,1) =2
       ltype(ix,:)=2
       ltype(:,jx)=2
#endif

#ifdef MPP_LAND  
  endif
     call decompose_rt_int (gLtype, ltype, global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gHC,hc,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gIHEAD,ihead,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gBOTELV,botelv,global_rt_nx, global_rt_ny, ix, jx)
     call decompose_rt_real(gPOR,por,global_rt_nx, global_rt_ny, ix, jx)
     deallocate(gHC, gIHEAD, gBOTELV, gPOR)
#endif
  !bftodo: make filename accessible in namelist
  return
  end subroutine readGW2d
  !BF
 
  subroutine output_hrldas(ounit, igrid, split_output_count, hgrid, range, ix, jx, &
       nsoil, sldpth, startdate, date,  &
       vegtyp, soltyp, skintemp, cmc, stc, smc, sh2o, soilmx, sfcrnoff, ugdrnoff, &
       intrflow, sfcevp, etakin, canevp, edirx, ettx, albedx, weasd, acrain, acsnom, &
       esnow2d, drip2d, dewfall, snodep, vegfra, hx, qfx, sw, infxsrt, prcp_out,  &
       etpndx,sfheadrt,smcmax1,wrfsi_static_flnm,forctyp)
    implicit none
#include <netcdf.inc>

    character(len=*),          intent(in)  :: wrfsi_static_flnm
    integer,                                  intent(in) :: ounit
    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count
    character,                                intent(in) :: hgrid
    character(len=*),                         intent(in) :: range
    integer,                                  intent(in) :: ix
    integer,                                  intent(in) :: jx
    integer,                                  intent(in) :: nsoil
!yw    integer,                                  intent(in) :: forctyp
    integer,                                  intent(inout) :: forctyp
    real,             dimension(nsoil),       intent(in) :: sldpth

    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: date
    integer,          dimension(ix,jx),       intent(in) :: vegtyp
    integer,          dimension(ix,jx),       intent(in) :: soltyp
    real,             dimension(ix,jx),       intent(in) :: skintemp
    real,             dimension(ix,jx),       intent(in) :: cmc
    real,             dimension(ix,jx,nsoil), intent(in) :: stc
    real,             dimension(ix,jx,nsoil), intent(in) :: smc
    real,             dimension(ix,jx,nsoil), intent(in) :: sh2o
    real,             dimension(ix,jx,nsoil), intent(in) :: soilmx
    real,             dimension(ix,jx),       intent(in) :: ugdrnoff
    real,             dimension(ix,jx),       intent(in) :: sfcrnoff
    real,             dimension(ix,jx),       intent(in) :: intrflow
    real,             dimension(ix,jx),       intent(in) :: sfcevp
    real,             dimension(ix,jx),       intent(in) :: etakin
    real,             dimension(ix,jx),       intent(in) :: canevp
    real,             dimension(ix,jx),       intent(in) :: edirx
    real,             dimension(ix,jx),       intent(in) :: ettx
    real,             dimension(ix,jx),       intent(in) :: albedx
    real,             dimension(ix,jx),       intent(in) :: weasd
    real,             dimension(ix,jx),       intent(in) :: acrain
    real,             dimension(ix,jx),       intent(in) :: acsnom
    real,             dimension(ix,jx),       intent(in) :: esnow2d
    real,             dimension(ix,jx),       intent(in) :: drip2d
    real,             dimension(ix,jx),       intent(in) :: dewfall
    real,             dimension(ix,jx),       intent(in) :: snodep
    real,             dimension(ix,jx),       intent(in) :: vegfra
    real,             dimension(ix,jx),       intent(in) :: hx
    real,             dimension(ix,jx),       intent(in) :: qfx
    real,             dimension(ix,jx),       intent(in) :: sw
    real,             dimension(ix,jx),       intent(in) :: infxsrt
    real,             dimension(ix,jx),       intent(in) :: prcp_out
    real,             dimension(ix,jx),       intent(in) :: etpndx
    real,             dimension(ix,jx),       intent(in) :: sfheadrt
    real,             dimension(ix,jx),       intent(in) :: smcmax1

    real, allocatable, dimension(:) :: corners
!-- wrfstatic file information to add to outputfile

    real,    dimension(ix,jx) :: terrain, latitude, longitude
    real,    dimension(nsoil) :: asldpth

    integer :: output_count = 0
    integer, save :: ncid
    character(len=256) :: units
    integer :: ierr

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: dimid_ix_stag, dimid_jx_stag, dimid_soil

    integer :: iret,dimid, ncstatic, staticid,maxlen,gfint
    real    :: gfreal
    character(len=256) :: output_flnm
    character(len=256) :: soilchar
    character(len=19)  :: date19

    real, dimension(ix,jx) :: xdum
    integer flag ! flag = 1 from wrfsi, flag =2 from WPS.

! Variables for lnd flux calib...
    real        :: tot_ET, tot_pcp, smc_tot
    integer     :: i,j

    print*, 'output date:  ', date



!DJG Dump timeseries of ET and PCP and soil moisture for lnd calibration...(10/12/09)
    tot_ET = 0.0
    tot_pcp = 0.0
    smc_tot = 0.
        do j=1,JX
          do i=1,IX
            if (vegtyp(i,j).ne.16) then
!             tot_pcp=tot_pcp+acrain(i,j)/1000.0*(1000.*1000.) !(units m^3)
!             tot_ET=tot_ET+sfcevp(i,j)/1000.0*(1000.*1000.) !(units m^3)
              smc_tot=smc_tot+(SMC(I,J,1)*0.1+SMC(I,J,2)*0.3   &
                +SMC(I,J,3)*0.3+SMC(I,J,4)*0.3+SMC(I,J,5)*0.3 &
                +SMC(I,J,6)*0.3+SMC(I,J,7)*0.3+SMC(I,J,8)*0.1)*1000.*1000.
            end if
          enddo
        enddo

!yw    return

    open (unit=47,file='lnd_fluxes.txt',form='formatted',&
             status='unknown',position='append')
    write (47,*) tot_ET/1000000.,tot_pcp/1000000.,smc_tot/1000000.  !output in Mm^3
747        FORMAT(F12.3,1X,F12.3,1X,F8.4)
    close (47)
!DJG end dump of lnd fluxes for calibration....


!yw    return

    asldpth = 0
    if (output_count == 0) then
       write(output_flnm, '(A12,".LDASOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
       print*, 'output_flnm = "'//trim(output_flnm)//'"'

    !-- Open the  wrf static files to obtain projection information
      write(*,'("wrfsi_static_flnm: ''", A, "''")') trim(wrfsi_static_flnm)
      iret = nf_open(wrfsi_static_flnm, NF_NOWRITE, ncstatic)

      if (iret /= 0) then
         write(*,'("Problem opening wrfsi_static file: ''", A, "''")') &
         trim(wrfsi_static_flnm)
       stop
      endif

!-- create a netcdf output file
    iret = nf_create(trim(output_flnm), 0, ncid)
    if (iret /= 0) stop "Problem nf_create"


        if (forctyp.ne.3)  then   !do if not COWS...
!-- inquire about the static file
!      iret = NF_INQ_ATTLEN(ncstatic, NF_GLOBAL, 'corner_lats', maxlen) !-- get the attribute length
!      allocate(corners(maxlen))  !-- allocate memory for it
      allocate(corners(16))  !-- allocate memory for it
!      iret = NF_GET_ATT_REAL(ncstatic, NF_GLOBAL, 'corner_lats', corners)  !-- read it from the static file
!      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'corner_lats', NF_DOUBLE, maxlen,corners)  !--write the global attribute

!      iret = NF_INQ_ATTLEN(ncstatic, NF_GLOBAL, 'corner_lons', maxlen) !-- get the attribute length
!      iret = NF_GET_ATT_REAL(ncstatic, NF_GLOBAL, 'corner_lons', corners)  !-- read it from the static file
!      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'corner_lons', NF_DOUBLE, maxlen,corners)  !--write the global attribute

      iret = NF_GET_ATT_INT(ncstatic, NF_GLOBAL, 'WEST-EAST_GRID_DIMENSION', gfint)  !-- read it from the static file
      print *, "west east dimension", gfint
      iret = NF_PUT_ATT_INT(ncid, NF_GLOBAL,'WEST-EAST_GRID_DIMENSION', NF_INT, 1, gfint)  !--write the global attribute

      iret = NF_GET_ATT_INT(ncstatic, NF_GLOBAL, 'SOUTH-NORTH_GRID_DIMENSION', gfint)
      iret = NF_PUT_ATT_INT(ncid, NF_GLOBAL, 'SOUTH-NORTH_GRID_DIMENSION', NF_INT, 1, gfint)  !--write the global attribute

      iret = NF_GET_ATT_REAL(ncstatic, NF_GLOBAL, 'DX', gfreal)
      print *, "DX  ", gfreal
      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'DX', NF_DOUBLE, 1, gfreal)  !--write the global attribute

      iret = NF_GET_ATT_REAL(ncstatic, NF_GLOBAL, 'DY', gfreal)
      print *, "DY ", gfreal
      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'DY', NF_REAL, 1, gfreal)  !--write the global attribute

      iret = NF_PUT_ATT_TEXT(ncid, NF_GLOBAL, 'map_projection',17,'LAMBERT CONFORMAL')

! Get and write Map projection attributes from NetCDF file	 
      iret = NF_GET_ATT_REAL(ncstatic,NF_GLOBAL,'CEN_LAT',gfreal) 
      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'CEN_LAT', NF_REAL, 1, gfreal)  !--write the global attribute

      iret = NF_GET_ATT_REAL(ncstatic,NF_GLOBAL,'CEN_LON',gfreal) 
      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'CEN_LON', NF_REAL, 1, gfreal)  !--write the global attribute

      iret = NF_PUT_ATT_TEXT(ncid, NF_GLOBAL, 'map_projection',17,'LAMBERT CONFORMAL')

      iret = NF_GET_ATT_REAL(ncstatic,NF_GLOBAL,'MOAD_CEN_LAT',gfreal)	
      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'MOAD_CEN_LAT', NF_REAL, 1, gfreal)  !--write the global attribute

      iret = NF_GET_ATT_REAL(ncstatic,NF_GLOBAL,'STAND_LON',gfreal)	
      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'STAND_LON', NF_REAL, 1, gfreal)  !--write the global attribute

      iret = NF_GET_ATT_REAL(ncstatic,NF_GLOBAL,'TRUELAT1',gfreal)	
      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'TRUELAT1', NF_REAL, 1, gfreal)  !--write the global attribute


      iret = NF_GET_ATT_REAL(ncstatic,NF_GLOBAL,'TRUELAT2',gfreal)
      iret = NF_PUT_ATT_REAL(ncid, NF_GLOBAL, 'TRUELAT2', NF_REAL, 1, gfreal)  !--write the global attribute

      iret = NF_GET_ATT_INT(ncstatic, NF_GLOBAL, 'MAP_PROJ', gfint)
      iret = NF_PUT_ATT_INT(ncid, NF_GLOBAL, 'MAP_PROJ', NF_INT, 1, gfint)  !--write the global attribute

! Add 'DYN_OPT' attribute to standard HRLDAS output file per instruction from Unidata....
      iret = NF_PUT_ATT_INT(ncid, NF_GLOBAL, 'DYN_OPT', NF_INT, 1, 2)  !--write the global attribute

!yw    return

    flag = -99 
    ierr = nf_inq_varid(ncstatic,"XLAT", varid)
    flag = 1
    if(ierr .ne. 0) then
        ierr = nf_inq_varid(ncstatic,"XLAT_M", varid)
        if(ierr .ne. 0) then
            write(6,*) "XLAT not found from wrfstatic file. "
            call land_finish()
        endif
        flag = 2
    endif

    ! Get Latitude (lat)
    if(flag.eq. 1) then
        call get_2d_netcdf("XLAT", ncstatic, latitude,  units, ix, jx, .TRUE., ierr)
    else
        call get_2d_netcdf("XLAT_M", ncstatic, latitude,  units, ix, jx, .TRUE., ierr)
    endif
    print*, 'latitude(1,1) = ', latitude(1,1)

    ! Get Longitude (lon)
    if(flag.eq. 1) then
       call get_2d_netcdf("XLONG", ncstatic, longitude, units, ix, jx, .TRUE., ierr)
    else
       call get_2d_netcdf("XLONG_M", ncstatic, longitude,  units, ix, jx, .TRUE., ierr)
    endif
    print*, 'longitude(1,1) = ', longitude(1,1)

    ! Get Terrain (avg)
    if(flag.eq. 1) then
       call get_2d_netcdf("HGT", ncstatic, terrain, units, ix, jx, .TRUE., ierr)
    else
       call get_2d_netcdf("HGT_M", ncstatic, terrain, units, ix, jx, .TRUE., ierr)
    endif
    print*, 'terrain(1,1) = ', terrain(1,1)

       iret = nf_def_dim(ncid, "Time", NF_UNLIMITED, dimid_times)
       iret = nf_def_dim(ncid, "DateStrLen", 19, dimid_datelen)
       iret = nf_def_dim(ncid, "west_east", ix, dimid_ix)
       iret = nf_def_dim(ncid, "south_north", jx, dimid_jx)
       iret = nf_def_dim(ncid, "west_east_stag", ix+1, dimid_ix_stag)
       iret = nf_def_dim(ncid, "south_north_stag", jx+1, dimid_jx_stag)
       iret = nf_def_dim(ncid, "soil_layers_stag", nsoil, dimid_soil)  !-- 3-d soils

!-- create the Times variable
       iret = nf_def_var(ncid,"Times",  NF_CHAR,  2, (/dimid_datelen,dimid_times/),varid)

!-- create the soil variable
       iret = nf_def_var(ncid,"ZS", NF_FLOAT,  2, (/dimid_soil,dimid_times/),varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',2,'Z ')
       iret = nf_put_att_text(ncid,varid,'units',1,'m')
       iret = nf_put_att_text(ncid,varid,'description',30,'DEPTH OF CENTER OF SOIL LAYERS')

!-- define domain
       iret = nf_def_var(ncid,"XLAT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',9,'deg north')
       iret = nf_put_att_text(ncid,varid,'description',44,'Latitude of each gridpoint of the mass grid')

       iret = nf_def_var(ncid,"XLONG",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',4,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',8,'deg east')
       iret = nf_put_att_text(ncid,varid,'description',45,'Longitude of each gridpoint of the mass grid')

       iret = nf_def_var(ncid,"HGT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'meters')
       iret = nf_put_att_text(ncid,varid,'description',32,'Topographic height of grid point')

    else   !if COWS  (forctyp=3)
      iret = nf_def_dim(ncid, "idim", ix, dimid_ix)
      iret = nf_def_dim(ncid, "jdim", jx, dimid_jx)
      iret = nf_def_dim(ncid, "Times", NF_UNLIMITED, dimid_times)
      iret = nf_def_dim(ncid, "DateStrLen", 19, dimid_datelen)

    endif  !COWS...

!-- define noah variables
        iret = nf_def_var(ncid,  "SOIL_T",  NF_FLOAT, 4, (/dimid_ix,dimid_jx,dimid_soil,dimid_times/), varid)
        iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
        iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XYZ')
        iret = nf_put_att_text(ncid,varid,'units',1,'K')
        iret = nf_put_att_text(ncid,varid,'description',16,'soil temperature')
        iret = nf_put_att_text(ncid,varid,'stagger',1,'Z')
        iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

        iret = nf_def_var(ncid,  "SOIL_M",  NF_FLOAT, 4, (/dimid_ix,dimid_jx,dimid_soil,dimid_times/), varid)
        iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
        iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XYZ')
        iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2K')
        iret = nf_put_att_text(ncid,varid,'description',16,'moisture content')
        iret = nf_put_att_text(ncid,varid,'stagger',1,'Z')
        iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

        iret = nf_def_var(ncid,  "SOIL_W",  NF_FLOAT, 4, (/dimid_ix,dimid_jx,dimid_soil,dimid_times/), varid)
        iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
        iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XYZ')
        iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
        iret = nf_put_att_text(ncid,varid,'description',34,'moisture content at field capacity')
        iret = nf_put_att_text(ncid,varid,'stagger',1,'Z')
        iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"VEGTYP",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',1,'')
       iret = nf_put_att_text(ncid,varid,'description',17,'Vegetation Class')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"SOLTYP",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',1,'')
       iret = nf_put_att_text(ncid,varid,'description',11,'Soil Class')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"TSKIN",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',1,'K')
       iret = nf_put_att_text(ncid,varid,'description',16,'skin temperature')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"HFX",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',5,'W m-2')
       iret = nf_put_att_text(ncid,varid,'description',18,'sensible heat flux')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"QFX",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',5,'W m-2')
       iret = nf_put_att_text(ncid,varid,'description',18,'ground heat flux')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"SFCEVP",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'description',19,'surface evaporation')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"CANEVP",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'description',18,'canopy evaporation')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"EDIRX",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'description',18,'soil evaporation')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"ETTX",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',13,'transpiration')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"ETPNDX",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',26,'ponded water evaporation')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"SW",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'W/m^2')
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',13,'incoming shortwave radiation')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"ALBEDX",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',1,'1')
       iret = nf_put_att_text(ncid,varid,'description',14,'surface albedo')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"VEGFRA",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',1,'1')
       iret = nf_put_att_text(ncid,varid,'description',19,'vegetation fraction')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"SNODEP",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'description',10,'snow depth')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"SWE",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',1,'mm')
       iret = nf_put_att_text(ncid,varid,'description',21,'snow water equivalent')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"ACRAIN",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'description',20,'accumulated total precipitation')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"ACSNOM",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'description',21,'accumulated snow melt')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"ESNOW2D",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'description',38,'accumulated snow evap. (sublimation)')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"SFCRNOFF",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'description',14,'surface runoff')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

!yw    return

       iret = nf_def_var(ncid,"UGDRNOFF",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'description',18,'sub-surface runoff')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"CMC", NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'description',23,'canopy moisture content')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"INFXSRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'description',19,'infiltration excess')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"SFCHEAD",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'description',12,'surface head')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

       iret = nf_def_var(ncid,"SMCMAX",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
       iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'description',20,'maximum surface head')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-1e33)

!      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -1.E33)
!      iret = nf_put_att_int(ncid,varid,'FieldType',NF_INT,1,104)
!      iret = nf_put_att_text(ncid,varid,'MemoryOrder',3,'XY ')

       date19(1:19) = "0000-00-00_00:00:00"
       date19(1:len_trim(startdate)) = startdate
       iret = nf_put_att_text(ncid, NF_GLOBAL, "Start_Date", 19, date19)

       iret = nf_enddef(ncid)
       if (forctyp.ne.3) then  !if not COWS...
          iret = nf_close(ncstatic)
          deallocate(corners)
       end if !if not COWS...

    endif

    output_count = output_count + 1

!yw    return

!-- output domain
       if (forctyp.ne.3) then  !if not COWS...
    xdum = latitude
    iret = nf_inq_varid(ncid,  "XLAT", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = longitude
    iret = nf_inq_varid(ncid,  "XLONG", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = terrain
    iret = nf_inq_varid(ncid,  "HGT", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)
       end if !if not COWS...

!-- soil depths at level
    iret = nf_inq_varid(ncid,"ZS", varid)

    do n = 1,nsoil
     if(n == 1) then
      asldpth(n) = -sldpth(n)
     else
      asldpth(n) = asldpth(n-1) - sldpth(n)
    endif
    enddo

    iret = nf_put_vara_real(ncid, varid, (/1,output_count/), (/nsoil,1/), asldpth)

!yw      return

!--- 3-d soils
    do n = 1, nsoil
       xdum = stc(:,:,n)
       where (vegtyp(:,:) == 16) xdum = -1.E33
       iret = nf_inq_varid(ncid,  "SOIL_T", varid)
       iret = nf_put_vara_real(ncid, varid, (/1,1,n,output_count/), (/ix,jx,1,1/), xdum)
    enddo

    do n = 1, nsoil
       xdum = smc(:,:,n)
       where (vegtyp(:,:) == 16) xdum = -1.E33
       iret = nf_inq_varid(ncid,  "SOIL_M", varid)
       iret = nf_put_vara_real(ncid, varid, (/1,1,n,output_count/), (/ix,jx,1,1/), xdum)
    enddo

    do n = 1, nsoil
       xdum = sh2o(:,:,n)
       where (vegtyp(:,:) == 16) xdum = -1.E33
       iret = nf_inq_varid(ncid,  "SOIL_W", varid)
       iret = nf_put_vara_real(ncid, varid, (/1,1,n,output_count/), (/ix,jx,1,1/), xdum)
    enddo

    xdum = vegtyp 
!    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "VEGTYP", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = soltyp 
!    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "SOLTYP", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = skintemp
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "TSKIN", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = hx
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "HFX", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = qfx
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "QFX", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = sfcevp
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "SFCEVP", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = canevp
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "CANEVP", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = edirx
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "EDIRX", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = ettx
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "ETTX", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = etpndx
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "ETPNDX", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = sw
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "SW", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = albedx
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "ALBEDX", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = vegfra
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "VEGFRA", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = snodep
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "SNODEP", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = weasd
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "SWE", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    print*, "iret, forctyp", iret, forctyp
    xdum = acrain
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "ACRAIN", varid)
    if (iret /= 0) stop "NF_INQ_VARID: ACRAIN "
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)
    if (iret /= 0) stop "NF_PUT_VARA_REAL:  ACRAIN"
    print*, "*** iret, forctyp", iret, forctyp

    xdum = acsnom
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "ACSNOM", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = esnow2d
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "ESNOW2D", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = sfcrnoff
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "SFCRNOFF", varid)
    if (iret /= 0) stop "inquire SFCRNFF"
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = ugdrnoff
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "UGDRNOFF", varid)
    if (iret /= 0) stop "inquire UGDRNOFF"
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = cmc
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "CMC", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = infxsrt 
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "INFXSRT", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = sfheadrt
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "SFCHEAD", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = smcmax1
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "SMCMAX", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

!- print date in Times variable
    date19(1:19) = "0000-00-00_00:00:00"
    date19(1:len_trim(date)) = date
     iret = nf_inq_varid(ncid, "Times", varid)
     print*, 'date varid = ', varid
    iret = nf_put_vara_text(ncid, varid, (/1,output_count/), (/19,1/), date)

!yw   return

    iret = nf_sync(ncid)

    if (output_count == split_output_count) then
       output_count = 0
       iret = nf_close(ncid)
    endif

!DJG debug
   print *, "End subroutine output_hrldas..."
    return
  end subroutine output_hrldas


  subroutine output_rt(igrid, split_output_count, ixrt, jxrt, nsoil, &
       startdate, date, QSUBRT,ZWATTABLRT,SMCRT,SUB_RESID,       &
       q_sfcflx_x,q_sfcflx_y,soxrt,soyrt,QSTRMVOLRT,SFCHEADSUBRT, &
       wrfsi_finegrid_flnm,dt,sldpth,LATVAL,LONVAL,dist,HIRES_OUT)

!output the routing variables over routing grid.
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count
    integer,                                  intent(in) :: ixrt,jxrt
    real,                                     intent(in) :: dt
    real,                                     intent(in) :: dist(ixrt,jxrt,9)
    integer,                                  intent(in) :: nsoil
    integer,                                  intent(in) :: HIRES_OUT 
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: date
    character(len=*),          intent(in)                :: wrfsi_finegrid_flnm
    real,             dimension(nsoil),       intent(in) :: sldpth
    real, allocatable, DIMENSION(:,:)                   :: xdumd  !-- decimated variable
    real*8, allocatable, DIMENSION(:)                   :: xcoord_d, xcoord
    real*8, allocatable, DIMENSION(:)                   :: ycoord_d, ycoord

    integer, save :: ncid,ncstatic
    integer, save :: output_count
    real,    dimension(nsoil) :: asldpth

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: iret, dimid_soil, i,j,ii,jj
    character(len=256) :: output_flnm
    character(len=19)  :: date19
    character(len=32)  :: convention
    character(len=34)  :: sec_since_date

    character(len=30)  :: soilm

    real                                :: long_cm,lat_po,fe,fn, chan_in
    real, dimension(2)                  :: sp

    real, dimension(ixrt,jxrt) :: xdum,QSUBRT,ZWATTABLRT,SUB_RESID
    real, dimension(ixrt,jxrt) :: q_sfcflx_x,q_sfcflx_y
    real, dimension(ixrt,jxrt) :: QSTRMVOLRT
    real, dimension(ixrt,jxrt) :: SFCHEADSUBRT
    real, dimension(ixrt,jxrt) :: soxrt,soyrt
    real, dimension(ixrt,jxrt) :: LATVAL,LONVAL
    real, dimension(ixrt,jxrt,nsoil) :: SMCRT

    integer :: seconds_since, decimation, ixrtd,jxrtd, hires_flag
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    seconds_since = int(dt)*output_count

    decimation = 1 !-- decimation factor
    ixrtd = int(ixrt/decimation)
    jxrtd = int(jxrt/decimation)
    allocate(xdumd(ixrtd,jxrtd))
    allocate(xcoord_d(ixrtd))
    allocate(ycoord_d(jxrtd))
    allocate(xcoord(ixrtd))
    allocate(ycoord(jxrtd))
    ii = 0
    jj = 0

!DJG Dump timeseries for channel inflow accum. for calibration...(8/28/09)
    chan_in = 0.0
    do j=1,jxrt
      do i=1,ixrt
        chan_in=chan_in+QSTRMVOLRT(I,J)/1000.0*(dist(i,j,9))  !(units m^3)
      enddo
    enddo
    open (unit=46,file='chan_inflow.txt',form='formatted',&
             status='unknown',position='append')
    write (46,*) chan_in
    close (46)
!    return
!DJG end dump of channel inflow for calibration....

    if (hires_out.eq.1) return  ! return if hires flag eq 1, if =2 output full grid

    if (output_count == 0) then

   !-- Open the  finemesh static files to obtain projection information
      write(*,'("wrfsi_finegrid_flnm: ''", A, "''")') trim(wrfsi_finegrid_flnm)
      iret = nf_open(trim(wrfsi_finegrid_flnm), NF_NOWRITE, ncstatic)

      if (iret /= 0) then
         write(*,'("Problem opening wrfsi_finegrid file: ''", A, "''")') &
         trim(wrfsi_finegrid_flnm)
         write(*,*) "HIRES_OUTPUT will not be georeferenced..."
         
!DJG       stop
        hires_flag = 0
      else
        hires_flag = 1
      endif

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get Latitude (X)
      iret = NF_INQ_VARID(ncstatic,'x',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, xcoord)
      ! Get Longitude (Y)
      iret = NF_INQ_VARID(ncstatic,'y',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, ycoord)
     else
      xcoord_d = 0.
      ycoord_d = 0.
     end if  !endif hires_georef 

     do j=jxrt,1,-1*decimation
        jj = jj+1
        if (jj<= jxrtd) then
         ycoord_d(jj) = ycoord(j)
        endif
     enddo

!yw     do i = 1,ixrt,decimation
!yw        ii = ii + 1
!yw        if (ii <= ixrtd) then 
!yw         xcoord_d(ii) = xcoord(i)
         xcoord_d = xcoord
!yw        endif
!yw     enddo
       

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get projection information from finegrid netcdf file
      iret = NF_INQ_VARID(ncstatic,'lambert_conformal_conic',varid)
      if(iret .eq. 0) iret = NF_GET_ATT_REAL(ncstatic, varid, 'longitude_of_central_meridian', long_cm)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'latitude_of_projection_origin', lat_po)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_easting', fe)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_northing', fn)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'standard_parallel', sp)  !-- read it from the static file
     end if  !endif hires_georef 
      iret = nf_close(ncstatic)

!-- create the fine grid routing file
       write(output_flnm, '(A12,".RTOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
       print*, 'output_flnm = "'//trim(output_flnm)//'"'

       iret = nf_create(trim(output_flnm), 0, ncid)
       if (iret /= 0) stop "Problem nf_create"

       iret = nf_def_dim(ncid, "time", NF_UNLIMITED, dimid_times)
       iret = nf_def_dim(ncid, "x", ixrtd, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jxrtd, dimid_jx)
       iret = nf_def_dim(ncid, "depth", nsoil, dimid_soil)  !-- 3-d soils

!--- define variables
!     !- time definition, timeObs
         iret = nf_def_var(ncid,"time",NF_INT, 1, (/dimid_times/), varid)
         iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)

       !- x-coordinate in cartesian system
        iret = nf_def_var(ncid,"x",NF_DOUBLE, 1, (/dimid_ix/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',26,'x coordinate of projection')
        iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_x_coordinate')
        iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- y-coordinate in cartesian ssystem
          iret = nf_def_var(ncid,"y",NF_DOUBLE, 1, (/dimid_jx/), varid)
          iret = nf_put_att_text(ncid,varid,'long_name',26,'y coordinate of projection')
          iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_y_coordinate')
          iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- LATITUDE
        iret = nf_def_var(ncid,"LATITUDE",NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',8,'LATITUDE')
        iret = nf_put_att_text(ncid,varid,'standard_name',8,'LATITUDE')
        iret = nf_put_att_text(ncid,varid,'units',5,'deg North')

       !- LONGITUDE
          iret = nf_def_var(ncid,"LONGITUDE",NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)
          iret = nf_put_att_text(ncid,varid,'long_name',9,'LONGITUDE')
          iret = nf_put_att_text(ncid,varid,'standard_name',9,'LONGITUDE')
          iret = nf_put_att_text(ncid,varid,'units',5,'deg east')

       !-- z-level is soil
        iret = nf_def_var(ncid,"depth", NF_FLOAT, 1, (/dimid_soil/),varid)
        iret = nf_put_att_text(ncid,varid,'units',2,'cm')
        iret = nf_put_att_text(ncid,varid,'long_name',19,'depth of soil layer')

         iret = nf_def_var(ncid,  "SOIL_M",  NF_FLOAT, 4, (/dimid_ix,dimid_jx,dimid_soil,dimid_times/), varid)
            iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
            iret = nf_put_att_text(ncid,varid,'description',16,'moisture content')
            iret = nf_put_att_text(ncid,varid,'long_name',26,soilm)
            iret = nf_put_att_text(ncid,varid,'coordinates',5,'x y z')
            iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
            iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!      iret = nf_def_var(ncid,"ESNOW2D",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)

!       iret = nf_def_var(ncid,"QSUBRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
!       iret = nf_put_att_text(ncid,varid,'long_name',15,'subsurface flow')
!       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

          iret = nf_def_var(ncid,"ZWATTABLRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
          iret = nf_put_att_text(ncid,varid,'units',1,'m')
          iret = nf_put_att_text(ncid,varid,'long_name',17,'water table depth')
          iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
          iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
          iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!       iret = nf_def_var(ncid,"Q_SFCFLX_X",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
!       iret = nf_put_att_text(ncid,varid,'long_name',14,'surface flux x')
!       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!       iret = nf_def_var(ncid,"Q_SFCFLX_Y",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
!       iret = nf_put_att_text(ncid,varid,'long_name',14,'surface flux y')
!       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"QSTRMVOLRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'channel flux')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"SFCHEADSUBRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'surface head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!      iret = nf_def_var(ncid,"SOXRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!      iret = nf_put_att_text(ncid,varid,'units',1,'1')
!      iret = nf_put_att_text(ncid,varid,'long_name',7,'slope x')
!      iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!      iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!      iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!      iret = nf_def_var(ncid,"SOYRT",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
!      iret = nf_put_att_text(ncid,varid,'units',1,'1')
!      iret = nf_put_att_text(ncid,varid,'long_name',7,'slope 7')
!      iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
!      iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
!      iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!       iret = nf_def_var(ncid,"SUB_RESID",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)

!-- place projection information
     if(hires_flag.eq.1) then !if/then hires_georef
      iret = nf_def_var(ncid,"lambert_conformal_conic",NF_INT,0, 0,varid)
      iret = nf_put_att_text(ncid,varid,'grid_mapping_name',23,'lambert_conformal_conic')
      iret = nf_put_att_real(ncid,varid,'longitude_of_central_meridian',NF_FLOAT,1,long_cm)
      iret = nf_put_att_real(ncid,varid,'latitude_of_projection_origin',NF_FLOAT,1,lat_po)
      iret = nf_put_att_real(ncid,varid,'false_easting',NF_FLOAT,1,fe)
      iret = nf_put_att_real(ncid,varid,'false_northing',NF_FLOAT,1,fn)
      iret = nf_put_att_real(ncid,varid,'standard_parallel',NF_FLOAT,2,sp)
     end if   !endif hires_georef

!      iret = nf_def_var(ncid,"Date",   NF_CHAR,  2, (/dimid_datelen,dimid_times/),     varid)

      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate
      convention(1:32) = "CF-1.0"
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",6, convention)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19)
      iret = nf_put_att_int(ncid,NF_GLOBAL,"output_decimation_factor",NF_INT, 1,decimation)

      iret = nf_enddef(ncid)

!!-- write latitude and longitude locations
!       xdumd = LATVAL
        iret = nf_inq_varid(ncid,"x", varid)
!       iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)
         iret = nf_put_vara_double(ncid, varid, (/1/), (/ixrtd/), xcoord_d) !-- 1-d array

!       xdumd = LONVAL
        iret = nf_inq_varid(ncid,"y", varid)
!       iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)
        iret = nf_put_vara_double(ncid, varid, (/1/), (/jxrtd/), ycoord_d) !-- 1-d array

        xdumd = LATVAL
        iret = nf_inq_varid(ncid,"LATITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)

        xdumd = LONVAL
        iret = nf_inq_varid(ncid,"LONGITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)



        write (*,*) "TEST....",LONVAL (1,1),(1,2)
        write (*,*) "TEST....",LATVAL (1,1),(1,2)




       do n = 1,nsoil
        if(n == 1) then
         asldpth(n) = -sldpth(n)
        else
         asldpth(n) = asldpth(n-1) - sldpth(n)
        endif
       enddo

       iret = nf_inq_varid(ncid,"depth", varid)
       iret = nf_put_vara_real(ncid, varid, (/1/), (/nsoil/), asldpth)
!yw       iret = nf_close(ncstatic)

    endif

    output_count = output_count + 1

!!-- time
        iret = nf_inq_varid(ncid,"time", varid)
        iret = nf_put_vara_int(ncid, varid, (/output_count/), (/1/), seconds_since)

!-- 3-d soils
     do n = 1, nsoil
!DJG inv      jj = int(jxrt/decimation)
      jj = 1
      ii = 0
!DJG inv      do j = jxrt,1,-decimation
       do j = 1,jxrt,decimation
       do i = 1,ixrt,decimation
        ii = ii + 1  
        if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
         xdumd(ii,jj) = smcrt(i,j,n)
        endif
      enddo 
       ii = 0
!DJG inv       jj = jj -1
       jj = jj + 1
     enddo
!       where (vegtyp(:,:) == 16) xdum = -1.E33
          iret = nf_inq_varid(ncid,  "SOIL_M", varid)
          iret = nf_put_vara_real(ncid, varid, (/1,1,n,output_count/), (/ixrtd,jxrtd,1,1/), xdumd)
    enddo !-n soils


!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!       jj = int(jxrt/decimation)
!       ii = 0
!!       do j = jxrt,1,-decimation
!      do j = 1,jxrt,decimation
!        do i = 1,ixrt,decimation
!         ii = ii + 1  
!         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
!          xdumd(ii,jj) = QSUBRT(i,j)
!         endif
!       enddo 
!       ii = 0
!       jj = jj - 1 
!      enddo
!     iret = nf_inq_varid(ncid,  "QSUBRT", varid)
!     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

!    xdum = ZWATTABLRT
!   where (vegtyp(:,:) == 16) xdum = -1.E33
!DJG inv       jj = int(jxrt/decimation)
       jj = 1
       ii = 0
!DJG inv       do j = jxrt,1,-decimation
       do j = 1,jxrt,decimation
        do i = 1,ixrt,decimation
         ii = ii + 1  
         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
          xdumd(ii,jj) = ZWATTABLRT(i,j)
         endif
       enddo 
       ii = 0
!DJG inv       jj = jj - 1 
       jj = jj + 1 
      enddo
        iret = nf_inq_varid(ncid,  "ZWATTABLRT", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)


!!    xdum = Q_SFCFLX_X
!!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!       jj = int(jxrt/decimation)
!       ii = 0
!!       do j = jxrt,1,-decimation
!      do j = 1,jxrt,decimation
!        do i = 1,ixrt,decimation
!         ii = ii + 1  
!         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
!          xdumd(ii,jj) = Q_SFCFLX_X(i,j)
!         endif
!       enddo 
!       ii = 0
!       jj = jj - 1 
!      enddo
!     iret = nf_inq_varid(ncid,  "Q_SFCFLX_X", varid)
!     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
!!
!!    xdum = Q_SFCFLX_Y
!!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!       jj = int(jxrt/decimation)
!       ii = 0
!!       do j = jxrt,1,-decimation
!      do j = 1,jxrt,decimation
!        do i = 1,ixrt,decimation
!         ii = ii + 1  
!         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
!          xdumd(ii,jj) = Q_SFCFLX_Y(i,j)
!         endif
!       enddo 
!       ii = 0
!       jj = jj - 1 
!      enddo
!     iret = nf_inq_varid(ncid,  "Q_SFCFLX_Y", varid)
!     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
 
!    xdum = QSTRMVOLRT
!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!DJG inv       jj = int(jxrt/decimation)
       jj = 1
       ii = 0
!DJG inv       do j = jxrt,1,-decimation
       do j = 1,jxrt,decimation
        do i = 1,ixrt,decimation
         ii = ii + 1  
         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
          xdumd(ii,jj) = QSTRMVOLRT(i,j)
         endif
       enddo 
       ii = 0
!DJG inv      jj = jj - 1 
       jj = jj + 1 
      enddo
     iret = nf_inq_varid(ncid,  "QSTRMVOLRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
 
!    xdum = SFCHEADSUBRT 
!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!DJG inv       jj = int(jxrt/decimation)
       jj = 1
       ii = 0
!DJG inv       do j = jxrt,1,-decimation
      do j = 1,jxrt,decimation
        do i = 1,ixrt,decimation
         ii = ii + 1  
         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
          xdumd(ii,jj) = SFCHEADSUBRT(i,j)
         endif
       enddo 
       ii = 0
!DJG inv       jj = jj - 1 
       jj = jj + 1 
      enddo
     iret = nf_inq_varid(ncid,  "SFCHEADSUBRT", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
 

!   iret = nf_inq_varid(ncid,  "SOXRT", varid)
!   iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

!!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!    iret = nf_inq_varid(ncid,  "SOYRT", varid)
!    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
!
!!    xdum = SUB_RESID
!!!   where (vegtyp(:,:) == 16) xdum = -1.E33
!!    iret = nf_inq_varid(ncid,  "SUB_RESID", varid)
!!    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
!
!!time in seconds since startdate

       iret = nf_redef(ncid)
       date19(1:len_trim(date)) = date
       iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)
 
       iret = nf_enddef(ncid)
      iret = nf_sync(ncid)
      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
      endif

     deallocate(xdumd)
     deallocate(xcoord_d)
     deallocate(xcoord)
     deallocate(ycoord_d)
     deallocate(ycoord)
     
     write(6,*) "end of output_rt" 
     call flush(6)

  end subroutine output_rt

!BF output section for gw2d model
!bftodo: clean up an customize for GW usage
  subroutine output_gw(igrid, split_output_count, ixrt, jxrt, nsoil, &
       startdate, date, HEAD, SMCRT, convgw, SFCHEADSUBRT, &
       wrfsi_finegrid_flnm,dt,sldpth,LATVAL,LONVAL,dist,HIRES_OUT)

#ifdef MPP_LAND
       USE module_mpp_land
#endif
!output the routing variables over routing grid.
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count
    integer,                                  intent(in) :: ixrt,jxrt
    real,                                     intent(in) :: dt
    real,                                     intent(in) :: dist(ixrt,jxrt,9)
    integer,                                  intent(in) :: nsoil
    integer,                                  intent(in) :: HIRES_OUT 
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: date
    character(len=*),          intent(in)                :: wrfsi_finegrid_flnm
    real,             dimension(nsoil),       intent(in) :: sldpth
    real, allocatable, DIMENSION(:,:)                   :: xdumd  !-- decimated variable
    real*8, allocatable, DIMENSION(:)                   :: xcoord_d, xcoord
    real*8, allocatable, DIMENSION(:)                   :: ycoord_d, ycoord

    integer, save :: ncid,ncstatic
    integer, save :: output_count
    real,    dimension(nsoil) :: asldpth

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: iret, dimid_soil, i,j,ii,jj
    character(len=256) :: output_flnm
    character(len=19)  :: date19
    character(len=32)  :: convention
    character(len=34)  :: sec_since_date

    character(len=30)  :: soilm

    real                                :: long_cm,lat_po,fe,fn, chan_in
    real, dimension(2)                  :: sp

    real, dimension(ixrt,jxrt) :: head, convgw
    real, dimension(ixrt,jxrt) :: SFCHEADSUBRT
    real, dimension(ixrt,jxrt) :: latval,lonval
    real, dimension(ixrt,jxrt,nsoil) :: SMCRT

    integer :: seconds_since, decimation, ixrtd,jxrtd, hires_flag
    
#ifdef MPP_LAND
    real, dimension(global_rt_nx,global_rt_ny) :: gHead, gConvgw, gSFCHEADSUBRT
    real, dimension(global_rt_nx,global_rt_ny) :: gLatval, gLonval
    real, dimension(global_rt_nx,global_rt_ny,nsoil) :: gSMCRT
#endif
    
#ifdef MPP_LAND
    call write_IO_rt_real(latval,gLatval)
    call write_IO_rt_real(lonval,gLonval)
    call write_IO_rt_real(SFCHEADSUBRT,gSFCHEADSUBRT)
    call write_IO_rt_real(head,gHead)
    call write_IO_rt_real(convgw,gConvgw)
    
    do i = 1, NSOIL
     call write_IO_rt_real(SMCRT(:,:,i),gSMCRT(:,:,i))
    end do

   if(my_id.eq.IO_id) then
     

#endif
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    seconds_since = int(dt)*output_count

    decimation = 1 !-- decimation factor
#ifdef MPP_LAND
    ixrtd = int(global_rt_nx/decimation)
    jxrtd = int(global_rt_ny/decimation)
#else
    ixrtd = int(ixrt/decimation)
    jxrtd = int(jxrt/decimation)
#endif
    allocate(xdumd(ixrtd,jxrtd))
    allocate(xcoord_d(ixrtd))
    allocate(ycoord_d(jxrtd))
    allocate(xcoord(ixrtd))
    allocate(ycoord(jxrtd))
    ii = 0
    jj = 0

    if (hires_out.eq.1) return  ! return if hires flag eq 1, if =2 output full grid

    if (output_count == 0) then

   !-- Open the  finemesh static files to obtain projection information
      write(*,'("wrfsi_finegrid_flnm: ''", A, "''")') trim(wrfsi_finegrid_flnm)
      iret = nf_open(trim(wrfsi_finegrid_flnm), NF_NOWRITE, ncstatic)

      if (iret /= 0) then
         write(*,'("Problem opening wrfsi_finegrid file: ''", A, "''")') &
         trim(wrfsi_finegrid_flnm)
         write(*,*) "HIRES_OUTPUT will not be georeferenced..."
        hires_flag = 0
      else
        hires_flag = 1
      endif

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get Latitude (X)
      iret = NF_INQ_VARID(ncstatic,'x',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, xcoord)
      ! Get Longitude (Y)
      iret = NF_INQ_VARID(ncstatic,'y',varid)
      if(iret .eq. 0) iret = NF_GET_VAR_DOUBLE(ncstatic, varid, ycoord)
     else
      xcoord_d = 0.
      ycoord_d = 0.
     end if  !endif hires_georef 

     do j=jxrt,1,-1*decimation
        jj = jj+1
        if (jj<= jxrtd) then
         ycoord_d(jj) = ycoord(j)
        endif
     enddo

!yw     do i = 1,ixrt,decimation
!yw        ii = ii + 1
!yw        if (ii <= ixrtd) then 
!yw         xcoord_d(ii) = xcoord(i)
         xcoord_d = xcoord
!yw        endif
!yw     enddo
       

     if(hires_flag.eq.1) then !if/then hires_georef
      ! Get projection information from finegrid netcdf file
      iret = NF_INQ_VARID(ncstatic,'lambert_conformal_conic',varid)
      if(iret .eq. 0) iret = NF_GET_ATT_REAL(ncstatic, varid, 'longitude_of_central_meridian', long_cm)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'latitude_of_projection_origin', lat_po)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_easting', fe)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_northing', fn)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'standard_parallel', sp)  !-- read it from the static file
     end if  !endif hires_georef 
      iret = nf_close(ncstatic)

!-- create the fine grid routing file
       write(output_flnm, '(A12,".GW_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
       print*, 'output_flnm = "'//trim(output_flnm)//'"'

       iret = nf_create(trim(output_flnm), 0, ncid)
       if (iret /= 0) stop "Problem nf_create"

       iret = nf_def_dim(ncid, "time", NF_UNLIMITED, dimid_times)
       iret = nf_def_dim(ncid, "x", ixrtd, dimid_ix)  !-- make a decimated grid
       iret = nf_def_dim(ncid, "y", jxrtd, dimid_jx)
       iret = nf_def_dim(ncid, "depth", nsoil, dimid_soil)  !-- 3-d soils

!--- define variables
       !- time definition, timeObs
       iret = nf_def_var(ncid,"time",NF_INT, 1, (/dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)

       !- x-coordinate in cartesian system
       iret = nf_def_var(ncid,"x",NF_DOUBLE, 1, (/dimid_ix/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',26,'x coordinate of projection')
       iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_x_coordinate')
       iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- y-coordinate in cartesian ssystem
       iret = nf_def_var(ncid,"y",NF_DOUBLE, 1, (/dimid_jx/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',26,'y coordinate of projection')
       iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_y_coordinate')
       iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- LATITUDE
       iret = nf_def_var(ncid,"LATITUDE",NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',8,'LATITUDE')
       iret = nf_put_att_text(ncid,varid,'standard_name',8,'LATITUDE')
       iret = nf_put_att_text(ncid,varid,'units',5,'deg North')

       !- LONGITUDE
       iret = nf_def_var(ncid,"LONGITUDE",NF_FLOAT, 2, (/dimid_ix,dimid_jx/), varid)
       iret = nf_put_att_text(ncid,varid,'long_name',9,'LONGITUDE')
       iret = nf_put_att_text(ncid,varid,'standard_name',9,'LONGITUDE')
       iret = nf_put_att_text(ncid,varid,'units',5,'deg east')

       !-- z-level is soil
       iret = nf_def_var(ncid,"depth", NF_FLOAT, 1, (/dimid_soil/),varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'cm')
       iret = nf_put_att_text(ncid,varid,'long_name',19,'depth of soil layer')

       iret = nf_def_var(ncid,  "SOIL_M",  NF_FLOAT, 4, (/dimid_ix,dimid_jx,dimid_soil,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',6,'kg m-2')
       iret = nf_put_att_text(ncid,varid,'description',16,'moisture content')
       iret = nf_put_att_text(ncid,varid,'long_name',26,soilm)
       iret = nf_put_att_text(ncid,varid,'coordinates',5,'x y z')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"HEAD",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',1,'m')
       iret = nf_put_att_text(ncid,varid,'long_name',17,'groundwater head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"CONVGW",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'channel flux')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

       iret = nf_def_var(ncid,"Platzhalter",NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_put_att_text(ncid,varid,'units',2,'mm')
       iret = nf_put_att_text(ncid,varid,'long_name',12,'surface head')
       iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
       iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
       iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)

!-- place projection information
     if(hires_flag.eq.1) then !if/then hires_georef
      iret = nf_def_var(ncid,"lambert_conformal_conic",NF_INT,0, 0,varid)
      iret = nf_put_att_text(ncid,varid,'grid_mapping_name',23,'lambert_conformal_conic')
      iret = nf_put_att_real(ncid,varid,'longitude_of_central_meridian',NF_FLOAT,1,long_cm)
      iret = nf_put_att_real(ncid,varid,'latitude_of_projection_origin',NF_FLOAT,1,lat_po)
      iret = nf_put_att_real(ncid,varid,'false_easting',NF_FLOAT,1,fe)
      iret = nf_put_att_real(ncid,varid,'false_northing',NF_FLOAT,1,fn)
      iret = nf_put_att_real(ncid,varid,'standard_parallel',NF_FLOAT,2,sp)
     end if   !endif hires_georef

!      iret = nf_def_var(ncid,"Date",   NF_CHAR,  2, (/dimid_datelen,dimid_times/),     varid)

      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate
      convention(1:32) = "CF-1.0"
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",6, convention)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19)
      iret = nf_put_att_int(ncid,NF_GLOBAL,"output_decimation_factor",NF_INT, 1,decimation)

      iret = nf_enddef(ncid)

!!-- write latitude and longitude locations
!       xdumd = LATVAL
        iret = nf_inq_varid(ncid,"x", varid)
!       iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)
	iret = nf_put_vara_double(ncid, varid, (/1/), (/ixrtd/), xcoord_d) !-- 1-d array

!       xdumd = LONVAL
        iret = nf_inq_varid(ncid,"y", varid)
!       iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)
        iret = nf_put_vara_double(ncid, varid, (/1/), (/jxrtd/), ycoord_d) !-- 1-d array

#ifdef MPP_LAND
        xdumd = gLATVAL
#else  
        xdumd = LATVAL
#endif
        iret = nf_inq_varid(ncid,"LATITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)

#ifdef MPP_LAND
        xdumd = gLONVAL
#else  
        xdumd = LONVAL
#endif
        iret = nf_inq_varid(ncid,"LONGITUDE", varid)
        iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ixrtd,jxrtd/), xdumd)

       do n = 1,nsoil
        if(n == 1) then
         asldpth(n) = -sldpth(n)
        else
         asldpth(n) = asldpth(n-1) - sldpth(n)
        endif
       enddo

       iret = nf_inq_varid(ncid,"depth", varid)
       iret = nf_put_vara_real(ncid, varid, (/1/), (/nsoil/), asldpth)
!yw       iret = nf_close(ncstatic)

    endif

    output_count = output_count + 1

!!-- time
        iret = nf_inq_varid(ncid,"time", varid)
        iret = nf_put_vara_int(ncid, varid, (/output_count/), (/1/), seconds_since)

!-- 3-d soils
     do n = 1, nsoil
#ifdef MPP_LAND
        xdumd = gSMCRT(:,:,n)
#else  
        xdumd = SMCRT(:,:,n)
#endif
! !DJG inv      jj = int(jxrt/decimation)
!       jj = 1
!       ii = 0
! !DJG inv      do j = jxrt,1,-decimation
!        do j = 1,jxrt,decimation
!        do i = 1,ixrt,decimation
!         ii = ii + 1  
!         if(ii <= ixrtd .and. jj <= jxrtd .and. jj >0) then
!          xdumd(ii,jj) = smcrt(i,j,n)
!         endif
!       enddo 
!        ii = 0
! !DJG inv       jj = jj -1
!        jj = jj + 1
!      enddo
!       where (vegtyp(:,:) == 16) xdum = -1.E33
          iret = nf_inq_varid(ncid,  "SOIL_M", varid)
          iret = nf_put_vara_real(ncid, varid, (/1,1,n,output_count/), (/ixrtd,jxrtd,1,1/), xdumd)
    enddo !-n soils

#ifdef MPP_LAND
        xdumd = gHead
#else  
        xdumd = head
#endif

     iret = nf_inq_varid(ncid,  "HEAD", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)

#ifdef MPP_LAND
        xdumd = gConvgw
#else  
        xdumd = convgw
#endif
     iret = nf_inq_varid(ncid,  "CONVGW", varid)
     iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrtd,jxrtd,1/), xdumd)
 
!!time in seconds since startdate

       iret = nf_redef(ncid)
       date19(1:len_trim(date)) = date
       iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)
 
       iret = nf_enddef(ncid)
      iret = nf_sync(ncid)
      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
      endif

     deallocate(xdumd)
     deallocate(xcoord_d)
     deallocate(xcoord)
     deallocate(ycoord_d)
     deallocate(ycoord)
     
     write(6,*) "end of output_ge" 
     call flush(6)
#ifdef MPP_LAND
    endif
#endif

  end subroutine output_gw

!-- output the channel route in an IDV 'station' compatible format
   subroutine output_chrt(igrid, split_output_count, NLINKS, ORDER, &
        startdate,date,chlon, chlat, hlink,zelev,qlink,dtrt,K,STRMFRXSTPTS)
     
     implicit none
#include <netcdf.inc>
!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS
     real, dimension(NLINKS),                  intent(in) :: chlon,chlat
     real, dimension(NLINKS),                  intent(in) :: hlink,zelev
     integer, dimension(NLINKS),               intent(in) :: ORDER
     integer, dimension(NLINKS),               intent(inout) :: STRMFRXSTPTS

     real,                                     intent(in) :: dtrt
     real, dimension(NLINKS,2),                intent(in) :: qlink

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date

     real, allocatable, DIMENSION(:)            :: chanlat,chanlon
     real, allocatable, DIMENSION(:)            :: chanlatO,chanlonO

     real, allocatable, DIMENSION(:)            :: elevation
     real, allocatable, DIMENSION(:)            :: elevationO

     integer, allocatable, DIMENSION(:)         :: station_id
     integer, allocatable, DIMENSION(:)         :: station_idO

     integer, allocatable, DIMENSION(:)         :: rec_num_of_station
     integer, allocatable, DIMENSION(:)         :: rec_num_of_stationO

     integer, allocatable, DIMENSION(:)         :: lOrder !- local stream order
     integer, allocatable, DIMENSION(:)         :: lOrderO !- local stream order

     integer, save  :: output_count
     integer, save  :: ncid,ncid2

     integer :: stationdim, dimdata, varid, charid, n
     integer :: obsdim, dimdataO, charidO

     integer :: iret,i, start_pos, prev_pos, order_to_write!-- order_to_write is the lowest stream order to output
     integer :: start_posO, prev_posO

     integer :: previous_pos  !-- used for the station model
     character(len=256) :: output_flnm,output_flnm2
     character(len=19)  :: date19,date19start
     character(len=34)  :: sec_since_date
     integer :: seconds_since,nstations,cnt,ObsStation,nobs
     character(len=32)  :: convention
     character(len=11),allocatable, DIMENSION(:)  :: stname
     character(len=11),allocatable, DIMENSION(:)  :: stnameO

    !--- all this for writing the station id string
     INTEGER   TDIMS, TXLEN
     PARAMETER (TDIMS=2)    ! number of TX dimensions
     PARAMETER (TXLEN = 11) ! length of example string
     INTEGER  TIMEID        ! record dimension id
     INTEGER  TXID          ! variable ID
     INTEGER  TXDIMS(TDIMS) ! variable shape
     INTEGER  TSTART(TDIMS), TCOUNT(TDIMS)

     !--  observation point  ids
     INTEGER   OTDIMS, OTXLEN
     PARAMETER (OTDIMS=2)    ! number of TX dimensions
     PARAMETER (OTXLEN = 11) ! length of example string
     INTEGER  OTIMEID        ! record dimension id
     INTEGER  OTXID          ! variable ID
     INTEGER  OTXDIMS(OTDIMS) ! variable shape
     INTEGER  OTSTART(OTDIMS), OTCOUNT(OTDIMS)

     write(6,*) "yyww dtrt =", dtrt , "k =", k
   
     seconds_since = int(dtrt)*K

     order_to_write = 3  !-- 1 all; 6 feweest
!    print *, "Subroutine Output_chrt,K, secs links", K, seconds_since, NLINKS

      nstations = 0  ! total number of channel points to display
      nobs      = 0  ! number of observation points

!-- output only the higher oder streamflows  and only observation points
       do i=1,NLINKS
        if(ORDER(i) .ge. order_to_write) then 
         nstations = nstations + 1
        endif
        if(STRMFRXSTPTS(i) .ne. -9999) then 
         nobs = nobs + 1
        endif
       enddo 

       if (nobs .eq. 0) then ! let's at least make one obs point
         nobs = 1
         STRMFRXSTPTS(1) = 1
       endif

       allocate(chanlat(nstations))
       allocate(chanlon(nstations))
       allocate(elevation(nstations))
       allocate(station_id(nstations))
       allocate(lOrder(nstations))
       allocate(rec_num_of_station(nstations))
       allocate(stname(nstations))

       allocate(chanlatO(nobs))
       allocate(chanlonO(nobs))
       allocate(elevationO(nobs))
       allocate(station_idO(nobs))
       allocate(lOrderO(nobs))
       allocate(rec_num_of_stationO(nobs))
       allocate(stnameO(nobs))

       if(output_count == 0) then 
!-- have moved sec_since_date from above here..
        sec_since_date = 'seconds since '//startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10) &
                  //' '//startdate(12:13)//':'//startdate(15:16)//' UTC'

        date19start(1:len_trim(startdate)) = startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10)//'_' &
                  //startdate(12:13)//':'//startdate(15:16)//':00'

        nstations = 0
        nobs = 0

        write(output_flnm, '(A12,".CHRTOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
        write(output_flnm2,'(A12,".CHANOBS_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid

        print*, 'output_flnm = "'//trim(output_flnm)//'"'

        iret = nf_create(trim(output_flnm), 0, ncid)
        if (iret /= 0) stop "Problem nf_create points"

        iret = nf_create(trim(output_flnm2), 0, ncid2)
        if (iret /= 0) stop "Problem nf_create observation"

       do i=1,NLINKS
        if(ORDER(i) .ge. order_to_write) then 
         nstations = nstations + 1
         chanlat(nstations) = chlat(i)
         chanlon(nstations) = chlon(i)
         elevation(nstations) = zelev(i)
         lOrder(nstations) = ORDER(i)
         station_id(nstations) = i
         if(STRMFRXSTPTS(nstations) .eq. -9999) then 
           ObsStation = 0
         else 
           ObsStation = 1
         endif
         write(stname(nstations),'(I6,"_",I1,"_S",I1)') nstations,lOrder(nstations),ObsStation
        endif
       enddo 


       do i=1,NLINKS
        if(STRMFRXSTPTS(i) .ne. -9999) then 
         nobs = nobs + 1
         chanlatO(nobs) = chlat(i)
         chanlonO(nobs) = chlon(i)
         elevationO(nobs) = zelev(i)
         lOrderO(nobs) = ORDER(i)
         station_idO(nobs) = i
         write(stnameO(nobs),'(I6,"_",I1)') nobs,lOrderO(nobs)
         print *,"stationobservation name",  stnameO(nobs)
        endif
       enddo 

       iret = nf_def_dim(ncid, "recNum", NF_UNLIMITED, dimdata)  !--for linked list approach


       iret = nf_def_dim(ncid, "station", nstations, stationdim)



       iret = nf_def_dim(ncid2, "recNum", NF_UNLIMITED, dimdataO)  !--for linked list approach
       iret = nf_def_dim(ncid2, "station", nobs, obsdim)


      !- station location definition all,  lat
        iret = nf_def_var(ncid,"latitude",NF_FLOAT, 1, (/stationdim/), varid)
       write(6,*) "iret 2.1,  ", iret, stationdim
        iret = nf_put_att_text(ncid,varid,'long_name',16,'Station latitude')
       write(6,*) "iret 2.2", iret
        iret = nf_put_att_text(ncid,varid,'units',13,'degrees_north')
       write(6,*) "iret 2.3", iret


      !- station location definition obs,  lat
        iret = nf_def_var(ncid2,"latitude",NF_FLOAT, 1, (/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',20,'Observation latitude')
        iret = nf_put_att_text(ncid2,varid,'units',13,'degrees_north')


      !- station location definition,  long
        iret = nf_def_var(ncid,"longitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',17,'Station longitude')
        iret = nf_put_att_text(ncid,varid,'units',12,'degrees_east')


      !- station location definition, obs long
        iret = nf_def_var(ncid2,"longitude",NF_FLOAT, 1, (/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',21,'Observation longitude')
        iret = nf_put_att_text(ncid2,varid,'units',12,'degrees_east')


!     !-- elevation is ZELEV
        iret = nf_def_var(ncid,"altitude",NF_FLOAT, 1, (/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',16,'Station altitude')
        iret = nf_put_att_text(ncid,varid,'units',6,'meters')


!     !-- elevation is obs ZELEV
        iret = nf_def_var(ncid2,"altitude",NF_FLOAT, 1, (/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',20,'Observation altitude')
        iret = nf_put_att_text(ncid2,varid,'units',6,'meters')


!     !--  gage observation
!       iret = nf_def_var(ncid,"gages",NF_FLOAT, 1, (/stationdim/), varid)
!       iret = nf_put_att_text(ncid,varid,'long_name',20,'Stream Gage Location')
!       iret = nf_put_att_text(ncid,varid,'units',4,'none')

!-- parent index
        iret = nf_def_var(ncid,"parent_index",NF_INT,1,(/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',36,'index of the station for this record')

        iret = nf_def_var(ncid2,"parent_index",NF_INT,1,(/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',36,'index of the station for this record')

     !-- prevChild
        iret = nf_def_var(ncid,"prevChild",NF_INT,1,(/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',57,'record number of the previous record for the same station')
!ywtmp        iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)
        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

        iret = nf_def_var(ncid2,"prevChild",NF_INT,1,(/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',57,'record number of the previous record for the same station')
!ywtmp        iret = nf_put_att_int(ncid2,varid,'_FillValue',NF_INT,2,-1)
        iret = nf_put_att_int(ncid2,varid,'_FillValue',2,-1)

     !-- lastChild
        iret = nf_def_var(ncid,"lastChild",NF_INT,1,(/stationdim/), varid)
        iret = nf_put_att_text(ncid,varid,'long_name',30,'latest report for this station')
!ywtmp        iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)
        iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

        iret = nf_def_var(ncid2,"lastChild",NF_INT,1,(/obsdim/), varid)
        iret = nf_put_att_text(ncid2,varid,'long_name',30,'latest report for this station')
!ywtmp        iret = nf_put_att_int(ncid2,varid,'_FillValue',NF_INT,2,-1)
        iret = nf_put_att_int(ncid2,varid,'_FillValue',2,-1)

!     !- flow definition, var
        iret = nf_def_var(ncid, "streamflow", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid,varid,'long_name',10,'River Flow')

        iret = nf_def_var(ncid2, "streamflow", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',13,'meter^3 / sec')
        iret = nf_put_att_text(ncid2,varid,'long_name',10,'River Flow')

!     !- flow definition, var
!       iret = nf_def_var(ncid, "pos_streamflow", NF_FLOAT, 1, (/dimdata/), varid)
!       iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')
!       iret = nf_put_att_text(ncid,varid,'long_name',14,'abs streamflow')

!     !- head definition, var
        iret = nf_def_var(ncid, "head", NF_FLOAT, 1, (/dimdata/), varid)
        iret = nf_put_att_text(ncid,varid,'units',5,'meter')
        iret = nf_put_att_text(ncid,varid,'long_name',11,'River Stage')

        iret = nf_def_var(ncid2, "head", NF_FLOAT, 1, (/dimdataO/), varid)
        iret = nf_put_att_text(ncid2,varid,'units',5,'meter')
        iret = nf_put_att_text(ncid2,varid,'long_name',11,'River Stage')

     !-- station  id
     ! define character-position dimension for strings of max length 11
         iret = NF_DEF_DIM(ncid, "id_len", 11, charid)
         TXDIMS(1) = charid   ! define char-string variable and position dimension first
         TXDIMS(2) = stationdim
         iret = nf_def_var(ncid,"station_id",NF_CHAR, TDIMS, TXDIMS, varid)
         iret = nf_put_att_text(ncid,varid,'long_name',10,'Station id')

         iret = NF_DEF_DIM(ncid2, "id_len", 11, charidO)
         OTXDIMS(1) = charidO   ! define char-string variable and position dimension first
         OTXDIMS(2) = obsdim
         iret = nf_def_var(ncid2,"station_id",NF_CHAR, OTDIMS, OTXDIMS, varid)
         iret = nf_put_att_text(ncid2,varid,'long_name',14,'Observation id')


!     !- time definition, timeObs
         iret = nf_def_var(ncid,"time_observation",NF_INT, 1, (/dimdata/), varid)
         iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)
         iret = nf_put_att_text(ncid,varid,'long_name',19,'time of observation')

         iret = nf_def_var(ncid2,"time_observation",NF_INT, 1, (/dimdataO/), varid)
         iret = nf_put_att_text(ncid2,varid,'units',34,sec_since_date)
         iret = nf_put_att_text(ncid2,varid,'long_name',19,'time of observation')

         iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "Conventions",32, convention)

         convention(1:32) = "Unidata Observation Dataset v1.0"
         iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid, NF_GLOBAL, "cdm_datatype",7, "Station")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
         iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19start)
         iret = nf_put_att_text(ncid, NF_GLOBAL, "stationDimension",7, "station")
         iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
         iret = nf_put_att_int(ncid, NF_GLOBAL, "stream order output",NF_INT,1,order_to_write)

         iret = nf_put_att_text(ncid2, NF_GLOBAL, "Conventions",32, convention)
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "cdm_datatype",7, "Station")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "time_coverage_start",19, date19start)
         iret = nf_put_att_text(ncid2, NF_GLOBAL, "stationDimension",7, "station")
         iret = nf_put_att_real(ncid2, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
         iret = nf_put_att_int(ncid2, NF_GLOBAL, "stream order output",NF_INT,1,order_to_write)

         iret = nf_enddef(ncid)
         iret = nf_enddef(ncid2)
        
        !-- write latitudes
         iret = nf_inq_varid(ncid,"latitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), chanlat)

         iret = nf_inq_varid(ncid2,"latitude", varid)
         iret = nf_put_vara_real(ncid2, varid, (/1/), (/nobs/), chanlatO)

        !-- write longitudes
         iret = nf_inq_varid(ncid,"longitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), chanlon)

         iret = nf_inq_varid(ncid2,"longitude", varid)
         iret = nf_put_vara_real(ncid2, varid, (/1/), (/nobs/), chanlonO)

        !-- write elevations
         iret = nf_inq_varid(ncid,"altitude", varid)
         iret = nf_put_vara_real(ncid, varid, (/1/), (/nstations/), elevation)

         iret = nf_inq_varid(ncid2,"altitude", varid)
         iret = nf_put_vara_real(ncid2, varid, (/1/), (/nobs/), elevationO)

      !-- write gage location
!      iret = nf_inq_varid(ncid,"gages", varid)
!      iret = nf_put_vara_int(ncid, varid, (/1/), (/nstations/), STRMFRXSTPTS)

        !-- write number_of_stations, OPTIONAL
      !!  iret = nf_inq_varid(ncid,"number_stations", varid)
      !!  iret = nf_put_var_int(ncid, varid, nstations)

        !-- write station id's 
         do i=1,nstations
          TSTART(1) = 1
          TSTART(2) = i
          TCOUNT(1) = TXLEN
          TCOUNT(2) = 1
          iret = nf_inq_varid(ncid,"station_id", varid)
          iret = nf_put_vara_text(ncid, varid, TSTART, TCOUNT, stname(i))
         enddo

        !-- write observation id's 
         do i=1, nobs
          OTSTART(1) = 1
          OTSTART(2) = i
          OTCOUNT(1) = OTXLEN
          OTCOUNT(2) = 1
          iret = nf_inq_varid(ncid2,"station_id", varid)
          iret = nf_put_vara_text(ncid2, varid, OTSTART, OTCOUNT, stnameO(i))
         enddo

     endif

     output_count = output_count + 1

     open (unit=999,file='frxst_pts_out.txt',status='unknown',position='append')

     cnt=0
     do i=1,NLINKS

       if(ORDER(i) .ge. order_to_write) then 
         start_pos = (cnt+1)+(nstations*(output_count-1))

         !!--time in seconds since startdate
          iret = nf_inq_varid(ncid,"time_observation", varid)
          iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), seconds_since)

         iret = nf_inq_varid(ncid,"streamflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), qlink(i,1))

!        iret = nf_inq_varid(ncid,"pos_streamflow", varid)
!        iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), abs(qlink(i,1)))

         iret = nf_inq_varid(ncid,"head", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), hlink(i))

         !-- station index.. will repeat for every timesstep
         iret = nf_inq_varid(ncid,"parent_index", varid)
         iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), cnt)

          !--record number of previous record for same station
!obsolete format         prev_pos = cnt+(nstations*(output_count-1))
         prev_pos = cnt+(nobs*(output_count-2))
         if(output_count.ne.1) then !-- only write next set of records
           iret = nf_inq_varid(ncid,"prevChild", varid)
           iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), prev_pos)
!IF block to add -1 to last element of prevChild array to designate end of list...
!           if(cnt+1.eq.nobs.AND.output_count.eq.split_output_count) then
!             iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), -1)
!           else
!             iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), prev_pos)
!           endif 
         endif

         cnt=cnt+1  !--indices are 0 based
         rec_num_of_station(cnt) = start_pos-1  !-- save position for last child, 0-based!!

!         if(STRMFRXSTPTS(i) .ne. -9999) then 
!           write(999,117) seconds_since,cnt,chanlonO(i),chanlatO(i),abs(qlink(i,1)),hlink(i)
!117 FORMAT(I8,1X,I7,1X,F10.5,1X,F8.5,1X,F7.3,1x,F6.3)
!         endif !end if for FRXSTPTS

       endif
    enddo
!    close(999) 

    !-- output  only observation points
    cnt=0
    do i=1,NLINKS

      if(STRMFRXSTPTS(i) .ne. -9999) then 
         print *, "Outputting frxst pt. :",STRMFRXSTPTS(i)
         start_posO = (cnt+1)+(nobs * (output_count-1))

!Write frxst_pts to text file...
           write(999,117) seconds_since,trim(date),cnt,chlon(i),chlat(i), &
                    abs(qlink(i,1)), abs(qlink(i,1))*35.315,hlink(i)
117 FORMAT(I8,1X,A25,1X,I7,1X,F10.5,1X,F8.5,1X,F9.3,1x,F12.3,1X,F6.3)

         !!--time in seconds since startdate
         iret = nf_inq_varid(ncid2,"time_observation", varid)
         iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), seconds_since)

         iret = nf_inq_varid(ncid2,"streamflow", varid)
         iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), qlink(i,1))

         iret = nf_inq_varid(ncid2,"head", varid)
         iret = nf_put_vara_real(ncid2, varid, (/start_posO/), (/1/), hlink(i))

         !-- station index.. will repeat for every timesstep
         iret = nf_inq_varid(ncid2,"parent_index", varid)
         iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), cnt)

          !--record number of previous record for same station
!obsolete format          prev_posO = cnt+(nobs*(output_count-1))
         prev_posO = cnt+(nobs*(output_count-2))
         if(output_count.ne.1) then !-- only write next set of records
           iret = nf_inq_varid(ncid2,"prevChild", varid)
           iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), prev_posO)

!IF block to add -1 to last element of prevChild array to designate end of list...
!           if(cnt+1.eq.nobs.AND.output_count.eq.split_output_count) then
!             iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), -1)
!           else
!             iret = nf_put_vara_int(ncid2, varid, (/start_posO/), (/1/), prev_posO)
!           endif 


         endif

         cnt=cnt+1  !--indices are 0 based
         rec_num_of_stationO(cnt) = start_posO - 1  !-- save position for last child, 0-based!!


      endif

    enddo
    close(999) 


      !-- lastChild variable gives the record number of the most recent report for the station
      iret = nf_inq_varid(ncid,"lastChild", varid)
      iret = nf_put_vara_int(ncid, varid, (/1/), (/nstations/), rec_num_of_station)

      !-- lastChild variable gives the record number of the most recent report for the station
      iret = nf_inq_varid(ncid2,"lastChild", varid)
      iret = nf_put_vara_int(ncid2, varid, (/1/), (/nobs/), rec_num_of_stationO)

      iret = nf_redef(ncid)
      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(date)) = date
      iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)

      iret = nf_redef(ncid2)
      iret = nf_put_att_text(ncid2, NF_GLOBAL, "time_coverage_end", 19, date19)

      iret = nf_enddef(ncid)
      iret = nf_sync(ncid)

      iret = nf_enddef(ncid2)
      iret = nf_sync(ncid2)

      if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
        iret = nf_close(ncid2)
     endif

     deallocate(chanlat)
     deallocate(chanlon)
     deallocate(elevation)
     deallocate(station_id)
     deallocate(lOrder)
     deallocate(rec_num_of_station)
     deallocate(stname)

     deallocate(chanlatO)
     deallocate(chanlonO)
     deallocate(elevationO)
     deallocate(station_idO)
     deallocate(lOrderO)
     deallocate(rec_num_of_stationO)
     deallocate(stnameO)

     print *, "Exited Subroutine output_chrt"
     close(16)

20 format(i8,',',f12.7,',',f10.7,',',f6.2,',',i3)

end subroutine output_chrt

#ifdef MPP_LAND
!-- output the channel route in an IDV 'station' compatible format
   subroutine mpp_output_chrt(mpp_nlinks,nlinks_index,igrid, &
        split_output_count, NLINKS, ORDER, &
        startdate, date, chlon, chlat, hlink,zelev,qlink,dtrt, &
        K,STRMFRXSTPTS)

       USE module_mpp_land

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid,K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS
     real, dimension(NLINKS),               intent(in) :: chlon,chlat
     real, dimension(NLINKS),                  intent(in) :: hlink,zelev

     integer, dimension(NLINKS),               intent(in) :: ORDER
     integer, dimension(NLINKS),               intent(inout) :: STRMFRXSTPTS

     real,                                     intent(in) :: dtrt
     real, dimension(NLINKS,2),                intent(in) :: qlink

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date
     integer  :: mpp_nlinks, nlinks_index(nlinks)

!    write(16,*) mpp_nlinks,nlinks_index,igrid, &
!       split_output_count, NLINKS
!    write(17,*) ORDER
!    write(18,*)   startdate, date
!    write(19,*) chlon, chlat
!    write(20,*) hlink
!    write(21,*) zelev
!    write(22,*) qlink
!    write(23,*) dtrt, K
!    write(24,*) STRMFRXSTPTS
!    call land_finish()



     call write_chanel_int(order,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(chlon,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(chlat,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(hlink,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(zelev,nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(qlink(:,1),nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(qlink(:,2),nlinks_index,mpp_nlinks,nlinks)

     if(my_id .eq. IO_id) then
       call output_chrt(igrid, split_output_count, NLINKS, ORDER, &
          startdate, date, chlon, chlat, hlink,zelev,qlink,dtrt,K,&
          STRMFRXSTPTS)
    end if

end subroutine mpp_output_chrt

!---------  lake netcdf output -----------------------------------------
!-- output the ilake info an IDV 'station' compatible format -----------
   subroutine mpp_output_lakes(lake_index,igrid, split_output_count, NLAKES, &
        startdate, date, latlake, lonlake, elevlake, &
        qlakei,qlakeo, resht,dtrt,K)
#ifdef MPP_LAND
   USE module_mpp_land
#endif

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid, K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLAKES
     real, dimension(NLAKES),                  intent(in) :: latlake,lonlake,elevlake,resht
     real, dimension(NLAKES),                  intent(in) :: qlakei,qlakeo  !-- inflow and outflow of lake
     real,                                     intent(in) :: dtrt

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date
     integer lake_index(nlakes)

     call write_lake_real(latlake,lake_index,nlakes)
     call write_lake_real(lonlake,lake_index,nlakes)
     call write_lake_real(elevlake,lake_index,nlakes)
     call write_lake_real(resht,lake_index,nlakes)
     call write_lake_real(qlakei,lake_index,nlakes)
     call write_lake_real(qlakeo,lake_index,nlakes)
     if(my_id.eq. IO_id) then
        call output_lakes(igrid, split_output_count, NLAKES, &
           startdate, date, latlake, lonlake, elevlake, &
           qlakei,qlakeo, resht,dtrt,K)
     end if
     return
     end subroutine mpp_output_lakes

#endif

!----------------------------------- lake netcdf output
!-- output the ilake info an IDV 'station' compatible format
   subroutine output_lakes(igrid, split_output_count, NLAKES, &
        startdate, date, latlake, lonlake, elevlake, &
        qlakei,qlakeo, resht,dtrt,K)

!!output the routing variables over just channel
     integer,                                  intent(in) :: igrid, K
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLAKES
     real, dimension(NLAKES),                  intent(in) :: latlake,lonlake,elevlake,resht
     real, dimension(NLAKES),                  intent(in) :: qlakei,qlakeo  !-- inflow and outflow of lake
     real,                                     intent(in) :: dtrt

     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date

     integer, allocatable, DIMENSION(:)                   :: station_id
     integer, allocatable, DIMENSION(:)                   :: rec_num_of_lake

     integer, save  :: output_count
     integer, save :: ncid

     integer :: stationdim, dimdata, varid, charid, n
     integer :: iret,i, start_pos, prev_pos  !-- 
     integer :: previous_pos        !-- used for the station model
     character(len=256) :: output_flnm
     character(len=19)  :: date19, date19start
     character(len=34)  :: sec_since_date
     integer :: seconds_since,cnt
     character(len=32)  :: convention
     character(len=6),allocatable, DIMENSION(:)  :: stname

    !--- all this for writing the station id string
     INTEGER   TDIMS, TXLEN
     PARAMETER (TDIMS=2)    ! number of TX dimensions
     PARAMETER (TXLEN = 6) ! length of example string
     INTEGER  TIMEID        ! record dimension id
     INTEGER  TXID          ! variable ID
     INTEGER  TXDIMS(TDIMS) ! variable shape
     INTEGER  TSTART(TDIMS), TCOUNT(TDIMS)

!    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
!    seconds_since = int(dtrt)*output_count
     seconds_since = int(dtrt)*K

     allocate(station_id(NLAKES))
     allocate(rec_num_of_lake(NLAKES))
     allocate(stname(NLAKES))

     if (output_count == 0) then

!-- have moved sec_since_date from above here..
      sec_since_date = 'seconds since '//startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10) &
                  //' '//startdate(12:13)//':'//startdate(15:16)//' UTC'

      date19start(1:len_trim(startdate)) = startdate(1:4)//'-'//startdate(6:7)//'-'//startdate(9:10)//'_' &
                  //startdate(12:13)//':'//startdate(15:16)//':00'

      write(output_flnm, '(A12,".LAKEOUT_DOMAIN",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
      print*, 'output_flnm = "'//trim(output_flnm)//'"'

      iret = nf_create(trim(output_flnm), 0, ncid)
      if (iret /= 0) stop "Problem nf_create"

      do i=1,NLAKES
         station_id(i) = i
         write(stname(i),'(I6)') i
      enddo 

      iret = nf_def_dim(ncid, "recNum", NF_UNLIMITED, dimdata)  !--for linked list approach
      iret = nf_def_dim(ncid, "station", nlakes, stationdim)

      !- station location definition,  lat
      iret = nf_def_var(ncid,"latitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',13,'Lake latitude')
      iret = nf_put_att_text(ncid,varid,'units',13,'degrees_north')

      !- station location definition,  long
      iret = nf_def_var(ncid,"longitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',14,'Lake longitude')
      iret = nf_put_att_text(ncid,varid,'units',12,'degrees_east')

!     !-- lake's phyical elevation
      iret = nf_def_var(ncid,"altitude",NF_FLOAT, 1, (/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',13,'Lake altitude')
      iret = nf_put_att_text(ncid,varid,'units',6,'meters')

     !-- parent index
      iret = nf_def_var(ncid,"parent_index",NF_INT,1,(/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',33,'index of the lake for this record')

     !-- prevChild
      iret = nf_def_var(ncid,"prevChild",NF_INT,1,(/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',54,'record number of the previous record for the same lake')
!ywtmp      iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)
      iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

     !-- lastChild
      iret = nf_def_var(ncid,"lastChild",NF_INT,1,(/stationdim/), varid)
      iret = nf_put_att_text(ncid,varid,'long_name',27,'latest report for this lake')
!ywtmp      iret = nf_put_att_int(ncid,varid,'_FillValue',NF_INT,2,-1)
      iret = nf_put_att_int(ncid,varid,'_FillValue',2,-1)

!     !- water surface elevation
      iret = nf_def_var(ncid, "elevation", NF_FLOAT, 1, (/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'units',6,'meters')
      iret = nf_put_att_text(ncid,varid,'long_name',14,'Lake Elevation')

!     !- inflow to lake
      iret = nf_def_var(ncid, "inflow", NF_FLOAT, 1, (/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')

!     !- outflow to lake
      iret = nf_def_var(ncid, "outflow", NF_FLOAT, 1, (/dimdata/), varid)
      iret = nf_put_att_text(ncid,varid,'units',13,'meter^3 / sec')

     !-- station  id
     ! define character-position dimension for strings of max length 6
         iret = NF_DEF_DIM(ncid, "id_len", 6, charid)
         TXDIMS(1) = charid   ! define char-string variable and position dimension first
         TXDIMS(2) = stationdim
         iret = nf_def_var(ncid,"station_id",NF_CHAR, TDIMS, TXDIMS, varid)
         iret = nf_put_att_text(ncid,varid,'long_name',10,'Station id')

!     !- time definition, timeObs
         iret = nf_def_var(ncid,"time_observation",NF_INT, 1, (/dimdata/), varid)
         iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)
         iret = nf_put_att_text(ncid,varid,'long_name',19,'time of observation')

!       date19(1:19) = "0000-00-00_00:00:00"
!       date19(1:len_trim(startdate)) = startdate
!       iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
!
        date19(1:19) = "0000-00-00_00:00:00"
        date19(1:len_trim(startdate)) = startdate
        convention(1:32) = "Unidata Observation Dataset v1.0"
        iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",32, convention)
        iret = nf_put_att_text(ncid, NF_GLOBAL, "cdm_datatype",7, "Station")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_max",4, "90.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lat_min",5, "-90.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_max",5, "180.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "geospatial_lon_min",6, "-180.0")
        iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19start)
        iret = nf_put_att_text(ncid, NF_GLOBAL, "stationDimension",7, "station")
!!       iret = nf_put_att_text(ncid, NF_GLOBAL, "observationDimension",6, "recNum")
!!        iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coordinate",16,"time_observation")
        iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
        iret = nf_enddef(ncid)

        !-- write latitudes
        iret = nf_inq_varid(ncid,"latitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), LATLAKE)

        !-- write longitudes
        iret = nf_inq_varid(ncid,"longitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), LONLAKE)

        !-- write physical height of lake
        iret = nf_inq_varid(ncid,"altitude", varid)
        iret = nf_put_vara_real(ncid, varid, (/1/), (/NLAKES/), elevlake)

        !-- write station id's 
         do i=1,nlakes
          TSTART(1) = 1
          TSTART(2) = i
          TCOUNT(1) = TXLEN
          TCOUNT(2) = 1
          iret = nf_inq_varid(ncid,"station_id", varid)
          iret = nf_put_vara_text(ncid, varid, TSTART, TCOUNT, stname(i))
         enddo

     endif

     output_count = output_count + 1

     cnt=0
     do i=1,NLAKES

         start_pos = (cnt+1)+(nlakes*(output_count-1))

         !!--time in seconds since startdate
         iret = nf_inq_varid(ncid,"time_observation", varid)
         iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), seconds_since)

         iret = nf_inq_varid(ncid,"elevation", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), resht(i))

         iret = nf_inq_varid(ncid,"inflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), qlakei(i))

         iret = nf_inq_varid(ncid,"outflow", varid)
         iret = nf_put_vara_real(ncid, varid, (/start_pos/), (/1/), qlakeo(i))

         !-- station index.. will repeat for every timesstep
         iret = nf_inq_varid(ncid,"parent_index", varid)
         iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), cnt)

          !--record number of previous record for same station
         prev_pos = cnt+(nlakes*(output_count-1))
         if(output_count.ne.1) then !-- only write next set of records
           iret = nf_inq_varid(ncid,"prevChild", varid)
           iret = nf_put_vara_int(ncid, varid, (/start_pos/), (/1/), prev_pos)
         endif

         cnt=cnt+1  !--indices are 0 based
         rec_num_of_lake(cnt) = start_pos-1  !-- save position for last child, 0-based!!

    enddo

      !-- lastChild variable gives the record number of the most recent report for the station
      iret = nf_inq_varid(ncid,"lastChild", varid)
      iret = nf_put_vara_int(ncid, varid, (/1/), (/nlakes/), rec_num_of_lake)

     !-- number of children reported for this station, OPTIONAL
     !--  iret = nf_inq_varid(ncid,"numChildren", varid)
     !--  iret = nf_put_vara_int(ncid, varid, (/1/), (/nlakes/), rec_num_of_lake)

    iret = nf_redef(ncid)
    date19(1:19) = "0000-00-00_00:00:00"
    date19(1:len_trim(date)) = date
    iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)
    iret = nf_enddef(ncid)

    iret = nf_sync(ncid)
     if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
     endif

     deallocate(station_id)
     deallocate(rec_num_of_lake)
     deallocate(stname)
     print *, "Exited Subroutine output_lakes"
     close(16)

 end subroutine output_lakes
!----------------------------------- lake netcdf output

#ifdef MPP_LAND

!-- output the channel route in an IDV 'grid' compatible format
   subroutine mpp_output_chrtgrd(igrid, split_output_count, ixrt,jxrt, &
        NLINKS,CH_NETRT_in, CH_NETLNK_in, ORDER, startdate, date, &
        qlink, dt, wrfsi_finegrid_flnm, mpp_nlinks,nlinks_index,g_ixrt,g_jxrt )
#ifdef MPP_LAND
   USE module_mpp_land
#endif
     implicit none
#include <netcdf.inc>
     integer g_ixrt,g_jxrt
     integer,                                  intent(in) :: igrid
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS,ixrt,jxrt
     real,                                     intent(in) :: dt
     real, dimension(NLINKS,2),                intent(in) :: qlink
     integer, dimension(g_IXRT,g_JXRT)         :: CH_NETRT,CH_NETLNK
     integer, dimension(IXRT,JXRT),            intent(in) :: CH_NETRT_in,CH_NETLNK_in
     integer, dimension(NLINKS),               intent(in) :: ORDER !--currently not used here, see finegrid.f
     character(len=*),          intent(in)     :: wrfsi_finegrid_flnm
     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date
    
     integer::  mpp_nlinks , nlinks_index(nlinks)
     call write_chanel_real(qlink(:,1),nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_real(qlink(:,2),nlinks_index,mpp_nlinks,nlinks)
     call write_chanel_int(order,nlinks_index,mpp_nlinks,nlinks)
     call write_IO_rt_int(CH_NETRT_in, CH_NETRT)
     call write_IO_rt_int(CH_NETLNK_in, CH_NETLNK)

     call  output_chrtgrd(igrid, split_output_count, ixrt,jxrt, &
        NLINKS,CH_NETRT, CH_NETLNK, ORDER, startdate, date, &
        qlink, dt, wrfsi_finegrid_flnm)
  
     return
     end subroutine mpp_output_chrtgrd
#endif

!-- output the channel route in an IDV 'grid' compatible format
   subroutine output_chrtgrd(igrid, split_output_count, ixrt,jxrt, &
        NLINKS,CH_NETRT, CH_NETLNK, ORDER, startdate, date, &
        qlink, dt, wrfsi_finegrid_flnm)

     integer,                                  intent(in) :: igrid
     integer,                                  intent(in) :: split_output_count
     integer,                                  intent(in) :: NLINKS,ixrt,jxrt
     real,                                     intent(in) :: dt
     real, dimension(NLINKS,2),                intent(in) :: qlink
     integer, dimension(IXRT,JXRT),            intent(in) :: CH_NETRT,CH_NETLNK
     integer, dimension(NLINKS),               intent(in) :: ORDER !--currently not used here, see finegrid.f
     character(len=*),          intent(in)     :: wrfsi_finegrid_flnm
     character(len=*),                         intent(in) :: startdate
     character(len=*),                         intent(in) :: date
     character(len=32)  :: convention
     integer,save  :: output_count
     integer, save :: ncid,ncstatic
     real, dimension(IXRT,JXRT)          :: tmpflow
     real, dimension(IXRT)            :: xcoord
     real, dimension(JXRT)            :: ycoord
     real                                :: long_cm,lat_po,fe,fn
     real, dimension(2)                  :: sp

    integer :: varid, n
    integer :: jxlatdim,ixlondim,timedim !-- dimension ids

    integer :: iret,i,j
    character(len=256) :: output_flnm
    character(len=19)  :: date19
    character(len=34)  :: sec_since_date
 

    integer :: seconds_since
    sec_since_date = 'seconds since '//date(1:4)//'-'//date(6:7)//'-'//date(9:10)//' '//date(12:13)//':'//date(15:16)//' UTC'
    seconds_since = dt*output_count

    !-- Open the  finemesh static files to obtain projection information
      write(*,'("wrfsi_finegrid_flnm: ''", A, "''")') trim(wrfsi_finegrid_flnm)
      iret = nf_open(wrfsi_finegrid_flnm, NF_NOWRITE, ncstatic)

      if (iret /= 0) then
         write(*,'("Problem opening wrfsi_finegrid file: ''", A, "''")') &
         trim(wrfsi_finegrid_flnm)
       stop
      endif

      ! Get Latitude (X)
      iret = NF_INQ_VARID(ncstatic,'x',varid)
      iret = NF_GET_VAR_DOUBLE(ncstatic, varid, xcoord) 
      ! Get Longitude (Y)
      iret = NF_INQ_VARID(ncstatic,'y',varid)
      iret = NF_GET_VAR_DOUBLE(ncstatic, varid, ycoord) 

      ! Get projection information from finegrid netcdf file
      iret = NF_INQ_VARID(ncstatic,'lambert_conformal_conic',varid)  
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'longitude_of_central_meridian', long_cm)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'latitude_of_projection_origin', lat_po)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_easting', fe)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'false_northing', fn)  !-- read it from the static file
      iret = NF_GET_ATT_REAL(ncstatic, varid, 'standard_parallel', sp)  !-- read it from the static file

      tmpflow = -9E15

      if (output_count == 0) then
 
        write(output_flnm, '(A12,".CHRTOUT_GRID",I1)') date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
        print*, 'output_flnm = "'//trim(output_flnm)//'"'
 
!--- define dimension
        iret = nf_create(trim(output_flnm), 0, ncid)
        if (iret /= 0) stop "Problem nf_create"

        iret = nf_def_dim(ncid, "time", NF_UNLIMITED, timedim)
        iret = nf_def_dim(ncid, "x", ixrt, ixlondim)
        iret = nf_def_dim(ncid, "y", jxrt, jxlatdim)

!--- define variables
!     !- time definition, timeObs
         iret = nf_def_var(ncid,"time",NF_INT, 1, (/timedim/), varid)
         iret = nf_put_att_text(ncid,varid,'units',34,sec_since_date)

       !- x-coordinate in cartesian system
          iret = nf_def_var(ncid,"x",NF_DOUBLE, 1, (/ixlondim/), varid)
          iret = nf_put_att_text(ncid,varid,'long_name',26,'x coordinate of projection')
          iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_x_coordinate')
          iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

       !- y-coordinate in cartesian ssystem
          iret = nf_def_var(ncid,"y",NF_DOUBLE, 1, (/jxlatdim/), varid)
          iret = nf_put_att_text(ncid,varid,'long_name',26,'y coordinate of projection')
          iret = nf_put_att_text(ncid,varid,'standard_name',23,'projection_y_coordinate')
          iret = nf_put_att_text(ncid,varid,'units',5,'Meter')

!     !- flow definition, var
        iret = nf_def_var(ncid,"flow",NF_REAL, 3, (/ixlondim,jxlatdim,timedim/), varid)
        iret = nf_put_att_text(ncid,varid,'units',6,'m3 s-1')
        iret = nf_put_att_text(ncid,varid,'long_name',15,'water flow rate')
        iret = nf_put_att_text(ncid,varid,'coordinates',3,'x y')
        iret = nf_put_att_text(ncid,varid,'grid_mapping',23,'lambert_conformal_conic')
        iret = nf_put_att_real(ncid,varid,'missing_value',NF_REAL,1,-9E15)


!-- place prjection information
      iret = nf_def_var(ncid,"lambert_conformal_conic",NF_INT,0, 0,varid)
      iret = nf_put_att_text(ncid,varid,'grid_mapping_name',23,'lambert_conformal_conic')
      iret = nf_put_att_real(ncid,varid,'longitude_of_central_meridian',NF_DOUBLE,1,long_cm)
      iret = nf_put_att_real(ncid,varid,'latitude_of_projection_origin',NF_DOUBLE,1,lat_po)
      iret = nf_put_att_real(ncid,varid,'false_easting',NF_DOUBLE,1,fe)
      iret = nf_put_att_real(ncid,varid,'false_northing',NF_DOUBLE,1,fn)
      iret = nf_put_att_real(ncid,varid,'standard_parallel',NF_DOUBLE,2,sp)


      date19(1:19) = "0000-00-00_00:00:00"
      date19(1:len_trim(startdate)) = startdate
      convention(1:32) = "CF-1.0"
      iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -9E15)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "Conventions",6, convention)
      iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_start",19, date19)

      iret = nf_enddef(ncid)

!!-- write latitude and longitude locations
        iret = nf_inq_varid(ncid,"y", varid)
        iret = nf_put_vara_double(ncid, varid, (/1/), (/jxrt/), ycoord) !-- 1-d array

        iret = nf_inq_varid(ncid,"x", varid)
        iret = nf_put_vara_double(ncid, varid, (/1/), (/ixrt/), xcoord) !-- 1-d array

      iret = nf_close(ncstatic)

     endif

    output_count = output_count + 1

!going to output a group at a given time

!DJG inv    do j=jxrt,1,-1
    do j=1,jxrt
     do i=1,ixrt
       if(CH_NETRT(i,j).GE.0) then
!        print *, CH_NETLNK(i,j),CH_NETRT(i,j),i,j
         tmpflow(i,j) = qlink(CH_NETLNK(i,j),1) 
       else
!       print *, CH_NETLNK(i,j),CH_NETRT(i,j),i,j
         tmpflow(i,j) = -9E15
       endif
     enddo
    enddo

!!time in seconds since startdate
    iret = nf_inq_varid(ncid,"time", varid)
    iret = nf_put_vara_int(ncid, varid, (/output_count/), (/1/), seconds_since)

    iret = nf_inq_varid(ncid,"flow", varid)
!      print*, 'flow varid = ', varid
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ixrt,jxrt,1/),tmpflow)

     iret = nf_redef(ncid)
     date19(1:19) = "0000-00-00_00:00:00"
     date19(1:len_trim(date)) = date
     iret = nf_put_att_text(ncid, NF_GLOBAL, "time_coverage_end", 19, date19)
     iret = nf_enddef(ncid)

     iret = nf_sync(ncid)

     if (output_count == split_output_count) then
        output_count = 0
        iret = nf_close(ncid)
     endif

 end subroutine output_chrtgrd

#ifdef MPP_LAND
  subroutine mpp_output_hrldas(second_flag,g_ix,g_jx, &
       ounit, igrid, split_output_count, hgrid, range, ix, jx, &
       nsoil, sldpth, startdate, olddate, &
       in_vegtyp, in_soltyp,in_t1, in_cmc, in_stc, in_smc, in_sh2ox, in_soilmx, in_runoff1x, in_runoff2x, &
       in_runoff3x, in_etax, in_etakin, in_ecx, in_edirx, in_ettx, in_albedx, in_weasd, in_acrain, in_acsnom, &
       in_esnow2d, in_drip2d, in_dewfall, in_snodep, in_fpar, in_hx, in_qfx, in_short, in_infxsrt, in_prcp_out,  &
       in_etpndx,in_sfcheadrt,in_smcmax1,wrfsi_static_flnm,FORC_TYP )

   USE module_mpp_land
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: ounit
    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count
    character,                                intent(in) :: hgrid
    character(len=*),                         intent(in) :: range
    integer,                                  intent(in) :: ix
    integer,                                  intent(in) :: jx
    integer,                                  intent(in) :: nsoil
    real,             dimension(nsoil),       intent(in) :: sldpth
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: olddate
    integer,          dimension(ix,jx),       intent(in) :: in_vegtyp
    integer,          dimension(ix,jx),       intent(in) :: in_soltyp
    real,             dimension(ix,jx),       intent(in) :: in_t1
    real,             dimension(ix,jx),       intent(in) :: in_cmc
    real,             dimension(ix,jx,nsoil), intent(in) :: in_stc
    real,             dimension(ix,jx,nsoil), intent(in) :: in_smc
    real,             dimension(ix,jx,nsoil), intent(in) :: in_sh2ox
    real,             dimension(ix,jx,nsoil), intent(in) :: in_soilmx
    real,             dimension(ix,jx),       intent(in) :: in_runoff2x
    real,             dimension(ix,jx),       intent(in) :: in_runoff1x
    real,             dimension(ix,jx),       intent(in) :: in_runoff3x
    real,             dimension(ix,jx),       intent(in) :: in_etax
    real,             dimension(ix,jx),       intent(in) :: in_etakin
    real,             dimension(ix,jx),       intent(in) :: in_ecx
    real,             dimension(ix,jx),       intent(in) :: in_edirx
    real,             dimension(ix,jx),       intent(in) :: in_ettx
    real,             dimension(ix,jx),       intent(in) :: in_albedx
    real,             dimension(ix,jx),       intent(in) :: in_weasd
    real,             dimension(ix,jx),       intent(in) :: in_acrain
    real,             dimension(ix,jx),       intent(in) :: in_acsnom
    real,             dimension(ix,jx),       intent(in) :: in_esnow2d
    real,             dimension(ix,jx),       intent(in) :: in_drip2d
    real,             dimension(ix,jx),       intent(in) :: in_dewfall
    real,             dimension(ix,jx),       intent(in) :: in_snodep
    real,             dimension(ix,jx),       intent(in) :: in_fpar
    real,             dimension(ix,jx),       intent(in) :: in_hx
    real,             dimension(ix,jx),       intent(in) :: in_qfx
    real,             dimension(ix,jx),       intent(in) :: in_short
    real,             dimension(ix,jx),       intent(in) :: in_infxsrt
    real,             dimension(ix,jx),       intent(in) :: in_prcp_out
    real,             dimension(ix,jx),       intent(in) :: in_etpndx
    real,             dimension(ix,jx),       intent(in) :: in_sfcheadrt
    real,             dimension(ix,jx),       intent(in) :: in_smcmax1
    character(len=*),          intent(in)  :: wrfsi_static_flnm
    integer FORC_TYP

    integer :: output_count = 0
    integer, save :: ncid
    logical second_flag

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: iret
    character(len=256) :: output_flnm
    character(len=19)  :: date19

    real, dimension(ix,jx) :: xdum
    integer g_ix,g_jx,i,j
!- ------------------------------------------
    integer,          dimension(g_ix,g_jx)         :: vegtyp
    integer,          dimension(g_ix,g_jx)         :: soltyp
    real,             dimension(g_ix,g_jx)         :: t1
    real,             dimension(g_ix,g_jx)         :: cmc
    real,             dimension(g_ix,g_jx,nsoil)   :: stc
    real,             dimension(g_ix,g_jx,nsoil)   :: smc
    real,             dimension(g_ix,g_jx,nsoil)   :: sh2ox
    real,             dimension(g_ix,g_jx,nsoil)   :: soilmx
    real,             dimension(g_ix,g_jx)         :: runoff2x
    real,             dimension(g_ix,g_jx)         :: runoff1x
    real,             dimension(g_ix,g_jx)         :: runoff3x
    real,             dimension(g_ix,g_jx)         :: etax
    real,             dimension(g_ix,g_jx)         :: etakin
    real,             dimension(g_ix,g_jx)         :: ecx
    real,             dimension(g_ix,g_jx)         :: edirx
    real,             dimension(g_ix,g_jx)         :: ettx
    real,             dimension(g_ix,g_jx)         :: albedx
    real,             dimension(g_ix,g_jx)         :: weasd
    real,             dimension(g_ix,g_jx)         :: acrain
    real,             dimension(g_ix,g_jx)         :: acsnom
    real,             dimension(g_ix,g_jx)         :: esnow2d
    real,             dimension(g_ix,g_jx)         :: drip2d
    real,             dimension(g_ix,g_jx)         :: dewfall
    real,             dimension(g_ix,g_jx)         :: snodep
    real,             dimension(g_ix,g_jx)         :: fpar
    real,             dimension(g_ix,g_jx)         :: hx
    real,             dimension(g_ix,g_jx)         :: qfx
    real,             dimension(g_ix,g_jx)         :: short
    real,             dimension(g_ix,g_jx)         :: infxsrt
    real,             dimension(g_ix,g_jx)         :: prcp_out
    real,             dimension(g_ix,g_jx)         :: etpndx
    real,             dimension(g_ix,g_jx)         :: sfcheadrt
    real,             dimension(g_ix,g_jx)         :: smcmax1


    call write_IO_int(in_VEGTYP,VEGTYP)
    call write_IO_int(in_soltyp,soltyp)
    call write_IO_real(in_t1,t1)
    call write_IO_real(in_cmc,cmc)
    do i = 1, NSOIL
       call write_IO_real(in_stc(:,:,i),stc(:,:,i))
       call write_IO_real(in_smc(:,:,i),smc(:,:,i))
       call write_IO_real(in_sh2ox(:,:,i),sh2ox(:,:,i))
       call write_IO_real(in_soilmx(:,:,i),soilmx(:,:,i))
    end do
    call write_IO_real(in_runoff2x,runoff2x)
    call write_IO_real(in_runoff1x,runoff1x)
    call write_IO_real(in_runoff3x,runoff3x)
    call write_IO_real(in_etax,etax)
    call write_IO_real(in_etakin,etakin)
    call write_IO_real(in_ecx,ecx)
    call write_IO_real(in_edirx,edirx)
    call write_IO_real(in_ettx,ettx)
    call write_IO_real(in_albedx,albedx)
    call write_IO_real(in_weasd,weasd)
    call write_IO_real(in_acrain,acrain)
    call write_IO_real(in_acsnom,acsnom)
    call write_IO_real(in_esnow2d,esnow2d)
    call write_IO_real(in_drip2d,drip2d)
    call write_IO_real(in_dewfall,dewfall)
    call write_IO_real(in_snodep,snodep)
    call write_IO_real(in_fpar,fpar)
    call write_IO_real(in_hx,hx)
    call write_IO_real(in_qfx,qfx)
    call write_IO_real(in_short,short)
    call write_IO_real(in_infxsrt,infxsrt)
    call write_IO_real(in_prcp_out,prcp_out)
    call write_IO_real(in_etpndx,etpndx)
    call write_IO_real(in_sfcheadrt,sfcheadrt)
    call write_IO_real(in_smcmax1,smcmax1)



    if(my_id.eq.IO_id) then 
     call output_hrldas( ounit,igrid,split_output_count, hgrid,range, g_ix, g_jx, &
       nsoil, sldpth, startdate, olddate, &
             vegtyp, soltyp, t1, cmc, stc, smc, sh2ox, soilmx, runoff1x, runoff2x, &
             runoff3x, etax, etakin, ecx, edirx, ettx, albedx, weasd, acrain, acsnom, &
             esnow2d, drip2d, dewfall, snodep, fpar, hx, qfx, short, infxsrt, prcp_out, &
             etpndx,sfcheadrt,smcmax1,wrfsi_static_flnm,forc_typ)
    end if

    return
    end subroutine mpp_output_hrldas
#endif

    subroutine read_stage4(flnm,IX,JX,pcp)
#ifdef MPP_LAND
   USE module_mpp_land
#endif
        integer IX,JX,ierr,ncid,i,j
        real pcp(IX,JX),buf(ix,jx)
        character(len=*),  intent(in)  :: flnm
        character(len=256) :: units

        ierr = nf_open(flnm, NF_NOWRITE, ncid)
        if(ierr .ne. 0) then
#ifdef MPP_LAND
            call MPI_finalize(ierr)
#endif
            stop 9998
        endif 
        call get_2d_netcdf("RAINRATE",ncid, buf,   units, ix, jx, .TRUE., ierr)
        ierr = nf_close(ncid)
        do j = 1, jx
        do i = 1, ix
            if(buf(i,j) .lt. 0) then 
                 buf(i,j) = pcp(i,j) 
                 write(99,*) "negative forcing. i=",i,"j=",j,flnm 
            end if 
        end do 
        end do 
        pcp = buf
        return
    END subroutine read_stage4

    
    subroutine get2d_hrldas(inflnm,ix,jx,nsoil,smc,stc,sh2ox,cmc,t1,weasd,snodep)
#ifdef MPP_LAND
   USE module_mpp_land
#endif
          implicit none
#include <netcdf.inc>
          integer :: iret,varid,ncid,ix,jx,nsoil,ierr
          real,dimension(ix,jx):: weasd,snodep,cmc,t1
          real,dimension(ix,jx,nsoil):: smc,stc,sh2ox
          character(len=*), intent(in) :: inflnm
          character(len=256)::   units
          iret = nf_open(trim(inflnm), NF_NOWRITE, ncid)
          if(iret .ne. 0 )then
              write(6,*) "Error: failed to open file :",trim(inflnm)
              call flush(6)
#ifdef MPP_LAND
             call mpp_land_abort()
#else
             stop
#endif
          endif

          call get2d_hrldas_real("CMC",     ncid, cmc,     ix, jx)
          call get2d_hrldas_real("TSKIN",   ncid, t1,      ix, jx)
          call get2d_hrldas_real("SWE",      ncid, weasd,   ix, jx)
          call get2d_hrldas_real("SNODEP",     ncid, snodep,   ix, jx)

    call get2d_hrldas_real("SOIL_T_1",    ncid, stc(:,:,1),  ix, jx)
    call get2d_hrldas_real("SOIL_T_2",    ncid, stc(:,:,2),  ix, jx)
    call get2d_hrldas_real("SOIL_T_3",    ncid, stc(:,:,3),  ix, jx)
    call get2d_hrldas_real("SOIL_T_4",    ncid, stc(:,:,4),  ix, jx)             
    call get2d_hrldas_real("SOIL_T_5",    ncid, stc(:,:,5),  ix, jx)             
    call get2d_hrldas_real("SOIL_T_6",    ncid, stc(:,:,6),  ix, jx)             
    call get2d_hrldas_real("SOIL_T_7",    ncid, stc(:,:,7),  ix, jx)             
    call get2d_hrldas_real("SOIL_T_8",    ncid, stc(:,:,8),  ix, jx)             

    call get2d_hrldas_real("SOIL_M_1",    ncid, SMC(:,:,1),  ix, jx)             
    call get2d_hrldas_real("SOIL_M_2",    ncid, SMC(:,:,2),  ix, jx)             
    call get2d_hrldas_real("SOIL_M_3",    ncid, SMC(:,:,3),  ix, jx)             
    call get2d_hrldas_real("SOIL_M_4",    ncid, SMC(:,:,4),  ix, jx)             
    call get2d_hrldas_real("SOIL_M_5",    ncid, SMC(:,:,5),  ix, jx)             
    call get2d_hrldas_real("SOIL_M_6",    ncid, SMC(:,:,6),  ix, jx)             
    call get2d_hrldas_real("SOIL_M_7",    ncid, SMC(:,:,7),  ix, jx)             
    call get2d_hrldas_real("SOIL_M_8",    ncid, SMC(:,:,8),  ix, jx)             

    call get2d_hrldas_real("SOIL_W_1",    ncid, SH2OX(:,:,1),  ix, jx)             
    call get2d_hrldas_real("SOIL_W_2",    ncid, SH2OX(:,:,2),  ix, jx)             
    call get2d_hrldas_real("SOIL_W_3",    ncid, SH2OX(:,:,3),  ix, jx)             
    call get2d_hrldas_real("SOIL_W_4",    ncid, SH2OX(:,:,4),  ix, jx)             
    call get2d_hrldas_real("SOIL_W_5",    ncid, SH2OX(:,:,5),  ix, jx)             
    call get2d_hrldas_real("SOIL_W_6",    ncid, SH2OX(:,:,6),  ix, jx)             
    call get2d_hrldas_real("SOIL_W_7",    ncid, SH2OX(:,:,7),  ix, jx)             
    call get2d_hrldas_real("SOIL_W_8",    ncid, SH2OX(:,:,8),  ix, jx)             

          iret = nf_close(ncid)
         return
      end subroutine get2d_hrldas

      subroutine get2d_hrldas_real(var_name,ncid,out_buff,ix,jx)
          implicit none
#include <netcdf.inc>
          integer ::iret,varid,ncid,ix,jx
          real out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          iret = nf_inq_varid(ncid,trim(var_name),  varid)
          iret = nf_get_var_real(ncid, varid, out_buff)
         return
      end subroutine get2d_hrldas_real

      SUBROUTINE READ_ROUTING_old(IXRT,JXRT,ELRT,CH_NETRT,LKSATFAC,route_topo_f,    &
            route_chan_f, wrfsi_finegrid_flnm,OVROUGHRTFAC,RETDEPRTFAC)


#include <netcdf.inc>
        INTEGER, INTENT(IN) :: IXRT,JXRT
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: ELRT,LKSATFAC
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT
!Dummy inverted grids
        REAL, DIMENSION(IXRT,JXRT) :: ELRT_inv,LKSATFAC_inv
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: OVROUGHRTFAC
        REAL, DIMENSION(IXRT,JXRT) :: OVROUGHRTFAC_inv
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: RETDEPRTFAC
        REAL, DIMENSION(IXRT,JXRT) :: RETDEPRTFAC_inv
        INTEGER, DIMENSION(IXRT,JXRT) :: CH_NETRT_inv

        INTEGER         :: I,J, iret, jj
        CHARACTER(len=256)        :: var_name
        CHARACTER(len=256)       :: route_topo_f
        CHARACTER(len=256)       :: route_chan_f
        CHARACTER(len=256)       :: wrfsi_finegrid_flnm

        var_name = "TOPOGRAPHY"
        iret = get2d_real(var_name,ELRT_inv,ixrt,jxrt,&
                     trim(wrfsi_finegrid_flnm))
        write(6,*) "read ",var_name
        call flush(6)

!!!DY to be fixed ... 6/27/08
!        var_name = "BED_ELEVATION"
!        iret = get2d_real(var_name,ELRT,ixrt,jxrt,&
!                     trim(wrfsi_finegrid_flnm))

        var_name = "CHANNELGRID"
        call get2d_int(var_name,CH_NETRT_inv,ixrt,jxrt,&
               trim(wrfsi_finegrid_flnm))

        write(6,*) "read ",var_name
        call flush(6)

        var_name = "LKSATFAC"
        LKSATFAC_inv = -9999.9
        iret = get2d_real(var_name,LKSATFAC_inv,ixrt,jxrt,&
               trim(wrfsi_finegrid_flnm))

        write(6,*) "read ",var_name
        call flush(6)

           where (LKSATFAC_inv == -9999.9) LKSATFAC_inv = 1000.0  !specify LKSAFAC if no term avail...


!1.12.2012...Read in routing calibration factors...
        var_name = "RETDEPRTFAC"
        iret = get2d_real(var_name,RETDEPRTFAC_inv,ixrt,jxrt,&
                     trim(wrfsi_finegrid_flnm))
        write(6,*) "read ",var_name, RETDEPRTFAC_inv(100,100)
        where (RETDEPRTFAC_inv < 0.) RETDEPRTFAC_inv = 1.0  ! reset grid to = 1.0 if non-valid value exists
        write(6,*) "read ",var_name, RETDEPRTFAC_inv(100,100)
        call flush(6)

        var_name = "OVROUGHRTFAC"
        iret = get2d_real(var_name,OVROUGHRTFAC_inv,ixrt,jxrt,&
                     trim(wrfsi_finegrid_flnm))
        write(6,*) "read ",var_name, OVROUGHRTFAC_inv(100,100)
        where (OVROUGHRTFAC_inv <= 0.) OVROUGHRTFAC_inv = 1.0 ! reset grid to = 1.0 if non-valid value exists
        write(6,*) "read ",var_name, OVROUGHRTFAC_inv(100,100)
        call flush(6)



!!!Flip y-dimension of highres grids from exported Arc files...

        do i=1,ixrt
        jj=jxrt
         do j=1,jxrt
           ELRT(i,j)=ELRT_inv(i,jj)
           CH_NETRT(i,j)=CH_NETRT_inv(i,jj)
           LKSATFAC(i,j)=LKSATFAC_inv(i,jj)
           RETDEPRTFAC(i,j)=RETDEPRTFAC_inv(i,jj)
           OVROUGHRTFAC(i,j)=OVROUGHRTFAC_inv(i,jj)
           jj=jxrt-j
         end do
        end do

        write(6,*) "after flip ",var_name, OVROUGHRTFAC(100,100)
        call flush(6)
        write(6,*) "after flip ",var_name, RETDEPRTFAC(100,100)
        call flush(6)

          
        write(6,*) "finish READ_ROUTING_old"
        call flush(6)

        return

!DJG -----------------------------------------------------
   END SUBROUTINE READ_ROUTING_old
!DJG _____________________________


#ifdef MPP_LAND
  subroutine mpp_output_rt(ixrt, jxrt,igrid, split_output_count, &
       ixrt_in, jxrt_in,nsoil, startdate, olddate, &
       QSUBRT_in,ZWATTABLRT_in,SMCRT_in,SUB_RESID_in,       &
       q_sfcflx_x_in,q_sfcflx_y_in,soxrt_in,soyrt_in,       &
       QSTRMVOLRT_in,SFCHEADSUBRT_in, &
       wrfsi_finegrid_flnm,dt,sldpth,LATVAL_in,LONVAL_in,dist,HIRES_OUT)

!output the routing variables over routing grid.
#ifdef MPP_LAND
   USE module_mpp_land
#endif
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count

! ixrt and jxrt are global. ixrt_in and jxrt_in are local array index.
    integer,                                  intent(in) :: ixrt,jxrt,ixrt_in,jxrt_in
    real,                                     intent(in) :: dt
    real,                                     intent(in) :: dist(ixrt_in,jxrt_in,9)
    integer,                                  intent(in) :: nsoil
    integer,                                  intent(in) :: HIRES_OUT
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: olddate
    character(len=*),          intent(in)                :: wrfsi_finegrid_flnm
    real,             dimension(nsoil),       intent(in) :: sldpth

    real, dimension(ixrt_in,jxrt_in) :: QSUBRT_in,ZWATTABLRT_in,SUB_RESID_in
    real, dimension(ixrt_in,jxrt_in) :: q_sfcflx_x_in,q_sfcflx_y_in
    real, dimension(ixrt_in,jxrt_in) :: QSTRMVOLRT_in
    real, dimension(ixrt_in,jxrt_in) :: SFCHEADSUBRT_in
    real, dimension(ixrt_in,jxrt_in) :: soxrt_in,soyrt_in
    real, dimension(ixrt_in,jxrt_in,nsoil) :: SMCRT_in
    real, dimension(ixrt_in,jxrt_in) :: LATVAL_in,LONVAL_in

    real, dimension(ixrt,jxrt) :: QSUBRT,ZWATTABLRT,SUB_RESID
    real, dimension(ixrt,jxrt) :: q_sfcflx_x,q_sfcflx_y
    real, dimension(ixrt,jxrt) :: QSTRMVOLRT
    real, dimension(ixrt,jxrt) :: SFCHEADSUBRT
    real, dimension(ixrt,jxrt) :: soxrt,soyrt
    real, dimension(ixrt,jxrt,nsoil) :: SMCRT
    real, dimension(ixrt,jxrt,9) :: dist_g
    real, dimension(ixrt,jxrt) :: LATVAL,LONVAL
    integer i



    write(6,*) "mpp_output_RT output file: ",trim(wrfsi_finegrid_flnm)
    call flush(6)

    call write_IO_rt_real(LATVAL_in,LATVAL)
    call write_IO_rt_real(LONVAL_in,LONVAL)
    call write_IO_rt_real(QSUBRT_in,QSUBRT)
   

    call write_IO_rt_real(ZWATTABLRT_in,ZWATTABLRT)


    call write_IO_rt_real(SUB_RESID_in,SUB_RESID)


    call write_IO_rt_real(QSTRMVOLRT_in,QSTRMVOLRT)



    call write_IO_rt_real(SFCHEADSUBRT_in,SFCHEADSUBRT)
    call write_IO_rt_real(soxrt_in,soxrt)




    call write_IO_rt_real(soyrt_in,soyrt)
    call write_IO_rt_real(q_sfcflx_x_in,q_sfcflx_x)
    call write_IO_rt_real(q_sfcflx_y_in,q_sfcflx_y)




    do i = 1, NSOIL
         call write_IO_rt_real(SMCRT_in(:,:,i),SMCRT(:,:,i))
    end do
    do i = 1, 9    
         call write_IO_rt_real(dist(:,:,i),dist_g(:,:,i))
    end do

!   yyywwww  ! temp test
!   if(my_id.eq. IO_id  ) write(14,*) dist(:,:,9)
!   if(my_id.eq. IO_id  ) write(12,*) dist_g(:,:,9)
!   call land_finish()




    if(my_id.eq.IO_id) then
       call output_rt(igrid, split_output_count, ixrt, jxrt, nsoil, &
          startdate, olddate, QSUBRT,ZWATTABLRT,SMCRT,SUB_RESID,       &
          q_sfcflx_x,q_sfcflx_y,soxrt,soyrt,QSTRMVOLRT,SFCHEADSUBRT, &
          wrfsi_finegrid_flnm,DT,SLDPTH,latval,lonval,dist_g,HIRES_OUT)
    end if

    write(6,*) "return from mpp_output_RT"
    call flush(6)
  end subroutine mpp_output_rt

#endif

!BF parallel version for output_gw
!bftodo: implementation


#ifdef MPP_LAND
  subroutine mpp_output_gw(ixrt, jxrt,igrid, split_output_count, &
       ixrt_in, jxrt_in,nsoil, startdate, olddate, &
       QSUBRT_in,ZWATTABLRT_in,SMCRT_in,SUB_RESID_in,       &
       q_sfcflx_x_in,q_sfcflx_y_in,soxrt_in,soyrt_in,       &
       QSTRMVOLRT_in,SFCHEADSUBRT_in, &
       wrfsi_finegrid_flnm,dt,sldpth,LATVAL_in,LONVAL_in,dist,HIRES_OUT)

!output the routing variables over routing grid.
   USE module_mpp_land
    implicit none
#include <netcdf.inc>

    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: split_output_count

! ixrt and jxrt are global. ixrt_in and jxrt_in are local array index.
    integer,                                  intent(in) :: ixrt,jxrt,ixrt_in,jxrt_in
    real,                                     intent(in) :: dt
    real,                                     intent(in) :: dist(ixrt_in,jxrt_in,9)
    integer,                                  intent(in) :: nsoil
    integer,                                  intent(in) :: HIRES_OUT
    character(len=*),                         intent(in) :: startdate
    character(len=*),                         intent(in) :: olddate
    character(len=*),          intent(in)                :: wrfsi_finegrid_flnm
    real,             dimension(nsoil),       intent(in) :: sldpth

    real, dimension(ixrt_in,jxrt_in) :: QSUBRT_in,ZWATTABLRT_in,SUB_RESID_in
    real, dimension(ixrt_in,jxrt_in) :: q_sfcflx_x_in,q_sfcflx_y_in
    real, dimension(ixrt_in,jxrt_in) :: QSTRMVOLRT_in
    real, dimension(ixrt_in,jxrt_in) :: SFCHEADSUBRT_in
    real, dimension(ixrt_in,jxrt_in) :: soxrt_in,soyrt_in
    real, dimension(ixrt_in,jxrt_in,nsoil) :: SMCRT_in
    real, dimension(ixrt_in,jxrt_in) :: LATVAL_in,LONVAL_in

    real, dimension(ixrt,jxrt) :: QSUBRT,ZWATTABLRT,SUB_RESID
    real, dimension(ixrt,jxrt) :: q_sfcflx_x,q_sfcflx_y
    real, dimension(ixrt,jxrt) :: QSTRMVOLRT
    real, dimension(ixrt,jxrt) :: SFCHEADSUBRT
    real, dimension(ixrt,jxrt) :: soxrt,soyrt
    real, dimension(ixrt,jxrt,nsoil) :: SMCRT
    real, dimension(ixrt,jxrt,9) :: dist_g 
    real, dimension(ixrt,jxrt) :: LATVAL,LONVAL
    integer i



    write(6,*) "mpp_output_GW output file: ",trim(wrfsi_finegrid_flnm)
    call flush(6)

    call write_IO_rt_real(LATVAL_in,LATVAL)
    call write_IO_rt_real(LONVAL_in,LONVAL)
    call write_IO_rt_real(QSUBRT_in,QSUBRT)
   

    call write_IO_rt_real(ZWATTABLRT_in,ZWATTABLRT)


    call write_IO_rt_real(SUB_RESID_in,SUB_RESID)


    call write_IO_rt_real(QSTRMVOLRT_in,QSTRMVOLRT)



    call write_IO_rt_real(SFCHEADSUBRT_in,SFCHEADSUBRT)
    call write_IO_rt_real(soxrt_in,soxrt)




    call write_IO_rt_real(soyrt_in,soyrt)
    call write_IO_rt_real(q_sfcflx_x_in,q_sfcflx_x)
    call write_IO_rt_real(q_sfcflx_y_in,q_sfcflx_y)




    do i = 1, NSOIL
         call write_IO_rt_real(SMCRT_in(:,:,i),SMCRT(:,:,i))
    end do
  
    do i = 1, 9    
         call write_IO_rt_real(dist(:,:,i),dist_g(:,:,i))
    end do




    if(my_id.eq.IO_id) then
       call output_rt(igrid, split_output_count, ixrt, jxrt, nsoil, &
          startdate, olddate, QSUBRT,ZWATTABLRT,SMCRT,SUB_RESID,       &
          q_sfcflx_x,q_sfcflx_y,soxrt,soyrt,QSTRMVOLRT,SFCHEADSUBRT, &
          wrfsi_finegrid_flnm,DT,SLDPTH,latval,lonval,dist_g,HIRES_OUT)
    end if

    write(6,*) "return from mpp_output_GW"
    call flush(6)
  end subroutine mpp_output_gw
#endif
! BF end mpp gw output section

 subroutine read_seq_forcing( &
       indir,range,olddate,startdate,hgrid,DOY, &
       ix,jx,forc_typ,snow_assim,  & 
       T2,q2x,u,v,pres,xlong,short,prcp1,&
       weasd,snodep,dt,k,igrid,acrain )
! This subrouting is going to read different forcing.
   implicit none
#include <netcdf.inc>
   ! in variable
   character(len=*) :: range,olddate,hgrid,indir,startdate, DOY
   character(len=256) :: filename
   integer :: ix,jx,forc_typ,k,snow_assim  ! k is time loop
   real,dimension(ix,jx):: T2,q2x,u,v,pres,xlong,short,prcp1,&
          prcpnew,weasd,snodep,acrain,prcp2,prcp_old
   real ::  dt
   ! tmp variable
   character(len=256) :: inflnm, inflnm2, product
   integer  :: i,j,mmflag,igrid,ierr_flg
   real,dimension(ix,jx):: lai,fpar
   character(len=4) nwxst_t

     if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid
     else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
     endif

!!!DJG... Call READFORC_(variable) Subroutine for forcing data...

!!!DJG HRLDAS Forcing...
   if(FORC_TYP.eq.1) then
!!Create forcing data filename...
      if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!minute forcing             olddate(15:16)//".LDASIN_DOMAIN"//hgrid
             ".LDASIN_DOMAIN"//hgrid
      else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
      endif
      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
   end if


!!!DJG RUC (1-KM) Forcing...
     if(FORC_TYP.eq.2) then
!!Create forcing data filename...
        inflnm = trim(indir)//"/"//&
             "wrfout_d0"//hgrid//"_"//&
             olddate(1:4)//"-"//olddate(6:7)//"-"//olddate(9:10)//&
             "_"//olddate(12:13)//":00:00"
        print *, "WRF/RUC forcing data...",inflnm
       CALL READFORC_RUC(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCPnew)
!       print *, "returned from READFORC_RUC..."
        PRCP1=(PRCPnew-ACRAIN)/dt   !Adjustment to convert accum to rate...(mm/s)
!       print *, T2,Q2X,U,PRES,XLONG,SHORT,PRCP1
        print *, ACRAIN(232,319),PRCPnew(232,319),PRCP1(232,319)
     end if



!!!DJG COWS Forcing...
     if(FORC_TYP.eq.3) then
!!Create forcing data filename...
     if (len_trim(range) == 0) then
      inflnm = trim(indir)//"/"//&
             startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid
!        "MET_LIS_CRO_2D_SANTEE_LU_1KM."//&
!        startdate(1:4)//DOY//startdate(12:13)//&
!        ".156hrfcst.radar"
     else
     endif
!     print *, "Reading forcing data..."
     CALL READFORC_COWS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,   &
          PRES,XLONG,SHORT,PRCP1,K)
!          print *, T2,Q2X,U,PRES,XLONG,SHORT,PRCP1

!       IF (K.GT.0.AND.K.LT.10) THEN
!         PRCP1 = 10.0/3600.0            ! units mm/s
!          PRCP1 = 0.254/3600.0
!       ELSE
!         PRCP1 = 0.
!       END IF

      endif



!!!DJG CONSTant, idealized forcing...
     if(FORC_TYP.eq.4) then
! Impose a fixed diurnal cycle...
! assumes K=1 is 12z (Ks or ~ sunrise)
! First Precip...
!       IF (K.GE.5 .and. K.LE. 10) THEN
!        PRCP1 =40./3600.0      !units mm/s
!       ELSEIF (K.GE.50 .and. K.LE. 60) THEN
!        PRCP1 =0./3600.0      !units mm/s
!       ELSE
!         PRCP1 = 0.
!       END IF
         PRCP1 = 0.
!       PRCP1 =10./3600.0      !units mm/s

! Other Met. Vars...
       T2=290.0 + 3.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
       Q2X = 0.01
       U = 1.0
       V = 1.0
       PRES = 100000.0
       XLONG=400.0 + 25.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
       SHORT=450.0 + 450.0*(cos((2*3.1416*K/24.0)-12.0*2*3.1416/24.0))
    end if



!!!DJG  Idealized Met. w/ Specified Precip. Forcing Data...
    if(FORC_TYP.eq.5) then

       T2=3000.0 + 3.0*(cos((2*3.1416*K/8.0)-12.0*2*3.1416/8.0))
       Q2X = 0.01
       U = 1.0
       PRES = 100000.0
       XLONG=400.0 + 25.0*(cos((2*3.1416*K/8.0)-12.0*2*3.1416/8.0))
       SHORT=500.0 + 450.0*(cos((2*3.1416*K/8.0)-12.0*2*3.1416/8.0))

!Get spec. precip....
!!!VIP, dimensions of grid are currently hardwired in input subroutine!!!
       product = "trmm"
       inflnm = trim(indir)//"/"//"sat_domain1.nc"
       PRCP1 = 0.
       CALL READFORC_NAMPCP(inflnm,IX,JX,   &
          PRCP1,K,product)
!Convert pcp grid to units of mm/s...
       PRCP1=PRCP1/(3.0*3600.0)     !3hrly precip product

    end if



!!!DJG  NARR Met. w/ Specified Precip. Forcing Data...
    if(FORC_TYP.eq.6) then !NARR Met. w/ Specified Precip.

!Check to make sure if Noah time step is 3 hrs as is NARR...

        print *, MOD(K*INT(DT),10800)
        PRCP_old = PRCP1

     if(K.eq.1.OR.(MOD((K-1)*INT(DT),10800)).eq.0) then   !if/then 3 hr check
!!Create forcing data filename...
      if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid
!        startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
!        ".48hrfcst.ncf"
      else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
      endif
        print *, "Reading NARR data...",inflnm
      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
!       PRCP1=PRCP1/(3.0*3600.0)     !NARR 3hrly precip product in mm
       PRCP1=PRCP1     !NAM model data in mm/s
    end if    !3 hr check


!Get spec. precip....
! NAM Remote sensing...
!!!VIP, dimensions of grid are currently hardwired in input subroutine!!!
!       product = "trmm"
!        print *, "Reading in Nesbitt R.S. data...reading from  nc file...",product !Use for NAME Nesbitt...
!       inflnm = trim(indir)//"/"//"sat_domain1.nc"
!!       inflnm = trim(indir)//"/"//"sat_domain2.nc"
!       PRCP1 = 0.
!       CALL READFORC_NAMPCP(inflnm,IX,JX,   &
!          PRCP2,K,product)
!       ierr_flg = 0
!       mmflag = 0
!!Convert pcp grid to units of mm/s...
!       PRCP1=PRCP1/(3.0*3600.0)     !3hrly precip product

!Read from filelist (NAME HE...,others)...
!        print *, "Reading in precip data...reading from filelist..." !Use for NAME HE...
!        if (K.eq.1) then
!          open(unit=93,file="filelist.txt",form="formatted",status="old")
!        end if
!        read (93,*) filename
!        inflnm = trim(indir)//"/"//trim(filename)


!Front Range MDV Radar...
        print *, "Reading in MDV radar data......"

!         inflnm = "/ptmp/weiyu/rt_2008/radar_obs/"//&
!             inflnm = "/d3/hydrolab/HRLDAS_forcing/FRNG_research/20080809/"//&
!              olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!              olddate(15:16)//"_radar.nc"
!              olddate(15:16)//"_chill.nc"

!        inflnm = "/d2/hydrolab/HRLDAS/forcing/FRNG/Big_Thomp_04/"//&
!       inflnm = "/d2/hydrolab/HRLDAS/forcing/FRNG/RT_2008/radar_obs/"//&
!             inflnm = "/d3/hydrolab/HRLDAS_forcing/FRNG_research/20080809/"//&
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//"_"//olddate(12:13)//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!             olddate(15:16)//"00_Pcp60min.nc"
!             olddate(15:16)//"00_Pcp30min.nc"
!             olddate(15:16)//"00_30min.nc"
             olddate(15:16)//"00_Pcp5min.nc"
!              olddate(15:16)//"_chill.nc"

!         inflnm = "/d2/hydrolab/HRLDAS/forcing/COWS/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//"_"//olddate(12:13)//&
!             olddate(15:16)//"00_Pcp5min.nc"
!              olddate(15:16)//"00_5.nc"

!         inflnm = ""     ! use this for NAM frxst runs with 30 min time-step
!


!        if (K.le.6) then   ! use for 30min nowcast...
!          if (K.eq.1) then
!             open(unit=94,file="start_file.txt",form="formatted",status="replace")
!!             inflnm2 = "/d2/hydrolab/HRLDAS/forcing/FRNG/RT_2008/radar_obs/"//&
!             inflnm2 = "/d3/hydrolab/HRLDAS_forcing/FRNG_research/"//&
!             olddate(1:4)//olddate(6:7)//olddate(9:10)//"_"//olddate(12:13)//&
!             olddate(15:16)//"00_"
!             write (94,*) inflnm  !fix part of input filename...
!             close(94)
!             nwxst_t = "5"! calc minutes from timestep and convert to char...
!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!          end if
!          if (K.eq.2) then
!             nwxst_t = "10" ! calc minutes from timestep and convert to char...
!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!             read (94,*) inflnm2
!             close(94)
!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!          end if
!          if (K.eq.3) then
!             nwxst_t = "15" ! calc minutes from timestep and convert to char...
!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!             read (94,*) inflnm
!             close(94)
!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!          end if
!          if (K.eq.4) then
!             nwxst_t = "20" ! calc minutes from timestep and convert to char...
!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!             read (94,*) inflnm
!             close(94)
!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!          end if
!          if (K.eq.5) then
!             nwxst_t = "25" ! calc minutes from timestep and convert to char...
!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!             read (94,*) inflnm
!             close(94)
!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!          end if
!          if (K.eq.6) then
!             nwxst_t = "30" ! calc minutes from timestep and convert to char...
!             open(unit=94,file="start_file.txt",form="formatted",status="old")
!             read (94,*) inflnm
!             close(94)
!             inflnm = trim(inflnm2)//trim(nwxst_t)//".nc"
!          end if
!        else
!          print *, "END OF 30min NOWCAST...STOPPING!"
!          stop
!          inflnm = ""     ! use this for NAM frxst runs with 30 min time-step
!        end if

!             olddate(1:4)//olddate(6:7)//olddate(9:10)//"_"//olddate(12:13)//&
!             olddate(15:16)//"00_Pcp30minMerge.nc"
        
        print *, "Opening MDV file...",inflnm
       CALL READFORC_MDV(inflnm,IX,JX,   &
          PRCP2,mmflag,ierr_flg)

!If radar or spec. data is ok use if not, skip to original NARR data...
      IF (ierr_flg.eq.0) then   ! use spec. precip
         PRCP1=PRCP2   !assumes PRCP2 is in mm/s
!Convert units if necessary
        IF (mmflag.eq.0) then    !Convert pcp grid to units of mm/s...
          PRCP1=PRCP2/DT     !convert from mm to mm/s 
        END IF  ! Endif mmflag
      ELSE   ! either stop or default to original forcing data...
        print *,"Current RADAR precip data not found !!! Using previous available file..."
        PRCP1 = PRCP_old
!        stop
      END IF  ! Endif ierr_flg

! Loop through data to screen for plausible values
       do i=1,ix
         do j=1,jx
           if (PRCP1(i,j).lt.0.) PRCP1(i,j)=0.0
           if (PRCP1(i,j).gt.0.0555) PRCP1(i,j)=0.0555  !set max pcp intens = 200 mm/h
!          PRCP1(i,j) = 0.
!          PRCP1(i,j) = 0.02   !override w/ const. precip for gw testing only...
         end do
       end do

!        if (K.eq.1) then  ! quick dump for site specific precip...
!          open(unit=94,file="Christman_accumpcp.txt",form="formatted",status="new")
!        end if
!        print *, "Christman Pcp...",PRCP1(130,223)
!        write (94,*) PRCP1(130,223)
!        call flush(94)

        
    end if  !NARR Met. w/ Specified Precip.



!!!DJG  NARR Met. w/ NARR Precip. Forcing Data...
    if(FORC_TYP.eq.7) then  !NARR Met. w/ NARR Precip.
!      write (*,*) "Reading NARR Forcing..."

!!Create forcing data filename...
      if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid
      else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
      endif
      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
!      write (*,*) "NARR Forcing read..."
      PRCP1=PRCP1/(3.0*3600.0)  ! convert to mm/s which is what HRLDAS expects?    
!      PRCP1=PRCP1  !Cedrics data already in mm/s (kg m^2/s)

    end if  !NARR Met. w/ NARR Precip.






!!!DJG  NLDAS Met. w/ NLDAS Precip. Forcing Data...
    if(FORC_TYP.eq.8) then  !NLDAS Met. w/ NLDAS Precip.
!      write (*,*) "Reading NLDAS Forcing..."

!!Create forcing data filename...
      if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!Use this for minute forcing...             olddate(15:16)//".LDASIN_DOMAIN"//hgrid
             ".LDASIN_DOMAIN"//hgrid
      else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
      endif
      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
!      write (*,*) "NLDAS Forcing read..."
      PRCP1=PRCP1/(1.0*3600.0)  ! convert hourly NLDAS total pcp to mm/s which is what HRLDAS expects

    end if  !NLDAS Met. w/ NLDAS Precip.







!!!DJG  NARR Met. w/ DMIP Precip. & Temp. Forcing Data...
    if(FORC_TYP.eq.9) then  ! If/Then for DMIP forcing data...

!Check to make sure if Noah time step is 3 hrs as is NARR...
!        print *, MOD(K*INT(DT),10800)

     if(K.eq.1.OR.(MOD((K-1)*INT(DT),10800)).eq.0) then   !if/then 3 hr check
        print *, "Reading NARR data..."
!!Create forcing data filename...
      if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid
!        startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
!        ".48hrfcst.ncf"
      else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
      endif
      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
          PRCP1=PRCP1/(3.0*3600.0)  ! convert to mm/s which is what HRLDAS expects    
    end if    !3 hr check

!Get DMIP Precip...
!       inflnm = "/d3/gochis/HRLDAS/forcing/DMIP_II/PRECIP_HRAP/precip_finished"//"/"//&
       inflnm = "/d2/hydrolab/HRLDAS/forcing/DMIP_II_AmerR/PRECIP_HRAP"//"/"//&
           "proj.xmrg"//&
           olddate(6:7)//olddate(9:10)//olddate(1:4)//olddate(12:13)//&
           "z.asc"
        PRCP1 = 0.
        CALL READFORC_DMIP(inflnm,IX,JX,PRCP1)
          PRCP1 = PRCP1 / 100.0    ! Convert from native hundreths of mm to mm
!       IF (K.LT.34) THEN
!        PRCP1 = 5.0/3600.0            ! units mm/s
!!       ELSE
!!         PRCP1 = 0.
!       END IF

!Get DMIP Temp...
!       inflnm = "/d3/gochis/HRLDAS/forcing/DMIP_II/TEMP_HRAP/tair_finished"//"/"//&
       inflnm = "/d2/hydrolab/HRLDAS/forcing/DMIP_II_AmerR/TEMP_HRAP"//"/"//&
           "proj.tair"//&
           olddate(6:7)//olddate(9:10)//olddate(1:4)//olddate(12:13)//&
           "z.asc"
        CALL READFORC_DMIP(inflnm,IX,JX,T2)
          T2 = (5./9.)*(T2-32.0) + 273.15         !Convert from deg F to deg K

    end if  !End if for DMIP forcing data...


!!!DJG HRLDAS Forcing with minute resolution...(NOTE: precip must be in mm/s!!!)
   if(FORC_TYP.eq.11) then
!!Create forcing data filename...
      if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             olddate(15:16)//".LDASIN_DOMAIN"//hgrid
      else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
      endif
      CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)
   end if



! : add reading forcing precipitation data
!       ywinflnm = "/ptmp/weiyu/hrldas/v2/st4"//"/"//&
!            olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
!            ".LDASIN_DOMAIN2"
!       call read_stage4(ywinflnm,IX,JX,PRCP1)
!end yw

!!!DJG Check for snow data assimilation...
    if (SNOW_ASSIM.eq.1) then
! Every 24 hours, update the snow field from analyses.
     if ( OLDDATE(12:13) == "00") then
        CALL READSNOW_HRLDAS(inflnm,IX,JX,OLDDATE,WEASD,SNODEP)
     endif
!     close(iunit)
   end if


 end subroutine read_seq_forcing


 subroutine read_chan_forcing( &
       indir,range,olddate,startdate,hgrid,DOY, &
       ixrt,jxrt,QSTRMVOLRT_ACC,QINFLOWBASE,QSUBRT)
! This subrouting is going to read channel forcing for
!  channel only simulations (ie when CHANRTSWCRT = 2)

   implicit none
#include <netcdf.inc>
   ! in variable
   character(len=*) :: range,olddate,hgrid,indir,startdate, DOY
   character(len=256) :: filename
   integer :: ixrt,jxrt
   real,dimension(ixrt,jxrt):: QSTRMVOLRT_ACC,QINFLOWBASE,QSUBRT
   ! tmp variable
   character(len=256) :: inflnm, product
   integer  :: i,j,mmflag
   character(len=256) :: units
   integer :: ierr
   integer :: ncid


!DJG Create filename...
     if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             olddate(15:16)//".RTOUT_DOMAIN"//hgrid
        print *, "Channel forcing file...",inflnm

     else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".RTOUT_DOMAIN"//hgrid//"."//trim(range)
     end if

!DJG Open NetCDF file...
    ierr = nf_open(inflnm, NF_NOWRITE, ncid)
    if (ierr /= 0) then
       write(*,'("READFORC_chan Problem opening netcdf file: ''", A, "''")') trim(inflnm)
       stop
    endif

!DJG read data...
    call get_2d_netcdf("QSTRMVOLRT",  ncid, QSTRMVOLRT_ACC, units, ixrt, jxrt, .TRUE., ierr)
!DJG TBC    call get_2d_netcdf("T2D", ncid, t,     units, ixrt, jxrt, .TRUE., ierr)
!DJG TBC    call get_2d_netcdf("T2D", ncid, t,     units, ixrt, jxrt, .TRUE., ierr)

    ierr = nf_close(ncid)

 end subroutine read_chan_forcing




      subroutine get2d_int(var_name,out_buff,ix,jx,fileName)
          implicit none
#include <netcdf.inc>
          integer :: iret,varid,ncid,ix,jx
          integer out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"aaa failed to open the netcdf file: ",trim(fileName)
            stop 9999
          endif
          iret = nf_inq_varid(ncid,trim(var_name),  varid)
          if(iret .ne. 0) then
            print*,"failed to read the variabe: ",trim(var_name)
            print*,"failed to read the netcdf file: ",trim(fileName)
          endif
          iret = nf_get_var_int(ncid, varid, out_buff)
          iret = nf_close(ncid)
         return
      end subroutine get2d_int

#ifdef MPP_LAND
       SUBROUTINE MPP_READ_ROUTEDIM(g_IXRT,g_JXRT, IXRT,JXRT, &
            route_chan_f,route_link_f, &
            route_direction_f, route_lake_f, NLINKS, NLAKES, &
            CH_NETLNK, channel_option, wrfsi_finegrid_flnm)


         USE module_mpp_land

         implicit none
#include <netcdf.inc>
        INTEGER                                      :: channel_option
        INTEGER                                      :: g_IXRT,g_JXRT
        INTEGER, INTENT(INOUT)                       :: NLINKS, NLAKES
        INTEGER, INTENT(IN)                          :: IXRT,JXRT
        INTEGER                                      :: CHNID,cnt
        INTEGER, DIMENSION(IXRT,JXRT)                :: CH_NETRT   !- binary channel mask
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETLNK  !- each node gets unique id
        INTEGER, DIMENSION(g_IXRT,g_JXRT) :: g_CH_NETLNK  ! temp array
        INTEGER, DIMENSION(IXRT,JXRT)                :: DIRECTION  !- flow direction
        INTEGER, DIMENSION(IXRT,JXRT)                :: LAKE_MSKRT
        REAL, DIMENSION(IXRT,JXRT)                   :: LAT, LON


        CHARACTER(len=256)       :: route_chan_f, route_link_f,route_direction_f,route_lake_f
        CHARACTER(len=256)       :: wrfsi_finegrid_flnm
!       CHARACTER(len=*)       :: wrfsi_finegrid_flnm

        if(my_id .eq. IO_id) then
          CALL READ_ROUTEDIM(g_IXRT, g_JXRT, route_chan_f, route_link_f, &
              route_direction_f, route_lake_f, NLINKS, NLAKES, &
              g_CH_NETLNK, channel_option,wrfsi_finegrid_flnm)
        endif
        

        call mpp_land_bcast_int1(NLAKES)
        call mpp_land_bcast_int1(NLINKS)


        call decompose_RT_int(g_CH_NETLNK,CH_NETLNK,g_IXRT,g_JXRT,ixrt,jxrt)
       

        return 
        end SUBROUTINE MPP_READ_ROUTEDIM

    subroutine mpp_readland_hrldas(wrfsi_static_flnm,&
          ix,jx,land_cat,soil_cat,& 
          vegtyp,soltyp,terrain,latitude,longitude,&
          global_nx,global_ny,SOLVEG_INITSWC)
#ifdef MPP_LAND
       use module_mpp_land
#endif
    implicit none
#include <netcdf.inc>
    character(len=*),          intent(in)  :: wrfsi_static_flnm
    integer,                   intent(in)  :: ix, jx, land_cat, soil_cat, &
              global_nx,global_ny,SOLVEG_INITSWC
    integer, dimension(ix,jx), intent(out) :: vegtyp, soltyp
    real,    dimension(ix,jx), intent(out) :: terrain, latitude, longitude
    real, dimension(global_nx,global_ny) ::g_terrain, g_latitude, g_longitude
    integer, dimension(global_nx,global_ny) :: g_vegtyp, g_soltyp

    character(len=256) :: units
    integer :: ierr
    integer :: ncid,varid
    real, dimension(ix,jx) :: xdum
    integer flag ! flag = 1 from wrfsi, flag =2 from WPS.
     if(my_id.eq.IO_id) then
        CALL READLAND_HRLDAS(wrfsi_static_flnm,global_nx,  &
               global_ny,LAND_CAT,SOIL_CAT,      &
               g_VEGTYP,g_SOLTYP,g_TERRAIN,g_LATITUDE,g_LONGITUDE, SOLVEG_INITSWC)
     end if
  ! distribute the data to computation node.
     call mpp_land_bcast_int1(LAND_CAT)
     call mpp_land_bcast_int1(SOIL_CAT)
     call decompose_data_int(g_VEGTYP,VEGTYP)
     call decompose_data_int(g_SOLTYP,SOLTYP)
     call decompose_data_real(g_TERRAIN,TERRAIN)
     call decompose_data_real(g_LATITUDE,LATITUDE)
     call decompose_data_real(g_LONGITUDE,LONGITUDE)
      return 
      end subroutine mpp_readland_hrldas

      SUBROUTINE MPP_READ_ROUTING(IXRT,JXRT,ELRT,  &
              CH_NETRT,LKSATFAC,route_topo_f,    &
            route_chan_f, wrfsi_finegrid_flnm,g_IXRT,g_JXRT, &
            OVROUGHRTFAC,RETDEPRTFAC)

#ifdef MPP_LAND
       use module_mpp_land
#endif


        implicit none
#include <netcdf.inc>
        INTEGER, INTENT(IN)                          :: IXRT,JXRT,g_IXRT,g_JXRT
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT)    :: ELRT,LKSATFAC
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT)    :: OVROUGHRTFAC,RETDEPRTFAC
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT

        REAL, DIMENSION(g_IXRT,g_JXRT)               :: g1_ELRT
        INTEGER,DIMENSION(g_IXRT,g_JXRT)             :: g1_CH_NETRT
        REAL, DIMENSION(g_IXRT,g_JXRT)               :: g1_LKSATFAC
        REAL, DIMENSION(g_IXRT,g_JXRT)               :: g1_OVROUGHRTFAC
        REAL, DIMENSION(g_IXRT,g_JXRT)               :: g1_RETDEPRTFAC

        CHARACTER(len=256)                           :: route_topo_f,route_chan_f,wrfsi_finegrid_flnm

        if(my_id .eq. IO_id)  then
          CALL READ_ROUTING_old(g_IXRT,g_JXRT,g1_ELRT,g1_CH_NETRT,g1_LKSATFAC,&
              route_topo_f, route_chan_f,wrfsi_finegrid_flnm,g1_OVROUGHRTFAC,&
               g1_RETDEPRTFAC)
        endif

        call decompose_RT_real(g1_ELRT,ELRT,g_IXRT,g_JXRT,IXRT,JXRT)
        call decompose_RT_int(g1_CH_NETRT,CH_NETRT,g_IXRT,g_JXRT,IXRT,JXRT)
        call decompose_RT_real(g1_LKSATFAC,LKSATFAC,g_IXRT,g_JXRT,IXRT,JXRT)
        call decompose_RT_real(g1_RETDEPRTFAC,RETDEPRTFAC,g_IXRT,g_JXRT,IXRT,JXRT)
        call decompose_RT_real(g1_OVROUGHRTFAC,OVROUGHRTFAC,g_IXRT,g_JXRT,IXRT,JXRT)

       return
       end SUBROUTINE MPP_READ_ROUTING

      subroutine MPP_READSNOW_HRLDAS(flnm,ix,jx,OLDDATE,weasd,snodep,&
                 global_nX, global_ny)
#ifdef MPP_LAND
       use module_mpp_land
#endif
        implicit none
#include <netcdf.inc>

        character(len=*),                   intent(in)  :: flnm,OLDDATE
        integer,  intent(in)  :: ix, global_nx,global_ny
        integer,                            intent(in)  :: jx
        real,             dimension(ix,jx), intent(out) :: weasd
        real,             dimension(ix,jx), intent(out) :: snodep

        real,dimension(global_nX, global_ny):: g_weasd, g_snodep
    
        character(len=256) :: units
        integer :: ierr
        integer :: ncid,i,j

        if(my_id .eq. IO_id) then
          CALL READSNOW_HRLDAS(trim(flnm),global_nX, global_ny,OLDDATE,g_WEASD,g_SNODEP)
       endif
       call decompose_data_real(g_WEASD,WEASD)
       call decompose_data_real(g_SNODEP,SNODEP)

        return 
        end  subroutine MPP_READSNOW_HRLDAS

      subroutine MPP_DEEPGW_HRLDAS(ix,jx,in_SMCMAX,&
                 global_nX, global_ny,nsoil,out_SMC,out_SH2OX)
#ifdef MPP_LAND
       use module_mpp_land
#endif
        implicit none
#include <netcdf.inc>

        integer,  intent(in)  :: ix,global_nx,global_ny
        integer,  intent(in)  :: jx,nsoil
        real,             dimension(ix,jx), intent(in) :: in_smcmax
        real,             dimension(ix,jx,nsoil), intent(out) :: out_smc,out_sh2ox

        real,dimension(global_nX, global_ny,nsoil):: g_smc, g_sh2ox
        real,dimension(global_nX, global_ny):: g_smcmax
        integer   :: i,j,k
       

          call write_IO_real(in_smcmax,g_smcmax)  ! get global grid of smcmax

          write (*,*) "In deep GW...", nsoil

!loop to overwrite soils to saturation...
        do i=1,global_nx
         do j=1,global_ny
            g_smc(i,j,1:NSOIL) = g_smcmax(i,j)
            g_sh2ox(i,j,1:NSOIL) = g_smcmax(i,j)
         end do 
        end do 

!decompose global grid to parallel tiles...
       do k=1,nsoil
        call decompose_data_real(g_smc(:,:,k),out_smc(:,:,k))
        call decompose_data_real(g_sh2ox(:,:,k),out_sh2ox(:,:,k))
       end do

        return 
        end  subroutine MPP_DEEPGW_HRLDAS


       SUBROUTINE MPP_READ_CHROUTING(IXRT,JXRT,ELRT,CH_NETRT, LAKE_MSKRT, &
            FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
            NLAKES, MUSK, MUSX, QLINK, CHANLEN, MannN, So, ChSSlp, Bw, &
            HRZAREA, LAKEMAXH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
            ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, &
            route_link_f, &
            route_lake_f, route_direction_f, route_order_f, &
            CHANRTSWCRT,dist, ZELEV, LAKENODE, CH_NETLNK, &
            CHANXI, CHANYJ, CHLAT, CHLON,  &
            channel_option,FORC_TYP,LATVAL,&
            LONVAL,STRMFRXSTPTS,wrfsi_finegrid_flnm,g_ixrt,g_jxrt)
#ifdef MPP_LAND
       use module_mpp_land
#endif
        implicit none
#include <netcdf.inc>
        INTEGER, INTENT(IN)                          :: IXRT,JXRT,g_IXRT,g_JXRT
!yw        INTEGER, INTENT(IN)                       :: CHANRTSWCRT, NLINKS, NLAKES
        INTEGER                                      :: CHANRTSWCRT, NLINKS, NLAKES
        INTEGER                                      :: I,J,channel_option
        INTEGER                                      :: FORC_TYP
        REAL, DIMENSION(g_IXRT,g_JXRT) :: g1_LATVAL, g1_LONVAL
        CHARACTER(len=28)                            :: dir

!----DJG,DNY New variables for channel and lake routing
        CHARACTER(len=155)	 :: header
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: FROM_NODE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ZELEV
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHLAT,CHLON

        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TYPEL
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: TO_NODE,ORDER
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: STRMFRXSTPTS

        INTEGER, INTENT(INOUT)                       :: MAXORDER
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MUSK, MUSX !muskingum
        REAL, INTENT(INOUT),  DIMENSION(NLINKS,2)    :: QLINK  !channel flow
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: CHANLEN   !channel length
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: MannN, So !mannings N
        INTEGER, INTENT(INOUT),  DIMENSION(NLINKS)   :: LAKENODE  ! identifies which nodes pour into which lakes
        REAL, INTENT(IN)                             :: dist(ixrt,jxrt,9)


!-- store the location x,y location of the channel element
         INTEGER, INTENT(INOUT), DIMENSION(NLINKS)   :: CHANXI, CHANYJ

!--reservoir/lake attributes
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: HRZAREA
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: LAKEMAXH
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: WEIRC
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: WEIRL
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEC
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEA
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: ORIFICEE
        REAL, INTENT(INOUT),  DIMENSION(NLINKS)      :: LATLAKE,LONLAKE,ELEVLAKE
        REAL, INTENT(INOUT), DIMENSION(NLINKS)       :: ChSSlp, Bw

        CHARACTER(len=256)                           :: route_link_f
        CHARACTER(len=256)                           :: route_lake_f
        CHARACTER(len=256)                           :: route_direction_f
        CHARACTER(len=256)                           :: route_order_f
        CHARACTER(len=256)                           :: wrfsi_finegrid_flnm
        CHARACTER(len=256)                           :: var_name

        INTEGER                                      :: tmp, cnt, ncid
        real                                         :: gc,n

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT)    :: CH_NETLNK
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)       :: ELRT
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: CH_NETRT
        INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT)    :: latval,lonval
        real      g1_elrt(g_ixrt,g_jxrt), g_dist(g_ixrt,g_jxrt,9)
        integer   g1_ch_netrt(g_ixrt,g_jxrt)
        INTEGER, DIMENSION(g_IXRT,g_JXRT)            :: g1_LAKE_MSKRT, g1_ch_netlnk
        integer :: k


        call write_IO_rt_real(elrt,g1_elrt)
        call write_IO_rt_int(ch_netrt,g1_ch_netrt)
        call write_IO_rt_int(CH_NETLNK,g1_CH_NETLNK)
!       if(dist(1,1,1) .ne. -999) then
            do k = 1, 9
                call write_IO_rt_real(dist(:,:,k),g_dist(:,:,k))
            end do
!       endif

        if(my_id .eq. IO_id) then
          CALL READ_CHROUTING(g_IXRT,g_JXRT,g1_ELRT,g1_CH_NETRT, g1_LAKE_MSKRT, &
               FROM_NODE, TO_NODE, TYPEL, ORDER, MAXORDER, NLINKS, &
               NLAKES, MUSK, MUSX, QLINK,CHANLEN, MannN, So, ChSSlp, Bw, &
               HRZAREA, LAKEMAXH, WEIRC, WEIRL, ORIFICEC, &
              ORIFICEA, ORIFICEE, LATLAKE, LONLAKE, ELEVLAKE, &
               route_link_f,route_lake_f, &
               route_direction_f, route_order_f, &
               CHANRTSWCRT,g_dist, ZELEV, LAKENODE, g1_CH_NETLNK, CHANXI, CHANYJ, &
               CHLAT, CHLON, channel_option, FORC_TYP,g1_latval,g1_lonval,&
               STRMFRXSTPTS,wrfsi_finegrid_flnm)
        endif

        call decompose_RT_int(g1_LAKE_MSKRT,LAKE_MSKRT,g_IXRT,G_JXRT,ixrt,jxrt)
        call decompose_RT_real(g1_latval,latval,g_IXRT,G_JXRT,ixrt,jxrt)
        call decompose_RT_real(g1_lonval,lonval,g_IXRT,G_JXRT,ixrt,jxrt)

!        do k = 1, 9
!           call decompose_RT_real(g_dist(:,:,k),dist(:,:,k),g_IXRT,G_JXRT,ixrt,jxrt)
!        end do

        return 
        end SUBROUTINE MPP_READ_CHROUTING

 subroutine mpp_read_forcing( &
       indir,range,olddate,startdate,hgrid,DOY, &
       ix,jx,forc_typ,snow_assim,  & 
       T2,q2x,u,v,pres,xlong,short,prcp1,&
       weasd,snodep,dt,k,g_ix,g_jx,igrid,acrain)
! This subrouting is going to read different forcing.
#ifdef MPP_LAND
       use module_mpp_land
#endif


   implicit none
#include <netcdf.inc>
   ! in variable
   character(len=*) :: range,olddate,hgrid,indir,startdate, DOY
   character(len=256) :: filename
   integer :: ix,jx,forc_typ,k,snow_assim,igrid  ! k is time loop
   real,dimension(ix,jx):: T2,q2x,u,v,pres,xlong,short,prcp1,&
          prcpnew,weasd,snodep,acrain
   real ::  dt
   ! tmp variable
   character(len=256) :: inflnm, product
   integer  :: i,j,mmflag, g_ix,g_jx
   real,dimension(ix,jx):: lai,fpar
   real,dimension(g_ix,g_jx):: g_T2,g_Q2X,g_U,g_V,g_XLONG, &
             g_SHORT,g_PRCP1,g_PRES,g_weasd,g_snodep,g_acrain
   integer flag 
   
   if(forc_typ .eq. 2) then
     call write_io_real(acrain,g_acrain)
   endif

   if(forc_typ .eq. 6 .OR. forc_typ .eq. 11) then  ! DJG (6-Spec. precip., 11-DESWAT)

     call write_io_real(T2,g_T2)
     call write_io_real(Q2X,g_Q2X)
     call write_io_real(U,g_U)
     call write_io_real(V,g_V)
     call write_io_real(XLONG,g_XLONG)
     call write_io_real(SHORT,g_SHORT)
     call write_io_real(PRCP1,g_PRCP1)
     call write_io_real(PRES,g_PRES)


   end if

   if(my_id .eq. IO_id) then
      call read_seq_forcing( &
        indir,range,olddate,startdate,hgrid,DOY, &
        global_nx,global_ny,forc_typ,snow_assim,  &
        g_T2,g_q2x,g_u,g_v,g_pres,g_xlong,g_short,g_prcp1,&
        g_weasd,g_snodep,dt,k,igrid,g_acrain )
     write(6,*) "finish read forcing,startdate,olddate ",startdate,olddate
   end if
!yw    call mpp_land_bcast_int1(SLPCATS)
     call decompose_data_real(g_T2,T2)
     call decompose_data_real(g_Q2X,Q2X)
     call decompose_data_real(g_U,U)
     call decompose_data_real(g_V,V)
     call decompose_data_real(g_XLONG,XLONG)
     call decompose_data_real(g_SHORT,SHORT)
     call decompose_data_real(g_PRCP1,PRCP1)
     call decompose_data_real(g_PRES,PRES)

     flag = -1
     if( my_id.eq.IO_id) then
       if(OLDDATE(12:16) == "00:00") flag = 99
     end if
     call mpp_land_bcast_int1(flag)
     if(flag .eq. 99 .and. snow_assim .eq. 1) then
        call decompose_data_real(g_weasd,weasd)
        call decompose_data_real(g_snodep,snodep)
     endif
   return
   end subroutine mpp_read_forcing
#endif

