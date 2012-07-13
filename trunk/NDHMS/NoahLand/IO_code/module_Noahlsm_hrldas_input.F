module module_sf_Noahlsm_hrldas_input
  use module_sf_Noahlsm_param_init
  use module_date_utilities
  implicit none

#include <netcdf.inc>

  logical, parameter :: FATAL = .TRUE.
  logical, parameter :: NOT_FATAL = .FALSE.



contains

  subroutine read_hrldas_hdrinfo(wrfsi_static_flnm, ix, jx, land_cat, soil_cat)
    ! Simply return the dimensions of the grid.
    implicit none
    character(len=*),          intent(in)  :: wrfsi_static_flnm
    integer, intent(out) :: ix, jx, land_cat, soil_cat ! dimensions

    integer :: iret, ncid, dimid

    ! Open the NetCDF file.
    write(*,'("wrfsi_static_flnm: ''", A, "''")') trim(wrfsi_static_flnm)
    iret = nf_open(wrfsi_static_flnm, NF_WRITE, ncid)
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

  subroutine readland_hrldas(wrfsi_static_flnm,ix,jx,land_cat,soil_cat,vegtyp,soltyp,terrain,latitude,longitude)
    implicit none
    character(len=*),          intent(in)  :: wrfsi_static_flnm
    integer,                   intent(in)  :: ix, jx, land_cat, soil_cat
    integer, dimension(ix,jx), intent(out) :: vegtyp, soltyp
    real,    dimension(ix,jx), intent(out) :: terrain, latitude, longitude

    character(len=256) :: units
    integer :: ierr
    integer :: ncid
    real, dimension(ix,jx) :: xdum


    ! Open the NetCDF file.
    write(*,'("wrfsi_static_flnm: ''", A, "''")') trim(wrfsi_static_flnm)
    ierr = nf_open(wrfsi_static_flnm, NF_WRITE, ncid)
    if (ierr /= 0) then
       write(*,'("Problem opening wrfsi_static file: ''", A, "''")') trim(wrfsi_static_flnm)
       stop
    endif

    ! Get Latitude (lat)
    call get_2d_netcdf("XLAT", ncid, latitude,  units, ix, jx, FATAL, ierr)
    print*, 'latitude(1,1) = ', latitude(1,1)

    ! Get Longitude (lon)
    call get_2d_netcdf("XLONG", ncid, longitude, units, ix, jx, FATAL, ierr)
    print*, 'longitude(1,1) = ', longitude(1,1)

    ! Get Terrain (avg)
    call get_2d_netcdf("HGT", ncid, terrain,   units, ix, jx, FATAL, ierr)
    print*, 'terrain(1,1) = ', terrain(1,1)

    ! Get Dominant Land Use categories (use)
    call get_landuse_netcdf(ncid, xdum ,   units, ix, jx, land_cat)
    vegtyp = nint(xdum)
    print*, 'vegtyp(1,1) = ', vegtyp(1,1)

    ! Get Dominant Soil Type categories in the top layer (stl)
    call get_soilcat_netcdf(ncid, xdum ,   units, ix, jx, soil_cat)
    soltyp = nint(xdum)
    print*, 'soltyp(1,1) = ', soltyp(1,1)

    ! Close the NetCDF file
    ierr = nf_close(ncid)
    if (ierr /= 0) stop "MODULE_NOAHLSM_HRLDAS_INPUT:  READLAND_HRLDAS:  NF_CLOSE"

    ! Make sure vegtyp and soltyp are consistent when it comes to water points,
    ! by setting soil category to water when vegetation category is water, and
    ! vice-versa.
    where (vegtyp == 16) soltyp = 14
    where (soltyp == 14) vegtyp = 16

  end subroutine readland_hrldas

  subroutine get_landuse_netcdf(ncid, array, units, idim, jdim, ldim)
    implicit none
    integer, intent(in) :: ncid
    integer, intent(in) :: idim, jdim, ldim
    real, dimension(idim,jdim), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    real, dimension(idim,jdim,ldim) :: xtmp
    integer, dimension(1) :: mp
    integer :: i, j
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
       enddo
    enddo

  end subroutine get_landuse_netcdf

  subroutine get_soilcat_netcdf(ncid, array, units, idim, jdim, ldim)
    implicit none
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

  end subroutine get_soilcat_netcdf

  subroutine get_2d_netcdf(name, ncid, array, units, idim, jdim, &
       fatal_if_error, ierr)
    implicit none
    character(len=*), intent(in) :: name
    integer, intent(in) :: ncid
    integer, intent(in) :: idim, jdim
    real, dimension(idim,jdim), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    ! FATAL_IF_ERROR:  an input code value:
    !      .TRUE. if an error in reading the data should stop the program.
    !      Otherwise the, IERR error flag is set, but the program continues.
    logical, intent(in) :: fatal_if_error 
    integer, intent(out) :: ierr

    units = ""

    iret = nf_inq_varid(ncid,  name,  varid)
    if (iret /= 0) then
       if (FATAL_IF_ERROR) then
          print*, 'name = "', trim(name)//'"'
          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf_inq_varid"
       else
          ierr = iret
          return
       endif
    endif

    iret = nf_get_att_text(ncid, varid, "units", units)
    if (iret /= 0) then
       if (FATAL_IF_ERROR) then
          print*, 'name = "', trim(name)//'"'
          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf_get_att_text"
       else
          ierr = iret
          return
       endif
    endif

    iret = nf_get_var_real(ncid, varid, array)
    if (iret /= 0) then
       if (FATAL_IF_ERROR) then
          print*, 'name = "', trim(name)//'"'
          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf_get_var_real"
       else
          ierr = iret
          return
       endif
    endif
!  print*, name//' = ', array(1,1), array(1,jdim), array(idim,jdim), array(idim,1)

    ierr = 0;
  end subroutine get_2d_netcdf


  subroutine readinit_hrldas(netcdf_flnm, ix, jx, nsoil, target_date, &
       smc, stc, sh2o, cmc, t1, weasd, snodep)
    implicit none
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
    integer :: ix_read, jx_read

    integer :: ierr, ncid, ierr_snodep
    integer :: idx

    logical :: found_canwat, found_skintemp, found_weasd, found_stemp, found_smois

    ! Open the NetCDF file.
    write(*,'("netcdf_flnm: ''", A, "''")') trim(netcdf_flnm)
    ierr = nf_open(netcdf_flnm, NF_WRITE, ncid)
    if (ierr /= 0) then
       write(*,'("Problem opening netcdf file: ''", A, "''")') &
            trim(netcdf_flnm)
       stop
    endif

    call get_2d_netcdf("CANWAT",     ncid, cmc,     units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("SKINTEMP",   ncid, t1,      units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("WEASD",      ncid, weasd,   units, ix, jx, FATAL, ierr)
    if (trim(units) == "m") then
       ! No conversion necessary
    else if (trim(units) == "mm") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else
       print*, 'units = "'//trim(units)//'"'
       stop "Unrecognized units on WEASD"
    endif
    call get_2d_netcdf("SNODEP",     ncid, weasd,   units, ix, jx, NOT_FATAL, ierr_snodep)
    call get_2d_netcdf("STEMP_1",    ncid, stc(:,:,1), units,  ix, jx, FATAL, ierr)
    call get_2d_netcdf("STEMP_2",    ncid, stc(:,:,2), units,  ix, jx, FATAL, ierr)
    call get_2d_netcdf("STEMP_3",    ncid, stc(:,:,3), units,  ix, jx, FATAL, ierr)
    call get_2d_netcdf("STEMP_4",    ncid, stc(:,:,4), units,  ix, jx, FATAL, ierr)
    call get_2d_netcdf("SMOIS_1",    ncid, smc(:,:,1), units,  ix, jx, FATAL, ierr)
    call get_2d_netcdf("SMOIS_2",    ncid, smc(:,:,2), units,  ix, jx, FATAL, ierr)
    call get_2d_netcdf("SMOIS_3",    ncid, smc(:,:,3), units,  ix, jx, FATAL, ierr)
    call get_2d_netcdf("SMOIS_4",    ncid, smc(:,:,4), units,  ix, jx, FATAL, ierr)

    if (ierr_snodep /= 0) then
       ! Quick assumption regarding snow depth.
       snodep = weasd * 10.
    endif

    sh2o = smc

    ierr = nf_close(ncid)
  end subroutine readinit_hrldas

  subroutine READVEG_HRLDAS(flnm, ix, jx, target_date, vegtyp, fpar, lai)

    implicit none
    character(len=*),                intent(in)  :: flnm
    integer,                         intent(in)  :: ix
    integer,                         intent(in)  :: jx
    character(len=*),                intent(in)  :: target_date
    integer, dimension(ix,jx),       intent(in)  :: vegtyp
    real,    dimension(ix,jx),       intent(out) :: fpar
    real,    dimension(ix,jx),       intent(out) :: lai

    character(len=8)   :: name
    character(len=256) :: units
    integer :: ierr
    integer :: ix_read, jx_read

    integer :: ierr_fpar
    integer :: ierr_lai

    integer :: i, j
    integer :: iret, ncid

    ! Open the NetCDF file.
    write(*,'("flnm: ''", A, "''")') trim(flnm)
    iret = nf_open(flnm, NF_WRITE, ncid)
    if (iret /= 0) then
       write(*,'("Problem opening netcdf file: ''", A, "''")') trim(flnm)
       stop
    endif

    call get_2d_netcdf("VEGFRA",     ncid, fpar,     units, ix, jx, NOT_FATAL, ierr_fpar)
    call get_2d_netcdf("LAI",        ncid, lai,      units, ix, jx, NOT_FATAL, ierr_lai)

    if (ierr_fpar == 0) then
       fpar = fpar * 1.E-2
    else if (ierr_fpar /= 0) then
       ! Get it from tables
       print*,' READVEG_HRLDAS:  VEGFRA not found.  Initializing FPAR from table SHDTBL.'
       do i = 1, ix
          do j = 1, jx
             fpar(i,j) = shdtbl(vegtyp(i,j))
          enddo
       enddo
    endif
    if (ierr_lai /= 0) then
       ! Get it from tables
       print*,' READVEG_HRLDAS:  LAI not found.  Initializing LAI from table LAITBL.'
       do i = 1, ix
          do j = 1, jx
             lai(i,j) = laitbl(vegtyp(i,j))
          enddo
       enddo
    endif

    iret = nf_close(ncid)
  end subroutine READVEG_HRLDAS

  subroutine READFORC_HRLDAS(flnm,ix,jx,target_date, t,q,u,v,p,lw,sw,pcp,lai,fpar)
    implicit none

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
    write(*,'("flnm: ''", A, "''")') trim(flnm)
    ierr = nf_open(flnm, NF_WRITE, ncid)
    if (ierr /= 0) then
       write(*,'("Problem opening netcdf file: ''", A, "''")') trim(flnm)
       stop
    endif

    call get_2d_netcdf("T2D",     ncid, t,     units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("Q2D",     ncid, q,     units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("U2D",     ncid, u,     units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("V2D",     ncid, v,     units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("PSFC",    ncid, p,     units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("LWDOWN",  ncid, lw,    units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("SWDOWN",  ncid, sw,    units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("RAINRATE",ncid, pcp,   units, ix, jx, FATAL, ierr)
    call get_2d_netcdf("VEGFRA",  ncid, fpar,  units, ix, jx, NOT_FATAL, ierr)
    if (ierr == 0) then
       fpar = fpar * 1.E-2
    endif
    call get_2d_netcdf("LAI",     ncid, lai,   units, ix, jx, NOT_FATAL, ierr)

  end subroutine READFORC_HRLDAS


  subroutine output_hrldas(ounit, split_output_count, hgrid, range, ix, jx, &
       nsoil, sldpth, startdate, date, &
       vegtyp, soltyp, skintemp, cmc, stc, smc, sh2o, soilmx, sfcrnoff, ugdrnoff, &
       intrflow, sfcevp, etakin, canevp, edirx, ettx, albedx, weasd, acrain, acsnom, &
       esnow2d, drip2d, dewfall, snodep, vegfra, hx, qfx, sw)
    implicit none

    integer,                                  intent(in) :: ounit
    integer,                                  intent(in) :: split_output_count
    character,                                intent(in) :: hgrid
    character(len=*),                         intent(in) :: range
    integer,                                  intent(in) :: ix
    integer,                                  intent(in) :: jx
    integer,                                  intent(in) :: nsoil
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

    integer :: output_count = 0
    integer, save :: ncid

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: iret
    character(len=256) :: output_flnm
    character(len=19)  :: date19

    real, dimension(ix,jx) :: xdum

    print*, 'output date:  ', date

    if (output_count == 0) then
       write(output_flnm, '(A10,".ldasout")') date(1:4)//date(6:7)//date(9:10)//date(12:13)
       print*, 'output_flnm = "'//trim(output_flnm)//'"'
       iret = nf_create(trim(output_flnm), 0, ncid)
       if (iret /= 0) stop "Problem nf_create"

       iret = nf_def_dim(ncid, "idim", ix, dimid_ix)
!KWM       print*, 'dimid_ix = ', dimid_ix
       iret = nf_def_dim(ncid, "jdim", jx, dimid_jx)
!KWM       print*, 'dimid_jx = ', dimid_jx
       iret = nf_def_dim(ncid, "Times", NF_UNLIMITED, dimid_times)
!KWM       print*, 'dimid_times = ', dimid_times
       iret = nf_def_dim(ncid, "DateLen", 19, dimid_datelen)
!KWM       print*, 'dimid_datelen = ', dimid_datelen

       iret = nf_put_att_real(ncid, NF_GLOBAL, "missing_value", NF_FLOAT, 1, -1.E33)

       date19(1:19) = "0000-00-00_00:00:00"
       date19(1:len_trim(startdate)) = startdate
       iret = nf_put_att_text(ncid, NF_GLOBAL, "Start_Date", 19, date19)

       do n = 1, nsoil
          iret = nf_def_var(ncid,  "SOIL_T_"//char(n+ichar("0")),  NF_FLOAT, 3, (/1,2,3/), varid)
       enddo
       do n = 1, nsoil
          iret = nf_def_var(ncid,  "SOIL_M_"//char(n+ichar("0")),  NF_FLOAT, 3, (/1,2,3/), varid)
       enddo

       iret = nf_def_var(ncid,  "HFX",    NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_def_var(ncid,  "QFX",    NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_def_var(ncid,  "SW",     NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_def_var(ncid,  "ALBEDX", NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_def_var(ncid,  "VEGFRA", NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_def_var(ncid,  "SNODEP", NF_FLOAT, 3, (/dimid_ix,dimid_jx,dimid_times/), varid)
       iret = nf_def_var(ncid,  "Date",   NF_CHAR,  2, (/dimid_datelen,dimid_times/),     varid)

       iret = nf_enddef(ncid)
    endif
    output_count = output_count + 1

    do n = 1, nsoil
       xdum = stc(:,:,n)
       where (vegtyp(:,:) == 16) xdum = -1.E33
       iret = nf_inq_varid(ncid,  "SOIL_T_"//char(n+ichar("0")), varid)
       iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)
    enddo

    do n = 1, nsoil
       xdum = smc(:,:,n)
       where (vegtyp(:,:) == 16) xdum = -1.E33
       iret = nf_inq_varid(ncid,  "SOIL_M_"//char(n+ichar("0")), varid)
       iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)
    enddo

    xdum = hx
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "HFX", varid)
    iret = nf_put_vara_real(ncid, varid, (/1,1,output_count/), (/ix,jx,1/), xdum)

    xdum = qfx
    where (vegtyp(:,:) == 16) xdum = -1.E33
    iret = nf_inq_varid(ncid,  "QFX", varid)
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

    date19(1:19) = "0000-00-00_00:00:00"
    date19(1:len_trim(date)) = date
    iret = nf_inq_varid(ncid, "Date", varid)
    iret = nf_put_vara_text(ncid, varid, (/1,output_count/), (/19,1/), date)

    iret = nf_sync(ncid)

    if (output_count == split_output_count) then
       output_count = 0
       iret = nf_close(ncid)
    endif

  end subroutine output_hrldas

end module module_sf_Noahlsm_hrldas_input
