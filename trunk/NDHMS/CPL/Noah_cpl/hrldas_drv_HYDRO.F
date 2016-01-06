!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!  <brief list of changes to this source file>
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

!2345678
       subroutine hrldas_drv_HYDRO(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk, qsgw)
          use module_hrldas_HYDRO, only: hrldas_cpl_HYDRO
          implicit none
          integer:: ii,jj,kk
          real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
          real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain, qsgw

          call hrldas_cpl_HYDRO(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk, qsgw)  
       end subroutine hrldas_drv_HYDRO

       subroutine hrldas_drv_HYDRO_ini(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt,dt,olddate,zsoil)
          use module_hrldas_HYDRO, only: hrldas_cpl_HYDRO_ini, open_print_mpp
          implicit none
          integer:: ii,jj,kk, kt
          character(len=*) :: olddate
          real :: dt
          real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
          real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain
          real, dimension(kk) :: zsoil
             call hrldas_cpl_HYDRO_ini(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt,dt,olddate,zsoil)  
#ifdef NCEP_WCOSS
             call open_print_mpp(78)
#else
             call open_print_mpp(6)
#endif
       end subroutine hrldas_drv_HYDRO_ini

       subroutine HYDRO_forcing_drv (indir,forc_typ, snow_assim,olddate,     &
          ixs, ixe,jxs,jxe,                       &
          T2,Q2X,U,V,PRES,XLONG,SHORT,PRCP1,lai,fpar,snodep, kt,FORCING_TIMESTEP,prcp_old)

         use module_lsm_forcing, only: read_hydro_forcing, read_hydro_forcing_seq
         use module_namelist, only:nlst_rt
         implicit none
         integer did, ixs,ixe,jxs,jxe
         integer ix,jx, kt
         character(len=19) :: olddate
         character(len=*) :: indir
         real, dimension(ixs:ixe,jxs:jxe):: T2,Q2X,U,V,PRES,XLONG,SHORT,PRCP1, &
                 lai, fpar,snodep  ,prcp_old
         integer :: forc_typ, snow_assim, FORCING_TIMESTEP

         ix = ixe-ixs+1
         jx = jxe-jxs+1
         did = 1
!yw          call read_hydro_forcing( &
         call read_hydro_forcing_seq( &
            indir, olddate, &
            nlst_rt(did)%hgrid,&
            ix,jx,forc_typ,snow_assim,  &
            T2,q2x,u,v,pres,xlong,short,prcp1,&
            lai,fpar,snodep,FORCING_TIMESTEP*1.0,kt ,prcp_old)
       end subroutine HYDRO_forcing_drv

       subroutine get_greenfrac(inFile,greenfrac, idim, jdim, olddate)
          use module_namelist, only:nlst_rt
          implicit none
          character(len=*) :: olddate, inFile
          integer :: idim, jdim 
          real, dimension(idim,jdim):: greenfrac
          integer:: mm, dd, did
          integer :: months
          months = 12
          did = 1
          read(olddate(6:7),*) mm
          read(olddate(9:10),*) dd
          call get_greenfrac_netcdf(inFile,greenfrac,idim, jdim, months,mm,dd) 
       end subroutine get_greenfrac


      subroutine get_greenfrac_netcdf(fileName,array3, idim, jdim, ldim,mm,dd)
          implicit none
#         include "netcdf.inc"
          character(len=*) :: fileName
          integer, intent(in) :: mm,dd
          integer, intent(in) :: idim, jdim, ldim
          real, dimension(idim,jdim) :: array
          real, dimension(idim,jdim) :: array2
          real, dimension(idim,jdim) :: diff
          real, dimension(idim,jdim), intent(out) :: array3
          integer :: iret, varid, ncid
          real, dimension(idim,jdim,ldim) :: xtmp
          integer, dimension(1) :: mp
          integer :: i, j, mm2,daytot
          real :: ddfrac
          character(len=24), parameter :: name = "GREENFRAC"
      
!          units = "fraction"

          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if(iret /= 0) then
              write(6,*) "FATAL ERROR: failed to open file in get_greenfrac: ", trim(fileName)
              call hydro_stop("In hrldas_drv_HYDRO.F get_greenfrac_netcdf() - failed to open file") 
          endif 
            
          iret = nf_inq_varid(ncid,  trim(name),  varid)
          if (iret /= 0) then
             print*, 'name = "', trim(name)//'"'
             print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_greenfrac_netcdf:  nf_inq_varid"
             call hydro_stop("In hrldas_drv_HYDRO.F get_greenfrac_netcdf() - nf_inq_varid problem")
          endif
      
          iret = nf_get_var_real(ncid, varid, xtmp)
          if (iret /= 0) then

             print*, 'name = "', trim(name)//'"'
             print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_greenfrac_netcdf:  nf_get_var_real"
             call hydro_stop("In hrldas_drv_HYDRO.F get_greenfrac_netcdf() - nf_get_var_real problem")
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
      
#ifdef HYDRO_D
               print *,"DJG_DES Made it past netcdf read...month = ",mm,mm2,dd,daytot,ddfrac
#endif
      
          do i = 1, idim
             do j = 1, jdim
                array(i,j) = xtmp(i,j,mm)   !GREENFRAC in geogrid in units of fraction from month 1
                array2(i,j) = xtmp(i,j,mm2)   !GREENFRAC in geogrid in units of fraction from month 1
                diff(i,j) = array2(i,j) - array(i,j)
                array3(i,j) = array(i,j) + ddfrac * diff(i,j) 
             enddo
          enddo
          iret=nf_close(ncid)
      end subroutine get_greenfrac_netcdf


      subroutine HYDRO_HRLDAS_ini(fileName,ix,jx, nsoil,smc,stc,sh2ox, cmc, t1, weasd, &
            snodep,lai, fpar, vegtyp,SHDMIN,SHDMAX,FNDSNOWH)
      ! read the field from wrfinput or output file
          implicit none
#         include "netcdf.inc"
          character(len=*) fileName
          integer :: ix,jx , nsoil, i, j
          integer :: iret, ncid, ierr, varid
          real,dimension(ix,jx,nsoil):: smc,stc,sh2ox
          real,dimension(ix,jx):: cmc, t1, weasd, snodep, lai, fpar ,SHDMIN,SHDMAX
          integer, dimension(ix,jx) ::    vegtyp
          logical :: FNDSNOWH
!yw          real, dimension(50) :: shdtbl
!yw          data shdtbl /0.1,0.8,0.8,0.8,0.8,0.8,0.8,0.7,0.7,0.5,  &
!yw              0.8,0.7,0.95,0.7,0.8,0,0.6,0.6,0.1, &
!yw              0.6,0.6,0.6,0.3,0,0.5,0,0,           &
!yw              0,0,0,0,0,0,0,0,0,0,0,0,0,          & 
!yw              0,0,0,0,0,0,0,0,0,0/


          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if(iret /= 0) then
              write(6,*) "FATAL ERROR: failed to open file in HYDRO_HRLDAS_ini: ", trim(fileName)
              call hydro_stop('In hrldas_drv_HYDRO.F HYDRO_HRLDAS_ini() - failed to open file.')
          else
#ifdef HYDRO_D
              write(6,*) "initialization from the file  : ", trim(fileName)

#endif
          endif 

           iret = nf_inq_varid(ncid,"VEGFRA",  varid)
           if (iret == 0) then 
               iret = nf_get_var_real(ncid, varid, fpar)
               if(maxval(fpar) .gt. 10 .and. (maxval(fpar) .lt. 1000)) fpar = fpar/100.
           endif

           iret = nf_inq_varid(ncid,"SHDMIN",  varid)
           if (iret == 0) then 
               iret = nf_get_var_real(ncid, varid, SHDMIN)
               if(maxval(SHDMIN) .gt. 10 .and. (maxval(SHDMIN) .lt. 1000)) SHDMIN = SHDMIN / 100.
           endif
             

           iret = nf_inq_varid(ncid,"SHDMAX",  varid)
           if (iret == 0) then 
              iret = nf_get_var_real(ncid, varid, SHDMAX)
               if(maxval(SHDMAX) .gt. 10 .and. (maxval(SHDMAX) .lt. 1000)) SHDMAX = SHDMAX / 100.
           endif

!yw
!yw           do j = 1, jx
!yw              do i = 1, ix
!yw                  fpar(i,j) = shdtbl(vegtyp(i,j))
!yw              end do
!yw           end do
!yw


          iret = nf_inq_varid(ncid,"LAI",  varid)
          if (iret == 0) iret = nf_get_var_real(ncid, varid, lai)

          iret = nf_inq_varid(ncid,"SNOWH",  varid)
          if (iret == 0) then
#ifdef HYDRO_D
             print*, "read snowh for initialization ...."
#endif
             iret = nf_get_var_real(ncid, varid, snodep)
             FNDSNOWH = .true.
          else
             FNDSNOWH = .false.
          endif  

          iret = nf_inq_varid(ncid,"SNOW",  varid)
          if (iret == 0) then
#ifdef HYDRO_D
             print*, "read snow for initialization ...."
#endif
             iret = nf_get_var_real(ncid, varid, weasd)
             weasd = weasd * 1.0E-3    ! transfer the unit from mm to m
          endif  
         
         
!         where(snodep < weasd) snodep = weasd * 10
!            write(6,*) "SNOW(20,1)=",weasd(20,1) 
!            write(6,*) "SNOWH(20,1)=",snodep(20,1) 
          
          iret = nf_inq_varid(ncid,"TSK",  varid)
          if (iret == 0) iret = nf_get_var_real(ncid, varid, t1)

          iret = nf_inq_varid(ncid,"CANWAT",  varid)
          if (iret == 0) iret = nf_get_var_real(ncid, varid, cmc)

          iret = nf_inq_varid(ncid,"SMOIS",  varid)
          if (iret == 0) iret = nf_get_var_real(ncid, varid, smc)

          iret = nf_inq_varid(ncid,"TSLB",  varid)
          if (iret == 0) iret = nf_get_var_real(ncid, varid, stc)

          iret = nf_inq_varid(ncid,"SH2O",  varid)
          if (iret == 0) iret = nf_get_var_real(ncid, varid, sh2ox)

          iret=nf_close(ncid)

!yw    added for 
             where( abs(t1) .gt. 500) t1 = 282
             where( abs(stc) .gt. 500) stc = 282
             where( abs(SMC) .gt. 1) SMC = 0.25
             where(sh2ox == 0) sh2ox = smc
             where( abs(SH2OX) .gt. 500) SH2OX = 0.25


 
       end subroutine HYDRO_HRLDAS_ini

       subroutine getHydroNameList(varName,pVar)
          use module_hrldas_HYDRO, only: getNameList 
          implicit none
          integer ::  pVar
          character(len=*)  ::varName
          call getNameList(varName,pVar)
       end subroutine getHydroNameList
