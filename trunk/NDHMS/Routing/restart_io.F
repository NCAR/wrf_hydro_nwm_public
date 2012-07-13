module module_restart_IO

#ifdef MPP_LAND
   USE module_mpp_land
#endif
#include <netcdf.inc>
#include "lsm_wrf_field.inc"
#include "rt_include.inc"
#include "namelist.inc"
#include "diag_field_include.inc"

Contains

   subroutine RESTART_OUT_nc(outFile,did)

   use module_namelist, only: nlst_rt, nlst_lsm
   use module_diag_field, only: lsm_diag
   use module_RT_data, only: rt_domain
   use module_LSM_WRF_data, only: lsm

   implicit none

   integer did

   character(len=*) outFile

    integer :: ncid,irt, dimid_ix, dimid_jx,  &
             dimid_ixrt, dimid_jxrt, varid, &
             dimid_links, dimid_basns, dimid_soil
    integer :: iret


#ifdef MPP_LAND
     if(IO_id.eq.my_id) &
#endif
       iret = nf_create(trim(outFile), 0, ncid)
#ifdef MPP_LAND
       call mpp_land_bcast_int1(iret)
#endif

       if (iret /= 0) stop "Problem nf_create"

#ifdef MPP_LAND
     if(IO_id.eq.my_id) then
#endif
! define dimension for variables 
          iret = nf_def_dim(ncid, "depth", nlst_lsm(did)%nsoil, dimid_soil)  !-- 3-d soils
   
#ifdef MPP_LAND
          iret = nf_def_dim(ncid, "ix", global_nx, dimid_ix)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iy", global_ny, dimid_jx)
          iret = nf_def_dim(ncid, "ixrt", global_rt_nx , dimid_ixrt)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iyrt", global_rt_ny, dimid_jxrt)
#else
          iret = nf_def_dim(ncid, "ix", rt_domain(did)%ix, dimid_ix)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iy", rt_domain(did)%jx, dimid_jx)
          iret = nf_def_dim(ncid, "ixrt", rt_domain(did)%ixrt , dimid_ixrt)  !-- make a decimated grid
          iret = nf_def_dim(ncid, "iyrt", rt_domain(did)%jxrt, dimid_jxrt)
#endif

          iret = nf_def_dim(ncid, "links", rt_domain(did)%nlinks, dimid_links)
          iret = nf_def_dim(ncid, "basns", rt_domain(did)%numbasns, dimid_basns)
    
!define variables
          iret = nf_def_var(ncid,"stc",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
          iret = nf_def_var(ncid,"smc",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
          iret = nf_def_var(ncid,"sh2ox",NF_FLOAT,3,(/dimid_ix,dimid_jx,dimid_soil/),varid)
          iret = nf_def_var(ncid,"snodep",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"weasd",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"t1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"cmc",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcmax1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcref1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"smcwlt1",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
!          iret = nf_def_var(ncid,"fpar",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"infxsrt",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)

! diagnostic variable
       if(nlst_lsm(did)%rst_diag .eq. 1) then
          iret = nf_def_var(ncid,"soilmx",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"runoff1x",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"runoff2x",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"runoff3x",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"etax",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"ecx",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"edirx",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"ettx",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"acrain",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"acsnom",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"esnow2d",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"drip2d",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"dewfall",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"fpar",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"qfx",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"prcp_out",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
          iret = nf_def_var(ncid,"etpndx",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
       endif

          if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1) then
            iret = nf_def_var(ncid,"sfcheadrt",NF_FLOAT,2,(/dimid_ix,dimid_jx/),varid)
            iret = nf_def_var(ncid,"infxswgt",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            iret = nf_def_var(ncid,"sh2owgt",NF_FLOAT,3,(/dimid_ixrt,dimid_jxrt,dimid_soil/),varid)
            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
              iret = nf_def_var(ncid,"hlink",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"cvol",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"resht",NF_FLOAT,1,(/dimid_links/),varid)
                 iret = nf_def_var(ncid,"qlakeo",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"cvol",NF_FLOAT,1,(/dimid_links/),varid)
              iret = nf_def_var(ncid,"lake_inflort",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
              iret = nf_def_var(ncid,"qstrmvolrt",NF_FLOAT,2,(/dimid_ixrt,dimid_jxrt/),varid)
            end if
            if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
              iret = nf_def_var(ncid,"z_gwsubbas",NF_FLOAT,1,(/dimid_basns/),varid)
            end if
          end if  

          iret = nf_enddef(ncid)

#ifdef MPP_LAND
    endif
#endif
        call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_lsm(did)%nsoil,rt_domain(did)%stc,"stc")
        call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_lsm(did)%nsoil,rt_domain(did)%smc,"smc")
        call w_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_lsm(did)%nsoil,rt_domain(did)%sh2ox,"sh2ox")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm(did)%snodep,"snodep")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm(did)%weasd,"weasd"  )
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm(did)%t1,"t1"  )
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm(did)%CMC,"cmc" ) 
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCMAX1,"smcmax1") 
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCREF1,"smcref1" )
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCWLT1,"smcwlt1"  ) 
!        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%FPAR,"fpar"  ) 
        call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%INFXSRT,"infxsrt"  ) 

! output diagnostic variables
       if(nlst_lsm(did)%rst_diag .eq. 1) then
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%soilmx,"soilmx")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%runoff1x,"runoff1x")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%runoff2x,"runoff2x")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%runoff3x,"runoff3x")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%etax,"etax")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%ecx,"ecx")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%edirx,"edirx")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%ettx,"ettx")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%acrain,"acrain")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%acsnom,"acsnom")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%esnow2d,"esnow2d")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%drip2d,"drip2d")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%dewfall,"dewfall")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%fpar,"fpar")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%qfx,"qfx")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%prcp_out,"prcp_out")
        call w_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%etpndx,"etpndx")
      endif

        if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1) then
            call w_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SFCHEADRT,"sfcheadrt" )
            call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%INFXSWGT, "infxswgt" )
            call w_rst_rt_nc3(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,nlst_lsm(did)%nsoil,rt_domain(did)%SH2OWGT, "sh2owgt" )
            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%HLINK,"hlink" &
#ifdef MPP_LAND
                 ,rt_domain(did)%nlinks_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )



              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%cvol,"cvol" &
#ifdef MPP_LAND
                 ,rt_domain(did)%nlinks_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )

              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%resht,"resht" &
#ifdef MPP_LAND
                 ,rt_domain(did)%lake_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )

              call w_rst_crt_nc1(ncid,rt_domain(did)%nlinks,rt_domain(did)%qlakeo,"qlakeo" &
#ifdef MPP_LAND
                 ,rt_domain(did)%lake_index, rt_domain(did)%mpp_nlinks  &
#endif
                  )


              call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%LAKE_INFLORT,"lake_inflort")
              call w_rst_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QSTRMVOLRT, "qstrmvolrt" )

            end if

            if(nlst_rt(did)%GWBASESWCRT.EQ.1) then
              call w_rst_crt_nc1g(ncid,rt_domain(did)%numbasns,rt_domain(did)%z_gwsubbas,"z_gwsubbas" )
            end if
        end if  

#ifdef MPP_LAND
        if(IO_id.eq.my_id) &
#endif
        iret = nf_close(ncid)

        return
        end subroutine RESTART_OUT_nc

        subroutine w_rst_rt_nc2(ncid,ix,jx,inVar,varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret
           character(len=*) varName
           real, dimension(ix,jx):: inVar
#ifdef MPP_LAND
           real, dimension(global_rt_nx, global_rt_ny):: varTmp
           call write_IO_rt_real(inVar,varTmp) 
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1,1/), (/global_rt_nx,global_rt_ny/),varTmp)
           endif
#else
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ix,jx/),inVar)
#endif
           
           return
        end subroutine w_rst_rt_nc2

        subroutine w_rst_rt_nc3(ncid,ix,jx,NSOIL,inVar, varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret, nsoil
           character(len=*) varName
           real,dimension(ix,jx,nsoil):: inVar
#ifdef MPP_LAND
           integer k
           real varTmp(global_rt_nx,global_rt_ny,nsoil)
           do k = 1, nsoil
              call write_IO_rt_real(inVar(:,:,k),varTmp(:,:,k)) 
           end do
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/global_rt_nx,global_rt_ny,nsoil/),varTmp)
           endif
#else
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/ix,jx,nsoil/),inVar)
#endif
           return
        end subroutine w_rst_rt_nc3

        subroutine w_rst_nc2(ncid,ix,jx,inVar,varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret
           character(len=*) varName
           real inVar(ix,jx)

#ifdef MPP_LAND
           real varTmp(global_nx,global_ny)
           call write_IO_real(inVar,varTmp) 
           if(my_id .eq. IO_id) then
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1,1/), (/global_nx,global_ny/),varTmp)
           endif
#else
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1/), (/ix,jx/),invar)
#endif
           
           return
        end subroutine w_rst_nc2

        subroutine w_rst_nc3(ncid,ix,jx,NSOIL,inVar, varName)
           implicit none
           integer:: ncid,ix,jx,varid , iret, nsoil
           character(len=*) varName
           real inVar(ix,jx,nsoil)
           integer k
#ifdef MPP_LAND
           real varTmp(global_nx,global_ny,nsoil)
           do k = 1, nsoil
              call write_IO_real(inVar(:,:,k),varTmp(:,:,k)) 
           end do
           if(my_id .eq. IO_id) then
             iret = nf_inq_varid(ncid,varName, varid)
             iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/global_nx,global_ny,nsoil/),varTmp)
           endif
#else
           iret = nf_inq_varid(ncid,varName, varid)
           iret = nf_put_vara_real(ncid, varid, (/1,1,1/), (/ix,jx,nsoil/),inVar)
#endif
           return
        end subroutine w_rst_nc3

        subroutine w_rst_crt_nc1(ncid,n,inVar,varName &
#ifdef MPP_LAND
                 ,index, mpp_n&
#endif
                  )
           implicit none
           integer:: ncid,n,varid , iret
           character(len=*) varName
           real inVar(n)
#ifdef MPP_LAND
           integer:: index(n),mpp_n
           call write_chanel_real(inVar,index,mpp_n,n)          
           if(my_id .eq. IO_id) then
#endif
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
#ifdef MPP_LAND
           endif
#endif
           return
        end subroutine w_rst_crt_nc1

        subroutine w_rst_crt_nc1g(ncid,n,inVar,varName)
           implicit none
           integer:: ncid,n,varid , iret
           character(len=*) varName
           real inVar(n)
#ifdef MPP_LAND
           if(my_id .eq. IO_id) then
#endif
              iret = nf_inq_varid(ncid,varName, varid)
              iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
#ifdef MPP_LAND
           endif
#endif
           return
        end subroutine w_rst_crt_nc1g

   subroutine RESTART_IN_NC(inFile,did)

   use module_namelist, only: nlst_rt, nlst_lsm
   use module_diag_field, only: lsm_diag
   use module_RT_data, only: rt_domain
   use module_LSM_WRF_data, only: lsm

   implicit none
   character(len=*) inFile
   integer  :: ierr, iret,ncid, did

    integer :: i, j

#ifdef MPP_LAND
     if(IO_id .eq. my_id) then
#endif
!open a netcdf file 
    iret = nf_open(trim(inFile), NF_NOWRITE, ncid)
#ifdef MPP_LAND
    endif
    call mpp_land_bcast_int1(iret)
#endif
    if (iret /= 0) then
       write(*,'("Problem opening file: ''", A, "''")') &
            trim(inFile)
       call land_finish()
    endif

        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_lsm(did)%nsoil,rt_domain(did)%stc,"stc")
        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_lsm(did)%nsoil,rt_domain(did)%smc,"smc")
        call read_rst_nc3(ncid,rt_domain(did)%ix,rt_domain(did)%jx,nlst_lsm(did)%nsoil,rt_domain(did)%sh2ox,"sh2ox")
        call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm(did)%snodep,"snodep")
        call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm(did)%weasd,"weasd")
        call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm(did)%t1,"t1")
        call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm(did)%CMC,"cmc")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCMAX1,"smcmax1")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCREF1,"smcref1")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SMCWLT1,"smcwlt1")
!        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SHORT,"short")
!       call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%FPAR,"fpar")
        call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%INFXSRT,"infxsrt")
        if(nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1) then
            call read_rst_nc2(ncid,rt_domain(did)%ix,rt_domain(did)%jx,rt_domain(did)%SFCHEADRT,"sfcheadrt")
            call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%INFXSWGT,"infxswgt")
            call read_rst_rt_nc3(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,nlst_lsm(did)%nsoil,rt_domain(did)%SH2OWGT,"sh2owgt")
!            call read_rst_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%ETPNDX,"etpndx")
            if(nlst_rt(did)%CHANRTSWCRT.EQ.1) then
              call read_rst_crt_nc(ncid,rt_domain(did)%HLINK,rt_domain(did)%NLINKS,"hlink")
              call read_rst_crt_nc(ncid,rt_domain(did)%CVOL,rt_domain(did)%NLINKS,"cvol")
              call read_rst_crt_nc(ncid,rt_domain(did)%RESHT,rt_domain(did)%nlinks,"resht")
              call read_rst_crt_nc(ncid,rt_domain(did)%QLAKEO,rt_domain(did)%nlinks,"qlakeo")
              call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%LAKE_INFLORT,"lake_inflort")
              call read_rt_nc2(ncid,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%QSTRMVOLRT,"qstrmvolrt")
            end if
            if(nlst_rt(did)%GWBASESWCRT.EQ.1.AND.nlst_rt(did)%GW_RESTART.NE.0) then
                 call read_rst_crt_nc(ncid,rt_domain(did)%z_gwsubbas,rt_domain(did)%numbasns,"z_gwsubbas")
            end if
        end if

          if(nlst_rt(did)%rstrt_swc.eq.1) then  !Switch for rest of restart accum vars...
            print *, "1 Resetting RESTART Accumulation Variables to 0...",nlst_rt(did)%rstrt_swc
            rt_domain(did)%INFXSRT=0.
            rt_domain(did)%LAKE_INFLORT=0.
            rt_domain(did)%QSTRMVOLRT=0.
          end if

       if(nlst_lsm(did)%rst_diag .eq. 1) then
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%soilmx,"soilmx")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%runoff1x,"runoff1x")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%runoff2x,"runoff2x")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%runoff3x,"runoff3x")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%etax,"etax")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%ecx,"ecx")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%edirx,"edirx")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%ettx,"ettx")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%acrain,"acrain")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%acsnom,"acsnom")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%esnow2d,"esnow2d")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%drip2d,"drip2d")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%dewfall,"dewfall")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%fpar,"fpar")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%qfx,"qfx")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%prcp_out,"prcp_out")
             call read_rst_nc2(ncid,lsm(did)%ix,lsm(did)%jx,lsm_diag(did)%etpndx,"etpndx")
          endif
      
#ifdef MPP_LAND
        if(my_id .eq. IO_id) &
#endif
        iret =  nf_close(ncid) 
        write(6,*) "end of RESTART_IN"
        call flush(6)
 
        return
        end subroutine RESTART_IN_nc

      subroutine read_rst_nc3(ncid,ix,jx,NSOIL,var,varStr)
         implicit none 
         integer ::  ix,jx,nsoil, ireg, ncid, varid, iret
         real,dimension(ix,jx,nsoil) ::  var
         character(len=*) :: varStr
#ifdef MPP_LAND
         real,dimension(global_nx,global_ny,nsoil) :: xtmp
         integer i

         if(my_id .eq. IO_id) & 
#endif
           iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif

         if (iret /= 0) then
            print*, 'variable not found: name = "', trim(varStr)//'"'
            return
         endif
         print*, "read restart variable ", varStr
#ifdef MPP_LAND
         if(my_id .eq. IO_id) & 
            iret = nf_get_var_real(ncid, varid, xtmp)

         do i = 1, nsoil
            call decompose_data_real(xtmp(:,:,i), var(:,:,i))
         end do
#else
            iret = nf_get_var_real(ncid, varid, var)
#endif

         return
      end subroutine read_rst_nc3

      subroutine read_rst_nc2(ncid,ix,jx,var,varStr)
         implicit none
         integer ::  ix,jx,ireg, ncid, varid, iret
         real,dimension(ix,jx) ::  var
         character(len=*) :: varStr
#ifdef MPP_LAND
         real,dimension(global_nx,global_ny) :: xtmp 
         if(my_id .eq. IO_id) & 
#endif
           iret = nf_inq_varid(ncid,  trim(varStr),  varid)

#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif

         if (iret /= 0) then
            print*, 'variable not found: name = "', trim(varStr)//'"'
            return
         endif
         print*, "read restart variable ", varStr
#ifdef MPP_LAND
         if(my_id .eq. IO_id) & 
            iret = nf_get_var_real(ncid, varid, xtmp)

         call decompose_data_real(xtmp, var)
#else
            iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rst_nc2

      subroutine read_rst_rt_nc3(ncid,ix,jx,NSOIL,var,varStr)
         implicit none
         integer ::  ix,jx,nsoil, ireg, ncid, varid, iret
         real,dimension(ix,jx,nsoil) ::  var
         character(len=*) :: varStr
#ifdef MPP_LAND
         real,dimension(global_rt_nx,global_rt_ny,nsoil) :: xtmp
         integer i
         if(my_id .eq. IO_id) & 
#endif
            iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
         if (iret /= 0) then
            print*, 'variable not found: name = "', trim(varStr)//'"'
            return
         endif
         print*, "read restart variable ", varStr
#ifdef MPP_LAND
         iret = nf_get_var_real(ncid, varid, xtmp)
         do i = 1, nsoil
            call decompose_RT_real(xtmp(:,:,i),var(:,:,i),global_rt_nx,global_rt_ny,ix,jx)
         end do
#else
         iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rst_rt_nc3

      subroutine read_rst_rt_nc2(ncid,ix,jx,var,varStr)
         implicit none
         integer ::  ix,jx,ireg, ncid, varid, iret
         real,dimension(ix,jx) ::  var
         character(len=*) :: varStr
#ifdef MPP_LAND
         real,dimension(global_rt_nx,global_rt_ny) :: xtmp 
#endif
         iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
         if (iret /= 0) then
            print*, 'variable not found: name = "', trim(varStr)//'"'
            return
         endif
         print*, "read restart variable ", varStr
#ifdef MPP_LAND
         if(my_id .eq. IO_id) &   
             iret = nf_get_var_real(ncid, varid, xtmp)
         call decompose_RT_real(xtmp,var,global_rt_nx,global_rt_ny,ix,jx)
#else
            iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rst_rt_nc2

      subroutine read_rt_nc2(ncid,ix,jx,var,varStr)
         implicit none
         integer ::  ix,jx, ncid, varid, iret
         real,dimension(ix,jx) ::  var
         character(len=*) :: varStr

#ifdef MPP_LAND
         real,dimension(global_rt_nx,global_rt_ny) :: xtmp
#endif
            iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
            if (iret /= 0) then
               print*, 'variable not found: name = "', trim(varStr)//'"'
               return
            endif
         print*, "read restart variable ", varStr
#ifdef MPP_LAND
         if(my_id .eq. IO_id) then
            iret = nf_get_var_real(ncid, varid, xtmp)
         endif
         call decompose_RT_real(xtmp,var,global_rt_nx,global_rt_ny,ix,jx)
#else
            iret = nf_get_var_real(ncid, varid, var)
#endif
         return
      end subroutine read_rt_nc2

      subroutine read_rst_crt_nc(ncid,var,n,varStr)
         implicit none
         integer ::  ireg, ncid, varid, n, iret
         real,dimension(n) ::  var
         character(len=*) :: varStr

#ifdef MPP_LAND
         if(my_id .eq. IO_id) & 
#endif
            iret = nf_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
            if (iret /= 0) then
               print*, 'variable not found: name = "', trim(varStr)//'"'
               return
            endif
         print*, "read restart variable ", varStr
#ifdef MPP_LAND
         if(my_id .eq. IO_id) then
#endif
            iret = nf_get_var_real(ncid, varid, var)
#ifdef MPP_LAND
         endif
         call mpp_land_bcast_real(n,var)
#endif
         return
      end subroutine read_rst_crt_nc 

      subroutine hrldas_out()
      end subroutine hrldas_out
end module module_restart_IO
