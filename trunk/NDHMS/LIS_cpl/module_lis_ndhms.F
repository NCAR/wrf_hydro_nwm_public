module module_lis_NDHMS
    use noah32_lsmMod, only : noah32_struc 
    use LIS_coreMod, only : LIS_rc, LIS_domain, LIS_masterproc
!    use LIS_historyMod, only: LIS_gather_gridded_output, lis_gather_tiled_vector_output

    contains

    subroutine lis_cpl_ndhms(n)

       use module_rt_data, only:  rt_domain
       use module_LSM_WRF_data, only:  lsm
       use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
       use module_mpp_land
       use module_namelist, only: nlst_lsm

        implicit none
        integer n, k, ix,jx

        integer ::  did, its,ite,jts,jte

        integer ntime, wrf_ix,wrf_jx
        real cpl_land_dt

!output flux and state variable

        integer :: i,j
        integer :: kstart


        kstart = 2 
        did = 1

            write(6,*) "yyyywww step 2"
            call flush(6)
        if(.not. RT_DOMAIN(did)%initialized) then

            write(cpl_outdate,'(I4,"-",I2,"-",I2,"_",I2,":",I2,":",I2)' )     &
                LIS_rc%yr, LIS_rc%mo, LIS_rc%da, LIS_rc%hr, LIS_rc%mn, LIS_rc%ss 
            ix = LIS_rc%lnr(n)
            jx = LIS_rc%lnc(n)
            cpl_land_dt = LIS_rc%ts
            write(6,*) "yyyywww step 3"
            call flush(6)
            kstart = 1 ! change to 2 if read restart file.

            write(6,*) "yyyywww step 4 before NDHMS_ini= "
            write(6,*) "its,ite,jts,jte"
            write(6,*)  its,ite,jts,jte

            call CPL_LAND_INIT(its,ite,jts,jte)


            write(6,*) "yyyywww step 4 before NDHMS_ini= "
            write(6,*) "its,ite,jts,jte"
            write(6,*)  its,ite,jts,jte


            call NDHMS_ini(ntime,did)

            write(6,*) "yyyywww step 5 my_id= ", my_id
            call flush(6)

            RT_DOMAIN(did)%initialized = .true.
            nlst_lsm(did)%dt = cpl_land_dt



            call lis2ndhms_2d(lsm(did)%CMC,noah32_struc(n)%noah(:)%cmc,n,ix,jx)
            call lis2ndhms_2d(lsm(did)%T1,noah32_struc(n)%noah(:)%t1,n,ix,jx)
            call lis2ndhms_2d(lsm(did)%weasd,noah32_struc(n)%noah(:)%sneqv,n,ix,jx)
            call lis2ndhms_2d(lsm(did)%snodep,noah32_struc(n)%noah(:)%snowh,n,ix,jx)

            RT_DOMAIN(did)%initialized = .true.
        endif

            ! get the initial data from LIS 
            do k = 1 , nlst_lsm(did)%nsoil
                call lis2ndhms_2d(RT_DOMAIN(did)%SMC,noah32_struc(n)%noah(:)%smc(k),n,ix,jx)
                call lis2ndhms_2d(RT_DOMAIN(did)%stc,noah32_struc(n)%noah(:)%stc(k),n,ix,jx)
                call lis2ndhms_2d(RT_DOMAIN(did)%SH2OX,noah32_struc(n)%noah(:)%sh2o(k),n,ix,jx)
            enddo

    
        call NDHMS_exe(did,ntime,kstart)

! add for update the lsm state variable.
     ! 3 d variable
        do k = 1 , nlst_lsm(did)%nsoil
             call ndhms2lis_2d(RT_DOMAIN(did)%SMC,noah32_struc(n)%noah(:)%smc(k),n,ix,jx)
             call ndhms2lis_2d(RT_DOMAIN(did)%stc,noah32_struc(n)%noah(:)%stc(k),n,ix,jx)
             call ndhms2lis_2d(RT_DOMAIN(did)%SH2OX,noah32_struc(n)%noah(:)%sh2o(k),n,ix,jx)
        end do

     return 
     end subroutine lis_cpl_ndhms

     subroutine lis2ndhms_2d(var,v1d,n,ix,jx)
        implicit none
        integer :: n, ix, jx, i, r,c
        real, dimension(ix,jx) :: var
        real, dimension(jx,ix) :: tmpVar
        real ::  v1d (ix*jx)

!        call LIS_gather_gridded_output(n, p1,tmpVar)
          do r=1,jx
             do c=1,ix                
                if(LIS_domain(n)%gindex(c,r).ne.-1) then
                   v1d(LIS_domain(n)%gindex(c,r)) = var(c,r)
                endif
             enddo
          enddo

     end subroutine lis2ndhms_2d        

     subroutine ndhms2lis_2d(var,v1d,n,ix,jx)
        implicit none
        integer n1d, n, ix,jx, i, r,c
        real, dimension(ix,jx) :: var
        real, dimension(jx,ix) :: tmpVar
        real ::  v1d (ix*jx)
        do i = 1, ix
           tmpVar(:,i) = Var(i,:)
        end do
          !call LIS_gather_tiled_vector_output(n,v1d,tmpVar)
          do r=1,jx
             do c=1,ix                
                if(LIS_domain(n)%gindex(c,r).ne.-1) then
                   var(c,r) = v1d(LIS_domain(n)%gindex(c,r))
                endif
             enddo
          enddo
         
      end subroutine  ndhms2lis_2d

end module module_lis_ndhms
