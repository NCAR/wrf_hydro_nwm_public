module module_lis_NDHMS
    use noah271_lsmMod, only : noah271_struc 
!   use LIS_coreMod, only : LIS_rc, LIS_domain, LIS_masterproc, &
!         LIS_ews_ind, LIS_ewe_ind, LIS_nss_ind, LIS_nse_ind
    use LIS_coreMod
!    use LIS_historyMod, only: LIS_gather_gridded_output, lis_gather_tiled_vector_output

    contains

    subroutine lis_cpl_ndhms(n)
       

       use module_rt_data, only:  rt_domain
       use module_LSM_WRF_data, only:  lsm
       use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
       use module_mpp_land
       use module_namelist, only: nlst_rt

        implicit none
        integer n, k, ix,jx

        integer ::  did, its,ite,jts,jte, ierr

        integer ntime, wrf_ix,wrf_jx
        logical mpi_inited
        real cpl_land_dt

!output flux and state variable

        integer :: i,j


        did = 1

            write(6,*) "yyyywww step 2, n =", n
            write(6,*) "npesx, npesy",LIS_rc%npesx, LIS_rc%npesy
            call flush(6)

      if(.not. RT_DOMAIN(did)%initialized) then
        CALL mpi_initialized( mpi_inited, ierr )
        if ( .NOT. mpi_inited ) then
           call MPI_INIT( ierr )  ! stand alone land model.
        endif
        call MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr )
        call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )
      endif


            write(cpl_outdate,'(I4,"-",I2,"-",I2,"_",I2,":",I2,":",I2)' )     &
                LIS_rc%yr, LIS_rc%mo, LIS_rc%da, LIS_rc%hr, LIS_rc%mn, LIS_rc%ss 
            jx = LIS_rc%lnr(n)
            ix = LIS_rc%lnc(n)
            cpl_land_dt = LIS_rc%ts
            its = LIS_ews_ind(LIS_rc%nnest,my_id+1)
            ite = LIS_ewe_ind(LIS_rc%nnest,my_id+1)
            jts = LIS_nss_ind(LIS_rc%nnest,my_id+1)
            jte = LIS_nse_ind(LIS_rc%nnest,my_id+1)

            write(6,*) "LIS_ews_ind",LIS_ews_ind 
            write(6,*) "LIS_ewe_ind",LIS_ewe_ind 
            write(6,*) "LIS_nss_ind",LIS_nss_ind 
            write(6,*) "LIS_nse_ind",LIS_nse_ind 
        if(.not. RT_DOMAIN(did)%initialized) then
            
            write(6,*) "yyyywww step 3"
            write(6,*) "ix,jx, cpl_land_dt",ix,jx, cpl_land_dt
            call flush(6)

            write(6,*) "yyyywww step 4 before NDHMS_ini= "
            write(6,*) "its,ite,jts,jte", its,ite,jts,jte
            write(6,*) "noah271_struc(n)%nslay=", noah271_struc(n)%nslay
            write(6,*) "noah271_struc(n)%lyrthk=", noah271_struc(n)%lyrthk


            call CPL_LAND_INIT(its,ite,jts,jte)


            write(6,*) "yyyywww step 4 before NDHMS_ini= "
            write(6,*) "its,ite,jts,jte", its,ite,jts,jte


            call NDHMS_ini(ntime,did)

            write(6,*) "yyyywww step 5 my_id= ", my_id
            call flush(6)

            RT_DOMAIN(did)%initialized = .true.
            nlst_rt(did)%dt = cpl_land_dt

          write(6,*) "LIS_rc%gnc(n),LIS_rc%gnr(n),LIS_rc%glbnch_red(n)", &
                 LIS_rc%gnc(n),LIS_rc%gnr(n),LIS_rc%glbnch_red(n)

          write(6,*) "LIS_rc%nch =", LIS_rc%nch(n)
          write(6,*) "size(noah271_struc(n)%noah%cmc)" , size(noah271_struc(n)%noah%cmc)

          write(6,*) "LIS_nss_halo_ind,LIS_nse_halo_ind", LIS_nss_halo_ind,LIS_nse_halo_ind
          write(6,*) "LIS_ews_halo_ind,LIS_ewe_halo_ind",LIS_ews_halo_ind,LIS_ewe_halo_ind


!         call lis2ndhms_2d(lsm(did)%CMC,noah271_struc(n)%noah%cmc,size(noah271_struc(n)%noah%cmc), n,ix,jx)
!         call lis2ndhms_2d(lsm(did)%T1,noah271_struc(n)%noah%t1,size(noah271_struc(n)%noah%t1),n,ix,jx)
!         call lis2ndhms_2d(lsm(did)%weasd,noah271_struc(n)%noah%sneqv,size(noah271_struc(n)%noah%sneqv), n,ix,jx)
!         call lis2ndhms_2d(lsm(did)%snodep,noah271_struc(n)%noah%snowh,size(noah271_struc(n)%noah%snowh),n,ix,jx)

            RT_DOMAIN(did)%initialized = .true.
        endif


        write(6,*) "before call lis2ndhms_2d"
        write(6,*) "size(noah271_struc(n)%noah%smc(1)),n,ix,jx" , size(noah271_struc(n)%noah%smc(1)),n,ix,jx
        write(6,*) "nlst_rt(did)%nsoil = ", nlst_rt(did)%nsoil

        call flush(6)
            ! get the initial data from LIS 
            do k = 1 , nlst_rt(did)%nsoil
                call lis2ndhms_2d(RT_DOMAIN(did)%SMC(:,:,k),noah271_struc(n)%noah%smc(k),size(noah271_struc(n)%noah%smc(k)),n,ix,jx)
                call lis2ndhms_2d(RT_DOMAIN(did)%stc(:,:,k),noah271_struc(n)%noah%stc(k),size(noah271_struc(n)%noah%stc(k)), n,ix,jx)
                call lis2ndhms_2d(RT_DOMAIN(did)%SH2OX(:,:,k),noah271_struc(n)%noah%sh2o(k),size(noah271_struc(n)%noah%sh2o(k)),n,ix,jx)
            enddo
       
       
        write(6,*) "NDHMS lis date ", LIS_rc%yr, LIS_rc%mo, LIS_rc%da, LIS_rc%hr, LIS_rc%mn, LIS_rc%ss 
        call flush(6)
!       write(11,*) "RT_DOMAIN(did)%stc",RT_DOMAIN(did)%stc(:,:,1)
!       write(12,*) "noah271_struc(n)%noah%stc(1)",noah271_struc(n)%noah%stc(1)
!       call land_finish() 
    
        call NDHMS_exe(did,ntime)

! add for update the lsm state variable.
     ! 3 d variable
        do k = 1 , nlst_rt(did)%nsoil
             call ndhms2lis_2d(RT_DOMAIN(did)%SMC(:,:,k),noah271_struc(n)%noah%smc(k),size(noah271_struc(n)%noah%smc(k)),n,ix,jx)
             call ndhms2lis_2d(RT_DOMAIN(did)%stc(:,:,k),noah271_struc(n)%noah%stc(k),size(noah271_struc(n)%noah%stc(k)),n,ix,jx)
             call ndhms2lis_2d(RT_DOMAIN(did)%SH2OX(:,:,k),noah271_struc(n)%noah%sh2o(k),size(noah271_struc(n)%noah%sh2o(k)),n,ix,jx)
        end do

     return 
     end subroutine lis_cpl_ndhms

     subroutine ndhms2lis_2d(var,v1d,size1,n,ix,jx)
        implicit none
        integer :: n, ix, jx, i, r,c, size1
        real, dimension(ix,jx) :: var
        real, dimension(jx,ix) :: tmpVar
        real ::  v1d (size1)

!        call LIS_gather_gridded_output(n, p1,tmpVar)
          do r=1,jx
             do c=1,ix                
                if(LIS_domain(n)%gindex(c,r).ne.-1) then
                   v1d(LIS_domain(n)%gindex(c,r)) = var(c,r)
                endif
             enddo
          enddo

     end subroutine ndhms2lis_2d        

     subroutine lis2ndhms_2d(var,v1d,size1,n,ix,jx)
        implicit none
        integer  n, ix,jx, r,c, size1
        real, dimension(ix,jx) :: var
!       real, dimension(jx,ix) :: tmpVar
        real ::  v1d (size1)
  

!       do i = 1, ix
!          tmpVar(:,i) = Var(i,:)
!       end do
!       call LIS_gather_tiled_vector_output(n,v1d,tmpVar)

          do r=1,jx
             do c=1,ix                
                if(LIS_domain(n)%gindex(c,r).ne.-1) then
                   var(c,r) = v1d(LIS_domain(n)%gindex(c,r))
                endif
             enddo
          enddo
         
      end subroutine  lis2ndhms_2d

end module module_lis_ndhms
