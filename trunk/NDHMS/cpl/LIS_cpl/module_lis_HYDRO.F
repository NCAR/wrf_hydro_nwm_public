module module_lis_HYDRO
    use noah271_lsmMod, only : noah271_struc 
!   use LIS_coreMod, only : LIS_rc, LIS_domain, LIS_masterproc, &
!         LIS_ews_ind, LIS_ewe_ind, LIS_nss_ind, LIS_nse_ind
    use LIS_coreMod
!    use LIS_historyMod, only: LIS_gather_gridded_output, lis_gather_tiled_vector_output

! use HYDRO module
       use module_rt_data, only:  rt_domain
       use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
       use module_mpp_land
       use module_namelist, only: nlst_rt
       use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe

!!! temp use 
       use module_yw, only: SFHEAD1RT,INFXS1RT, soldrain
    contains

    subroutine lis_cpl_HYDRO(n)
       


        implicit none
        integer n, k, ix,jx

        integer ::  did, its,ite,jts,jte, ierr

        integer ntime, wrf_ix,wrf_jx
        logical mpi_inited

!output flux and state variable

        integer :: i,j, kz


        did = 1
        ntime = 1

!           write(6,*) "yyyywww step 2, n =", n
!           write(6,*) "npesx, npesy",LIS_rc%npesx, LIS_rc%npesy

      if(.not. RT_DOMAIN(did)%initialized) then

!!! get soil layers from lis
!!! ++++++++++++++
        nlst_rt(did)%nsoil = noah271_struc(n)%nslay
#ifdef MPP_LAND
        call mpp_land_bcast_int1(nlst_rt(did)%nsoil)
#endif
        if(nlst_rt(did)%nsoil < 1) then
           write(6,*) "Error: nsoil is less than 1"
           call hydro_stop 
        endif
        allocate(nlst_rt(did)%zsoil8(nlst_rt(did)%nsoil))
        nlst_rt(did)%zsoil8(1) = -noah271_struc(n)%lyrthk(1)
        DO KZ = 2,nlst_rt(did)%nsoil
           nlst_rt(did)%zsoil8(KZ) = -noah271_struc(n)%lyrthk(KZ)+nlst_rt(did)%zsoil8(KZ-1)
        END DO
!!! ----------
        write(6,*) "zsoil8 = ",nlst_rt(did)%zsoil8


        CALL mpi_initialized( mpi_inited, ierr )
        if ( .NOT. mpi_inited ) then
           call MPI_INIT( ierr )  ! stand alone land model.
        endif
        call MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr )
        call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )
      endif


        write(cpl_outdate(1:5),'(I4,"-")') LIS_rc%yr

        if(LIS_rc%mo .lt. 10) then
           write(cpl_outdate(6:8),'("0",I1,"-")') LIS_rc%mo
        else
           write(cpl_outdate(6:8),'(I2,"-")') LIS_rc%mo
        endif

        if(LIS_rc%da .lt. 10) then
           write(cpl_outdate(9:11),'("0",I1,"_")') LIS_rc%da
        else
           write(cpl_outdate(9:11),'(I2,"_")') LIS_rc%da
        endif

        if(LIS_rc%hr .lt. 10) then
           write(cpl_outdate(12:14),'("0",I1,":")') LIS_rc%hr
        else
           write(cpl_outdate(12:14),'(I2,":")') LIS_rc%hr
        endif

        if(LIS_rc%mn .lt. 10) then
           write(cpl_outdate(15:17),'("0",I1,":")') LIS_rc%mn
        else
           write(cpl_outdate(15:17),'(I2,":")') LIS_rc%mn
        endif
       
        if(LIS_rc%ss .lt. 10) then
           write(cpl_outdate(18:19),'("0",I1)') LIS_rc%ss
        else
           write(cpl_outdate(18:19),'(I2)') LIS_rc%ss
        endif


            nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)


            jx = LIS_rc%lnr(n)
            ix = LIS_rc%lnc(n)
            its = LIS_ews_ind(LIS_rc%nnest,my_id+1)
            ite = LIS_ewe_ind(LIS_rc%nnest,my_id+1)
            jts = LIS_nss_ind(LIS_rc%nnest,my_id+1)
            jte = LIS_nse_ind(LIS_rc%nnest,my_id+1)

!           write(6,*) "LIS_ews_ind",LIS_ews_ind 
!           write(6,*) "LIS_ewe_ind",LIS_ewe_ind 
!           write(6,*) "LIS_nss_ind",LIS_nss_ind 
!           write(6,*) "LIS_nse_ind",LIS_nse_ind 

        if(.not. RT_DOMAIN(did)%initialized) then
!           write(6,*) "yyyywww step 3"
            write(6,*) "ix,jx, cpl_land_dt",ix,jx, LIS_rc%ts  

!           write(6,*) "yyyywww step 4 before HYDRO_ini= "
            write(6,*) "its,ite,jts,jte", its,ite,jts,jte
            write(6,*) "noah271_struc(n)%nslay=", noah271_struc(n)%nslay
            write(6,*) "noah271_struc(n)%lyrthk=", noah271_struc(n)%lyrthk

            nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)

            call CPL_LAND_INIT(its,ite,jts,jte)


!           write(6,*) "yyyywww step 4 before HYDRO_ini= "
!           write(6,*) "its,ite,jts,jte", its,ite,jts,jte

            call HYDRO_ini(ntime,did=did,ix0=1,jx0=1)

            if(nlst_rt(did)%sys_cpl .ne. 3) then
               write(6,*) "Error: sys_cpl should be 3."
               call hydro_stop()
            endif
!           write(6,*) "yyyywww step 5 my_id= ", my_id

            nlst_rt(did)%dt = LIS_rc%ts   

!         write(6,*) "LIS_rc%gnc(n),LIS_rc%gnr(n),LIS_rc%glbnch_red(n)", &
!                LIS_rc%gnc(n),LIS_rc%gnr(n),LIS_rc%glbnch_red(n)

!         write(6,*) "LIS_rc%nch =", LIS_rc%nch(n)
!         write(6,*) "size(noah271_struc(n)%noah%cmc)" , size(noah271_struc(n)%noah%cmc)

!         write(6,*) "LIS_nss_halo_ind,LIS_nse_halo_ind", LIS_nss_halo_ind,LIS_nse_halo_ind
!         write(6,*) "LIS_ews_halo_ind,LIS_ewe_halo_ind",LIS_ews_halo_ind,LIS_ewe_halo_ind

!            nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)

          RT_DOMAIN(did)%initialized = .true.
        endif


!       write(6,*) "before call lis2HYDRO"

!       write(6,*) "size(noah271_struc(n)%noah%smc(1)),n,ix,jx" , size(noah271_struc(n)%noah%smc(1)),n,ix,jx
!       write(6,*) "nlst_rt(did)%nsoil = ", nlst_rt(did)%nsoil

            ! get the initial data from LIS 
            do k = 1 , nlst_rt(did)%nsoil
                call lis2HYDRO(RT_DOMAIN(did)%SMC(:,:,k),noah271_struc(n)%noah%smc(k),size(noah271_struc(n)%noah%smc(k)),n,ix,jx)
                call lis2HYDRO(RT_DOMAIN(did)%stc(:,:,k),noah271_struc(n)%noah%stc(k),size(noah271_struc(n)%noah%stc(k)), n,ix,jx)
                call lis2HYDRO(RT_DOMAIN(did)%SH2OX(:,:,k),noah271_struc(n)%noah%sh2o(k),size(noah271_struc(n)%noah%sh2o(k)),n,ix,jx)
            enddo
       
       
        write(6,*) "NDHMS lis date ", LIS_rc%yr, LIS_rc%mo, LIS_rc%da, LIS_rc%hr, LIS_rc%mn, LIS_rc%ss 
!       write(11,*) "RT_DOMAIN(did)%stc",RT_DOMAIN(did)%stc(:,:,1)
!       write(12,*) "noah271_struc(n)%noah%stc(1)",noah271_struc(n)%noah%stc(1)
!       call land_finish() 

        call lis2HYDRO(RT_DOMAIN(did)%infxsrt(:,:),INFXS1RT,size(INFXS1RT),n,ix,jx)
        call lis2HYDRO(RT_DOMAIN(did)%soldrain(:,:),SOLDRAIN,size(SOLDRAIN),n,ix,jx)
        
    
        call HYDRO_exe(did)

! add for update the land surface model state variable.
     ! 3 d variable
        do k = 1 , nlst_rt(did)%nsoil
             call HYDRO2lis_2d(RT_DOMAIN(did)%SMC(:,:,k),noah271_struc(n)%noah%smc(k),size(noah271_struc(n)%noah%smc(k)),n,ix,jx)
             call HYDRO2lis_2d(RT_DOMAIN(did)%stc(:,:,k),noah271_struc(n)%noah%stc(k),size(noah271_struc(n)%noah%stc(k)),n,ix,jx)
             call HYDRO2lis_2d(RT_DOMAIN(did)%SH2OX(:,:,k),noah271_struc(n)%noah%sh2o(k),size(noah271_struc(n)%noah%sh2o(k)),n,ix,jx)
        end do

        call HYDRO2lis_2d(RT_DOMAIN(did)%sfcheadrt(:,:),SFHEAD1RT,size(SFHEAD1RT),n,ix,jx)

     return 
     end subroutine lis_cpl_HYDRO

     subroutine HYDRO2lis_2d(var,v1d,size1,n,ix,jx)
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

     end subroutine HYDRO2lis_2d        

     subroutine lis2HYDRO(var,v1d,size1,n,ix,jx)
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
         
      end subroutine  lis2HYDRO

end module module_lis_HYDRO
