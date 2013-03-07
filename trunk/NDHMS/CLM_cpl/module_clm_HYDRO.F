module module_CLM_HYDRO
    use domainMod        , only : ldomain
    use clmtype   , only : clm3
    use clm_varpar   , only : nlevgrnd
    use decompMod    , only : get_proc_bounds, get_proc_global
    use clm_varcon  , only : zsoi
    ; use subgridAveMod, only: l2g_1d
    use subgridAveMod, only: c2g_1d, c2g_2d

! NDHMS  module   
    use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id
    use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe

    implicit none
    integer begg, endg
    integer :: numg, numl, numc, nump
    INTEGER, PARAMETER :: double=8
    real(kind=double), pointer :: r2p(:,:) , r1p(:)
     
    integer ::  begl, endl, begc, endc, begp, endp
    real(kind=double), allocatable, dimension(:) :: clm_g1d
    real(kind=double), allocatable, dimension(:,:) :: clm_g2d

    real, allocatable, dimension(:,:) :: vg_test



CONTAINS

    subroutine clm_cpl_HYDRO()

       use module_rt_data, only:  rt_domain
       use module_CPL_LAND, only: CPL_LAND_INIT, cpl_outdate
       use module_mpp_land
       use module_namelist, only: nlst_rt 

        implicit none
        integer k, ix,jx, nn

        integer ::  did

        integer ntime, wrf_ix,wrf_jx
        real cpl_land_dt

        integer :: i,j
        integer clm_lev

!output flux and state variable

        did = 1

        call get_cpl_date(cpl_land_dt)

        write(6,*) "cpl_land_dt = ",cpl_land_dt

        ntime = 1

        if(.not. RT_DOMAIN(did)%initialized) then




            call HYDRO_ini(ntime,did,1,1)

            if(nlst_rt(did)%sys_cpl .ne. 4) then
               write(6,*) "Error: sys_cpl should be 4."
               call hydro_stop()
            endif

            RT_DOMAIN(did)%initialized = .true.
            nlst_rt(did)%dt = cpl_land_dt
            nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
            nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)
        endif

        ix = rt_domain(did)%ix
        jx = rt_domain(did)%jx

        ! get the initial data from CLM 
            call get_proc_global(numg, numl, numc, nump)

            call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
            write(6,*) "begg, endg, ", begg, endg
            write(6,*) "begl, endl, ", begl, endl
            write(6,*) "begc, endc, ", begc, endc
            write(6,*) "begp, endp, ", begp, endp

!            call get_proc_bounds( begg=begg, endg=endg )
            nn = endg - begg + 1

            allocate(clm_g1d(endg-begg + 1))
            allocate(clm_g2d(endg-begg+1, nlevgrnd) )





            r1p => clm3%g%l%c%cwf%qflx_surf
            call c2g_1d(begc, endc, begl, endl, begg, endg, r1p(begc:endc), clm_g1d, &
                'urbanf', 'unity')
            call clm2ND2d(clm_g1d,nn,RT_DOMAIN(did)%INFXSRT,ix,jx)

            write(6,*) "finish qflx_surf"


            r1p => clm3%g%l%c%cwf%qflx_drain  
            call c2g_1d(begc, endc, begl, endl, begg, endg, r1p(begc:endc), clm_g1d, &
                'urbanf', 'unity')
            call clm2ND2d(clm_g1d,nn,RT_DOMAIN(did)%SOLDRAIN,ix,jx)


            write(6,*) "finish qflx_drain "
             
 
            r2p =>clm3%g%l%c%ces%t_soisno 
                call c2g_2d(begc, endc, begl, endl, begg, endg, nlevgrnd, r2p(begc:endc,1:nlevgrnd), clm_g2d, &
                'urbanh', 'unity')
            call clm2ND3d (clm_g2d,nn,nlevgrnd,zsoi(1:nlevgrnd), &
                      ix,jx, nlst_rt(did)%nsoil,abs(RT_DOMAIN(did)%ZSOIL(1:nlst_rt(did)%nsoil)),&
                      RT_DOMAIN(did)%STC)

            write(6,*) "finish t_soisno   "


            r2p => clm3%g%l%c%cws%h2osoi_vol  
                call c2g_2d(begc, endc, begl, endl, begg, endg, nlevgrnd, r2p(begc:endc,1:nlevgrnd), clm_g2d, &
                'urbanh', 'unity')
            call clm2ND3d (clm_g2d,nn,nlevgrnd,zsoi(1:nlevgrnd), &
                     ix,jx, nlst_rt(did)%nsoil,abs(RT_DOMAIN(did)%ZSOIL(1:nlst_rt(did)%nsoil)),&
                     RT_DOMAIN(did)%SMC)
            
            write(6,*) "finish h2osoi_vol "


            r2p => clm3%g%l%c%cws%h2osoi_liq  
            call c2g_2d(begc, endc, begl, endl, begg, endg, nlevgrnd, r2p(begc:endc,1:nlevgrnd), clm_g2d, &
                'urbanh', 'unity')
            call clm2ND3d (clm_g2d, nn,nlevgrnd,zsoi(1:nlevgrnd), &
                     ix,jx, nlst_rt(did)%nsoil,abs(RT_DOMAIN(did)%ZSOIL(1:nlst_rt(did)%nsoil)),&
                     RT_DOMAIN(did)%SH2OX)
            RT_DOMAIN(did)%SH2OX =  RT_DOMAIN(did)%SH2OX /1000.
   
            write(6,*) "finish h2osoi_liq "

            write(6,*) "before call HYDRO_exe" 



            call HYDRO_exe(did)

            write(6,*) "after call HYDRO_exe" 

! add for update the CLM state variable.

           clm_lev = 10

            r2p =>clm3%g%l%c%ces%t_soisno 
            call Toclm3d (clm_g2d,nn,clm_lev,zsoi(1:clm_lev), &
                     ix,jx, nlst_rt(did)%nsoil,abs(RT_DOMAIN(did)%ZSOIL(1:nlst_rt(did)%nsoil)),&
                     RT_DOMAIN(did)%STC)
            call g2c_2d(begc, endc, begl, endl, begg, endg,clm_lev,r2p(begc:endc,1:clm_lev), clm_g2d, &
                'urbanh', 'unity')

            


            r2p => clm3%g%l%c%cws%h2osoi_vol  
            call Toclm3d (clm_g2d,nn,clm_lev,zsoi(1:clm_lev), &
                     ix,jx, nlst_rt(did)%nsoil,abs(RT_DOMAIN(did)%ZSOIL(1:nlst_rt(did)%nsoil)), &
                     RT_DOMAIN(did)%SMC)
            call g2c_2d_tmp(begc, endc, begl, endl, begg, endg,clm_lev,r2p(begc:endc,1:clm_lev), clm_g2d, &
                'urbanh', 'unity', 0.01, 10.)



            r2p => clm3%g%l%c%cws%h2osoi_liq  
            call Toclm3d(clm_g2d,nn,clm_lev,zsoi(1:clm_lev), &
                     ix,jx, nlst_rt(did)%nsoil,abs(RT_DOMAIN(did)%ZSOIL(1:nlst_rt(did)%nsoil)),&
                     RT_DOMAIN(did)%SH2OX*1000)
            call g2c_2d_tmp(begc, endc, begl, endl, begg, endg,clm_lev,r2p(begc:endc,1:clm_lev), clm_g2d, &
                'urbanh', 'unity',0.01, 10.)




     ! 2d variable
     !      t_soisno
     ! 3 d variable
     deallocate(clm_g1d)
     deallocate(clm_g2d)
            write(6,*) "end of drive ndhms"

     return 
     end subroutine clm_cpl_HYDRO

      subroutine Toclm3d (v1d_out,nn,kk1,z1_in,ix,jx,kk2,z2,v2_in)
        implicit none
          integer :: nn,ix,jx, kk2, kk1
          integer :: k
          real:: v2_in(ix,jx,kk2)
          real:: v1(ix,jx,kk1)
          real, dimension(kk2) :: z2
          REAL (KIND=double),dimension(nn,kk1):: v1d_out
          REAL (KIND=double),dimension(kk1):: z1_in
          REAL ,dimension(kk1):: z1

          z1 = z1_in
         
! v1 is the out from interp3D 
             call Interp3D (z2,v2_in,kk2,z1,v1,ix,jx,kk1)



             do k = 1, kk1
                call Toclm2d(v1(:,:,k),ix,jx,v1d_out(:,k),nn)
             end do

        return
      end subroutine Toclm3d

      subroutine Toclm2d(v1,ix,jx,v1d_out,nn)
         use module_mpp_land, only: my_id,IO_id,mpp_land_bcast_real 
         implicit none
         integer ix,jx, nn
         real v1(ix,jx)
         REAL (KIND=double),dimension(nn) :: v1d_out
         real, dimension(global_nx*global_ny)::vg
         real, dimension(global_nx,global_ny)::vg2d


            call write_io_real(v1,vg2d)

            vg = 0.0
            if(my_id .eq. IO_id) then
                call TO1d(vg2d,vg,global_nx,global_ny)
            end if

            write(6,*) "before scatter_1d_r"


            call scatter_1d_r(v1d_out,nn,vg,global_nx*global_ny)

            write(6,*) "after scatter_1d_r"

         return
      end subroutine Toclm2d
      
      subroutine TO1d(vg2d,v1d,nx,ny)
         implicit none
         integer nx,ny, n, i,j
         real vg2d(nx,ny)
         real v1d(nx*ny)
             do j = 1,ny 
             do i = 1,nx 
                n = (j-1)*nx + i
                v1d(n) = vg2d(i,j)
             enddo
             enddo
         return
      end subroutine TO1d

      subroutine clm2ND3d (v1d_in,nn,kk1,z1_in,ix,jx,kk2,z2,vout)
        implicit none
          integer :: nn,kk1,ix,jx,kk2
          integer :: k
          real:: vout(ix,jx,kk2)
          real:: v1(ix,jx,kk1)
          real(kind=double),dimension(nn,kk1):: v1d_in
          real(kind=double), dimension(kk1) :: z1_in
          real, dimension(kk1) :: z1
          real, dimension(kk2) :: z2
        
 
          z1 = z1_in

          do k = 1, kk1 
             call clm2ND2d(v1d_in(:,k),nn,v1(:,:,k),ix,jx)
          end do
             call Interp3D (z1(1:kk1),v1,kk1,z2(1:kk2),vout,ix,jx,kk2)


        return
      end subroutine clm2ND3d

      subroutine clm2ND2d (v1d_in,nn,vout,ix,jx)
          use clmtype, only: grlnd
          use module_mpp_land, only: my_id,IO_id,mpp_land_bcast_real 
          implicit none
          integer :: ix,jx,nn, n
          real vout(ix,jx)
          integer :: i,j,gni,gnj
!         ! real, allocatable, dimension(:)  :: g1d
          real(kind=double), dimension(global_nx*global_ny)  :: g1d
          real v2d_g(global_nx,global_ny)
          real(kind=double),dimension(nn) ::  v1d_in


          gni = global_nx
          gnj = global_ny
!     gather 1d global array

          if(endg .gt. gni*gnj) then
            write(6,*) "Error: endg > gni*gnj", endg, gni*gnj
            ! need to stop
          endif
          call gather_1d_real(v1d_in,nn,g1d,gni*gnj)
!  transfer 1d   to 2d 

          if(my_id.eq.IO_id) then
             do j = 1,gnj
             do i = 1,gni
                n = (j-1)*gni + i  
                v2d_g(i,j) = g1d(n)
             enddo
             enddo
          endif

!         if(my_id .eq. 0) then
!            call output_nc(v2d_g,gni,gnj, "test", "test.nc")
!         endif
           
! domain decomposition
          call decompose_data_real(v2d_g,vout)

      return
      end subroutine clm2ND2d

      subroutine Interp3D (z1,v1,kk1,z,vout,ix,jx,kk)
!  input: z1,v1,kk1,z,ix,jx,kk
!  output: vout
!  interpolate based on soil layer: z1 and z 
!  z :  soil layer of output variable.
!  z1: array of soil layers of input variable.
         implicit none
         integer:: i,j,k
         integer:: kk1, ix,jx,kk
         real :: z1(kk1), z(kk), v1(ix,jx,kk1),vout(ix,jx,kk)

!        write(6,*) "k, kk1 ", k, kk1
!        write(6,*) "z1(kk1), z(k) ", z1(kk1), z(k)
       
         do j = 1, jx
            do i = 1, ix
              do k = 1, kk
!                call interpLayer(abs(Z1),v1(i,j,1:kk1),kk1,abs( Z(k) ),vout(i,j,k)) 
                call interpLayer(Z1(1:kk1),v1(i,j,1:kk1),kk1,Z(k),vout(i,j,k)) 
              end do
            end do
         end do
      end subroutine Interp3D

      subroutine interpLayer(inZ,inV,inK,outZ,outV)
         implicit none
         integer:: k, k1, k2
         integer :: inK
         real:: inV(inK),inZ(inK)
         real:: outV, outZ, w1, w2

         if(outZ .le. inZ(1)) then
             if(inZ(2) .eq. inZ(1)) then 
                write(6,*) "Error: inZ(2)=inZ(1) ", inZ(2),inZ(1)
                stop 99
             end if
             w1 = (inZ(2)-outZ)/(inZ(2)-inZ(1))
             w2 = (inZ(1)-outZ)/(inZ(2)-inZ(1))
             outV = inV(1)*w1-inV(2)*w2
             return
         elseif(outZ .ge. inZ(inK)) then
             if(inZ(inK-1) .eq. inZ(inK)) then 
                write(6,*) "Error: inZ(inK-1)=inZ(inK) ", inZ(inK-1),inZ(inK)
                stop 99
             end if
             w1 = (outZ-inZ(inK-1))/(inZ(inK)-inZ(inK-1)) 
             w2 = (outZ-inZ(inK))  /(inZ(inK)-inZ(inK-1))
             outV = inV(inK)*w1 -inV(inK-1)* w2
             return
         else  
            do k = 2, inK
             if((inZ(k) .ge. outZ).and.(inZ(k-1) .le. outZ) ) then
                k1  = k-1
                k2 = k
             if(inZ(k1) .eq. inZ(k2)) then 
                write(6,*) "Error: inZ(k1)=inZ(k2) ", inZ(k1),inZ(k2)
                stop 99
             end if
                w1 = (outZ-inZ(k1))/(inZ(k2)-inZ(k1))
                w2 = (inZ(k2)-outZ)/(inZ(k2)-inZ(k1))
                outV = inV(k2)*w1 + inV(k1)*w2
                return 
             end if 
            end do
         endif
      end subroutine interpLayer

    subroutine get_cpl_date(cpl_land_dt)
      use clm_time_manager, only : get_step_size, get_curr_date
      use module_CPL_LAND, only: cpl_outdate
      implicit none
      integer :: yr, mo, da, hr, mn, ss , sec
      real :: cpl_land_dt
      integer cmdate

      cpl_land_dt  = 1.0* get_step_size()
      
      call get_curr_date(yr, mo, da, sec)     

    
      hr = sec/3600 

      sec = mod(sec,3600)
      mn = sec/60

      ss = mod(sec,60)

      if(yr .lt. 1000) yr = 2000+yr
      write(cpl_outdate(1:4),'(I4)') yr
     
      if(mo .lt. 10 ) then
         write(cpl_outdate(5:7),'("-0",I1)') mo
      else
         write(cpl_outdate(5:7),'("-",I2)') mo
      endif

      if(da .lt. 10 ) then
         write(cpl_outdate(8:10),'("-0",I1)') da
      else
         write(cpl_outdate(8:10),'("-",I2)') da
      endif
          
      if(hr .lt. 10 ) then
         write(cpl_outdate(11:13),'("_0",I1)') hr
      else
         write(cpl_outdate(11:13),'("_",I2)') hr
      endif

      if(mn .lt. 10 ) then
         write(cpl_outdate(14:16),'(":0",I1)') mn
      else
         write(cpl_outdate(14:16),'(":",I2)') mn
      endif

      if(ss .lt. 10 ) then
         write(cpl_outdate(17:19),'(":0",I1)') ss
      else
         write(cpl_outdate(17:19),'(":",I2)') ss
      endif
          
          
          
!      write(cpl_outdate,'(I4,"-",I2,"-",I2,"_",I2,":",I2,":",I2)' )     &
!            yr, mo, da, hr, mn, ss 

      write(6,*) "cpl_outdate = ",cpl_outdate

    end subroutine get_cpl_date

!    transfer the grid column to gridcell.
  subroutine g2c_1d(lbc, ubc, lbl, ubl, lbg, ubg, carr, garr, &
       c2l_scale_type, l2g_scale_type)
!
! !DESCRIPTION:
! Perfrom subgrid-average from columns to gridcells.
! Averaging is only done for points that are not equal to "spval".
!
! !ARGUMENTS:
     use clm_varcon, only : spval, isturb,  icol_roof, icol_sunwall, icol_shadewall, &
                         icol_road_perv, icol_road_imperv
     use clm_varctl, only : iulog


    implicit none
    integer , intent(in)  :: lbc, ubc              ! beginning and ending column indices
    integer , intent(in)  :: lbl, ubl              ! beginning and ending landunit indices
    integer , intent(in)  :: lbg, ubg              ! beginning and ending landunit indices
    real(kind=double), intent(out)  :: carr(lbc:ubc)         ! output column array
    real(kind=double), intent(in) :: garr(lbg:ubg)         ! input  gridcell array
    character(len=*), intent(in) :: c2l_scale_type ! scale factor type for averaging
    character(len=*), intent(in) :: l2g_scale_type ! scale factor type for averaging
!
! !REVISION HISTORY:
! Created by Mariana Vertenstein 12/03
!
!
! !LOCAL VARIABLES:
!EOP
    integer  :: ci,c,l,g,index         ! indices
    integer  :: max_col_per_gcell      ! max columns per gridcell; on the fly
    logical  :: found                  ! temporary for error check
    real(kind=double) :: scale_c2l(lbc:ubc)     ! scale factor
    real(kind=double) :: scale_l2g(lbl:ubl)     ! scale factor
    real(kind=double) :: sumwt(lbg:ubg)         ! sum of weights
    real(kind=double), pointer :: wtgcell(:)    ! weight of columns relative to gridcells
    integer , pointer :: clandunit(:)  ! landunit of corresponding column
    integer , pointer :: cgridcell(:)  ! gridcell of corresponding column
    integer , pointer :: ncolumns(:)   ! number of columns in gridcell
    integer , pointer :: coli(:)       ! initial column index in gridcell
    integer , pointer :: ctype(:)      ! column type
    integer , pointer :: ltype(:)      ! landunit type
    real(kind=double), pointer :: canyon_hwr(:) ! urban canyon height to width ratio
    integer :: gcount(lbg:ubg)
!------------------------------------------------------------------------


    ctype      => clm3%g%l%c%itype
    ltype      => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    wtgcell    => clm3%g%l%c%wtgcell
    clandunit  => clm3%g%l%c%landunit
    cgridcell  => clm3%g%l%c%gridcell
    ncolumns   => clm3%g%ncolumns
    coli       => clm3%g%coli

    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0
       end do
    end if

    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l) 
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l) 
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    end if

    sumwt(:) = 0.
    gcount(:) = 0
    do c = lbc,ubc
          if (carr(c) /= spval .and. scale_c2l(c) /= spval) then
             l = clandunit(c)
             g = cgridcell(c)
             if(wtgcell(c) .ne. 0) then
                carr(c) = garr(g) 
             endif
          end if
    end do
    end subroutine g2c_1d
   
    subroutine gather_1d_real(l1d,lnn,g1d,gnn)
       use spmdGathScatMod
       use clmtype   , only : grlnd, nameg

       implicit none
       integer lnn, gnn
       real(kind=double),dimension(lnn) :: l1d
       real(kind=double),dimension(gnn) :: g1d
       real(kind=double), pointer :: lp_yw(:), gp_yw(:)

       allocate(lp_yw(lnn) )
       allocate(gp_yw(gnn) )
       lp_yw = l1d


       call gather_data_to_master(lp_yw,gp_yw,grlnd)

       g1d = gp_yw
       deallocate(lp_yw)
       deallocate(gp_yw)
    end subroutine gather_1d_real 

    subroutine scatter_1d_r(l1d,lnn,g1d,gnn)
       use spmdGathScatMod, only: scatter_1darray_real
       use clmtype   , only : grlnd, nameg

       implicit none
       integer lnn, gnn
       real(kind=double),dimension(lnn) :: l1d
       real,dimension(gnn) :: g1d
       real(kind=double), pointer :: lp_yw(:), gp_yw(:)




       allocate(lp_yw(begg:endg) )
       allocate(gp_yw(gnn) )

       
       gp_yw(:) = g1d(:)


!      call scatter_data_from_master(lp, gp, grlnd)

       call scatter_1darray_real(lp_yw, gp_yw, grlnd)


       l1d = lp_yw(begg:endg) 

       deallocate(lp_yw)
       deallocate(gp_yw)
    end subroutine scatter_1d_r 

      subroutine output_nc(array,idim,jdim, var_name, file_name)
      implicit none
#include <netcdf.inc>
          integer idim,jdim
          real array(idim,jdim)
          integer dim(2)
          character(len=*) file_name,var_name
          integer   iret,ncid,varid,idim_id,jdim_id
          integer i,j
          iret = nf_create(trim(file_name), 0, ncid)
          iret = nf_def_dim(ncid, "idim", idim,idim_id)
          iret = nf_def_dim(ncid, "jdim", jdim,jdim_id)
          dim(1)=idim_id
          dim(2)=jdim_id
          iret = nf_put_att_real(ncid, NF_GLOBAL,  &
                "missing_value", NF_FLOAT, 1, -1.E33)

          iret = nf_def_var(ncid,var_name,NF_FLOAT,2,dim,varid)
          iret = nf_enddef(ncid)

!   output
          iret = nf_inq_varid(ncid,var_name,varid)
          iret = nf_put_var_real(ncid,varid,array)
          iret=nf_close(ncid)
      return
      end subroutine output_nc

  subroutine g2c_2d(lbc, ubc, lbl, ubl, lbg, ubg, num2d, carr, garr, &
       c2l_scale_type, l2g_scale_type)
!
! !DESCRIPTION:
! Perfrom subgrid-average from columns to gridcells.
! Averaging is only done for points that are not equal to "spval".
!
     use clm_varcon, only : spval, isturb,  icol_roof, icol_sunwall, icol_shadewall, &
                         icol_road_perv, icol_road_imperv
     use clm_varctl, only : iulog
! !ARGUMENTS:
    implicit none
    integer , intent(in)  :: lbc, ubc              ! beginning and ending column indices
    integer , intent(in)  :: lbl, ubl              ! beginning and ending landunit indices
    integer , intent(in)  :: lbg, ubg              ! beginning and ending gridcell indices
    integer , intent(in)  :: num2d                 ! size of second dimension
    real(kind=double), intent(inout)  :: carr(lbc:ubc,num2d)   ! input column array
    real(kind=double), intent(in) :: garr(lbg:ubg,num2d)   ! output gridcell array
    character(len=*), intent(in) :: c2l_scale_type ! scale factor type for averaging
    character(len=*), intent(in) :: l2g_scale_type ! scale factor type for averaging

    real(kind=double) :: yw_r
!
! !REVISION HISTORY:
! Created by Mariana Vertenstein 12/03
!
!
! !LOCAL VARIABLES:
!EOP
    integer  :: j,ci,c,g,l,index       ! indices
    integer  :: max_col_per_gcell      ! max columns per gridcell; on the fly
    logical  :: found                  ! temporary for error check
    real(kind=double) :: scale_c2l(lbc:ubc)     ! scale factor
    real(kind=double) :: scale_l2g(lbl:ubl)     ! scale factor
    real(kind=double) :: sumwt(lbg:ubg)         ! sum of weights
    real(kind=double), pointer :: wtgcell(:)    ! weight of columns relative to gridcells
    integer , pointer :: clandunit(:)  ! landunit of corresponding column
    integer , pointer :: cgridcell(:)  ! gridcell of corresponding column
    integer , pointer :: ncolumns(:)   ! number of columns in gridcell
    integer , pointer :: coli(:)       ! initial column index in gridcell
    integer , pointer :: ctype(:)      ! column type
    integer , pointer :: ltype(:)      ! landunit type
    real(kind=double), pointer :: canyon_hwr(:) ! urban canyon height to width ratio
    real :: w_yw
!------------------------------------------------------------------------

    ctype      => clm3%g%l%c%itype
    ltype      => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    wtgcell    => clm3%g%l%c%wtgcell
    clandunit  => clm3%g%l%c%landunit
    cgridcell  => clm3%g%l%c%gridcell
    ncolumns   => clm3%g%ncolumns
    coli       => clm3%g%coli

    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0
       end do
    else
       write(iulog,*)'c2g_2d error: scale type ',l2g_scale_type,' not supported'
    end if
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l) 
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l) 
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    else
       write(iulog,*)'c2g_2d error: scale type ',c2l_scale_type,' not supported'
    end if

    do j = 1,num2d
       sumwt(:) = 0.
       do c = lbc,ubc
          if (wtgcell(c) /= 0) then
             if (carr(c,j) /= spval .and. scale_c2l(c) /= spval) then
                l = clandunit(c)
                g = cgridcell(c)
                sumwt(g) = sumwt(g) + wtgcell(c)
             end if
          end if
       end do
       do c = lbc,ubc
          if (wtgcell(c) /= 0) then
             if (carr(c,j) /= spval .and. scale_c2l(c) /= spval) then
                l = clandunit(c)
                g = cgridcell(c)
                   !garr(g,j) = garr(g,j) + carr(c,j) * scale_c2l(c) * scale_l2g(l) * wtgcell(c)
                   !w_yw = scale_c2l(c) * scale_l2g(l) * wtgcell(c)
                   w_yw = scale_c2l(c) * scale_l2g(l) 
                   if(w_yw .ne. 0) then 
                      ! carr(c,j) =  garr(g,j) / w_yw *sumwt(g)
                      yw_r = garr(g,j) / w_yw
                      if(abs(yw_r - carr(c,j)) .lt. 400 .and. yw_r .ne. 0 ) then
                         carr(c,j) =  garr(g,j) / w_yw 
                      endif
                   endif
            end if
          end if
       end do
    end do

  end subroutine g2c_2d

  subroutine g2c_2d_tmp(lbc, ubc, lbl, ubl, lbg, ubg, num2d, carr, garr, &
       c2l_scale_type, l2g_scale_type, minv, maxv)
!
! !DESCRIPTION:
! Perfrom subgrid-average from columns to gridcells.
! Averaging is only done for points that are not equal to "spval".
!
     use clm_varcon, only : spval, isturb,  icol_roof, icol_sunwall, icol_shadewall, &
                         icol_road_perv, icol_road_imperv
     use clm_varctl, only : iulog
! !ARGUMENTS:
    implicit none
    integer , intent(in)  :: lbc, ubc              ! beginning and ending column indices
    integer , intent(in)  :: lbl, ubl              ! beginning and ending landunit indices
    integer , intent(in)  :: lbg, ubg              ! beginning and ending gridcell indices
    integer , intent(in)  :: num2d                 ! size of second dimension
    real(kind=double), intent(inout)  :: carr(lbc:ubc,num2d)   ! input column array
    real(kind=double), intent(in) :: garr(lbg:ubg,num2d)   ! output gridcell array
    character(len=*), intent(in) :: c2l_scale_type ! scale factor type for averaging
    character(len=*), intent(in) :: l2g_scale_type ! scale factor type for averaging

    real(kind=double) :: yw_r
    real :: minv, maxv
!
! !REVISION HISTORY:
! Created by Mariana Vertenstein 12/03
!
!
! !LOCAL VARIABLES:
!EOP
    integer  :: j,ci,c,g,l,index       ! indices
    integer  :: max_col_per_gcell      ! max columns per gridcell; on the fly
    logical  :: found                  ! temporary for error check
    real(kind=double) :: scale_c2l(lbc:ubc)     ! scale factor
    real(kind=double) :: scale_l2g(lbl:ubl)     ! scale factor
    real(kind=double) :: sumwt(lbg:ubg)         ! sum of weights
    real(kind=double), pointer :: wtgcell(:)    ! weight of columns relative to gridcells
    integer , pointer :: clandunit(:)  ! landunit of corresponding column
    integer , pointer :: cgridcell(:)  ! gridcell of corresponding column
    integer , pointer :: ncolumns(:)   ! number of columns in gridcell
    integer , pointer :: coli(:)       ! initial column index in gridcell
    integer , pointer :: ctype(:)      ! column type
    integer , pointer :: ltype(:)      ! landunit type
    real(kind=double), pointer :: canyon_hwr(:) ! urban canyon height to width ratio
    real :: w_yw
!------------------------------------------------------------------------

    ctype      => clm3%g%l%c%itype
    ltype      => clm3%g%l%itype
    canyon_hwr => clm3%g%l%canyon_hwr
    wtgcell    => clm3%g%l%c%wtgcell
    clandunit  => clm3%g%l%c%landunit
    cgridcell  => clm3%g%l%c%gridcell
    ncolumns   => clm3%g%ncolumns
    coli       => clm3%g%coli

    if (l2g_scale_type == 'unity') then
       do l = lbl,ubl
          scale_l2g(l) = 1.0
       end do
    else
       write(iulog,*)'c2g_2d error: scale type ',l2g_scale_type,' not supported'
    end if
    if (c2l_scale_type == 'unity') then
       do c = lbc,ubc
          scale_c2l(c) = 1.0
       end do
    else if (c2l_scale_type == 'urbanf') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l) 
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = 3.0 * canyon_hwr(l) 
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    else if (c2l_scale_type == 'urbans') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = (3.0 * canyon_hwr(l)) / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = 3.0 / (2.*canyon_hwr(l) + 1.)
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = 1.0
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    else if (c2l_scale_type == 'urbanh') then
       do c = lbc,ubc
          l = clandunit(c) 
          if (ltype(l) == isturb) then
             if (ctype(c) == icol_sunwall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_shadewall) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_road_perv .or. ctype(c) == icol_road_imperv) then
                scale_c2l(c) = spval
             else if (ctype(c) == icol_roof) then
                scale_c2l(c) = spval
             end if
          else
             scale_c2l(c) = 1.0
          end if
       end do
    else
       write(iulog,*)'c2g_2d error: scale type ',c2l_scale_type,' not supported'
    end if

    do j = 1,num2d
       sumwt(:) = 0.
       do c = lbc,ubc
          if (wtgcell(c) /= 0) then
             if (carr(c,j) /= spval .and. scale_c2l(c) /= spval) then
                l = clandunit(c)
                g = cgridcell(c)
                sumwt(g) = sumwt(g) + wtgcell(c)
             end if
          end if
       end do
       do c = lbc,ubc
          if (wtgcell(c) /= 0) then
             if (carr(c,j) /= spval .and. scale_c2l(c) /= spval) then
                    l = clandunit(c)
                    g = cgridcell(c)
                    !garr(g,j) = garr(g,j) + carr(c,j) * scale_c2l(c) * scale_l2g(l) * wtgcell(c)
                    !w_yw = scale_c2l(c) * scale_l2g(l) * wtgcell(c)
                    w_yw = scale_c2l(c) * scale_l2g(l) 
                    if(w_yw .ne. 0) then 
                    if(abs(garr(g,j)) .gt. minv .and. abs(garr(g,j)) .lt. maxv) then 
                       ! carr(c,j) =  garr(g,j) / w_yw *sumwt(g)
                             yw_r =  garr(g,j) / w_yw 
                             if(yw_r .gt. 0) then
                                carr(c,j) =  garr(g,j) / w_yw 
                             endif
                    endif
                    endif
             end if
          end if
       end do
    end do

  end subroutine g2c_2d_tmp
end module module_clm_HYDRO
