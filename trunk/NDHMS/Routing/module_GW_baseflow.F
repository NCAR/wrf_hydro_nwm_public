module module_GW_baseflow

#ifdef MPP_LAND
   use module_mpp_land
#endif
   implicit none

#include "gw_field_include.inc"
#include "rt_include.inc"
#include "namelist.inc"
contains

!------------------------------------------------------------------------------
!DJG   Simple GW Bucket Model
!------------------------------------------------------------------------------

   subroutine simp_gw_buck(ix,jx,ixrt,jxrt,numbasns,basns_area,&
                            gwsubbasmsk, runoff1x, runoff2x, z_gwsubbas, qin_gwsubbas,&
                            qout_gwsubbas,qinflowbase,gw_strm_msk,gwbas_pix_ct,dist,DT,&
                            C,ex,z_mx,GWBASESWCRT,OVRTSWCRT)
   implicit none
   
!!!Declarations...
   integer, intent(in)                               :: ix,jx,ixrt,jxrt
   integer, intent(in)                               :: numbasns
   integer, intent(in), dimension(ix,jx)             :: gwsubbasmsk
   real, intent(in), dimension(ix,jx)                :: runoff2x 
   real, intent(in), dimension(ix,jx)                :: runoff1x 
   real, intent(in)                                  :: basns_area(numbasns),dist(ixrt,jxrt,9),DT
   real, intent(in),dimension(numbasns)              :: C,ex,z_mx
   real, intent(out),dimension(numbasns)             :: qout_gwsubbas
   real, intent(out),dimension(numbasns)             :: qin_gwsubbas
   real, intent(out),dimension(numbasns)             :: z_gwsubbas
   real, intent(out),dimension(ixrt,jxrt)            :: qinflowbase
   integer, intent(in),dimension(ixrt,jxrt)          :: gw_strm_msk
   integer, intent(in)                               :: GWBASESWCRT
   integer, intent(in)                               :: OVRTSWCRT

   real*8, dimension(numbasns)                      :: sum_perc8,ct_bas8
   real, dimension(numbasns)                        :: sum_perc
   real, dimension(numbasns)                        :: net_perc

   real, dimension(numbasns)                        :: ct_bas
   real, dimension(numbasns)                        :: gwbas_pix_ct
   integer                                          :: i,j,bas
   real                                             :: zbastmp
   character(len=19)				    :: header
   character(len=1)				    :: jnk


!!!Initialize variables...
   ct_bas8 = 0
   sum_perc8 = 0.
   net_perc = 0.
   qout_gwsubbas = 0.
   qin_gwsubbas = 0.



!!!Calculate aggregated percolation from deep runoff into GW basins...
   do i=1,ix
     do j=1,jx
       do bas=1,numbasns
         if(gwsubbasmsk(i,j).eq.bas) then
           if(OVRTSWCRT.ne.0) then
             sum_perc8(bas) = sum_perc8(bas)+runoff2x(i,j)  !Add only drainage to bucket...runoff2x in (mm)
           else
             sum_perc8(bas) = sum_perc8(bas)+runoff1x(i,j)+runoff2x(i,j)  !Add sfc water & drainage to bucket...runoff1x and runoff2x in (mm)
           end if
           ct_bas8(bas) = ct_bas8(bas) + 1
         end if
       end do
     end do
   end do

#ifdef MPP_LAND
   call sum_real8(sum_perc8,numbasns)
   call sum_real8(ct_bas8,numbasns)
#endif
   sum_perc = sum_perc8
   ct_bas = ct_bas8
   



!!!Loop through GW basins to adjust for inflow/outflow

   DO bas=1,numbasns     ! Loop for GW bucket calcs...
#ifdef MPP_LAND
     if(ct_bas(bas) .gt. 0) then
#endif

     net_perc(bas) = sum_perc(bas) / ct_bas(bas)   !units (mm)
     qin_gwsubbas(bas) = net_perc(bas)/1000. * ct_bas(bas) * basns_area(bas) !units (m^3)

!Adjust level of GW depth...(conceptual GW bucket units (m))
     z_gwsubbas(bas) = z_gwsubbas(bas) + net_perc(bas) / 1000.0   ! (m)
     zbastmp = z_gwsubbas(bas)

!Calculate baseflow as a function of GW depth...

     if(GWBASESWCRT.eq.1) then  !active exponential bucket...
! Assuming and exponential relation between z and Q...
       qout_gwsubbas(bas) = C(bas)*EXP(ex(bas)*z_gwsubbas(bas)/z_mx(bas)) !Exp.model. q_out (m^3/s)

!Adjust level of GW depth...
       z_gwsubbas(bas) = z_gwsubbas(bas) - qout_gwsubbas(bas)*DT &
                       / (ct_bas(bas)*basns_area(bas))   !units(m)



     elseif (GWBASESWCRT.eq.2) then  !Pass through/steady-state bucket

! Assuming a steady-state (inflow=outflow) model...
       qout_gwsubbas(bas) = qin_gwsubbas(bas)  !steady-state model...(m^3)



     end if



#ifdef MPP_LAND
     endif
#endif
   END DO                 ! End loop for GW bucket calcs...



!!!Distribute basin integrated baseflow to stream pixels as stream 'inflow'...

      qinflowbase = 0.


      do i=1,ixrt
        do j=1,jxrt
!!!    -simple uniform disaggregation (8.31.06)
           if (gw_strm_msk(i,j).gt.0) then

            if(GWBASESWCRT.eq.1) then  !calc stream inflow from exponential bucket... (m^3/s to mm)
             qinflowbase(i,j) = qout_gwsubbas(gw_strm_msk(i,j))*1000.*DT/ &
                gwbas_pix_ct(gw_strm_msk(i,j))/dist(i,j,9) !(mm)

            elseif (GWBASESWCRT.eq.2) then  !calc stream inflow from passthrough/steady-state bucket (m^3 to mm)
             qinflowbase(i,j) = qout_gwsubbas(gw_strm_msk(i,j))*1000./ &
                gwbas_pix_ct(gw_strm_msk(i,j))/dist(i,j,9) !(mm)

            end if

           end if
        end do
      end do


!!!    - weighted redistribution...(need to pass accum weights (slope) in...)
!        NOT FINISHED just BASIC framework...
!         do bas=1,numbasns
!           do k=1,gwbas_pix_ct(bas)
!             qinflowbase(i,j) = k*slope
!           end do
!         end do

   return

!------------------------------------------------------------------------------
   End subroutine simp_gw_buck
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!DJG   Wedge-Aquifer Scheme (TBA)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!DJG   TOPMODEL Scheme (TBA)
!------------------------------------------------------------------------------
#ifdef MPP_LAND
   subroutine pix_ct_1(in_gw_strm_msk,ixrt,jxrt,gwbas_pix_ct,numbasns)
      USE module_mpp_land
      implicit none
      integer ::    i,j,ixrt,jxrt,numbasns, bas
      integer,dimension(ixrt,jxrt) :: in_gw_strm_msk
      integer,dimension(global_rt_nx,global_rt_ny) :: gw_strm_msk
      real,dimension(numbasns) :: gwbas_pix_ct 

      gw_strm_msk = 0
      call write_IO_rt_int(in_gw_strm_msk, gw_strm_msk)    

      if(my_id .eq. IO_id) then
         gwbas_pix_ct = 0.
         do bas = 1,numbasns  
         do i=1,global_rt_nx
           do j=1,global_rt_ny
             if(gw_strm_msk(i,j) .eq. bas) then
                gwbas_pix_ct(gw_strm_msk(i,j)) = gwbas_pix_ct(gw_strm_msk(i,j)) &
                     + 1.0
             endif
           end do
         end do
         end do
      end if
      call mpp_land_bcast_real(numbasns,gwbas_pix_ct)

      return
   end subroutine pix_ct_1
#endif


!------------------------------------------------------------------------------
! Benjamin Fersch  2d groundwater model
!------------------------------------------------------------------------------
   subroutine gw2d_ini(did,dt,dx)
     use module_GW_baseflow_data, only: gw2d
     use module_rt_data, only: rt_domain
     implicit none
     integer did
     real dt,dx
     integer :: jj, ii, iter

	   gw2d(did)%dx=dx
           gw2d(did)%dt=dt
           ! bftodo: develop proper landtype mask
           
           gw2d(did)%compres=0. ! currently not implemented

           
          do iter=1,100
#ifdef HYDRO_D                        
          write(6,*) "Spinup cycle", iter, "of 100"
#endif
           call gwstep(gw2d(did)%ix, gw2d(did)%jx, gw2d(did)%dx, &
             gw2d(did)%ltype, gw2d(did)%elev, gw2d(did)%bot, &
             gw2d(did)%hycond, gw2d(did)%poros, gw2d(did)%compres, &
             gw2d(did)%ho, gw2d(did)%h, gw2d(did)%convgw, &
             gw2d(did)%ebot, gw2d(did)%eocn, gw2d(did)%dt, &
!              gw2d(did)%istep)
             iter)
	   
	     gw2d(did)%ho = gw2d(did)%h
	  
	  ! remove surface excess groundwater for faster initialization
     	  do jj=1,RT_DOMAIN(did)%JXRT
	    do ii=1,RT_DOMAIN(did)%IXRT
	      if((gw2d(did)%elev(ii,jj) - gw2d(did)%ho(ii,jj)) .lt. 0.) then
		gw2d(did)%ho(ii,jj) = gw2d(did)%ho(ii,jj) &
		                    - (gw2d(did)%ho(ii,jj)-gw2d(did)%elev(ii,jj))
	      end if
	    end do
	  end do

          end do

   return
   end subroutine gw2d_ini

   subroutine gw2d_allocate(did, ix, jx, nsoil)
      use module_GW_baseflow_data, only: gw2d
      use module_rt_data, only: rt_domain
      implicit none
      integer ix, jx, nsoil
      integer istatus, did
      
      if(gw2d(did)%allo_status .eq. 1) return
      gw2d(did)%allo_status = 1
      
      gw2d(did)%ix = ix
      gw2d(did)%jx = jx
      
#ifdef MPP_LAND
      if(down_id == -1)  then !  if south border
       gw2d(did)%jts = 1 
      else
       gw2d(did)%jts = 2
endif

      if(up_id == -1)    then !if north border
        gw2d(did)%jte = jx
      else
        gw2d(did)%jte = jx-1
      endif

      if(left_id == -1)  then !if west border
        gw2d(did)%its = 1
      else
        gw2d(did)%its = 2
      endif

      if(right_id == -1) then ! if east border
        gw2d(did)%ite = ix
      else
        gw2d(did)%ite = ix-1
      endif

#else
      gw2d(did)%its = 1
      gw2d(did)%ite = ix
      gw2d(did)%jts = 1
      gw2d(did)%jte = jx
#endif

      allocate(gw2d(did)%ltype  (ix,jx))
      allocate(gw2d(did)%elev   (ix,jx))
      allocate(gw2d(did)%bot    (ix,jx))
      allocate(gw2d(did)%hycond (ix,jx))
      allocate(gw2d(did)%poros  (ix,jx))
      allocate(gw2d(did)%compres(ix,jx))
      allocate(gw2d(did)%ho     (ix,jx))
      allocate(gw2d(did)%h      (ix,jx))
      allocate(gw2d(did)%convgw (ix,jx))

      allocate(gw2d(did)%qgw_chanrt (ix,jx))
      
      
      ! TODO allocate only if gwSoilCoupling is active
      allocate(gw2d(did)%qsgwrt (ix,jx))
      allocate(gw2d(did)%qsgw   (rt_domain(did)%ix,rt_domain(did)%jx))

    end subroutine gw2d_allocate


    subroutine gwstep(ix, jx, dx,              &
		      ltype, elev, bot,        &
		      hycond, poros, compres,  &
                      ho, h, convgw,           &
                      ebot, eocn,              &
		      dt, istep)

! New (volug): calling routines use change in head, convgw = d(h-ho)/dt.

! Steps ground-water hydrology (head) through one timestep.
! Modified from Prickett and Lonnquist (1971), basic one-layer aquifer 
! simulation program, with mods by Zhongbo Yu(1997).
! Solves S.dh/dt = d/dx(T.dh/dx) + d/dy(T.dh/dy) + "external sources"
! for a single layer, where h is head, S is storage coeff and T is 
! transmissivity. 3-D arrays in main program (hycond,poros,h,bot)
! are 2-D here, since only a single (uppermost) layer is solved.
! Uses an iterative time-implicit ADI method.

! use module_hms_constants



      integer, intent(in) :: ix, jx

      integer, intent(in), dimension(ix,jx) ::  ltype     ! land-sfc type  (supp)
      real,    intent(in), dimension(ix,jx) ::  &
        elev,           &  ! elev/bathymetry of sfc rel to sl (m) (supp)
        bot,            &  ! elev. aquifer bottom rel to sl (m)   (supp)
        hycond,         &  ! hydraulic conductivity (m/s per m/m) (supp)
        poros,          &  ! porosity (m3/m3)                     (supp)
        compres,        &  ! compressibility (1/Pa)               (supp)
        ho                 ! head at start of timestep (m)        (supp)

      real,    intent(inout), dimension(ix,jx) ::  &
        h,              &  ! head, after ghmcompute (m)           (ret)
        convgw             ! convergence due to gw flow (m/s)     (ret)

      real, intent(inout) :: ebot, eocn
     


      integer ::  istep !, dt
      real, intent(in) :: dt, dx

! #endif      
!       eocn  = mean spurious sink for h_ocn = sealev fix (m/s)(ret)
!               This equals the total ground-water flow across 
!               land->ocean boundaries.
!       ebot  = mean spurious source for "bot" fix (m/s) (returned)
!       time  = elapsed time from start of run (sec)
!       dt = timestep length (sec)
!       istep = timestep counter

! Local arrays:

      real, dimension(ix,jx)   :: sf2    ! storage coefficient (m3 of h2o / bulk m3)
      real, dimension(ix,jx,2) ::   t    ! transmissivity (m2/s)..1 for N-S,..2 for E-W

#ifdef MPP_LAND
      real, dimension(:,:), allocatable :: aa, &         ! tridiagonal matrix lower diagonal
                                           bb, &         ! tridiagonal matrix main diagonal
                                           cc, &         ! tridiagonal matrix upper diagonal
                                           dd, &         ! right hand side
                                           b2, &          
                                           c2, &          
                                           rhs, &          
                                           wk, &           
                                           hh           
      real, dimension(:), allocatable ::   xfac, &
                                           zfac
#else                                         
      real, dimension(:), allocatable :: aa, &         ! tridiagonal matrix lower diagonal
                                         bb, &         ! tridiagonal matrix main diagonal
                                         cc, &         ! tridiagonal matrix upper diagonal
                                         dd, &         ! right hand side
                                         hh            ! solution vector
#endif
      real, parameter    :: botinc = 0.01  ! re-wetting increment to fix h < bot
!     parameter (botinc = 0.  )  ! re-wetting increment to fix h < bot
                                 ! (m); else no flow into dry cells
      real, parameter    :: delskip = 0.005 ! av.|dhead| value for iter.skip out(m)
      integer, parameter :: itermax = 1    ! maximum number of iterations
      integer, parameter :: itermin = 1    ! minimum number of iterations
      real, parameter    :: sealev = 1000.     ! sea-level elevation (m)

      integer            :: its, ite, jts, jte, ifs, ife, jfs, jfe, &
                            xdim, ydim, fxdim, fydim
                          
! die müssen noch sortiert, geprüft und aufgeräumt werden
      integer ::                &
        iter,                   &
        j,                      &
        i,                      &
        jp,                     &
        ip,                     &
        n,                      &
        ierr,                   &
        ier,                    &
        ioffs,                  &
        joffs
        
!       real :: su, sc, shp, bb, aa, cc, w, zz, tareal, dtoa, dtot
      real ::                   &
        dy,                     &
        e,                      &
        su,                     &
        sc,                     &
        shp,                    &
        w,                      &
        ha,                     &
        delcur,                 &
        dtot,                   &
        dtoa,                   &
        darea,                  &
        tareal,                 &
        zz

#ifdef MPP_LAND
      real mpiDelcur
      integer mpiSize
#endif



dy = dx
darea = dx*dy

! define indexes for parallel execution

#ifdef MPP_LAND
if(down_id == -1)  then !  if south border
  jts = 1 
else
  jts = 2
endif

if(up_id == -1)    then !if north border
  jte = jx
else
  jte = jx-1
endif

if(left_id == -1)  then !if west border
  its = 1
else
  its = 2
endif

if(right_id == -1) then ! if east border
  ite = ix
else
  ite = ix-1
endif

#else
its = 1
ite = ix
jts = 1
jte = jx
#endif

ifs = 1
ife = ix
jfs = 1
jfe = jx


fxdim = ife-ifs+1 
fydim = jfe-jfs+1
 xdim = ite-its+1 
 ydim = jte-jts+1

     
      call scopy (fxdim*fydim, ho(ifs:ife,jfs:jfe), 1,    &
                  h(ifs:ife,jfs:jfe), 1)


!       Top of iterative loop for (not anymore ADI) solution

      iter = 0
!~~~~~~~~~~~~~
   80 continue
!~~~~~~~~~~~~~
      iter = iter+1

      
#ifdef MPP_LAND

       call MPP_LAND_COM_REAL(h, fxdim, fydim, 99)

#endif
      e    = 0.       ! absolute changes in head (for iteration control)
!      eocn = 0.       ! accumulated fixes for h = 0 over ocean (diag)
!      ebot = 0.       ! accumulated fixes for h < bot (diagnostic)

!       Set storage coefficient (sf2)
   
   

    tareal = 0.
      do j=jts,jte
        do i=its,ite


        if(ltype(i,j) .ge. 1) tareal = tareal + darea

!         unconfined water table (h < e): V = poros*(h-b)
!                                         dV/dh = poros
!         saturated to surface (h >= e) : V = poros*(e-b) + (h-e)
!                                         dV/dh = 1
!         (compressibility is ignored)
!
!         su = poros(i,j)*(1.-theta(i,j))    ! old (pre-volug)
          su = poros(i,j)                    ! new (volug)
          sc = 1.
 
          if      (ho(i,j).le.elev(i,j) .and. h(i,j).le.elev(i,j)) then
            sf2(i,j) = su
          else if (ho(i,j).ge.elev(i,j) .and. h(i,j).ge.elev(i,j)) then
            sf2(i,j) = sc
          else if (ho(i,j).le.elev(i,j) .and. h(i,j).ge.elev(i,j)) then
            shp = sf2(i,j) * (h(i,j) - ho(i,j))
            sf2(i,j) = shp * sc / (shp - (su-sc)*(elev(i,j)-ho(i,j)))
          else if (ho(i,j).ge.elev(i,j) .and. h(i,j).le.elev(i,j)) then
            shp = sf2(i,j) * (ho(i,j) - h(i,j))
            sf2(i,j) = shp * su / (shp + (su-sc)*(ho(i,j)-elev(i,j)))
          endif

        enddo
      enddo

#ifdef MPP_LAND
       ! communicate storage coefficient
       call MPP_LAND_COM_REAL(sf2, fxdim, fydim, 99)

#endif

!==========================
!       Column calculations
!==========================

!       Set transmissivities. Use min(h,elev)-bot instead of h-bot,
!       since if h > elev, thickness of groundwater flow is just
!       elev-bot.


      do j=jts,jte
        jp = min (j+1,jfe)
        do i=its,ite
          ip = min (i+1,ife)

          t(i,j,2) = sqrt( abs(                                           &
                        hycond(i, j)*(min(h(i ,j),elev(i ,j))-bot(i ,j))  &
                       *hycond(ip,j)*(min(h(ip,j),elev(ip,j))-bot(ip,j))  &
                         )    )                                           &
                   * (0.5*(dy+dy)) & ! in WRF the dx and dy are usually equal
                   / (0.5*(dx+dx))

          t(i,j,1) = sqrt( abs(                                           &
                        hycond(i,j )*(min(h(i,j ),elev(i,j ))-bot(i,j ))  &
                       *hycond(i,jp)*(min(h(i,jp),elev(i,jp))-bot(i,jp))  &
                         )    )                                           &
                   * (0.5*(dx+dx))  &
                   / (0.5*(dy+dy))


        enddo
      enddo





#ifdef MPP_LAND
      ! communicate transmissivities in x and y direction
       call MPP_LAND_COM_REAL(t(:,:,1), fxdim, fydim, 99)
       call MPP_LAND_COM_REAL(t(:,:,2), fxdim, fydim, 99)
#endif

#ifdef MPP_LAND
allocate(aa(jts:jte,its:ite))
allocate(bb(jts:jte,its:ite))
allocate(cc(jts:jte,its:ite))
allocate(dd(jts:jte,its:ite))
allocate(c2(1:ydim,1:xdim))
allocate(b2(1:ydim,1:xdim))
allocate(wk(1:ydim,1:xdim))
allocate(hh(0:ydim+1,0:xdim+1))
allocate(xfac(1:ydim))
allocate(zfac(1:ydim))
#else
allocate(aa(jfs:jfe))
allocate(bb(jfs:jfe))
allocate(cc(jfs:jfe))
allocate(dd(jfs:jfe))
allocate(hh(jfs:jfe))

!-------------------
      do i=ifs,ife
!-------------------

!>>>>>>>>>>>>>>>>>>>>
        do j=jfs,jfe
!>>>>>>>>>>>>>>>>>>>>
#endif
#ifndef MPP_LAND
          bb(j) = (sf2(i,j)/dt) * darea
          dd(j) = ( ho(i,j)*sf2(i,j)/dt ) * darea
          aa(j) = 0.0
          cc(j) = 0.0

          if ((j-jfs) /= 0) then 
           aa(j) = -t(i,j-1,1)
           bb(j) = bb(j) + t(i,j-1,1)
	  endif

          if ((j-jfe) /= 0) then
           cc(j) = -t(i,j,1)
           bb(j) = bb(j) + t(i,j,1)
	  endif

          if ((i-ifs) /= 0) then
           bb(j) = bb(j) + t(i-1,j,2)
           dd(j) = dd(j) + h(i-1,j)*t(i-1,j,2)
	  endif

          if ((i-ife) /= 0) then
           bb(j) = bb(j) + t(i,j,2)
           dd(j) = dd(j) + h(i+1,j)*t(i,j,2)
	  endif

!>>>>>>>>>>>>>>>
	end do
!>>>>>>>>>>>>>>>

  call trdiagSolve(aa, bb, cc, dd, hh, fydim)

  h(i,:) = hh
  end do
  
deallocate(aa)
deallocate(bb)
deallocate(cc)
deallocate(dd)
deallocate(hh)

#else
!-------------------
      do i=its,ite
!-------------------

!>>>>>>>>>>>>>>>>>>>>
        do j=jts,jte
!>>>>>>>>>>>>>>>>>>>>
          bb(j,i) = (sf2(i,j)/dt) * darea
          dd(j,i) = ( ho(i,j)*sf2(i,j)/dt ) * darea
          aa(j,i) = 0.0
          cc(j,i) = 0.0

          if (((j-jfs) /= 0)) then 
           aa(j,i) = -t(i,j-1,1)
           bb(j,i) = bb(j,i) + t(i,j-1,1)
	  endif

          if (((j-jfe) /= 0)) then
           cc(j,i) = -t(i,j,1)
           bb(j,i) = bb(j,i) + t(i,j,1)
	  endif

          if (((i-ifs) /= 0)) then
           bb(j,i) = bb(j,i) + t(i-1,j,2)
           dd(j,i) = dd(j,i) + h(i-1,j)*t(i-1,j,2)
	  endif

          if (((i-ife) /= 0)) then
           bb(j,i) = bb(j,i) + t(i,j,2)
           dd(j,i) = dd(j,i) + h(i+1,j)*t(i,j,2)
	  endif

!>>>>>>>>>>>>>>>
	end do
!>>>>>>>>>>>>>>>

!-------------
  end do
!-------------

    if(up_down_np .gt. 1) then
        call sub_n_form(xdim, ydim, aa, &
                        bb, cc, &
                        dd, &
                        c2, b2, hh, wk, xfac, zfac, &
                        up_down_p+1, up_down_np, 2)

	
	call parzsolv1(c2, b2, hh, 1., my_id+1, up_down_p+1, &
	                xdim, ydim, left_right_np, up_down_np)

    else
        call sub_tri_solv(xdim,ydim,aa(jts:jte,its:ite), &
                          bb(jts:jte,its:ite), cc(jts:jte,its:ite), &
                          dd(jts:jte,its:ite), &
                          hh, wk,xfac,zfac,2)
    endif

ioffs = its-1
joffs = jts-1
!-------------------
      do i=its,ite
!-------------------

!>>>>>>>>>>>>>>>>>>>>
        do j=jts,jte
!>>>>>>>>>>>>>>>>>>>>

              h(i,j) = hh(j-joffs,i-ioffs)
	      
	 end do
     end do
	      
#endif 

#ifdef MPP_LAND

       call MPP_LAND_COM_REAL(h, fxdim, fydim, 99)

#endif


!=======================
!       Row calculations
!=======================

!       set transmissivities (same as above)


      do j=jts,jte
        jp = min (j+1,jfe)
        do i=its,ite
          ip = min (i+1,ife)
          t(i,j,2) = sqrt( abs(                                            &
                        hycond(i, j)*(min(h(i ,j),elev(i ,j))-bot(i ,j))   &
                       *hycond(ip,j)*(min(h(ip,j),elev(ip,j))-bot(ip,j))   &
                         )    )                                            &
                   * (0.5*(dy+dy))                                         &
                   / (0.5*(dx+dx))

          t(i,j,1) = sqrt( abs(                                            &
                        hycond(i,j )*(min(h(i,j ),elev(i,j ))-bot(i,j ))   &
                       *hycond(i,jp)*(min(h(i,jp),elev(i,jp))-bot(i,jp))   &
                         )    )                                            &
                   * (0.5*(dx+dx))                                         &
                   / (0.5*(dy+dy))


        enddo
      enddo

#ifdef MPP_LAND
      ! communicate transmissivities in x and y direction
       call MPP_LAND_COM_REAL(t(:,:,1), fxdim, fydim, 99)
       call MPP_LAND_COM_REAL(t(:,:,2), fxdim, fydim, 99)
#endif

#ifndef MPP_LAND     
allocate(aa(ifs:ife))
allocate(bb(ifs:ife))
allocate(cc(ifs:ife))
allocate(dd(ifs:ife))
allocate(hh(ifs:ife))
#endif

#ifndef MPP_LAND     

!-------------------
      do j=jfs,jfe
!-------------------


!>>>>>>>>>>>>>>>>>>>>
        do i=ifs,ife
!>>>>>>>>>>>>>>>>>>>>
          bb(i) = (sf2(i,j)/dt) * darea
          dd(i) = ( ho(i,j)*sf2(i,j)/dt ) * darea
          aa(i) = 0.0
          cc(i) = 0.0

          if ((j-jfs) /= 0) then
           bb(i) = bb(i) + t(i,j-1,1)
           dd(i) = dd(i) + h(i,j-1)*t(i,j-1,1)
	  endif
 
          if ((j-jfe) /= 0) then
           dd(i) = dd(i) + h(i,j+1)*t(i,j,1)
           bb(i) = bb(i) + t(i,j,1)
	  endif

          if ((i-ifs) /= 0) then
           bb(i) = bb(i) + t(i-1,j,2)
           aa(i) = -t(i-1,j,2)
	  endif

          if ((i-ife) /= 0) then
           bb(i) = bb(i) + t(i,j,2)
           cc(i) = -t(i,j,2)
	  endif

!>>>>>>>>>>>>>>>
	end do
!>>>>>>>>>>>>>>>

  call trdiagSolve(aa, bb, cc, dd, hh, fxdim)

  h(:,j) = hh
  end do
  
#else
!-------------------
      do i=its,ite
!-------------------

!>>>>>>>>>>>>>>>>>>>>
        do j=jts,jte
!>>>>>>>>>>>>>>>>>>>>
          bb(j,i) = (sf2(i,j)/dt) * darea
          dd(j,i) = ( ho(i,j)*sf2(i,j)/dt ) * darea
          aa(j,i) = 0.0
          cc(j,i) = 0.0

          if (((j-jfs) /= 0)) then
           bb(j,i) = bb(j,i) + t(i,j-1,1)
           dd(j,i) = dd(j,i) + h(i,j-1)*t(i,j-1,1)
	  endif
 
          if (((j-jfe) /= 0)) then
           dd(j,i) = dd(j,i) + h(i,j+1)*t(i,j,1)
           bb(j,i) = bb(j,i) + t(i,j,1)
	  endif

          if (((i-ifs) /= 0)) then
           bb(j,i) = bb(j,i) + t(i-1,j,2)
           aa(j,i) = -t(i-1,j,2)
	  endif

          if (((i-ife) /= 0)) then
           bb(j,i) = bb(j,i) + t(i,j,2)
           cc(j,i) = -t(i,j,2)
	  endif
	  
!>>>>>>>>>>>>>>>
	end do
!>>>>>>>>>>>>>>>

!-------------
end do
!-------------

    if(left_right_np .gt. 1) then

! 3 c(,)  -- subdiagonal elements of tridiagonal systems
! 4 a(,)  -- diagonal elements of tridiagonal systems
! 5 b(,)  -- superdiagonal elements of tridiagonal systems
! 6 r(,)  -- right-hand side elements of tridiagonal systems
! 7 c2(,) -- front-leg elements of N-systems
! 8 b2(,) -- back-leg elements of N-systems
! 9 r2(,) -- right-hand side elements of N-systems (0:ydim+1,0:xdim+1)
! 10 wk(,) -- work array with same dimensions as a, b, c, etc.

        call sub_n_form(xdim, ydim, aa, &
                        bb, cc, &
                        dd, &
                        c2, b2, hh, wk, xfac, zfac, &
                        left_right_p+1, left_right_np, 1)
	
        call parxsolv1(c2, b2, hh, 1., my_id+1, left_right_p+1, &
	                xdim, ydim, left_right_np, up_down_np)

    else
        call sub_tri_solv(xdim,ydim,aa, &
                          bb, cc, &
                          dd, &
                          hh, wk,xfac,zfac,1)
    endif
ioffs = its-1
joffs = jts-1
!-------------------
      do i=its,ite
!-------------------

!>>>>>>>>>>>>>>>>>>>>
        do j=jts,jte
!>>>>>>>>>>>>>>>>>>>>

               h(i,j) = hh(j-joffs,i-ioffs)

      end do
     end do
	      
deallocate(b2)
deallocate(c2)
deallocate(wk)
deallocate(xfac)
deallocate(zfac)
#endif 
deallocate(aa)
deallocate(bb)
deallocate(cc)
deallocate(dd)
deallocate(hh)

! fix head < bottom of aquifer
 
      do j=jts,jte
        do i=its,ite
          if (ltype(i,j).eq.1 .and. h(i,j).le.bot(i,j)+botinc) then

            e = e +  bot(i,j) + botinc - h(i,j)
!             ebot = ebot + (bot(i,j)+botinc-h(i,j))*sf2(i,j)*darea(i,j)
            ebot = ebot + (bot(i,j)+botinc-h(i,j))*sf2(i,j)*darea

            h(i,j) = bot(i,j) + botinc
          endif
        enddo
      enddo
!        maintain head = sea level for ocean (only for adjacent ocean,
!        rest has hycond=0)

      do j=jts,jte
        do i=its,ite
          if (ltype(i,j).eq.2) then

	    eocn = eocn + (h(i,j)-sealev)*sf2(i,j)*darea
!             eocn = eocn + (h(i,j)-sealev)*sf2(i,j)*darea(i,j)

!             h(i,j) = sealev (no update of outer boundary cells)
          endif
        enddo
      enddo

!        Loop back for next ADI iteration

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      delcur = e/(xdim*ydim)

!       print*, 'delcur before mpi:', delcur

#ifdef MPP_LAND

call mpi_reduce(delcur, mpiDelcur, 1, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
call MPI_COMM_SIZE( MPI_COMM_WORLD, mpiSize, ierr ) 

if(my_id .eq. IO_id) delcur = mpiDelcur/mpiSize

call mpi_bcast(delcur, 1, mpi_real, 0, MPI_COMM_WORLD, ierr)

#endif

!       if ( (delcur.gt.delskip*dt/86400. .and. iter.lt.itermax)      &
      if ( (delcur.gt.delskip .and. iter.lt.itermax)      &
           .or. iter.lt.itermin ) then
           
#ifdef HYDRO_D 

#ifdef MPP_LAND
if(my_id .eq. IO_id)  write(6,*) "Iteration", iter, "of", itermax, "error:", delcur
#else
                      write(6,*) "Iteration", iter, "of", itermax, "error:", delcur
#endif

#endif

      goto 80
      endif
      
#ifdef MPP_LAND

       call MPP_LAND_COM_REAL(h, fxdim, fydim, 99)

#endif

      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!        Compute convergence rate due to ground water flow (returned)

      do j=jts,jte
        do i=its,ite
          if (ltype(i,j).eq.1) then
            convgw(i,j) = sf2(i,j) * (h(i,j)-ho(i,j)) / dt
          else
            convgw(i,j) = 0.
          endif
        enddo
      enddo

!      call MPP_LAND_COM_REAL(convgw, fxdim, fydim, 99)

!        Diagnostic water conservation check for this timestep

      dtot = 0.     ! total change in water storage (m3)
      dtoa = 0.

      do j=jts,jte
        do i=its,ite
          if (ltype(i,j).eq.1) then

	    dtot = dtot + sf2(i,j) *(h(i,j)-ho(i,j)) * darea
            dtoa = dtoa + sf2(i,j) * abs(h(i,j)-ho(i,j)) * darea

!             dtot = dtot + sf2(i,j) *(h(i,j)-ho(i,j)) * darea(i,j)
!             dtoa = dtoa + sf2(i,j) * abs(h(i,j)-ho(i,j)) * darea(i,j)
          endif
        enddo
      enddo

      dtot = (dtot/tareal)/dt   ! convert to m/s, rel to land area
      dtoa = (dtoa/tareal)/dt
      eocn = (eocn/tareal)/dt
      ebot = (ebot/tareal)/dt

      zz = 1.e3 * 86400.                    ! convert printout to mm/day
#ifdef HYDRO_D
        write (*,900)                         &
          dtot*zz, dtoa*zz, -eocn*zz, ebot*zz,     &
          (dtot-(-eocn+ebot))*zz
#endif
  900 format                                       &
        (3x,'    dh/dt       |dh/dt|        ocnflx        botfix',&
            '                  ','      ghmerror'  &
!         /3x,4f9.4,2(9x),e14.4)
        /3x,5(e14.4))
      
      return
      end subroutine gwstep
      
      
      SUBROUTINE SCOPY (NT, ARR, INCA, BRR, INCB)
!
!        Copies array ARR to BRR, incrementing by INCA and INCB
!        respectively, up to a total length of NT words of ARR.
!        (Same as Cray SCOPY.)
!
      real, DIMENSION(*) :: ARR, BRR
      integer :: ia, nt, inca, incb, ib
!
      IB = 1
      DO 10 IA=1,NT,INCA
         BRR(IB) = ARR(IA)
         IB = IB + INCB
   10 CONTINUE
!
      RETURN
      END SUBROUTINE SCOPY

      
subroutine trdiagSolve(a,b,c,rhs,x,n)

      implicit none
      
      integer,intent(in) :: n
      real,dimension(n),intent(in) :: a, b, c, rhs
      real,dimension(n),intent(out) :: x
      real,dimension(n) :: cp, dp
      real :: m
      integer i

! initialize c-prime and d-prime
        cp(1) = c(1)/b(1)
        dp(1) = rhs(1)/b(1)
! solve for vectors c-prime and d-prime
         do i = 2,n
           m = b(i)-cp(i-1)*a(i)
           cp(i) = c(i)/m
           dp(i) = (rhs(i)-dp(i-1)*a(i))/m
         enddo
! initialize x
         x(n) = dp(n)
! solve for x from the vectors c-prime and d-prime
        do i = n-1, 1, -1
          x(i) = dp(i)-cp(i)*x(i+1)
        end do
      

end subroutine trdiagSolve
      
      
subroutine gwSoilFlux(did)

  use module_namelist, only: nlst_rt
  use module_RT_data, only: rt_domain  
  use module_GW_baseflow_data, only: gw2d
  
  implicit none
  
  integer, intent(in)	:: did
  
  
  real, dimension(rt_domain(did)%ixrt,rt_domain(did)%jxrt) :: smcrel, ztrans, headChange
  integer :: nsoil, i, j, k
  
  
  
! Step 1, collect data

! relative soil moisture content of lowest soil layer (1 = saturated)
  nsoil = nlst_rt(did)%nsoil
  smcrel = RT_DOMAIN(did)%SMCRT(:,:,nsoil) / RT_DOMAIN(did)%SMCMAXRT(:,:,nsoil)

! depth of transition zone from lowest soil layer to groundwater head (in cm)
  ztrans = (rt_domain(did)%elrt + nlst_rt(did)%zsoil8(nsoil)) - gw2d(did)%ho
  ztrans = ztrans * 100
  
! Step 2, compute flux  TODO: deal with cases where gwhead > soil model layers

  do j=gw2d(did)%jts, gw2d(did)%jte
    do i=gw2d(did)%its, gw2d(did)%ite
        if((ztrans(i,j) > 0) .and. (rt_domain(did)%soiltypRT(i,j) < 13)) then
	  call  darcyGwSoil(ztrans(i,j), smcrel(i,j), rt_domain(did)%soiltypRT(i,j), gw2d(did)%qsgwrt(i,j))
	else if(ztrans(i,j) < 0) then
	  do k=nsoil,1,-1
	     if(ztrans(i,j) .le. nlst_rt(did)%zsoil8(nsoil)*100) then
	       RT_DOMAIN(did)%SMCRT(i,j,k) = RT_DOMAIN(did)%SMCMAXRT(i,j,k)
	     else
	       
	     end if
	  end do
	end if
    end do
  end do

          ! sign convention
          ! qsgwrt < 0 -> downward flux
          ! qsgwrt > 0 -> upward flux

! TOcheck Step 3, adapt groundwater head (assuming not time lag for percolation / capillary rise flow)

! 	   modify gw-head before gwstep call with respect to specific yield of the 
! 	   aquifer and the computed flux (qsgwrt)

 headChange = gw2d(did)%qsgwrt * gw2d(did)%dt / gw2d(did)%poros
 gw2d(did)%ho = gw2d(did)%ho + headChange
  
end subroutine gwSoilFlux
      
subroutine darcyGwSoil(Z, s, soil, q_darcy)

implicit none

INTEGER, INTENT (IN)  :: soil ! soiltype

REAL :: sig_a, sig_b, sig_c

REAL, DIMENSION(9)    :: k_para
REAL, INTENT (IN)     :: Z, s
REAL, INTENT (OUT)    :: q_darcy
real                  :: beta,alpha,q_cap,b,ks,aep,c,q_grav,y,fac

real, dimension(9,12) :: &
      k_soil = reshape((/&
0.0778, 3.9939, 0.2913, 4.0801, 0.1386, 4.0500, -12.10, 0.3950, 1.0560,&
0.0924, 4.8822, 0.2674, 3.8915, 0.1365, 4.3800, -09.00, 0.4100, 0.9380,&
0.0367, 4.5259, 0.2446, 4.2849, 0.1208, 4.9000, -21.80, 0.4350, 0.2080,&
0.0101, 3.6896, 0.2153, 4.2765, 0.0887, 5.3000, -78.60, 0.4850, 0.0432,&
0.0101, 3.6896, 0.2153, 4.2765, 0.0887, 5.3000, -78.60, 0.4850, 0.0432,&
0.0169, 2.9936, 0.2858, 4.3738, 0.1026, 5.3900, -47.80, 0.4510, 0.0417,&
0.0271, 4.4743, 0.2587, 3.9055, 0.0920, 7.1200, -29.90, 0.4200, 0.0378,&
0.0227, 4.3768, 0.2658, 3.8234, 0.0843, 7.7500, -35.60, 0.4770, 0.0102,&
0.0127, 6.6836, 0.1725, 3.7512, 0.0703, 8.5200, -63.00, 0.4760, 0.0147,&
0.0530, 9.2423, 0.1859, 3.3688, 0.0728, 10.400, -15.30, 0.4260, 0.0130,&
0.0165, 5.3972, 0.2479, 3.5549, 0.0641, 10.400, -49.00, 0.4920, 0.0062,&
0.0200, 6.0106, 0.2474, 3.4788, 0.0622, 11.400, -40.50, 0.4820, 0.0077/&
),(/9,12/))



 k_para  = k_soil(:,soil)
 sig_a   = 1 - exp( -1 * k_para(1) * Z)
 sig_b   = k_para(2) * Z**k_para(3)
 sig_c   = k_para(4) * exp( -1 * Z**k_para(5))
 y       = sig_a/(1  + exp(sig_b * (s - sig_c))) !solving equation (20) in Boogart et al.

 b   =   k_para(6)
 ks  =   k_para(9)
 aep =  -k_para(7)

 c      =  2 * b  + 3
 q_grav = -1 * ks * s**c

! alp is constant from equation (13) of paper
beta  = 2 + 3 / b
alpha = 1 + 1.5 /  (beta - 1)
q_cap = ks * alpha * (aep / Z)**beta


! limit for exteme gradients with q >> saturated hydraulic conductivity
if(q_cap > ks) q_cap = ks
if(q_grav < -ks) q_grav = -ks

q_darcy = y * q_cap + q_grav ![cm/min]

fac     = 1./6000.
q_darcy = q_darcy * fac
q_cap   = q_cap   * fac
q_grav  = q_grav  * fac

!returns q_darcy in [m/s]

end subroutine darcyGwSoil



subroutine aggregateQsgw(did)

  use module_namelist, only: nlst_rt
  use module_RT_data, only: rt_domain  
  use module_GW_baseflow_data, only: gw2d

  implicit none

   integer, intent(in) :: did
   integer :: j,i, ixxRT, jyyRT, m,n
   real :: agg


    do j=1,rt_domain(did)%jx
     do i=1,rt_domain(did)%ix

       agg= 0.
       
       do m=nlst_rt(did)%aggfactRT-1,0,-1
         do n=nlst_rt(did)%aggfactRT-1,0,-1


	    ixxRT = i * nlst_rt(did)%aggfactRT-n
	    jyyRT = j * nlst_rt(did)%aggfactRT-m

           
#ifdef MPP_LAND
	    if(left_id.ge.0) ixxRT=ixxRT+1
	    if(down_id.ge.0) jyyRT=jyyRT+1
#endif
             agg = agg + gw2d(did)%qsgwRT(ixxRT, jyyRT)
           end do
         end do
	
            gw2d(did)%qsgw(i,j) = agg/(nlst_rt(did)%aggfactRT**2)
       end do
     end do



end subroutine aggregateQsgw

!23456789012345678901234567890123456789012345678901234567890123456789012
! Parallel machine precision tridiagonal solver.
! The send-receive fuse is lit from both ends in this subroutine.
! The processors are self-synchronized by blocking receives,
! and the load balance is so good that non-blocking receives
! are not needed.

      subroutine parzsolv1(c,b,r,ct,pid,z_pid, &
	                    xsps, zsps, xdns, zdns)

      implicit none

      integer, intent(in) :: XSPS, &
                             ZSPS, &
                             XDNS, &
                             ZDNS
                             
      real, dimension(ZSPS, XSPS), intent(inout) ::  c, &
                                                     b
      real	CLK_PER
      parameter	(CLK_PER = 6.66666667e-9)

      real, dimension(0:ZSPS+1, 0:XSPS+1), intent(inout) ::  r
      
      real, dimension(XSPS,2) :: zn, zntmp
      
      real, dimension(XSPS)   :: t1, t2, fac

      real :: clockdt, click
      real :: ct, ti, tf, dt

      integer :: pid, z_pid
      integer :: i, j, sndr_pid, msg_type, cnt, ackn
      integer :: stat
      
      integer	ZN_REC
      parameter	(ZN_REC = 46)

#ifdef TIMING
      dt = clockdt()
#endif

      if (z_pid .eq. 1) then

! Load (ZSPS,j)th equations into passing arrays.
        do 10 j = 1, XSPS
          zntmp(j,1) = b(ZSPS,j)
          zntmp(j,2) = r(ZSPS,j)
   10   continue

        cnt = 2*XSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (ZSPS,j)th equations.
! !       call SEND(pid+XDNS,ZN_REC,zn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,zn,len,1,stat)
!         call PVMFSEND(pid+XDNS,ZN_REC,stat)
! ! Receive (ZSPS+1,j)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,zn,pid+XDNS,ZN_REC,len)
!         call PVMFRECV(pid+XDNS,ZN_REC,stat)
!         call PVMFUNPACK(BYTE1,zn,len,1,stat)
call MPI_SEND(zntmp, cnt, MPI_REAL, pid+XDNS-1, ZN_REC, MPI_COMM_WORLD, stat)
call MPI_RECV(zn, cnt, MPI_REAL, pid+XDNS-1, ZN_REC, MPI_COMM_WORLD, mpp_status, stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 20 j = 1, XSPS
! Backward elimination in (ZSPS,j)th equations to get
! r(ZSPS,j).
        fac(j) = 1./(1. - b(ZSPS,j)*zn(j,1))
	r(ZSPS,j) = (r(ZSPS,j)-b(ZSPS,j)*zn(j,2))*fac(j)
! Forward elimination in (ZSPS+1,j)th equations to get
! r(ZSPS+1,j).
        r(ZSPS+1,j) = zn(j,2) - zn(j,1)*r(ZSPS,j)
! Completion of backward elimination to get remaining unknowns.
        do 30 i = 1, ZSPS-1
          r(i,j) = r(i,j) - b(i,j)*r(ZSPS,j)
   30   continue
   20   continue

      else if (z_pid .le. ZDNS/2) then

        cnt = 2*XSPS
#ifdef TIMING
        ti = click()
#endif
! ! Receive (0,j)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,zn,pid-XDNS,ZN_REC,len)
!         call PVMFRECV(pid-XDNS,ZN_REC,stat)
!         call PVMFUNPACK(BYTE1,zn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

! Forward elimination in (j,1)th equations.
	do 40 j = 1, XSPS
          fac(j) = 1./(1. - c(1,j)*zn(j,1))
! Check for singular matrix (debugging only)
          b(1,j) = b(1,j)*fac(j)
          r(1,j) = (r(1,j) - c(1,j)*zn(j,2))*fac(j)
! Forward elimination in (ZSPS,j)th equations.
          fac(j) = 1./(1. - c(ZSPS,j)*b(1,j))
! Check for singular matrix (debugging only)
          b(ZSPS,j) = b(ZSPS,j)*fac(j)
          r(ZSPS,j) = (r(ZSPS,j)-c(ZSPS,j)*r(1,j))*fac(j)
! Store (0,j)th equations for later recovery of r(0,j).
          t1(j) = zn(j,1)
          t2(j) = zn(j,2)
! Load (ZSPS,j)th equations into passing arrays.
          zn(j,1) = b(ZSPS,j)
          zn(j,2) = r(ZSPS,j)
   40   continue

        cnt = 2*XSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (ZSPS,j)th equations.
! !       call SEND(pid+XDNS,ZN_REC,zn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,zn,len,1,stat)
!         call PVMFSEND(pid+XDNS,ZN_REC,stat)
! ! Receive (ZSPS+1,j)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,zn,pid+XDNS,ZN_REC,len)
!         call PVMFRECV(pid+XDNS,ZN_REC,stat)
!         call PVMFUNPACK(BYTE1,zn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 50 j = 1, XSPS
! Backward elimination in (ZSPS,j)th equations.
          fac(j) = 1./(1. - b(ZSPS,j)*zn(j,1))
! Check for singular matrix (debugging only)
          r(ZSPS,j) = (r(ZSPS,j) - b(ZSPS,j)*zn(j,2))*fac(j)
! Backward elimination in (ZSPS+1,j)th equations.
          r(ZSPS+1,j) = zn(j,2) - zn(j,1)*r(ZSPS,j)
! Backward elimination in (ZSPS,j)th equations.
          r(1,j) = r(1,j) - b(1,j)*r(ZSPS,j)
! Load (1,j)th equations into passing arrays.
          zn(j,1) = 0.
          zn(j,2) = r(1,j)
   50   continue

        cnt = 2*XSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (1,j)th equations.
! !       call SEND(pid-XDNS,ZN_REC,zn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,zn,len,1,stat)
!         call PVMFSEND(pid-XDNS,ZN_REC,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 60 j = 1, XSPS
! Backward elimination in (0,j)th equations.
	r(0,j) = t2(j) - t1(j)*r(1,j)
        do 70 i = 2, ZSPS-1
! Completion of forward and backward elimination to get remaining
! unknowns.
          r(i,j) = r(i,j) - b(i,j)*r(ZSPS,j) - c(i,j)*r(1,j)
   70   continue
   60   continue

      else if (z_pid .lt. ZDNS) then

        cnt = 2*XSPS
#ifdef TIMING
        ti = click()
#endif
! ! Receive (ZSPS+1,j)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,zn,pid+XDNS,ZN_REC,len)
!         call PVMFRECV(pid+XDNS,ZN_REC,stat)
!         call PVMFUNPACK(BYTE1,zn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

	do 80 j = 1, XSPS
! Backward elimination in (ZSPS,j)th equations.
          fac(j) = 1./(1. - b(ZSPS,j)*zn(j,1))
! Check for singular matrix (debugging only)
          c(ZSPS,j) = c(ZSPS,j)*fac(j)
          r(ZSPS,j) = (r(ZSPS,j)-b(ZSPS,j)*zn(j,2))*fac(j)
! Backward elimination in (1,j)th equations.
          fac(j) = 1./(1. - b(1,j)*c(ZSPS,j))
! Check for singular matrix (debugging only)
          c(1,j) = c(1,j)*fac(j)
          r(1,j) = (r(1,j) - b(1,j)*r(ZSPS,j))*fac(j)
! Store (ZSPS+1,j)th equations for later recovery of
! r(ZSPS+1,j).
          t1(j) = zn(j,1)
          t2(j) = zn(j,2)
! Load passing arrays with (1,j)th equations.
          zn(j,1) = c(1,j)
          zn(j,2) = r(1,j)
   80   continue

        cnt = 2*XSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (1,j)th equations.
! !       call SEND(pid-XDNS,ZN_REC,zn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,zn,len,1,stat)
!         call PVMFSEND(pid-XDNS,ZN_REC,stat)
! ! Receive (0,j)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,zn,pid-XDNS,ZN_REC,len)
!         call PVMFRECV(pid-XDNS,ZN_REC,stat)
!         call PVMFUNPACK(BYTE1,zn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

	do 90 j = 1, XSPS
! Forward elimination in (1,j)th equations
          fac(j) = 1./(1. - c(1,j)*zn(j,1))
! Check for singular matrix (debugging only)
          r(1,j) = (r(1,j) - c(1,j)*zn(j,2))*fac(j)
! Backward elimination in (0,j)th equations.
          r(0,j) = zn(j,2) - zn(j,1)*r(1,j)
! Forward elimination in (ZSPS,j)th equations.
          r(ZSPS,j) = r(ZSPS,j) - c(ZSPS,j)*r(1,j)
! Load (ZSPS,j)th equations into passing arrays.
          zn(j,1) = 0.
          zn(j,2) = r(ZSPS,j)
   90   continue

        cnt = 2*XSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (ZSPS,j)th equations.
! !       call SEND(pid+XDNS,ZN_REC,zn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,zn,len,1,stat)
!         call PVMFSEND(pid+XDNS,ZN_REC,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 100 j = 1, XSPS
! Forward elimination in (ZSPS+1,j)th equations to get
! r(ZSPS+1,j).
	r(ZSPS+1,j) = t2(j) - t1(j)*r(ZSPS,j)
        do 110 i = 2, ZSPS-1
! Completion of forward and backward elimination to get remaining unknowns.
          r(i,j) = r(i,j) - c(i,j)*r(1,j) - b(i,j)*r(ZSPS,j)
  110   continue
  100   continue

      else

! Load (1,j)th equations into passing arrays.
	do 120 j = 1, XSPS
          zntmp(j,1) = c(1,j)
          zntmp(j,2) = r(1,j)
  120   continue

        cnt = 2*XSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (1,j)th equations.
! !       call SEND(pid-XDNS,ZN_REC,zn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,zn,len,1,stat)
!         call PVMFSEND(pid-XDNS,ZN_REC,stat)
! ! Receive (0,j)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,zn,pid-XDNS,ZN_REC,len)
!         call PVMFRECV(pid-XDNS,ZN_REC,stat)
!         call PVMFUNPACK(BYTE1,zn,len,1,stat)
call MPI_RECV(zn, cnt, MPI_REAL, pid-XDNS-1, ZN_REC, MPI_COMM_WORLD, mpp_status, stat)
call MPI_SEND(zntmp, cnt, MPI_REAL, pid-XDNS-1, ZN_REC, MPI_COMM_WORLD, stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 130 j = 1, XSPS
! Forward elimination in (1,j)th equations to get r(1,j).
	fac(j) = 1./(1. - c(1,j)*zn(j,1))
! Check for singular matrix (debugging only)
        r(1,j) = (r(1,j) - c(1,j)*zn(j,2))*fac(j)
! Backward elimination in (0,j)th equations to get remaining unknowns.
        r(0,j) = zn(j,2) - zn(j,1)*r(1,j)
	do 140 i = 2, ZSPS
! Completion of forward elimination to get remaining unknowns.
          r(i,j) = r(i,j) - c(i,j)*r(1,j)
  140   continue
  130   continue

      endif

      return
      end subroutine


#ifdef MPP_LAND
!23456789012345678901234567890123456789012345678901234567890123456789012
! Parallel machine precision tridiagonal solver.
! The send-receive fuse is lit from both ends in this subroutine.
! The processors are self-synchronized by blocking receives,
! and the load balance is so good that non-blocking receives
! are not needed.

      subroutine dparzsolv1(c,b,r,ch,bh,rh,ct,pid,z_pid, &
	                    xsps, zsps, xdns, zdns)

      implicit none

! Parameters specifying the size of the problem
! XSPS     XS Per Subdomain
! ZSPS     ZS Per Subdomain
! XDNS     X-Direction Number of Subdomains
! ZDNS     Z-Direction Number of Subdomains
! NSBS   Number of SUBdomainS
! N_XS     Number of X-valueS across the whole grid
! N_ZS     Number of Z-valueS across the whole grid
! N_PTS    Number of PoinTS in the whole grid
! Specify problem size by choosing XSPS, ZSPS, XDNS, and ZDNS

      integer, intent(in) :: XSPS, &
                             ZSPS, &
                             XDNS, &
                             ZDNS
      
      !local
      integer             :: NSBS, &
                             n_xs, &
                             n_zs, &
                             n_pts
                 



      integer	ZN_REC
      parameter	(ZN_REC = 46)


! Clock parameters
!   CLK_PER   CLocK PERiod
      real	CLK_PER
      parameter	(CLK_PER = 6.66666667e-9)



      real, dimension(ZSPS, XSPS), intent(inout) ::  c, &
                                                     b, &
                                                     ch,&
                                                     bh

      real, dimension(0:ZSPS+1, 0:XSPS+1), intent(inout) ::  r, &
                                                             rh

      real, dimension(XSPS)   :: t1, t2, fac, t1h, t2h, fach
      real, dimension(XSPS,4) :: zn, zntmp
      
      real :: clockdt, click
      real :: ct, ti, tf, dt

      integer :: pid, z_pid
      integer :: i, j, sndr_pid, msg_type, cnt, ackn
      integer :: stat


#ifdef TIMING
      dt = clockdt()
#endif

      if (z_pid .eq. 1) then

	print*, 'Node', my_id, "in mode 1"
! Load (ZSPS,j)th equations into passing arrays.
        do j = 1, XSPS
          zntmp(j,1) = b(ZSPS,j)
          zntmp(j,2) = r(ZSPS,j)
          zntmp(j,3) = bh(ZSPS,j)
          zntmp(j,4) = rh(ZSPS,j)
        end do

        cnt = 4*XSPS
#ifdef TIMING
        ti = click()
#endif
! Send (ZSPS,j)th equations.
!       call SEND(pid+XDNS,ZN_REC,zn,len,ackn)
!pvm        call PVMFINITSEND(PVMRAW, stat)
!pvm        call PVMFPACK(BYTE1,zn,len,1,stat)
!pvm        call PVMFSEND(pid+XDNS,ZN_REC,stat)
! call MPI_SENDRECV_REPLACE(zn, cnt, MPI_REAL, pid+XDNS-1, ZN_REC, &
!                           pid+XDNS-1, ZN_REC, &
!                           MPI_COMM_WORLD, stat)

call MPI_SEND(zntmp, cnt, MPI_REAL, pid+XDNS-1, ZN_REC, MPI_COMM_WORLD, stat)
call MPI_RECV(zn, cnt, MPI_REAL, pid+XDNS-1, ZN_REC, MPI_COMM_WORLD, mpp_status, stat)

! Receive (ZSPS+1,j)th equations.
!       call SRECEIVE(sndr_pid,msg_type,zn,pid+XDNS,ZN_REC,len)
!pvm        call PVMFRECV(pid+XDNS,ZN_REC,stat)
!pvm        call PVMFUNPACK(BYTE1,zn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do j = 1, XSPS
! Backward elimination in (ZSPS,j)th equations to get
! r(ZSPS,j).
          fac(j) = 1./(1. - b(ZSPS,j)*zn(j,1))
          r(ZSPS,j) = (r(ZSPS,j)-b(ZSPS,j)*zn(j,2))*fac(j)
! Forward elimination in (ZSPS+1,j)th equations to get
! r(ZSPS+1,j).
          r(ZSPS+1,j) = zn(j,2) - zn(j,1)*r(ZSPS,j)

! Backward elimination in (ZSPS,j)th equations to get
! r(ZSPS,j).
          fach(j) = 1./(1. - bh(ZSPS,j)*zn(j,3))
          rh(ZSPS,j) = (rh(ZSPS,j)-bh(ZSPS,j)*zn(j,4))*fach(j)
! Forward elimination in (ZSPS+1,j)th equations to get
! r(ZSPS+1,j).
          rh(ZSPS+1,j) = zn(j,4) - zn(j,3)*rh(ZSPS,j)
! Completion of backward elimination to get remaining unknowns.
          do i = 1, ZSPS-1
            r(i,j) = r(i,j) - b(i,j)*r(ZSPS,j)
            rh(i,j) = rh(i,j) - bh(i,j)*rh(ZSPS,j)
          end do
         end do

      else if (z_pid .le. ZDNS/2) then
	
	print*, 'Node', my_id, "in mode 2"

        cnt = 4*XSPS
#ifdef TIMING
        ti = click()
#endif
! Receive (0,j)th equations.
!       call SRECEIVE(sndr_pid,msg_type,zn,pid-XDNS,ZN_REC,len)
!pvm        call PVMFRECV(pid-XDNS,ZN_REC,stat)
!pvm        call PVMFUNPACK(BYTE1,zn,len,1,stat)
! call MPI_SENDRECV_REPLACE(zn, cnt, MPI_REAL, pid-XDNS-1, ZN_REC, &
!                           pid-XDNS-1, ZN_REC, &
!                           MPI_COMM_WORLD, stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

! Forward elimination in (j,1)th equations.
	do j = 1, XSPS
          fac(j) = 1./(1. - c(1,j)*zn(j,1))
! Check for singular matrix (debugging only)
          b(1,j) = b(1,j)*fac(j)
          r(1,j) = (r(1,j) - c(1,j)*zn(j,2))*fac(j)
! Forward elimination in (ZSPS,j)th equations.
          fac(j) = 1./(1. - c(ZSPS,j)*b(1,j))
! Check for singular matrix (debugging only)
          b(ZSPS,j) = b(ZSPS,j)*fac(j)
          r(ZSPS,j) = (r(ZSPS,j)-c(ZSPS,j)*r(1,j))*fac(j)
! Store (0,j)th equations for later recovery of r(0,j).
          t1(j) = zn(j,1)
          t2(j) = zn(j,2)
! Load (ZSPS,j)th equations into passing arrays.
          zn(j,1) = b(ZSPS,j)
          zn(j,2) = r(ZSPS,j)

          fach(j) = 1./(1. - ch(1,j)*zn(j,3))
! Check for singular matrix (debugging only)
          bh(1,j) = bh(1,j)*fach(j)
          rh(1,j) = (rh(1,j) - ch(1,j)*zn(j,4))*fach(j)
! Forward elimination in (ZSPS,j)th equations.
          fach(j) = 1./(1. - ch(ZSPS,j)*bh(1,j))
! Check for singular matrix (debugging only)
          bh(ZSPS,j) = bh(ZSPS,j)*fach(j)
          rh(ZSPS,j) = (rh(ZSPS,j)-ch(ZSPS,j)*rh(1,j))*fach(j)
! Store (0,j)th equations for later recovery of r(0,j).
          t1h(j) = zn(j,3)
          t2h(j) = zn(j,4)
! Load (ZSPS,j)th equations into passing arrays.
          zn(j,3) = bh(ZSPS,j)
          zn(j,4) = rh(ZSPS,j)
        end do

        cnt = 4*XSPS
#ifdef TIMING
        ti = click()
#endif
! Send (ZSPS,j)th equations.
!       call SEND(pid+XDNS,ZN_REC,zn,len,ackn)
!pvm        call PVMFINITSEND(PVMRAW, stat)
!pvm        call PVMFPACK(BYTE1,zn,len,1,stat)
!pvm        call PVMFSEND(pid+XDNS,ZN_REC,stat)
! Receive (ZSPS+1,j)th equations.
!       call SRECEIVE(sndr_pid,msg_type,zn,pid+XDNS,ZN_REC,len)
!pvm        call PVMFRECV(pid+XDNS,ZN_REC,stat)
!pvm        call PVMFUNPACK(BYTE1,zn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do j = 1, XSPS
! Backward elimination in (ZSPS,j)th equations.
          fac(j) = 1./(1. - b(ZSPS,j)*zn(j,1))
! Check for singular matrix (debugging only)
          r(ZSPS,j) = (r(ZSPS,j) - b(ZSPS,j)*zn(j,2))*fac(j)
! Backward elimination in (ZSPS+1,j)th equations.
          r(ZSPS+1,j) = zn(j,2) - zn(j,1)*r(ZSPS,j)
! Backward elimination in (ZSPS,j)th equations.
          r(1,j) = r(1,j) - b(1,j)*r(ZSPS,j)
! Load (1,j)th equations into passing arrays.
          zn(j,1) = 0.
          zn(j,2) = r(1,j)

! Backward elimination in (ZSPS,j)th equations.
          fach(j) = 1./(1. - bh(ZSPS,j)*zn(j,3))
! Check for singular matrix (debugging only)
          rh(ZSPS,j) = (rh(ZSPS,j) - bh(ZSPS,j)*zn(j,4))*fach(j)
! Backward elimination in (ZSPS+1,j)th equations.
          rh(ZSPS+1,j) = zn(j,4) - zn(j,3)*rh(ZSPS,j)
! Backward elimination in (ZSPS,j)th equations.
          rh(1,j) = rh(1,j) - bh(1,j)*rh(ZSPS,j)
! Load (1,j)th equations into passing arrays.
          zn(j,3) = 0.
          zn(j,4) = rh(1,j)
        end do

        cnt = 4*XSPS
#ifdef TIMING
        ti = click()
#endif
! Send (1,j)th equations.
!       call SEND(pid-XDNS,ZN_REC,zn,len,ackn)
!pvm        call PVMFINITSEND(PVMRAW, stat)
!pvm        call PVMFPACK(BYTE1,zn,len,1,stat)
!pvm        call PVMFSEND(pid-XDNS,ZN_REC,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do j = 1, XSPS
! Backward elimination in (0,j)th equations.
          r(0,j) = t2(j) - t1(j)*r(1,j)
          rh(0,j) = t2h(j) - t1h(j)*rh(1,j)
          do i = 2, ZSPS-1
! Completion of forward and backward elimination to get remaining
! unknowns.
            r(i,j) = r(i,j) - b(i,j)*r(ZSPS,j) - c(i,j)*r(1,j)
            rh(i,j) = rh(i,j)-bh(i,j)*rh(ZSPS,j)-ch(i,j)*rh(1,j)
          end do
         end do

      else if (z_pid .lt. ZDNS) then
	print*, 'Node', my_id, "in mode 3"

        cnt = 4*XSPS
#ifdef TIMING
        ti = click()
#endif
! Receive (ZSPS+1,j)th equations.
!       call SRECEIVE(sndr_pid,msg_type,zn,pid+XDNS,ZN_REC,len)
!pvm        call PVMFRECV(pid+XDNS,ZN_REC,stat)
!pvm        call PVMFUNPACK(BYTE1,zn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

	do j = 1, XSPS
! Backward elimination in (ZSPS,j)th equations.
          fac(j) = 1./(1. - b(ZSPS,j)*zn(j,1))
! Check for singular matrix (debugging only)
          c(ZSPS,j) = c(ZSPS,j)*fac(j)
          r(ZSPS,j) = (r(ZSPS,j)-b(ZSPS,j)*zn(j,2))*fac(j)
! Backward elimination in (1,j)th equations.
          fac(j) = 1./(1. - b(1,j)*c(ZSPS,j))
! Check for singular matrix (debugging only)
          c(1,j) = c(1,j)*fac(j)
          r(1,j) = (r(1,j) - b(1,j)*r(ZSPS,j))*fac(j)
! Store (ZSPS+1,j)th equations for later recovery of
! r(ZSPS+1,j).
          t1(j) = zn(j,1)
          t2(j) = zn(j,2)
! Load passing arrays with (1,j)th equations.
          zn(j,1) = c(1,j)
          zn(j,2) = r(1,j)

! Backward elimination in (ZSPS,j)th equations.
          fach(j) = 1./(1. - bh(ZSPS,j)*zn(j,3))
! Check for singular matrix (debugging only)
          ch(ZSPS,j) = ch(ZSPS,j)*fach(j)
          rh(ZSPS,j) = (rh(ZSPS,j)-bh(ZSPS,j)*zn(j,4))*fach(j)
! Backward elimination in (1,j)th equations.
          fach(j) = 1./(1. - bh(1,j)*ch(ZSPS,j))
! Check for singular matrix (debugging only)
          ch(1,j) = ch(1,j)*fach(j)
          rh(1,j) = (rh(1,j) - bh(1,j)*rh(ZSPS,j))*fach(j)
! Store (ZSPS+1,j)th equations for later recovery of
! r(ZSPS+1,j).
          t1h(j) = zn(j,3)
          t2h(j) = zn(j,4)
! Load passing arrays with (1,j)th equations.
          zn(j,3) = ch(1,j)
          zn(j,4) = rh(1,j)
        end do

        cnt = 4*XSPS
#ifdef TIMING
        ti = click()
#endif
! Send (1,j)th equations.
!       call SEND(pid-XDNS,ZN_REC,zn,len,ackn)
!pvm        call PVMFINITSEND(PVMRAW, stat)
!pvm        call PVMFPACK(BYTE1,zn,len,1,stat)
!pvm        call PVMFSEND(pid-XDNS,ZN_REC,stat)
! Receive (0,j)th equations.
!       call SRECEIVE(sndr_pid,msg_type,zn,pid-XDNS,ZN_REC,len)
!pvm        call PVMFRECV(pid-XDNS,ZN_REC,stat)
!pvm        call PVMFUNPACK(BYTE1,zn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

	do j = 1, XSPS
! Forward elimination in (1,j)th equations
          fac(j) = 1./(1. - c(1,j)*zn(j,1))
! Check for singular matrix (debugging only)
          r(1,j) = (r(1,j) - c(1,j)*zn(j,2))*fac(j)
! Backward elimination in (0,j)th equations.
          r(0,j) = zn(j,2) - zn(j,1)*r(1,j)
! Forward elimination in (ZSPS,j)th equations.
          r(ZSPS,j) = r(ZSPS,j) - c(ZSPS,j)*r(1,j)
! Load (ZSPS,j)th equations into passing arrays.
          zn(j,1) = 0.
          zn(j,2) = r(ZSPS,j)

! Forward elimination in (1,j)th equations
          fach(j) = 1./(1. - ch(1,j)*zn(j,3))
! Check for singular matrix (debugging only)
          rh(1,j) = (rh(1,j) - ch(1,j)*zn(j,4))*fach(j)
! Backward elimination in (0,j)th equations.
          rh(0,j) = zn(j,4) - zn(j,3)*rh(1,j)
! Forward elimination in (ZSPS,j)th equations.
          rh(ZSPS,j) = rh(ZSPS,j) - ch(ZSPS,j)*rh(1,j)
! Load (ZSPS,j)th equations into passing arrays.
          zn(j,3) = 0.
          zn(j,4) = rh(ZSPS,j)
        end do

        cnt = 4*XSPS
#ifdef TIMING
        ti = click()
#endif
! Send (ZSPS,j)th equations.
!       call SEND(pid+XDNS,ZN_REC,zn,len,ackn)
!pvm        call PVMFINITSEND(PVMRAW, stat)
!pvm        call PVMFPACK(BYTE1,zn,len,1,stat)
!pvm        call PVMFSEND(pid+XDNS,ZN_REC,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do j = 1, XSPS
! Forward elimination in (ZSPS+1,j)th equations to get
! r(ZSPS+1,j).
          r(ZSPS+1,j) = t2(j) - t1(j)*r(ZSPS,j)
! Forward elimination in (ZSPS+1,j)th equations to get
! r(ZSPS+1,j).
          rh(ZSPS+1,j) = t2h(j) - t1h(j)*rh(ZSPS,j)
          do i = 2, ZSPS-1
! Completion of forward and backward elimination to get remaining unknowns.
            r(i,j) = r(i,j) - c(i,j)*r(1,j) - b(i,j)*r(ZSPS,j)
            rh(i,j) = rh(i,j)-ch(i,j)*rh(1,j)-bh(i,j)*rh(ZSPS,j)
          end do
         end do

      else
	print*, 'Node', my_id, "in mode 4"

! Load (1,j)th equations into passing arrays.
	do j = 1, XSPS
          zntmp(j,1) = c(1,j)
          zntmp(j,2) = r(1,j)
          zntmp(j,3) = ch(1,j)
          zntmp(j,4) = rh(1,j)
        end do
          
        cnt = 4*XSPS
#ifdef TIMING
        ti = click()
#endif
	
! Send (1,j)th equations.
!       call SEND(pid-XDNS,ZN_REC,zn,len,ackn)
!pvm        call PVMFINITSEND(PVMRAW, stat)
!pvm        call PVMFPACK(BYTE1,zn,len,1,stat)
!pvm        call PVMFSEND(pid-XDNS,ZN_REC,stat)
! Receive (0,j)th equations.
!       call SRECEIVE(sndr_pid,msg_type,zn,pid-XDNS,ZN_REC,len)
!pvm        call PVMFRECV(pid-XDNS,ZN_REC,stat)
!pvm        call PVMFUNPACK(BYTE1,zn,len,1,stat)
! call MPI_SENDRECV_REPLACE(zn, cnt, MPI_REAL, pid-XDNS-1, ZN_REC, &
!                           pid-XDNS-1, ZN_REC, &
!                           MPI_COMM_WORLD, stat)

call MPI_RECV(zn, cnt, MPI_REAL, pid-XDNS-1, ZN_REC, MPI_COMM_WORLD, mpp_status, stat)
call MPI_SEND(zntmp, cnt, MPI_REAL, pid-XDNS-1, ZN_REC, MPI_COMM_WORLD, stat)

#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do j = 1, XSPS
! Forward elimination in (1,j)th equations to get r(1,j).
          fac(j) = 1./(1. - c(1,j)*zn(j,1))
! Check for singular matrix (debugging only)
          r(1,j) = (r(1,j) - c(1,j)*zn(j,2))*fac(j)
! Backward elimination in (0,j)th equations to get remaining unknowns.
          r(0,j) = zn(j,2) - zn(j,1)*r(1,j)

! Forward elimination in (1,j)th equations to get r(1,j).
          fach(j) = 1./(1. - ch(1,j)*zn(j,3))
! Check for singular matrix (debugging only)
          rh(1,j) = (rh(1,j) - ch(1,j)*zn(j,4))*fach(j)
! Backward elimination in (0,j)th equations to get remaining unknowns.
          rh(0,j) = zn(j,4) - zn(j,3)*rh(1,j)
          do i = 2, ZSPS
! Completion of forward elimination to get remaining unknowns.
            r(i,j) = r(i,j) - c(i,j)*r(1,j)
            rh(i,j) = rh(i,j) - ch(i,j)*rh(1,j)
          end do
         end do

      endif

      return
      end subroutine

!23456789012345678901234567890123456789012345678901234567890123456789012
! Parallel machine precision tridiagonal solver.
! The send-receive fuse is lit from both ends in this subroutine.
! The processors are self-synchronized by blocking receives,
! and the load balance is so good that non-blocking receives
! are not needed.
      subroutine parxsolv1(c,b,r,ct,pid,x_pid, &
	                    xsps, zsps, xdns, zdns)

      implicit none

       integer, intent(in) :: XSPS, &
                              ZSPS, &
                              XDNS, &
                              ZDNS
                              
      real, dimension(ZSPS, XSPS), intent(inout) ::  c, &
                                                     b
                                                     

      real, dimension(0:ZSPS+1, 0:XSPS+1), intent(inout) ::  r
 
      real, dimension(ZSPS,2) :: xn, xntmp
      
      integer	XN_REC
      parameter	(XN_REC = 45)

      real, dimension(ZSPS)   :: t1, t2, fac
      real :: clockdt, click
      real :: ct, ti, tf, dt

      integer :: pid, x_pid
      integer :: i, j, sndr_pid, msg_type, cnt, ackn
      integer :: stat

#ifdef TIMING
      dt = clockdt()
#endif

      if (x_pid .eq. 1) then

! Load passing (i,XSPS)th equations into passing arrays.
        do 10 i = 1, ZSPS
          xntmp(i,1) = b(i,XSPS)
          xntmp(i,2) = r(i,XSPS)
   10   continue

        cnt = 2*ZSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (i,XSPS)th equations.
! !       call SEND(pid+1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid+1,XN_REC,stat)
! ! Receive (i,(XSPS + 1))th equations.
! !       call SRECEIVE(sndr_pid,msg_type,xn,pid+1,XN_REC,len)
!         call PVMFRECV(pid+1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
call MPI_SEND(xntmp, cnt, MPI_REAL, pid, XN_REC, MPI_COMM_WORLD, stat)
call MPI_RECV(xn, cnt, MPI_REAL, pid, XN_REC, MPI_COMM_WORLD, mpp_status, stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 20 i = 1, ZSPS
! Backward elimination in (i,XSPS)th equations to get
! r(i,XSPS)
          fac(i) = 1./(1. - b(i,XSPS)*xn(i,1))
          r(i,XSPS) = (r(i,XSPS)-b(i,XSPS)*xn(i,2))*fac(i)
! Forward elimination in (i,XSPS+1)th equations to get
! r(i,XSPS+1)
          r(i,XSPS+1) = xn(i,2) - xn(i,1)*r(i,XSPS)
   20   continue

! Completion of backward elimination to get remaining unknowns.
        do 30 j = 1, XSPS-1
        do 30 i = 1, ZSPS
          r(i,j) = r(i,j) - b(i,j)*r(i,XSPS)
   30   continue

      else if (x_pid .le. XDNS/2) then

        cnt = 2*ZSPS
#ifdef TIMING
        ti = click()
#endif
! ! Receive (i,0)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,xn,pid-1,XN_REC,len)
!         call PVMFRECV(pid-1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

! Forward elimination in (i,1)th equations of subdomain.
	do 40 i = 1, ZSPS
          fac(i) = 1./(1. - c(i,1)*xn(i,1))
          b(i,1) = b(i,1)*fac(i)
          r(i,1) = (r(i,1) - c(i,1)*xn(i,2))*fac(i)
! Forward elimination in (i,XSPS)th equations of subdomain.
          fac(i) = 1./(1. - c(i,XSPS)*b(i,1))
          b(i,XSPS) = b(i,XSPS)*fac(i)
          r(i,XSPS)=(r(i,XSPS)-c(i,XSPS)*r(i,1))*fac(i)
! Store (i,0)th equations for later recovery of r(i,0).
          t1(i) = xn(i,1)
          t2(i) = xn(i,2)
! Load (i,XSPS)th equations into passing arrays.
          xn(i,1) = b(i,XSPS)
          xn(i,2) = r(i,XSPS)
   40   continue

        cnt = 2*ZSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (i,XSPS)th equations.
! !       call SEND(pid+1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid+1,XN_REC,stat)
! ! Receive (i,(XSPS + 1))th equations.
! !       call SRECEIVE(sndr_pid,msg_type,xn,pid+1,XN_REC,len)
!         call PVMFRECV(pid+1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 50 i = 1, ZSPS
! Backward elimination in (i,XSPS)th equations.
          fac(i) = 1./(1. - b(i,XSPS)*xn(i,1))
          r(i,XSPS) = (r(i,XSPS) - b(i,XSPS)*xn(i,2))*fac(i)
! Backward elimination in (i,XSPS+1)th equations.
          r(i,XSPS+1) = xn(i,2) - xn(i,1)*r(i,XSPS)
! Backward elimination in (i,1)th equations to get r(i,1).
          r(i,1) = r(i,1) - b(i,1)*r(i,XSPS)
! Load (i,1)th equations into passing array.
          xn(i,1) = 0.
          xn(i,2) = r(i,1)
   50   continue

        cnt = 2*ZSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (i,1)th equations.
! !       call SEND(pid-1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid-1,XN_REC,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 60 i = 1, ZSPS
! Backward elimination in (i,0)th equations.
          r(i,0) = t2(i) - t1(i)*r(i,1)
   60   continue

! Completion of forward and backward elimination for solution of
! unknowns.
        do 70 j = 2, XSPS-1
        do 70 i = 1, ZSPS
          r(i,j) = r(i,j) - b(i,j)*r(i,XSPS) - c(i,j)*r(i,1)
   70   continue

      else if (x_pid .lt. XDNS) then 

        cnt = 2*ZSPS
#ifdef TIMING
        ti = click()
#endif
! ! Receive (i,XSPS+1)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,xn,pid+1,XN_REC,len)
!         call PVMFRECV(pid+1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 80 i = 1, ZSPS
! Backward elimination in (i,XSPS)th equations.
          fac(i) = 1./(1. - b(i,XSPS)*xn(i,1))
          c(i,XSPS) = c(i,XSPS)*fac(i)
          r(i,XSPS) = (r(i,XSPS) - b(i,XSPS)*xn(i,2))*fac(i)
! Backward elimination in (i,1)th equations.
          fac(i) = 1./(1. - b(i,1)*c(i,XSPS))
          c(i,1) = c(i,1)*fac(i)
          r(i,1) = (r(i,1) - b(i,1)*r(i,XSPS))*fac(i)
! Store (i,XSPS+1)th equations for later recovery of r(i,XSPS+1).
          t1(i) = xn(i,1)
          t2(i) = xn(i,2)
! Load passing arrays with (i,1)th equations.
          xn(i,1) = c(i,1)
          xn(i,2) = r(i,1)
   80   continue

        cnt = 2*ZSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (i,1)th equations.
! !       call SEND(pid-1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid-1,XN_REC,stat)
! ! Receive (i,0)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,xn,pid-1,XN_REC,len)
!         call PVMFRECV(pid-1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 90 i = 1, ZSPS
! Forward elimination in (i,1)th equations
          fac(i) = 1./(1. - c(i,1)*xn(i,1))
          r(i,1) = (r(i,1) - c(i,1)*xn(i,2))*fac(i)
! Backward elimination in (i,0)th equations.
          r(i,0) = xn(i,2) - xn(i,1)*r(i,1)
! Forward elimination in (i,XSPS)th equations.
          r(i,XSPS) = r(i,XSPS) - c(i,XSPS)*r(i,1)
! Load (i,XSPS)th equations into passing arrays.
          xn(i,1) = 0.
          xn(i,2) = r(i,XSPS)
   90   continue

        cnt = 2*ZSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (i,XSPS)th equations.
! !       call SEND(pid+1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid+1,XN_REC,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

! Forward elimination in (i,XSPS)th equations to get
! r(i,XSPS+1).	
        do 100 i = 1, ZSPS
          r(i,XSPS+1) = t2(i) - t1(i)*r(i,XSPS)
  100   continue

! Completion of forward and backward elimination to get remaining unknowns.
        do 110 j = 2, XSPS-1
	do 110 i = 1, ZSPS
          r(i,j) = r(i,j) - c(i,j)*r(i,1) - b(i,j)*r(i,XSPS)
  110   continue

      else

! Load (i,1)th equations into passing arrays.
	do 120 i = 1, ZSPS
          xntmp(i,1) = c(i,1)
          xntmp(i,2) = r(i,1)
  120   continue

        cnt = 2*ZSPS
#ifdef TIMING
        ti = click()
#endif
! ! Send (i,1)th equations.
! !       call SEND(pid-1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid-1,XN_REC,stat)
! ! Receive (i,0)th equations.
! !       call SRECEIVE(sndr_pid,msg_type,xn,pid-1,XN_REC,len)
!         call PVMFRECV(pid-1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
call MPI_RECV(xn, cnt, MPI_REAL, pid-2, XN_REC, MPI_COMM_WORLD, mpp_status, stat)
call MPI_SEND(xntmp, cnt, MPI_REAL, pid-2, XN_REC, MPI_COMM_WORLD, stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

	do 130 i = 1, ZSPS
! Forward elimination in (i,1)th equations to get r(i,1).
          fac(i) = 1./(1. - c(i,1)*xn(i,1))
          r(i,1) = (r(i,1) - c(i,1)*xn(i,2))*fac(i)
! Backward elimination in (i,0)th equations to get r(i,0).
          r(i,0) = xn(i,2) - xn(i,1)*r(i,1)
  130   continue

! Completion of forward elimination to get remaining unknowns.
        do 140 j = 2, XSPS
        do 140 i = 1, ZSPS
          r(i,j) = r(i,j) - c(i,j)*r(i,1)
  140   continue

      endif

      return
      end subroutine

      
! Parallel machine precision tridiagonal solver.
! The send-receive fuse is lit from both ends in this subroutine.
! The processors are self-synchronized by blocking receives,
! and the load balance is so good that non-blocking receives
! are not needed.

! 10/2013 Adaption for WRF-Hydro by Benjamin Fersch (KIT)
! 
! For the MPI calls all local pids are converted to MPI pids
! First PVM rank was 1, first MPI rank is 0
! No changes to the convention in the code, only MPI calls are affected

      subroutine dparxsolv1(c,b,r,ch,bh,rh,ct,pid,x_pid, &
	                    xsps, zsps, xdns, zdns)

      implicit none

! Parameters specifying the size of the problem
! XSPS     XS Per Subdomain
! ZSPS     ZS Per Subdomain
! XDNS     X-Direction Number of Subdomains
! ZDNS     Z-Direction Number of Subdomains
! NSBS   Number of SUBdomainS
! N_XS     Number of X-valueS across the whole grid
! N_ZS     Number of Z-valueS across the whole grid
! N_PTS    Number of PoinTS in the whole grid
! Specify problem size by choosing XSPS, ZSPS, XDNS, and ZDNS

      integer, intent(in) :: XSPS, &
                             ZSPS, &
                             XDNS, &
                             ZDNS
      
      !local
      integer             :: NSBS, &
                             n_xs, &
                             n_zs, &
                             n_pts
                 
      integer	XN_REC
      parameter	(XN_REC = 45)

      real, dimension(ZSPS, XSPS), intent(inout) ::  c, &
                                                     b, &
                                                     ch,&
                                                     bh

      real, dimension(0:ZSPS+1, 0:XSPS+1), intent(inout) ::  r, &
                                                             rh
      real, dimension(ZSPS)   :: t1, t2, fac, t1h, t2h, fach
      real, dimension(ZSPS,4) :: xn, xntmp
      real :: clockdt, click
      real :: ct, ti, tf, dt

      integer :: pid, x_pid
      integer :: i, j, sndr_pid, msg_type, cnt, ackn
      integer :: stat

#ifdef TIMING
      dt = clockdt()
#endif

      if (x_pid .eq. 1) then

! Load passing (i,XSPS)th equations into passing arrays.
        do 10 i = 1, ZSPS
          xntmp(i,1) = b(i,XSPS)
          xntmp(i,2) = r(i,XSPS)
          xntmp(i,3) = bh(i,XSPS)
          xntmp(i,4) = rh(i,XSPS)
   10   continue

        cnt = 4*ZSPS
#ifdef TIMING
        ti = click()
#endif
! Send (i,XSPS)th equations.
!       call SEND(pid+1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid+1,XN_REC,stat)
! Receive (i,(XSPS + 1))th equations.
!       call SRECEIVE(sndr_pid,msg_type,xn,pid+1,XN_REC,len)
!         call PVMFRECV(pid+1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
call MPI_SEND(xntmp, cnt, MPI_REAL, pid, XN_REC, MPI_COMM_WORLD, stat)
call MPI_RECV(xn, cnt, MPI_REAL, pid, XN_REC, MPI_COMM_WORLD, mpp_status, stat)

#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 20 i = 1, ZSPS
! Backward elimination in (i,XSPS)th equations to get
! r(i,XSPS)
          fac(i) = 1./(1. - b(i,XSPS)*xn(i,1))
          r(i,XSPS) = (r(i,XSPS)-b(i,XSPS)*xn(i,2))*fac(i)
! Forward elimination in (i,XSPS+1)th equations to get
! r(i,XSPS+1)
          r(i,XSPS+1) = xn(i,2) - xn(i,1)*r(i,XSPS)

          fach(i) = 1./(1. - bh(i,XSPS)*xn(i,3))
          rh(i,XSPS) = (rh(i,XSPS)-bh(i,XSPS)*xn(i,4))*fach(i)
          rh(i,XSPS+1) = xn(i,4) - xn(i,3)*rh(i,XSPS)
   20   continue

! Completion of backward elimination to get remaining unknowns.
        do 30 j = 1, XSPS-1
        do 30 i = 1, ZSPS
          r(i,j) = r(i,j) - b(i,j)*r(i,XSPS)
          rh(i,j) = rh(i,j) - bh(i,j)*rh(i,XSPS)
   30   continue

      else if (x_pid .le. XDNS/2) then

        cnt = 4*ZSPS
#ifdef TIMING
        ti = click()
#endif
! Receive (i,0)th equations.
!       call SRECEIVE(sndr_pid,msg_type,xn,pid-1,XN_REC,len)
!         call PVMFRECV(pid-1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

! Forward elimination in (i,1)th equations of subdomain.
	do 40 i = 1, ZSPS
          fac(i) = 1./(1. - c(i,1)*xn(i,1))
! Check for singular matrix (debugging only)
          b(i,1) = b(i,1)*fac(i)
          r(i,1) = (r(i,1) - c(i,1)*xn(i,2))*fac(i)
! Forward elimination in (i,XSPS)th equations of subdomain.
          fac(i) = 1./(1. - c(i,XSPS)*b(i,1))
! Check for singular matrix (debugging only)
          b(i,XSPS) = b(i,XSPS)*fac(i)
          r(i,XSPS)=(r(i,XSPS)-c(i,XSPS)*r(i,1))*fac(i)
! Store (i,0)th equations for later recovery of r(i,0).
          t1(i) = xn(i,1)
          t2(i) = xn(i,2)
! Load (i,XSPS)th equations into passing arrays.
          xn(i,1) = b(i,XSPS)
          xn(i,2) = r(i,XSPS)

          fach(i) = 1./(1. - ch(i,1)*xn(i,3))
! Check for singular matrix (debugging only)
          bh(i,1) = bh(i,1)*fach(i)
          rh(i,1) = (rh(i,1) - ch(i,1)*xn(i,4))*fach(i)
! Forward elimination in (i,XSPS)th equations of subdomain.
          fach(i) = 1./(1. - ch(i,XSPS)*bh(i,1))
! Check for singular matrix (debugging only)
          bh(i,XSPS) = bh(i,XSPS)*fach(i)
          rh(i,XSPS) = (rh(i,XSPS)-ch(i,XSPS)*rh(i,1))*fach(i)
! Store (i,0)th equations for later recovery of r(i,0).
          t1h(i) = xn(i,3)
          t2h(i) = xn(i,4)
! Load (i,XSPS)th equations into passing arrays.
          xn(i,3) = bh(i,XSPS)
          xn(i,4) = rh(i,XSPS)
   40   continue

        cnt = 4*ZSPS
#ifdef TIMING
        ti = click()
#endif
! Send (i,XSPS)th equations.
!       call SEND(pid+1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid+1,XN_REC,stat)
! Receive (i,(XSPS + 1))th equations.
!       call SRECEIVE(sndr_pid,msg_type,xn,pid+1,XN_REC,len)
!         call PVMFRECV(pid+1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 50 i = 1, ZSPS
! Backward elimination in (i,XSPS)th equations.
          fac(i) = 1./(1. - b(i,XSPS)*xn(i,1))
! Check for singular matrix (debugging only)
          r(i,XSPS) = (r(i,XSPS) - b(i,XSPS)*xn(i,2))*fac(i)
! Backward elimination in (i,XSPS+1)th equations.
          r(i,XSPS+1) = xn(i,2) - xn(i,1)*r(i,XSPS)
! Backward elimination in (i,1)th equations to get r(i,1).
          r(i,1) = r(i,1) - b(i,1)*r(i,XSPS)
! Load (i,1)th equations into passing array.
          xn(i,1) = 0.
          xn(i,2) = r(i,1)

! Backward elimination in (i,XSPS)th equations.
          fach(i) = 1./(1. - bh(i,XSPS)*xn(i,3))
! Check for singular matrix (debugging only)
          rh(i,XSPS) = (rh(i,XSPS) - bh(i,XSPS)*xn(i,4))*fach(i)
! Backward elimination in (i,XSPS+1)th equations.
          rh(i,XSPS+1) = xn(i,4) - xn(i,3)*rh(i,XSPS)
! Backward elimination in (i,1)th equations to get r(i,1).
          rh(i,1) = rh(i,1) - bh(i,1)*rh(i,XSPS)
! Load (i,1)th equations into passing array.
          xn(i,3) = 0.
          xn(i,4) = rh(i,1)
   50   continue

        cnt = 4*ZSPS
#ifdef TIMING
        ti = click()
#endif
! Send (i,1)th equations.
!       call SEND(pid-1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid-1,XN_REC,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 60 i = 1, ZSPS
! Backward elimination in (i,0)th equations.
          r(i,0) = t2(i) - t1(i)*r(i,1)
          rh(i,0) = t2h(i) - t1h(i)*rh(i,1)
   60   continue

! Completion of forward and backward elimination for solution of
! unknowns.
        do 70 j = 2, XSPS-1
        do 70 i = 1, ZSPS
          r(i,j) = r(i,j) - b(i,j)*r(i,XSPS) - c(i,j)*r(i,1)
          rh(i,j) = rh(i,j)-bh(i,j)*rh(i,XSPS)-ch(i,j)*rh(i,1)
   70   continue

      else if (x_pid .lt. XDNS) then 

        cnt = 4*ZSPS
#ifdef TIMING
        ti = click()
#endif
! Receive (i,XSPS+1)th equations.
!       call SRECEIVE(sndr_pid,msg_type,xn,pid+1,XN_REC,len)
!         call PVMFRECV(pid+1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 80 i = 1, ZSPS
! Backward elimination in (i,XSPS)th equations.
          fac(i) = 1./(1. - b(i,XSPS)*xn(i,1))
! Check for singular matrix (debugging only)
          c(i,XSPS) = c(i,XSPS)*fac(i)
          r(i,XSPS) = (r(i,XSPS) - b(i,XSPS)*xn(i,2))*fac(i)
! Backward elimination in (i,1)th equations.
          fac(i) = 1./(1. - b(i,1)*c(i,XSPS))
! Check for singular matrix (debugging only)
          c(i,1) = c(i,1)*fac(i)
          r(i,1) = (r(i,1) - b(i,1)*r(i,XSPS))*fac(i)
! Store (i,XSPS+1)th equations for later recovery of r(i,XSPS+1).
          t1(i) = xn(i,1)
          t2(i) = xn(i,2)
! Load passing arrays with (i,1)th equations.
          xn(i,1) = c(i,1)
          xn(i,2) = r(i,1)

! Backward elimination in (i,XSPS)th equations.
          fach(i) = 1./(1. - bh(i,XSPS)*xn(i,3))
! Check for singular matrix (debugging only)
          ch(i,XSPS) = ch(i,XSPS)*fach(i)
          rh(i,XSPS) = (rh(i,XSPS)-bh(i,XSPS)*xn(i,4))*fach(i)
! Backward elimination in (i,1)th equations.
          fach(i) = 1./(1. - bh(i,1)*ch(i,XSPS))
! Check for singular matrix (debugging only)
          ch(i,1) = ch(i,1)*fach(i)
          rh(i,1) = (rh(i,1) - bh(i,1)*rh(i,XSPS))*fach(i)
! Store (i,XSPS+1)th equations for later recovery of r(i,XSPS+1).
          t1h(i) = xn(i,3)
          t2h(i) = xn(i,4)
! Load passing arrays with (i,1)th equations.
          xn(i,3) = ch(i,1)
          xn(i,4) = rh(i,1)
   80   continue

        cnt = 4*ZSPS
#ifdef TIMING
        ti = click()
#endif
! Send (i,1)th equations.
!       call SEND(pid-1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid-1,XN_REC,stat)
! Receive (i,0)th equations.
!       call SRECEIVE(sndr_pid,msg_type,xn,pid-1,XN_REC,len)
!         call PVMFRECV(pid-1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

        do 90 i = 1, ZSPS
! Forward elimination in (i,1)th equations
          fac(i) = 1./(1. - c(i,1)*xn(i,1))
! Check for singular matrix (debugging only)
          r(i,1) = (r(i,1) - c(i,1)*xn(i,2))*fac(i)
! Backward elimination in (i,0)th equations.
          r(i,0) = xn(i,2) - xn(i,1)*r(i,1)
! Forward elimination in (i,XSPS)th equations.
          r(i,XSPS) = r(i,XSPS) - c(i,XSPS)*r(i,1)
! Load (i,XSPS)th equations into passing arrays.
          xn(i,1) = 0.
          xn(i,2) = r(i,XSPS)

! Forward elimination in (i,1)th equations
          fach(i) = 1./(1. - ch(i,1)*xn(i,3))
! Check for singular matrix (debugging only)
          rh(i,1) = (rh(i,1) - ch(i,1)*xn(i,4))*fach(i)
! Backward elimination in (i,0)th equations.
          rh(i,0) = xn(i,4) - xn(i,3)*rh(i,1)
! Forward elimination in (i,XSPS)th equations.
          rh(i,XSPS) = rh(i,XSPS) - ch(i,XSPS)*rh(i,1)
! Load (i,XSPS)th equations into passing arrays.
          xn(i,3) = 0.
          xn(i,4) = rh(i,XSPS)
   90   continue

        cnt = 4*ZSPS
#ifdef TIMING
        ti = click()
#endif
! Send (i,XSPS)th equations.
!       call SEND(pid+1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid+1,XN_REC,stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

! Forward elimination in (i,XSPS)th equations to get r(i,XSPS+1).	
        do 100 i = 1, ZSPS
          r(i,XSPS+1) = t2(i) - t1(i)*r(i,XSPS)
          rh(i,XSPS+1) = t2h(i) - t1h(i)*rh(i,XSPS)
  100   continue

! Completion of forward and backward elimination to get remaining unknowns.
        do 110 j = 2, XSPS-1
	do 110 i = 1, ZSPS
          r(i,j) = r(i,j) - c(i,j)*r(i,1) - b(i,j)*r(i,XSPS)
          rh(i,j) = rh(i,j)-ch(i,j)*rh(i,1)-bh(i,j)*rh(i,XSPS)
  110   continue

      else

! Load (i,1)th equations into passing arrays.
	do 120 i = 1, ZSPS
          xntmp(i,1) = c(i,1)
          xntmp(i,2) = r(i,1)
          xntmp(i,3) = ch(i,1)
          xntmp(i,4) = rh(i,1)
  120   continue

        cnt = 4*ZSPS
#ifdef TIMING
        ti = click()
#endif
! Send (i,1)th equations.
!       call SEND(pid-1,XN_REC,xn,len,ackn)
!         call PVMFINITSEND(PVMRAW, stat)
!         call PVMFPACK(BYTE1,xn,len,1,stat)
!         call PVMFSEND(pid-1,XN_REC,stat)
! Receive (i,0)th equations.
!       call SRECEIVE(sndr_pid,msg_type,xn,pid-1,XN_REC,len)
!         call PVMFRECV(pid-1,XN_REC,stat)
!         call PVMFUNPACK(BYTE1,xn,len,1,stat)
call MPI_RECV(xn, cnt, MPI_REAL, pid-2, XN_REC, MPI_COMM_WORLD, mpp_status, stat)
call MPI_SEND(xntmp, cnt, MPI_REAL, pid-2, XN_REC, MPI_COMM_WORLD, stat)
#ifdef TIMING
        tf = click()
        call add_dt(ct,tf,ti,dt)
#endif

	do 130 i = 1, ZSPS
! Forward elimination in (i,1)th equations to get r(i,1).
          fac(i) = 1./(1. - c(i,1)*xn(i,1))
! Check for singular matrix (debugging only)
          r(i,1) = (r(i,1) - c(i,1)*xn(i,2))*fac(i)
! Backward elimination in (i,0)th equations to get r(i,0).
          r(i,0) = xn(i,2) - xn(i,1)*r(i,1)

! Forward elimination in (i,1)th equations to get rh(i,1).
          fach(i) = 1./(1. - ch(i,1)*xn(i,3))
! Check for singular matrix (debugging only)
          rh(i,1) = (rh(i,1) - ch(i,1)*xn(i,4))*fach(i)
! Backward elimination in (i,0)th equations to get rh(i,0).
          rh(i,0) = xn(i,4) - xn(i,3)*rh(i,1)
  130   continue

! Completion of forward elimination to get remaining unknowns.
        do 140 j = 2, XSPS
        do 140 i = 1, ZSPS
          r(i,j) = r(i,j) - c(i,j)*r(i,1)
          rh(i,j) = rh(i,j) - ch(i,j)*rh(i,1)
  140   continue

      endif

      return
      end subroutine
     
!23456789012345678901234567890123456789012345678901234567890123456789012

      subroutine sub_n_form(n_xs,n_zs,c,a,b,r,c2,b2,r2,wk,xfac,zfac, &
                            dpid,dn_subs,dir)

      implicit none

      integer n_xs,n_zs

!  c(,)  -- subdiagonal elements of tridiagonal systems
!  a(,)  -- diagonal elements of tridiagonal systems
!  b(,)  -- superdiagonal elements of tridiagonal systems
!  r(,)  -- right-hand side elements of tridiagonal systems
!  c2(,) -- front-leg elements of N-systems
!  b2(,) -- back-leg elements of N-systems
!  r2(,) -- right-hand side elements of N-systems
!  wk(,) -- work array with same dimensions as a, b, c, etc.
      real c(n_zs,n_xs)
      real a(n_zs,n_xs)
      real b(n_zs,n_xs)
      real r(n_zs,n_xs)
      real c2(n_zs,n_xs)
      real b2(n_zs,n_xs)
      real r2(0:n_zs+1,0:n_xs+1)
      real wk(n_zs,n_xs)
      real fac
      real xfac(n_zs)
      real zfac(n_xs)

      integer dpid,dn_subs,dir
      integer i, j, XDIR, ZDIR
      parameter (XDIR = 1, ZDIR = 2)

      if (dir .eq. XDIR) then

! Forward elimination of subdiagonal elements
	if (dpid .eq. 1) then

          do 10 i = 1, n_zs
            xfac(i) = 1./a(i,1)
            c2(i,1) = 0.
            r2(i,1) = r(i,1)*xfac(i)
   10     continue

          do 20 j = 2, n_xs
	  do 20 i = 1, n_zs
            wk(i,j-1) = b(i,j-1)*xfac(i)
            xfac(i) = 1./(a(i,j) - c(i,j)*wk(i,j-1))
            c2(i,j) = 0.
            r2(i,j) = (r(i,j) - c(i,j)*r2(i,j-1))*xfac(i)
   20     continue

	  do 40 i = 1, n_zs
            b2(i,n_xs) = b(i,n_xs)*xfac(i)
   40     continue

	else

          do 50 i = 1, n_zs
            xfac(i) = 1./a(i,1)
            c2(i,1) = c(i,1)*xfac(i)
	    wk(i,1) = b(i,1)*xfac(i)
            r2(i,1) = r(i,1)*xfac(i)
	    xfac(i) = 1./a(i,2)
	    c2(i,2) = c(i,2)*xfac(i)
	    r2(i,2) = r(i,2)*xfac(i)
   50     continue

          do 60 j = 3, n_xs
	  do 60 i = 1, n_zs
            wk(i,j-1) = b(i,j-1)*xfac(i)
            xfac(i) = 1./(a(i,j) - c(i,j)*wk(i,j-1))
            c2(i,j) = -c(i,j)*c2(i,j-1)*xfac(i)
            r2(i,j) = (r(i,j) - c(i,j)*r2(i,j-1))*xfac(i)
   60     continue

	  do 80 i = 1, n_zs
            b2(i,n_xs) = b(i,n_xs)*xfac(i)
   80     continue

	endif

! Backward elimination of superdiagonal elements
        if (dpid .eq. dn_subs) then

          do 90 j = n_xs-1, 2, -1
          do 90 i = 1, n_zs
            c2(i,j) = c2(i,j) - wk(i,j)*c2(i,j+1)
            b2(i,j) = 0.
            r2(i,j) = r2(i,j) - wk(i,j)*r2(i,j+1)
   90     continue

	  do 100 i = 1, n_zs
            fac = 1./(1. - wk(i,1)*c2(i,2))
            c2(i,1) = c2(i,1)*fac
            b2(i,1) = 0.
            r2(i,1) = (r2(i,1) - wk(i,1)*r2(i,2))*fac
  100     continue

        else 

          do 110 i = 1, n_zs
            b2(i,n_xs-1) = wk(i,n_xs-1)
  110     continue

          do 120 j = n_xs-2, 2, -1
	  do 120 i = 1, n_zs
            c2(i,j) = c2(i,j) - wk(i,j)*c2(i,j+1)
            b2(i,j) = -wk(i,j)*b2(i,j+1)
            r2(i,j) = r2(i,j) - wk(i,j)*r2(i,j+1)
  120     continue

! If only 2 points in X-direction, do not execute these statements.
          if (n_xs .gt. 2) then
	    do 130 i = 1, n_zs
              fac = 1./(1. - wk(i,1)*c2(i,2))
              c2(i,1) = c2(i,1)*fac
              r2(i,1) = (r2(i,1) - wk(i,1)*r2(i,2))*fac
              b2(i,1) = -wk(i,1)*b2(i,2)*fac
  130       continue
	  endif

        endif

      else if (dir .eq. ZDIR) then

! Forward elimination of subdiagonal elements
	if (dpid .eq. 1) then

          do 140 j = 1, n_xs
            zfac(j) = 1./a(1,j)
            c2(1,j) = 0.
            r2(1,j) = r(1,j)*zfac(j)
  140     continue

          do 150 i = 2, n_zs
          do 150 j = 1, n_xs
            wk(i-1,j) = b(i-1,j)*zfac(j)
            zfac(j) = 1./(a(i,j) - c(i,j)*wk(i-1,j))
            c2(i,j) = 0.
            r2(i,j) = (r(i,j) - c(i,j)*r2(i-1,j))*zfac(j)
  150     continue

          do 170 j = 1, n_xs
            b2(n_zs,j) = b(n_zs,j)*zfac(j)
  170     continue

        else

          do 180 j = 1, n_xs
            zfac(j) = 1./a(1,j)
            c2(1,j) = c(1,j)*zfac(j)
            wk(1,j) = b(1,j)*zfac(j)
            r2(1,j) = r(1,j)*zfac(j)
            zfac(j) = 1./a(2,j)
            c2(2,j) = c(2,j)*zfac(j)
            r2(2,j) = r(2,j)*zfac(j)
  180     continue

          do 190 i = 3, n_zs
          do 190 j = 1, n_xs
            wk(i-1,j) = b(i-1,j)*zfac(j)
            zfac(j) = 1./(a(i,j) - c(i,j)*wk(i-1,j))
            c2(i,j) = -c(i,j)*c2(i-1,j)*zfac(j)
            r2(i,j) = (r(i,j) - c(i,j)*r2(i-1,j))*zfac(j)
  190     continue

          do 210 j = 1, n_xs
            b2(n_zs,j) = b(n_zs,j)*zfac(j)
  210     continue

        endif

! Backward elimination of superdiagonal elements
        if (dpid .eq. dn_subs) then

          do 220 j = 1, n_xs
          do 220 i = n_zs - 1, 2, -1
            c2(i,j) = c2(i,j) - wk(i,j)*c2(i+1,j)
            b2(i,j) = 0.
            r2(i,j) = r2(i,j) - wk(i,j)*r2(i+1,j)
  220     continue

	  do 230 j = 1, n_xs
            fac = 1./(1. - wk(1,j)*c2(2,j))
            c2(1,j) = c2(1,j)*fac
            b2(1,j) = 0.
            r2(1,j) = (r2(1,j) - wk(1,j)*r2(2,j))*fac
  230     continue

        else

          do 240 j = 1, n_xs
            b2(n_zs-1,j) = wk(n_zs-1,j)
  240     continue

          do 250 j = 1, n_xs
          do 250 i = n_zs - 2, 2, -1
            c2(i,j) = c2(i,j) - wk(i,j)*c2(i+1,j)
            b2(i,j) = -wk(i,j)*b2(i+1,j)
            r2(i,j)  = r2(i,j) - wk(i,j)*r2(i+1,j)
  250     continue

! If only 2 points in Z-direction, do not execute these statements.
          if (n_zs .gt. 2) then
	    do 260 j = 1, n_xs
	      fac = 1./(1. - wk(1,j)*c2(2,j))
	      c2(1,j) = c2(1,j)*fac
	      r2(1,j) = (r2(1,j) - wk(1,j)*r2(2,j))*fac
              b2(1,j) = -wk(1,j)*b2(2,j)*fac
  260       continue
	  endif

        endif

! Announce bad direction specifier (debugging only)
!     else
!       write(*,*) 'sub_n_form:  What direction?'
!       stop
      endif

      return
      end subroutine
      
      
      subroutine sub_tri_solv(n_xs,n_zs,c,a,b,r,x,wk,xfac,zfac,dir)

      implicit none

      integer n_xs,n_zs

!  c(,)  -- subdiagonal elements of tridiagonal systems
!  a(,)  -- diagonal elements of tridiagonal systems
!  b(,)  -- superdiagonal elements of tridiagonal systems
!  r(,)  -- right-hand side elements of tridiagonal systems
!  x(,)  -- solutions
!  wk(,) -- work array w/ same dimensions as c, a, b, etc.

      real c(n_zs,n_xs)
      real a(n_zs,n_xs)
      real b(n_zs,n_xs)
      real r(n_zs,n_xs)
      real x(0:n_zs+1,0:n_xs+1)
      real wk(n_zs,n_xs)
      real xfac(n_zs)
      real zfac(n_xs)

      integer dir
      integer i,j,XDIR,ZDIR

      parameter (XDIR = 1, ZDIR = 2)

      if (dir .eq. XDIR) then

        do 10 i = 1, n_zs
! Check for need to pivot (debugging only)
        xfac(i) = 1./a(i,1)
        x(i,1)  = r(i,1)*xfac(i)
   10   continue

! Forward subdiagonal elimination
        do 20 j = 2, n_xs
	do 20 i = 1, n_zs
        wk(i,j-1) = b(i,j-1)*xfac(i)
        xfac(i) = 1./(a(i,j) - c(i,j)*wk(i,j-1))
! Check for need to pivot (debugging only)
        x(i,j) = (r(i,j) - c(i,j)*x(i,j-1))*xfac(i)
   20   continue

! Backsubstitution
        do 30 j = n_xs - 1, 1, -1
	do 30 i = 1, n_zs
        x(i,j)  = x(i,j) - wk(i,j)*x(i,j+1)
   30   continue

   
      else if (dir .eq. ZDIR) then

       do j = 1, n_xs
! Check for need to pivot (debugging only)
        zfac(j) = 1./a(1,j)
        x(1,j)  = r(1,j)*zfac(j)
       end do

! Forward subdiagonal elimination
      do j = 1, n_xs
       do i = 2, n_zs
        wk(i-1,j) = b(i-1,j)*zfac(j)
        zfac(j) = 1./(a(i,j) - c(i,j)*wk(i-1,j))
! Check for need to pivot (debugging only)
        x(i,j) = (r(i,j) - c(i,j)*x(i-1,j))*zfac(j)
       end do
      end do

! Backsubstitution
      do j = 1, n_xs
       do i = n_zs - 1, 1, -1
        x(i,j)  =  x(i,j) - wk(i,j)*x(i+1,j)
       end do
      end do

! Announce bad direction specifier (debugging only)
!     else
!       write(*,*) 'sub_tri_solv:  What direction?'
!       stop
      endif

      return
      end  subroutine

#endif

end module module_GW_baseflow   
