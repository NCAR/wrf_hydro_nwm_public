module module_GW_baseflow

#ifdef MPP_LAND
   use module_mpp_land
#endif
   implicit none

#include "rt_include.inc"
!yw #include "namelist.inc"
contains

!------------------------------------------------------------------------------
!DJG   Simple GW Bucket Model
!------------------------------------------------------------------------------

   subroutine simp_gw_buck(ix,jx,ixrt,jxrt,numbasns,basns_area,&
                            gwsubbasmsk, runoff1x_in, runoff2x_in, z_gwsubbas_tmp, qin_gwsubbas,&
                            qout_gwsubbas,qinflowbase,gw_strm_msk,gwbas_pix_ct,dist,DT,&
                            C,ex,z_mx,GWBASESWCRT,OVRTSWCRT)
   implicit none
   
!!!Declarations...
   integer, intent(in)                               :: ix,jx,ixrt,jxrt
   integer, intent(in)                               :: numbasns
   integer, intent(in), dimension(ix,jx)             :: gwsubbasmsk
   real, intent(in), dimension(ix,jx)                :: runoff2x_in 
   real, dimension(ix,jx)                            :: runoff2x 
   real, intent(in), dimension(ix,jx)                :: runoff1x_in
   real, dimension(ix,jx)                            :: runoff1x
   real, intent(in)                                  :: basns_area(numbasns),dist(ixrt,jxrt,9),DT
   real, intent(in),dimension(numbasns)              :: C,ex,z_mx
   real, intent(out),dimension(numbasns)             :: qout_gwsubbas
   real, intent(out),dimension(numbasns)             :: qin_gwsubbas
   real*8                                            :: z_gwsubbas(numbasns)
   real                                              :: qout_max, qout_spill, z_gw_spill
   real, intent(inout),dimension(numbasns)           :: z_gwsubbas_tmp
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
   character(len=19)				    :: header
   character(len=1)				    :: jnk


!!!Initialize variables...
   ct_bas8 = 0
   sum_perc8 = 0.
   net_perc = 0.
   qout_gwsubbas = 0.
   qin_gwsubbas = 0.
   z_gwsubbas = z_gwsubbas_tmp

!Assign local value of runoff2 (drainage) for flux caluclation to buckets...
   runoff2x = runoff2x_in
   runoff1x = runoff1x_in




!!!Calculate aggregated percolation from deep runoff into GW basins...
   do i=1,ix
     do j=1,jx

!!DJG 4/15/2015...reset runoff2x, runoff1x, values to 0 where extreme values exist...(<0 or
!> 1000)
       if((runoff2x(i,j).lt.0.).OR.(runoff2x(i,j).gt.1000.)) then
         runoff2x(i,j)=0.
       end if
       if((runoff1x(i,j).lt.0.).OR.(runoff1x(i,j).gt.1000.)) then
         runoff1x(i,j)=0.
       end if

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
! #ifdef MPP_LAND
!      if(ct_bas(bas) .gt. 0) then
! #endif

     net_perc(bas) = sum_perc(bas) / ct_bas(bas)   !units (mm)
!DJG...old change to cms     qin_gwsubbas(bas) = net_perc(bas)/1000. * ct_bas(bas) * basns_area(bas) !units (m^3)
     qin_gwsubbas(bas) = net_perc(bas)/1000.* &
                         ct_bas(bas)*basns_area(bas)/DT    !units (m^3/s)


!Adjust level of GW depth...(conceptual GW bucket units (mm))
!DJG...old change to cms inflow...     z_gwsubbas(bas) = z_gwsubbas(bas) + net_perc(bas) / 1000.0   ! (m)

!DJG...debug    write (6,*) "DJG...before",C(bas),ex(bas),z_gwsubbas(bas),z_mx(bas),z_gwsubbas(bas)/z_mx(bas)

     z_gwsubbas(bas) = z_gwsubbas(bas) + qin_gwsubbas(bas)*DT/( &
                       ct_bas(bas)*basns_area(bas))*1000.   !  units (mm)





!Calculate baseflow as a function of GW bucket depth...

     if(GWBASESWCRT.eq.1) then  !active exponential bucket... if/then for bucket model discharge type...

!DJG...Estimation of bucket 'overflow' (qout_spill) if/when bucket gets filled...
     qout_spill = 0.
     z_gw_spill = 0.
     if (z_gwsubbas(bas).gt.z_mx(bas)) then  !If/then for bucket overflow case...
       z_gw_spill = z_gwsubbas(bas) - z_mx(bas)
       z_gwsubbas(bas) = z_mx(bas)
       write (6,*) "Bucket spilling...", bas, z_gwsubbas(bas), z_mx(bas), z_gw_spill
     else
       z_gw_spill = 0.
     end if   ! End if for bucket overflow case...

     qout_spill = z_gw_spill/1000.*(ct_bas(bas)*basns_area(bas))/DT  !amount spilled from bucket overflow...units (cms)


!DJG...Maximum estimation of bucket outlfow that is limited by total quantity in bucket...
     qout_max = z_gwsubbas(bas)/1000.*(ct_bas(bas)*basns_area(bas))/DT   ! Estimate max bucket disharge limit to total volume in bucket...(m^3/s)


! Assume exponential relation between z/zmax and Q...
!DJG...old...creates non-asymptotic flow...   qout_gwsubbas(bas) = C(bas)*EXP(ex(bas)*z_gwsubbas(bas)/z_mx(bas)) !Exp.model. q_out (m^3/s)
!DJG force asymptote to zero to prevent 'overdraft'... 
!DJG debug hardwire test...       qout_gwsubbas(bas) = 1*(EXP(7.0*10./100.)-1) !Exp.model. q_out (m^3/s)
     qout_gwsubbas(bas) = C(bas)*(EXP(ex(bas)*z_gwsubbas(bas)/z_mx(bas))-1) !Exp.model. q_out (m^3/s)
       
!DJG...Calculation of max bucket outlfow that is limited by total quantity in bucket...
     qout_gwsubbas(bas) = MIN(qout_max,qout_gwsubbas(bas))   ! Limit bucket discharge to max. bucket limit

!DJG...debug...     write (6,*) "DJG-exp bucket...during",C(bas),ex(bas),z_gwsubbas(bas),qin_gwsubbas(bas),z_mx(bas),z_gwsubbas(bas)/z_mx(bas), qout_gwsubbas(bas), qout_max, qout_spill



     elseif (GWBASESWCRT.eq.2) then  !Pass through/steady-state bucket

! Assuming a steady-state (inflow=outflow) model...
!DJG convert input and output units to cms...       qout_gwsubbas(bas) = qin_gwsubbas(bas)  !steady-state model...(m^3)
       qout_gwsubbas(bas) = qin_gwsubbas(bas)  !steady-state model...(m^3/s)

!DJG...debug       write (6,*) "DJG-pass through...during",C(bas),ex(bas),qin_gwsubbas(bas), z_gwsubbas(bas),z_mx(bas),z_gwsubbas(bas)/z_mx(bas), qout_gwsubbas(bas), qout_max

     end if    ! End if for bucket model discharge type....




!Adjust level of GW depth...
!DJG bug adjust output to be mm and correct area bug...       z_gwsubbas(bas) = z_gwsubbas(bas) - qout_gwsubbas(bas)*DT &
!DJG bug adjust output to be mm and correct area bug...                       / (ct_bas(bas)*basns_area(bas))   !units(m)

     z_gwsubbas(bas) = z_gwsubbas(bas) - qout_gwsubbas(bas)*DT/( &
                       ct_bas(bas)*basns_area(bas))*1000.   ! units (mm)	

!DJG...Combine calculated bucket discharge and amount spilled from bucket...
     qout_gwsubbas(bas) = qout_gwsubbas(bas) + qout_spill   ! units (cms)


!DJG...debug     write (6,*) "DJG...after",C(bas),ex(bas),z_gwsubbas(bas),z_mx(bas),z_gwsubbas(bas)/z_mx(bas), qout_gwsubbas(bas), qout_spill
!DJG...debug     write (6,*) "DJG...after...calc",bas,ct_bas(bas),ct_bas(bas)*basns_area(bas),basns_area(bas),DT




! #ifdef MPP_LAND
!      endif
! #endif
   END DO                 ! End loop for GW bucket calcs...

   z_gwsubbas_tmp = z_gwsubbas


!!!Distribute basin integrated baseflow to stream pixels as stream 'inflow'...

      qinflowbase = 0.


      do i=1,ixrt
        do j=1,jxrt
!!!    -simple uniform disaggregation (8.31.06)
           if (gw_strm_msk(i,j).gt.0) then

             qinflowbase(i,j) = qout_gwsubbas(gw_strm_msk(i,j))*1000.*DT/ &
                gwbas_pix_ct(gw_strm_msk(i,j))/dist(i,j,9)     ! units (mm) that gets passed into chan routing as stream inflow

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

        z_gwsubbas = z_gwsubbas_tmp

   return

!------------------------------------------------------------------------------
   End subroutine simp_gw_buck
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





end module module_GW_baseflow   
