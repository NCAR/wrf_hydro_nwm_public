!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
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

module module_GW_baseflow

!   use overland_data
#ifdef MPP_LAND
   use module_mpp_land
   use MODULE_mpp_GWBUCKET, only: gw_sum_real, gw_write_io_real
   use MODULE_mpp_ReachLS, only : updatelinkv
#endif
   implicit none

!#include "rt_include.inc"
!yw #include "namelist.inc"
contains

!------------------------------------------------------------------------------
!DJG   Simple GW Bucket Model 
!      for NHDPLUS mapping
!------------------------------------------------------------------------------

   subroutine simp_gw_buck_nhd(           &
        ix,            jx,                &
        ixrt,          jxrt,              &
        numbasns,      AGGFACTRT,         &
        DT,            INFXSWGT,          &
        runoff1x_in,   runoff2x_in,       &
        cellArea,      area_lsm,          &
        c,             ex,                &
        z_mx,          z_gwsubbas_tmp,    &
        qout_gwsubbas, qin_gwsubbas,      &
        GWBASESWCRT,   OVRTSWCRT,         &
        LNLINKSL,                         & 
        basns_area,                       &
        nhdBuckMask,   channelBucket_only &
        ,wvtflag_pass                     & !wt var
        ,runoff1x_in_tr, runoff2x_in_tr   & !wt var
        ,z_gwsubbas_tmp_tr                & !wt var
        ,qout_gwsubbas_tr_pass,qin_gwsubbas_tr_pass & !wt var
        ,pmopt_pass                       & !pm gw var
        ,z_gwsubbas_tmp_passive           & !pm gw var
        ,z_gwsubbas_tmp_active            & !pm gw var
        ,z_gwsubbas_tmp_passive_tr        & !pm gw var
        ,z_gwsubbas_tmp_active_tr         & !pm gw var
        ,cm_gwsubbas_tmp                  & !pm gw var
        ) 

   use module_UDMAP, only: LNUMRSL, LUDRSL

   implicit none
   
!!!Declarations...
   integer, intent(in)                               :: ix,jx,ixrt,jxrt
   integer, intent(in)                               :: numbasns, lnlinksl
   real, intent(in), dimension(ix,jx)                :: runoff2x_in 
   real, dimension(ixrt,jxrt)                            :: runoff2x , runoff1x
   real, intent(in), dimension(ix,jx)                :: runoff1x_in, area_lsm
   real, intent(in)                                  :: cellArea(ixrt,jxrt),DT
   real, intent(in),dimension(numbasns)              :: C,ex
   real, intent(inout),dimension(numbasns)              :: z_mx
   real, intent(out),dimension(numbasns)             :: qout_gwsubbas
   !! intent inout for channelBucket_only .eq. 1
   real, intent(inout),dimension(numbasns)           :: qin_gwsubbas
   real*8                                            :: z_gwsubbas(numbasns)
   real                                              :: qout_max, qout_spill, z_gw_spill
   real, intent(inout),dimension(:)                  :: z_gwsubbas_tmp
   real, intent(in),dimension(ixrt,jxrt)             :: INFXSWGT
   integer, intent(in)                               :: GWBASESWCRT
   integer, intent(in)                               :: OVRTSWCRT
   real, intent(in), dimension(numbasns)             :: basns_area
   integer, intent(in)                               :: channelBucket_only   

!-----water tracer vars--------
   integer, optional, intent(in)                       :: wvtflag_pass
   real,    optional, intent(in),   dimension(ix,jx)   :: runoff1x_in_tr, runoff2x_in_tr
   real,    optional, intent(inout),dimension(numbasns):: z_gwsubbas_tmp_tr
   real,    optional, intent(out),  dimension(numbasns):: qout_gwsubbas_tr_pass
   real,    optional, intent(inout),dimension(numbasns):: qin_gwsubbas_tr_pass
   !----local vars-----
   real,                            dimension(numbasns):: net_perc_tr
   real*8,                          dimension(numbasns):: z_gwsubbas_tr
   real*8,                          dimension(LNLINKSL):: LQLateral_tr
   real,                            dimension(ixrt,jxrt):: runoff1x_tr, runoff2x_tr
   real                                                 :: PROPOR_TR
!-----partial mixing for gw buckt------
   integer, optional, intent(in)                       :: pmopt_pass
   real,    optional, intent(inout),dimension(numbasns):: z_gwsubbas_tmp_passive
   real,    optional, intent(inout),dimension(numbasns):: z_gwsubbas_tmp_active
   real,    optional, intent(inout),dimension(numbasns):: z_gwsubbas_tmp_passive_tr
   real,    optional, intent(inout),dimension(numbasns):: z_gwsubbas_tmp_active_tr
   real,    optional, intent(in),   dimension(numbasns):: cm_gwsubbas_tmp
   !-----local vars------
   real,                            dimension(numbasns):: net_perc_in_tr, net_perc_out_tr
   real,                            dimension(numbasns):: z_gwsubbas_passive, z_gwsubbas_active
   real,                            dimension(numbasns):: z_gwsubbas_passive_tr, z_gwsubbas_active_tr
   real,                            dimension(numbasns):: z_gwsubbas_decrease
   real,                            dimension(numbasns):: z_gwsubbas_passive_new, z_gwsubbas_active_new
   real,                            dimension(numbasns):: z_gwsubbas_passive_diff, z_gwsubbas_active_diff
   real                                                :: PROPOR_qin, PROPOR_passive, PROPOR_active
!------------------------------ 

   real, dimension(numbasns)                         :: net_perc
   integer, dimension(numbasns)                      :: nhdBuckMask

   integer                                           :: i,j,bas, k, m, ii,jj

   integer :: AGGFACYRT, AGGFACTRT, AGGFACXRT, IXXRT, JYYRT
   real*8,  dimension(LNLINKSL) :: LQLateral



!!!Initialize variables...
   net_perc = 0.
   qout_gwsubbas = 0.
   z_gwsubbas(1:numbasns) = z_gwsubbas_tmp(1:numbasns)

   if (present(wvtflag_pass)) then
     net_perc_tr = 0.
     qout_gwsubbas_tr_pass = 0.
     z_gwsubbas_tr(1:numbasns) = z_gwsubbas_tmp_tr(1:numbasns)
   end if
   if (present(pmopt_pass)) then
     z_gwsubbas_passive(1:numbasns)    = z_gwsubbas_tmp_passive(1:numbasns)
     z_gwsubbas_active(1:numbasns)     = z_gwsubbas_tmp_active(1:numbasns)
     z_gwsubbas_passive_tr(1:numbasns) = z_gwsubbas_tmp_passive_tr(1:numbasns)
     z_gwsubbas_active_tr(1:numbasns)  = z_gwsubbas_tmp_active_tr(1:numbasns)
   end if

   if(channelBucket_only .eq. 0) then

      !! Initialize if not passed in
      qin_gwsubbas = 0.
      if (present(wvtflag_pass)) qin_gwsubbas_tr_pass = 0.

!Assign local value of runoff2 (drainage) for flux caluclation to buckets...

        do J=1,JX
        do I=1,IX
             do AGGFACYRT=AGGFACTRT-1,0,-1
             do AGGFACXRT=AGGFACTRT-1,0,-1
               IXXRT=I*AGGFACTRT-AGGFACXRT
               JYYRT=J*AGGFACTRT-AGGFACYRT
#ifdef MPP_LAND  
       if(left_id.ge.0) IXXRT=IXXRT+1
       if(down_id.ge.0) JYYRT=JYYRT+1
!              if(AGGFACTRT .eq. 1) then
!                  IXXRT=I
!                  JYYRT=J
!             endif
#endif
!DJG Implement subgrid weighting routine...
               if( (runoff1x_in(i,j) .lt. 0) .or. (runoff1x_in(i,j) .gt. 1000) ) then
                    runoff1x(IXXRT,JYYRT) = 0
                    if (present(wvtflag_pass)) runoff1x_tr(IXXRT,JYYRT) = 0
               else
                    runoff1x(IXXRT,JYYRT)=runoff1x_in(i,j)*area_lsm(I,J)     &
                        *INFXSWGT(IXXRT,JYYRT)/cellArea(IXXRT,JYYRT)
                    if (present(wvtflag_pass)) then
                      call calculate_wt_ratio(runoff1x_in_tr(i,j),runoff1x_in(i,j),PROPOR_TR)
                      runoff1x_tr(IXXRT,JYYRT) =  runoff1x(IXXRT,JYYRT)*PROPOR_TR
                    end if
               endif

               if( (runoff2x_in(i,j) .lt. 0) .or. (runoff2x_in(i,j) .gt. 1000) ) then
                    runoff2x(IXXRT,JYYRT) = 0
                    if (present(wvtflag_pass)) runoff2x_tr(IXXRT,JYYRT) = 0
               else
                  runoff2x(IXXRT,JYYRT)=runoff2x_in(i,j)*area_lsm(I,J)     &
                      *INFXSWGT(IXXRT,JYYRT)/cellArea(IXXRT,JYYRT)
                  if (present(wvtflag_pass)) then
                    call calculate_wt_ratio(runoff2x_in_tr(i,j),runoff2x_in(i,j),PROPOR_TR)
                    runoff2x_tr(IXXRT,JYYRT)=runoff2x(IXXRT,JYYRT)*PROPOR_TR
                  end if
               endif
             enddo
             enddo
        enddo
        enddo


       LQLateral = 0
       if (present(wvtflag_pass)) LQLateral_tr = 0
       do k = 1, LNUMRSL
              ! get from land grid runoff
               do m = 1, LUDRSL(k)%ncell 
                   ii =  LUDRSL(k)%cell_i(m)
                   jj =  LUDRSL(k)%cell_j(m)
                   if(ii .gt. 0 .and. jj .gt. 0) then
                      if(OVRTSWCRT.ne.1) then
                           LQLateral(k) = LQLateral(k)+runoff1x(ii,jj)*LUDRSL(k)%cellWeight(m)/1000 &
                               *cellArea(ii,jj)
!                           if (present(wvtflag_pass)) then
!                             LQLateral_tr(k) = LQLateral_tr(k) + runoff1x_tr(ii,jj)*LUDRSL(k)%cellWeight(m)/1000 &
!                               *cellArea(ii,jj)
!                           end if
                      endif
                           LQLateral(k) = LQLateral(k)+runoff2x(ii,jj)*LUDRSL(k)%cellWeight(m)/1000 &
                               *cellArea(ii,jj)
                           if (present(wvtflag_pass)) then
                             LQLateral_tr(k) = LQLateral_tr(k) + runoff2x_tr(ii,jj)*LUDRSL(k)%cellWeight(m)/1000 &
                               *cellArea(ii,jj)
                           end if
                   endif
               end do
       end do


#ifdef MPP_LAND
       call updateLinkV(LQLateral, net_perc)      ! m^3
       if (present(wvtflag_pass)) call updateLinkV(LQLateral_tr, net_perc_tr)   !wt var

#else
       net_perc = LQLateral        ! m^3
       if (present(wvtflag_pass)) net_perc_tr = LQLateral_tr     !wt var
#endif

    endif !! if channelBucket_only .eq. 0 else

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Loop through GW basins to adjust for inflow/outflow
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



   DO bas=1,numbasns     ! Loop for GW bucket calcs...
      if(nhdBuckMask(bas) .eq. 1) then     ! if the basn is masked

         if(channelBucket_only .eq. 0) then
            !! If not using channelBucket_only, save qin_gwsubbas
            qin_gwsubbas(bas) = net_perc(bas)             !units (m^3)
            if (present(wvtflag_pass)) qin_gwsubbas_tr_pass(bas) = net_perc_tr(bas)
         else 
            !! If using channelBucket_only, get net_perc from the passed qin_gwsubbas
            net_perc(bas)     = qin_gwsubbas(bas)         !units (m^3)
            if (present(wvtflag_pass)) net_perc_tr(bas) = qin_gwsubbas_tr_pass(bas)
         end if

         ! !Adjust level of GW depth...(conceptual GW bucket units (mm))
         z_gwsubbas(bas) = z_gwsubbas(bas) + net_perc(bas) / basns_area(bas)   ! m
         if (present(wvtflag_pass)) then
           z_gwsubbas_tr(bas) = z_gwsubbas_tr(bas) + net_perc_tr(bas) / basns_area(bas)   ! m
           if (present(pmopt_pass)) then
             z_gwsubbas_passive_new(bas)  = min(z_gwsubbas(bas), 0.1)  !was 0.1
             z_gwsubbas_passive_diff(bas) = z_gwsubbas_passive_new(bas) - z_gwsubbas_passive(bas)
             z_gwsubbas_active_new(bas)   = z_gwsubbas(bas) - z_gwsubbas_passive_new(bas)
             z_gwsubbas_active_diff(bas)  = z_gwsubbas_active_new(bas) - z_gwsubbas_active(bas)
             call calculate_wt_ratio(qin_gwsubbas_tr_pass(bas), qin_gwsubbas(bas), PROPOR_qin)
             if (z_gwsubbas_passive_diff(bas) > 1.0E-10) then
                z_gwsubbas_passive_tr(bas)  = z_gwsubbas_passive_tr(bas) + z_gwsubbas_passive_diff(bas)*PROPOR_qin
                z_gwsubbas_active_tr(bas)   = z_gwsubbas_active_tr(bas)  + z_gwsubbas_active_diff(bas)*PROPOR_qin
                call calculate_wt_ratio(z_gwsubbas_passive_tr(bas), z_gwsubbas_passive_new(bas), PROPOR_passive)
                call calculate_wt_ratio(z_gwsubbas_active_tr(bas),  z_gwsubbas_active_new(bas),  PROPOR_active)
             else
                if (abs(z_gwsubbas_passive_diff(bas)) <= 1.0E-10) then
                  call calculate_wt_ratio(z_gwsubbas_active_tr(bas)+z_gwsubbas_active_diff(bas)*PROPOR_qin,z_gwsubbas_active_new(bas), PROPOR_active)
                  net_perc_in_tr(bas)  = net_perc(bas)*cm_gwsubbas_tmp(bas)*PROPOR_active
                  call calculate_wt_ratio(z_gwsubbas_passive_tr(bas) + net_perc_in_tr(bas)/basns_area(bas),z_gwsubbas_passive_new(bas)+net_perc(bas)*cm_gwsubbas_tmp(bas)/basns_area(bas), PROPOR_passive)
                  net_perc_out_tr(bas) = net_perc(bas)*cm_gwsubbas_tmp(bas)*PROPOR_passive*cm_gwsubbas_tmp(bas)
                  z_gwsubbas_passive_tr(bas) = z_gwsubbas_passive_tr(bas) + (net_perc_in_tr(bas)-net_perc_out_tr(bas))/basns_area(bas)
                  z_gwsubbas_active_tr(bas)  = z_gwsubbas_active_tr(bas) + z_gwsubbas_active_diff(bas)*PROPOR_qin - (net_perc_in_tr(bas)-net_perc_out_tr(bas))/basns_area(bas)
                  call calculate_wt_ratio(z_gwsubbas_passive_tr(bas), z_gwsubbas_passive_new(bas), PROPOR_passive)
                  call calculate_wt_ratio(z_gwsubbas_active_tr(bas), z_gwsubbas_active_new(bas), PROPOR_active)
                end if
             end if
             z_gwsubbas_passive(bas)  = z_gwsubbas_passive_new(bas)
             z_gwsubbas_active(bas)   = z_gwsubbas_active_new(bas)
           end if
         end if

         ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !Calculate baseflow as a function of GW bucket depth...
         ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(GWBASESWCRT.eq.1) then  !active exponential bucket for bucket model discharge type

            !DJG...Estimation of bucket 'overflow' (qout_spill) if/when bucket gets filled...
            qout_spill = 0.
            z_gw_spill = 0.
            if (present(wvtflag_pass)) then
              if (z_gwsubbas(bas) .gt. 1.0E-10) then
                PROPOR_TR = min(1.0, max(0.0, z_gwsubbas_tr(bas)/z_gwsubbas(bas)))
              else
                PROPOR_TR = 0.0
              end if
            end if

            !!DJG...convert z_mx to millimeters...for v2 and later...
            !yw  added by Wei Yu...If block is to accomodate old parameter file...
            !                    if(z_mx(bas) .gt. 5) then
            !                         z_mx(bas) = z_mx(bas) /1000    ! change from mm to meters
            !                    endif
 
               if (z_gwsubbas(bas).gt.z_mx(bas)/1000.) then  !If/then for bucket overflow case...

                    z_gw_spill = z_gwsubbas(bas) - z_mx(bas)/1000.    ! meters
                    z_gwsubbas(bas) = z_mx(bas)/1000.    ! meters

               else
                      z_gw_spill = 0.
               end if   ! End if for bucket overflow case...

               qout_spill = z_gw_spill*(basns_area(bas))/DT  !amount spilled from bucket overflow...units (m^3/s)

!DJG...Maximum estimation of bucket outlfow that is limited by total quantity in bucket...
               qout_max = z_gwsubbas(bas)*(basns_area(bas))/DT   ! (m^3/s)   ! Estimate max bucket disharge limit to total volume in bucket...(m^3/s)


! Assume exponential relation between z/zmax and Q...
!DJG force asymptote to zero to prevent 'overdraft'... 
               qout_gwsubbas(bas) = C(bas)*(EXP(ex(bas)*z_gwsubbas(bas)/(z_mx(bas)/1000.))-1) !Exp.model. q_out (m^3/s)
       
!DJG...Calculation of max bucket outlfow that is limited by total quantity in bucket...
               qout_gwsubbas(bas) = MIN(qout_max,qout_gwsubbas(bas))   ! Limit bucket discharge to max. bucket limit   (m^3/s)
               !----water tracer calc----
               if (present(wvtflag_pass)) then
                 qout_gwsubbas_tr_pass(bas) = qout_gwsubbas(bas)*PROPOR_TR
               end if

          elseif (GWBASESWCRT.eq.2) then  !Pass through/steady-state bucket

! Assuming a steady-state (inflow=outflow) model...
!DJG convert input and output units to cms...       qout_gwsubbas(bas) = qin_gwsubbas(bas)  !steady-state model...(m^3)
               qout_gwsubbas(bas) = qin_gwsubbas(bas)/DT  !steady-state model...(m^3/s)
               if (present(wvtflag_pass)) qout_gwsubbas_tr_pass(bas) = qin_gwsubbas_tr_pass(bas)/DT  !wt var

          end if    ! End if for bucket model discharge type....


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Adjust level of GW depth in bucket...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


          z_gwsubbas(bas) = z_gwsubbas(bas) - qout_gwsubbas(bas)*DT/( &
                       basns_area(bas) )   ! units (meters)	
          

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Combine calculated bucket discharge and amount spilled from bucket...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          qout_gwsubbas(bas) = qout_gwsubbas(bas) + qout_spill   ! units (m^3/s)
          if (present(wvtflag_pass)) then
            z_gwsubbas_tr(bas)         = z_gwsubbas(bas)*PROPOR_TR
            qout_gwsubbas_tr_pass(bas) = qout_gwsubbas_tr_pass(bas) + qout_spill*PROPOR_TR
            if (present(pmopt_pass)) then
              z_gwsubbas_decrease(bas)   = qout_gwsubbas(bas)*DT/(basns_area(bas) )
              if (z_gwsubbas_decrease(bas) > z_gwsubbas_active(bas)) then
                qout_gwsubbas_tr_pass(bas) = (z_gwsubbas_active(bas)*PROPOR_active + (z_gwsubbas_decrease(bas)-z_gwsubbas_active(bas))*PROPOR_passive)*(basns_area(bas) )/DT
                z_gwsubbas_passive_tr(bas) = z_gwsubbas_passive_tr(bas) - (z_gwsubbas_decrease(bas)-z_gwsubbas_active(bas))*PROPOR_passive
                z_gwsubbas_active_tr(bas)  = 0.0
                z_gwsubbas_passive(bas)    = z_gwsubbas_passive(bas) - (z_gwsubbas_decrease(bas)-z_gwsubbas_active(bas))
                z_gwsubbas_active(bas)     = 0.0
              else
                qout_gwsubbas_tr_pass(bas) = (z_gwsubbas_decrease(bas)*PROPOR_active)*(basns_area(bas) )/DT
                z_gwsubbas_active_tr(bas)  = z_gwsubbas_active_tr(bas) -z_gwsubbas_decrease(bas)*PROPOR_active
                z_gwsubbas_active(bas)     = z_gwsubbas_active(bas) - z_gwsubbas_decrease(bas)
              end if
            end if
          end if
      else
          qout_gwsubbas(bas) = 0.0
          if (present(wvtflag_pass)) qout_gwsubbas_tr_pass(bas) = 0.0
      endif   ! the basns is masked


   END DO                 ! End loop for GW bucket calcs...

   z_gwsubbas_tmp(1:numbasns) = z_gwsubbas(1:numbasns)     ! units (meters)
   if (present(wvtflag_pass)) then
     z_gwsubbas_tmp_tr(1:numbasns)  = z_gwsubbas_tr(1:numbasns)
     if (present(pmopt_pass)) then
       z_gwsubbas_tmp_passive(1:numbasns)    = z_gwsubbas_passive(1:numbasns)
       z_gwsubbas_tmp_active(1:numbasns)     = z_gwsubbas_active(1:numbasns)
       z_gwsubbas_tmp_passive_tr(1:numbasns) = z_gwsubbas_passive_tr(1:numbasns)
       z_gwsubbas_tmp_active_tr(1:numbasns)  = z_gwsubbas_active_tr(1:numbasns)
     end if
   end if
    
   return

!------------------------------------------------------------------------------
   End subroutine simp_gw_buck_nhd
!------------------------------------------------------------------------------

!-----------------------------------------
! Huancui Hu: calculate_wt_ratio
subroutine calculate_wt_ratio (a,b,c)

  implicit none
  real,    intent(in)     :: a
  real,    intent(in)     :: b
  real,    intent(inout)  :: c

  if (b .gt. 1.0E-10) then
    c = min(1.0,max(a/b, 0.0))
  else
    c = 0.0
  end if

end subroutine
!-----------------------------------------


!------------------------------------------------------------------------------
!DJG   Simple GW Bucket Model
!------------------------------------------------------------------------------

   subroutine simp_gw_buck(ix,jx,ixrt,jxrt,numbasns,gnumbasns,basns_area,basnsInd,gw_strm_msk_lind,&
                            gwsubbasmsk, runoff1x_in, runoff2x_in, z_gwsubbas_tmp, qin_gwsubbas,&
                            qout_gwsubbas,qinflowbase,gw_strm_msk,gwbas_pix_ct,dist,DT,&
                            C,ex,z_mx,GWBASESWCRT,OVRTSWCRT)
   implicit none
   
!!!Declarations...
   integer, intent(in)                               :: ix,jx,ixrt,jxrt
   integer, intent(in)                               :: numbasns, gnumbasns
   integer, intent(in), dimension(ix,jx)             :: gwsubbasmsk
   real, intent(in), dimension(ix,jx)                :: runoff2x_in 
   real, dimension(ix,jx)                            :: runoff2x 
   real, intent(in), dimension(ix,jx)                :: runoff1x_in
   real, dimension(ix,jx)                            :: runoff1x
   real, intent(in)                                  :: basns_area(numbasns),dist(ixrt,jxrt,9),DT
   integer, intent(in)                                  :: basnsInd(numbasns)
   real, intent(in),dimension(numbasns)              :: C,ex,z_mx
   real, intent(out),dimension(numbasns)             :: qout_gwsubbas
   real, intent(out),dimension(numbasns)             :: qin_gwsubbas
   real*8                                            :: z_gwsubbas(numbasns)
   real                                              :: qout_max, qout_spill, z_gw_spill
   real, intent(inout),dimension(numbasns)           :: z_gwsubbas_tmp
   real, intent(out),dimension(ixrt,jxrt)            :: qinflowbase
   integer, intent(in),dimension(ixrt,jxrt)          :: gw_strm_msk, gw_strm_msk_lind
   integer, intent(in)                               :: GWBASESWCRT
   integer, intent(in)                               :: OVRTSWCRT
   

   real*8, dimension(numbasns)                      :: sum_perc8,ct_bas8
   real, dimension(numbasns)                        :: sum_perc
   real, dimension(numbasns)                        :: net_perc

   real, dimension(numbasns)                        :: ct_bas
   real, dimension(numbasns)                        :: gwbas_pix_ct
   integer                                          :: i,j,bas, k
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
         if(gwsubbasmsk(i,j).eq.basnsInd(bas) ) then
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
    call gw_sum_real(sum_perc8,numbasns,gnumbasns,basnsInd)
    call gw_sum_real(ct_bas8,numbasns,gnumbasns,basnsInd)
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
#ifdef HYDRO_D
       write (6,*) "Bucket spilling...", bas, z_gwsubbas(bas), z_mx(bas), z_gw_spill
#endif
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
           if (gw_strm_msk_lind(i,j).gt.0) then

             qinflowbase(i,j) = qout_gwsubbas(gw_strm_msk_lind(i,j))*1000.*DT/ &
                gwbas_pix_ct(gw_strm_msk_lind(i,j))/dist(i,j,9)     ! units (mm) that gets passed into chan routing as stream inflow

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
   subroutine pix_ct_1(in_gw_strm_msk,ixrt,jxrt,gwbas_pix_ct,numbasns,gnumbasns,basnsInd)
      USE module_mpp_land
      implicit none
      integer ::    i,j,ixrt,jxrt,numbasns, bas, gnumbasns, k
      integer,dimension(ixrt,jxrt) :: in_gw_strm_msk
      integer,dimension(global_rt_nx,global_rt_ny) :: gw_strm_msk
      real,dimension(numbasns) :: gwbas_pix_ct 
      real,dimension(gnumbasns) :: tmp_gwbas_pix_ct 
      integer, intent(in), dimension(:) :: basnsInd

      gw_strm_msk = 0


      call write_IO_rt_int(in_gw_strm_msk, gw_strm_msk)    
    
      call mpp_land_sync() 

      if(my_id .eq. IO_id) then
!        tmp_gwbas_pix_ct = 0.0
!         do bas = 1,gnumbasns  
!         do i=1,global_rt_nx
!           do j=1,global_rt_ny
!             if(gw_strm_msk(i,j) .eq. bas) then
!                tmp_gwbas_pix_ct(bas) = tmp_gwbas_pix_ct(bas) + 1.0
!             endif
!           end do
!         end do
!         end do

            tmp_gwbas_pix_ct = 0.0
            do i=1,global_rt_nx
              do j=1,global_rt_ny
                if(gw_strm_msk(i,j) .gt. 0) then
                   bas = gw_strm_msk(i,j)
                   tmp_gwbas_pix_ct(bas) = tmp_gwbas_pix_ct(bas) + 1.0
               endif
              end do
            end do
      end if

      call mpp_land_sync() 

      if(gnumbasns .gt. 0) then
         call mpp_land_bcast_real(gnumbasns,tmp_gwbas_pix_ct)
      endif
      do k = 1, numbasns
         bas = basnsInd(k)
         gwbas_pix_ct(k) = tmp_gwbas_pix_ct(bas)
      end do


      return
   end subroutine pix_ct_1
#endif





end module module_GW_baseflow   
