!> @brief simple data module for groundwater
!!
!! Contains the data structures and code used to implement and exponential bucket model for simulation of groundwater
!! in simulations that use the simple scheme

module module_simple_groundwater_data
    use module_bucket_impl
    use module_simple_basin
    use MODULE_mpp_GWBUCKET, only: gw_sum_real
    use config_base, only: nlst
    ! use module_RT_data, only: rt_domain
    implicit none

    !> data type that hold necessary data for exponential bucket groundwater model

    type, extends(bucket_groundwater_struct) :: simple_groundwater_struct

        type (simple_basin_interface), pointer :: basin => null()           !< structure to hold basin interface variables

        logical, private :: pointer_allocation_guard = .false.

    contains

        procedure :: init => simple_groundwater_struct_init
        procedure :: destroy => simple_groundwater_struct_destroy
        procedure :: run_timestep => run_simple_groundwater_timestep
    end type simple_groundwater_struct

contains

    !> Constructor for the simple groundwater struct.
    !! @param this  The simple ground water object being constructed
    !! @param ix  The x dimension of the lsm grid
    !! @param jx  The y dimension of the lsm grid
    !! @param ixrt  The x dimension of the routing grid
    !! @param jxrt  The y dimensions of the routing grid
    !! @param dt  The size of the time step
    !! @param gwbaseswcrt  Groundwater configuration option: 0 = disabled, 1 = exp bucket, 2 = passthrough.
    !! @param ovrtswcrt  Overland routing contributes to groundwater: 0 = off, 1 = on
    !! @param numbasins  The number of basins being modeled in this process
    !! @param gnumbasins  The global number of basins
    !! @param agg_factor  The aggregation factor between the lsm and routing domains
    !! @param cell_area  2D array containing the area of each routing cell
    !! @param cell_area_lsm  2D array contain the area of lsm cell

    subroutine simple_groundwater_struct_init(this, ix, jx, ixrt, jxrt, dt, gwbaseswcrt, ovrtswcrt, numbasins, gnumbasins, agg_factor, cell_area, cell_area_lsm)
        implicit none
        class (simple_groundwater_struct), intent(inout) :: this ! object being initialized
        integer, intent(in) :: ix
        integer, intent(in) :: jx
        integer, intent(in) :: ixrt
        integer, intent(in) :: jxrt
        real, intent(in) :: dt
        integer, intent(in) :: gwbaseswcrt
        integer, intent(in) :: ovrtswcrt
        integer, intent(in) :: numbasins
        integer, intent(in) :: gnumbasins
        integer, intent(in) :: agg_factor
        real, dimension(:,:), intent(in) :: cell_area
        real, dimension(:,:), intent(in) :: cell_area_lsm

        call bucket_groundwater_struct_init( &
            this, &
            ix, jx, &
            ixrt, jxrt, &
            dt, &
            gwbaseswcrt, &
            ovrtswcrt, &
            numbasins, &
            agg_factor, &
            cell_area, &
            cell_area_lsm)

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate basin
            allocate ( this%basin )
            if ( .not. associated(this%basin) ) then
                ! if the basin structure could not be created write and error
                write(0,*) "Failure to allocate groundwater properties structure"
            else
                ! initialize the basin structure
                call this%basin%init(ix, jx, ixrt, jxrt, numbasins, gnumbasins)
            end if

            this%pointer_allocation_guard = .true.
        end if

    end subroutine simple_groundwater_struct_init

    !> Destructor for the simple groundwater struct. Calling this subroutine will deallocate all memory allocated to a
    !! nhd_ground_water structure. If one or more pointer where disassociated or deallocated from code outside this
    !! module they will not be deallocated and an error message will be displayed.
    !! @param this  The nhd groundwater struct being destroyed

    subroutine simple_groundwater_struct_destroy(this)
        implicit none
        class (simple_groundwater_struct), intent(inout) :: this ! object being destroyed

        logical :: io_err = .false.

        call bucket_groundwater_struct_destroy(this)

        ! pointer allocation guard should be set if data structures where properly initalized
        if (this%pointer_allocation_guard  ) then
            !check each pointer then deallocate

            ! check the output structure
            if ( associated(this%basin)) then
                deallocate(this%basin)
            else
                write(0,*) "Error the basin structure the simple groundwater object was not associated"
                io_err = .true.
            end if

            this%pointer_allocation_guard = .false.
        end if

    end subroutine simple_groundwater_struct_destroy

    subroutine run_simple_groundwater_timestep(ground_water_data)
      implicit none
      class(simple_groundwater_struct), intent(inout) :: ground_water_data
      logical :: rv
      real, dimension(ground_water_data%static_data%ix_rt, ground_water_data%static_data%jx_rt,9) :: dist

                      !call simp_gw_buck(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,RT_DOMAIN(did)%ixrt,&
                !    RT_DOMAIN(did)%jxrt,RT_DOMAIN(did)%numbasns,RT_DOMAIN(did)%gnumbasns,&
                !RT_DOMAIN(did)%basns_area,&
                !    RT_DOMAIN(did)%basnsInd, RT_DOMAIN(did)%gw_strm_msk_lind,             &
                !    rt_domain(did)%gwsubbasmsk, RT_DOMAIN(did)%groundwater%input%infxsrt, &
                !    rt_domain(did)%groundwater%input%soldrain, &
                !    RT_DOMAIN(did)%z_gwsubbas,&
                !    rt_domain(did)%groundwater%input%qin_gwsubbas,RT_DOMAIN(did)%groundwater%output%qout_gwsubbas,&
                !    RT_DOMAIN(did)%groundwater&output%qinflowbase,&
                !    RT_DOMAIN(did)%gw_strm_msk,RT_DOMAIN(did)%gwbas_pix_ct, &
                !    rt_domain(did)%overland%properties%distance_to_neighbor,nlst(did)%DT,&
                !    RT_DOMAIN(did)%gw_buck_coeff,RT_DOMAIN(did)%gw_buck_exp, &
                !    RT_DOMAIN(did)%z_max,&
                !    nlst(did)%GWBASESWCRT,nlst(did)%OVRTSWCRT)


        call simp_gw_buck(ground_water_data%static_data%ix_lsm, &
            ground_water_data%static_data%jx_lsm,&
            ground_water_data%static_data%ix_rt, &
            ground_water_data%static_data%jx_rt, &
            ground_water_data%basin%num_basins, &
            ground_water_data%basin%global_num_basins, &
            ground_water_data%basin%basin_area, &
            ground_water_data%basin%basin_index, &
            ground_water_data%basin%stream_mask_index, & !not sure
            ground_water_data%basin%sub_basin_mask, &
            ground_water_data%input%infxsrt, &
            ground_water_data%input%soldrain, &
            ground_water_data%state%z_gwsubbas, &
            ground_water_data%input%qin_gwsubbas, &
            ground_water_data%output%qout_gwsubbas, &
            ground_water_data%output%qinflowbase, &
            ground_water_data%basin%stream_mask, &
            ground_water_data%basin%basin_stream_pixels, &
            ! rt_domain(1)%overland%properties%distance_to_neighbor, &
            dist, &
            nlst(1)%dt, & !supposed to be nlst(did)
            ground_water_data%properties%gw_buck_coeff, &
            ground_water_data%properties%gw_buck_exp, &
            ground_water_data%properties%z_max, &
            ground_water_data%static_data%gwbaseswcrt, &
            ground_water_data%static_data%ovrtswcrt)

!             ground_water_data%aggregation%agg_factor, &
!             ground_water_data%static_data%dt, &
!             ground_water_data%aggregation%infxswgt, &
!             ground_water_data%input%infxsrt, &
!             ground_water_data%properties%cell_area, &
!             ground_water_data%properties%cell_area_lsm, &
! #ifdef MPP_LAND
!             ground_water_data%basin%lnlinksl, &
! #else
!             ground_water_data%basin%num_basins, &
! #endif
!             ground_water_data%basin%basin_area, &
!             ground_water_data%basin%nhd_bucket_mask, &
!             ground_water_data%basin%channel_bucket_only)
      
    end subroutine run_simple_groundwater_timestep

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

end module module_simple_groundwater_data
