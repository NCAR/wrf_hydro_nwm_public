module module_nhd_data
    use module_nhd_state
    use module_nhd_basin
    use module_nhd_properties
    use module_groundwater_output
    use module_groundwater_input
    use module_groundwater_static_data
    use module_groundwater_base
    implicit none

    type, extends(groundwater_base_data_struct) :: nhd_groundwater_struct

        type (nhd_state_interface), pointer :: state => null()
        type (nhd_properties_interface), pointer :: properties => null()
        type (nhd_basin_interface), pointer :: basin => null()
        type (groundwater_input_struct), pointer :: input => null()
        type (groundwater_output_struct), pointer :: output => null()
        type (groundwater_static_struct), pointer :: static_data => null()

    contains

        procedure :: init => nhd_groundwater_struct_init
        procedure :: destroy => nhd_groundwater_struct_destroy
        procedure :: run_timestep => run_nhd_groundwater_timestep
    end type nhd_groundwater_struct

contains

    !constructor for the ground water struct
    subroutine nhd_groundwater_struct_init(this)
        implicit none
        class (nhd_groundwater_struct), intent(inout) :: this ! object being initialized
    end subroutine nhd_groundwater_struct_init

    ! destructor for the ground water struct
    subroutine nhd_groundwater_struct_destroy(this)
        implicit none
        class (nhd_groundwater_struct), intent(inout) :: this ! object being destroyed
    end subroutine nhd_groundwater_struct_destroy

    subroutine run_nhd_groundwater_timestep(ground_water_data)
        class(nhd_groundwater_struct), intent(inout) :: ground_water_data

        use module_UDMAP, only: LNUMRSL, LUDRSL

        implicit nonerunoff1x_in

        !!!Declarations...
        !integer, intent(in)                               :: ix,jx,ixrt,jxrt
        !integer, intent(in)                               :: numbasns, lnlinksl
        !real, intent(in), dimension(ix,jx)                :: runoff2x_in

        !real, intent(in), dimension(ix,jx)                :: runoff1x_in, area_lsm
        !real, intent(in)                                  :: cellArea(ixrt,jxrt),DT
        !real, intent(in),dimension(numbasns)              :: C,ex
        !real, intent(inout),dimension(numbasns)              :: z_mx
        !real, intent(out),dimension(numbasns)             :: qout_gwsubbas
        !! intent inout for channelBucket_only .eq. 1
        !real, intent(inout),dimension(numbasns)           :: qin_gwsubbas

        !real, intent(inout),dimension(:)                  :: z_gwsubbas_tmp
        !real, intent(in),dimension(ixrt,jxrt)             :: INFXSWGT
        !integer, intent(in)                               :: GWBASESWCRT
        !integer, intent(in)                               :: OVRTSWCRT
        !real, intent(in), dimension(numbasns)             :: basns_area
        !integer, intent(in)                               :: channelBucket_only
        !integer, intent(in)                               :: AGGFACTRT
        real, dimension(ixrt,jxrt)                            :: runoff2x , runoff1x
        real*8,  dimension(LNLINKSL) :: LQLateral
        real*8                                            :: z_gwsubbas(numbasns)
        real                                              :: qout_max, qout_spill, z_gw_spill
        real, dimension(numbasns)                         :: net_perc
        integer, dimension(numbasns)                      :: nhdBuckMask
        integer                                           :: i,j,bas, k, m, ii,jj
        integer :: aggfacyrt, aggfacxrt, ixxrt, jyyrt


        !!!Initialize variables...
        net_perc = 0.
        ground_water_data%output%qout_gwsubbas = 0.
        z_gwsubbas(1:numbasns) = ground_water_data%state%z_gwsubbas(1:numbasns)

        if(ground_water_data%static_data%channel_bucket_only .eq. 0) then

            !! Initialize if not passed in
            ground_water_data%input%qin_gwsubbas = 0.

            !Assign local value of runoff2 (drainage) for flux caluclation to buckets...

            ! -------------------------------- TODO -------------------------------
            ! instead of creating two run off arrays only populate a single run off
            ! array that holds the data that will actually be used
            ! ----------------------------------------------------------------------
            do j=1,ground_water_data%static_data%jx_lsm
                do i=1,ground_water_data%static_data%ix_lsm
                    do aggfacyrt=ground_water_data%basin%agg_factor -1, 0, -1
                        do aggfacxrt=ground_water_data%basin%agg_factor - 1, 0, -1
                            ixxrt=i*ground_water_data%basin%agg_factor - aggfacxrt
                            jyyrt=j*ground_water_data%basin%agg_factor - aggfacyrt
#ifdef MPP_LAND
                            if(left_id.ge.0) ixxrt=ixxrt+1
                            if(down_id.ge.0) jyyrt=jyyrt+1
                            !              if(AGGFACTRT .eq. 1) then
                            !                  IXXRT=I
                            !                  JYYRT=J
                            !             endif
#endif
                            !DJG Implement subgrid weighting routine...
                            if( (ground_water_data%input%infxsrt(i,j) .lt. 0) .or. (ground_water_data%input%infxsrt(i,j) .gt. 1000) ) then
                                runoff1x(ixxrt,jyyrt) = 0
                            else
                                runoff1x(ixxrt,jyyrt) = ground_water_data%input%infxsrt(i,j) * area_lsm(i,j)     &
                                    * ground_water_data%basin%infxswgt(ixxrt,jyyrt)/cellarea(ixxrt,jyyrt)
                            endif

                            if( (ground_water_data%input%soldrain(i,j) .lt. 0) .or. (ground_water_data%input%soldrain(i,j) .gt. 1000) ) then
                                runoff2x(ixxrt,jyyrt) = 0
                            else
                                runoff2x(ixxrt,jyyrt) = ground_water_data%input%soldrain(i,j) * area_lsm(i,j)     &
                                    * ground_water_data%basin%infxswgt(ixxrt,jyyrt)/cellarea(ixxrt,jyyrt)
                            endif
                        enddo
                    enddo
                enddo
            enddo

            LQLateral = 0
            do k = 1, LNUMRSL
                ! get from land grid runoff
                do m = 1, LUDRSL(k)%ncell
                    ii =  LUDRSL(k)%cell_i(m)
                    jj =  LUDRSL(k)%cell_j(m)
                    if(ii .gt. 0 .and. jj .gt. 0) then
                        if(ground_water_data%static_data%ovrtswcrt .ne. 1) then
                            LQLateral(k) = LQLateral(k) + runoff1x(ii,jj) * LUDRSL(k)%cellWeight(m)/1000 * cellArea(ii,jj)
                        endif
                        LQLateral(k) = LQLateral(k) + runoff2x(ii,jj) * LUDRSL(k)%cellWeight(m)/1000 * cellArea(ii,jj)
                    endif
                end do
            end do


#ifdef MPP_LAND
            call updateLinkV(LQLateral, net_perc)      ! m^3
#else
            net_perc = LQLateral        ! m^3
#endif

        endif !! if ground_water_data%static_data%channel_bucket_only .eq. 0 else

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!Loop through GW basins to adjust for inflow/outflow
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        DO bas=1,numbasns     ! Loop for GW bucket calcs...
            if(ground_water_data%basin%nhd_bucket_mask(bas) .eq. 1) then     ! if the basn is masked

                if(ground_water_data%basin%channel_bucket_only .eq. 0) then
                    !! If not using channelBucket_only, save qin_gwsubbas
                    ground_water_data%input%qin_gwsubbas(bas) = net_perc(bas)             !units (m^3)
                else
                    !! If using channelBucket_only, get net_perc from the passed qin_gwsubbas
                    net_perc(bas)     = ground_water_data%input%qin_gwsubbas(bas)         !units (m^3)
                end if

                ! !Adjust level of GW depth...(conceptual GW bucket units (mm))
                ground_water_data%state%z_gwsubbas(bas) = ground_water_data%state%z_gwsubbas(bas) + net_perc(bas) / basns_area(bas)   ! m

                ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                !Calculate baseflow as a function of GW bucket depth...
                ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                if(ground_water_data%static_data%gwbaseswcrt .eq. 1) then  !active exponential bucket for bucket model discharge type

                    !DJG...Estimation of bucket 'overflow' (qout_spill) if/when bucket gets filled...
                    qout_spill = 0.
                    z_gw_spill = 0.

                    !!DJG...convert z_mx to millimeters...for v2 and later...
                    !yw  added by Wei Yu...If block is to accomodate old parameter file...
                    !                    if(z_mx(bas) .gt. 5) then
                    !                         z_mx(bas) = z_mx(bas) /1000    ! change from mm to meters
                    !                    endif

                    if (ground_water_data%state%z_gwsubbas(bas) .gt. z_mx(bas) / 1000.) then  !If/then for bucket overflow case...

                        z_gw_spill = ground_water_data%state%z_gwsubbas(bas) - z_mx(bas)/1000.    ! meters
                        ground_water_data%state%z_gwsubbas(bas) = z_mx(bas)/1000.    ! meters

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

                elseif (ground_water_data%static_data%gwbaseswcrt .eq. 2) then  !Pass through/steady-state bucket

                    ! Assuming a steady-state (inflow=outflow) model...
                    !DJG convert input and output units to cms...       qout_gwsubbas(bas) = qin_gwsubbas(bas)  !steady-state model...(m^3)
                    ground_water_data%output%qout_gwsubbas(bas) = ground_water_data%input%qin_gwsubbas(bas)/DT  !steady-state model...(m^3/s)
                else
                    ! TODO make model fail if we have an undefined bucket type
                end if    ! End if for bucket model discharge type....

                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                !Adjust level of GW depth in bucket...
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                ground_water_data%state%z_gwsubbas(bas) = ground_water_data%state%z_gwsubbas(bas) - &
                    ground_water_data%output%qout_gwsubbas(bas) * DT/( ground_water_data%basin%basin_area(bas) )   ! units (meters)

                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                !Combine calculated bucket discharge and amount spilled from bucket...
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                ground_water_data%output%qout_gwsubbas(bas) = ground_water_data%output%qout_gwsubbas(bas) + qout_spill   ! units (m^3/s)
            else
                ground_water_data%output%qout_gwsubbas(bas) = 0.0
            endif   ! the basns is masked

        END DO   ! End loop for GW bucket calcs...

        ground_water_data%state%z_gwsubbas(1:numbasns) = z_gwsubbas(1:numbasns)     ! units (meters)

        return

    end subroutine
end module module_nhd_data
