!> @brief NHD data module for groundwater
!!
!! Contains the data structures and code used to implement and exponential bucket model for simulation of groundwater
!! in simulations that use the NHD stream network

module module_nhd_data
    use module_groundwater_static_data
    use module_bucket_impl
    use nhd_groundwater_tests
    implicit none

    !> data type that hold necessary data for exponential bucket groundwater model

    type, extends(bucket_groundwater_struct) :: nhd_groundwater_struct

        !> sub structure to hold state data
        !type (nhd_state_interface), pointer :: state => null()

        !> sub structure to hold property data
        !type (nhd_properties_interface), pointer :: properties => null()

        !> sub structure to hold basin data
        type (nhd_basin_interface), pointer :: basin => null()

        !> sub structure to hold static data
        !type (groundwater_static_struct), pointer :: static_data => null()

        logical, private :: pointer_allocation_guard = .false.
        !integer :: gnumlinks

    contains

        procedure :: init => nhd_groundwater_struct_init
        procedure :: destroy => nhd_groundwater_struct_destroy
        procedure :: run_timestep => run_nhd_groundwater_timestep
    end type nhd_groundwater_struct

contains

    !> Constructor for the nhd groundwater struct.
    !! @param this  The nhd ground water object being constructed
    !! @param ix  The x dimension of the lsm grid
    !! @param jx  The y dimension of the lsm grid
    !! @param ixrt  The x dimension of the routing grid
    !! @param jxrt  The y dimensions of the routing grid
    !! @param dt  The size of the time step
    !! @param gwbaseswcrt  Groundwater configuration option: 0 = disabled, 1 = exp bucket, 2 = passthrough.
    !! @param ovrtswcrt  Overland routing contributes to groundwater: 0 = off, 1 = on
    !! @param numbasins  The number of basins being modeled in this process
    !! @param num_local_links  The number of nhd links modeled in this process
    !! @param cb_only  The flag for channel bucket only
    !! @param agg_factor  The aggregation factor between the lsm and routing domains
    !! @param cell_area  2D array containing the area of each routing cell
    !! @param cell_area_lsm  2D array contain the area of lsm cell

    subroutine nhd_groundwater_struct_init(this, ix, jx, ixrt, jxrt, dt, gwbaseswcrt, ovrtswcrt, numbasins, num_local_links, cb_only, agg_factor, cell_area, cell_area_lsm)
        implicit none
        class (nhd_groundwater_struct), intent(inout) :: this ! object being initialized
        integer, intent(in) :: ix
        integer, intent(in) :: jx
        integer, intent(in) :: ixrt
        integer, intent(in) :: jxrt
        real, intent(in) :: dt
        integer, intent(in) :: gwbaseswcrt
        integer, intent(in) :: ovrtswcrt
        integer, intent(in) :: numbasins
        integer, intent(in) :: num_local_links
        integer, intent(in) :: cb_only
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
                call this%basin%init(ix, jx, numbasins, num_local_links, cb_only, agg_factor)
            end if

            this%pointer_allocation_guard = .true.
        end if

    end subroutine nhd_groundwater_struct_init

    !> Destructor for the nhd groundwater struct. Calling this subroutine will deallocate all memory allocated to a
    !! nhd_ground_water structure. If one or more pointer where disassociated or deallocated from code outside this
    !! module they will not be deallocated and an error message will be displayed.
    !! @param this  The nhd groundwater struct being destroyed

    subroutine nhd_groundwater_struct_destroy(this)
        implicit none
        class (nhd_groundwater_struct), intent(inout) :: this ! object being destroyed

        logical :: io_err = .false.

        call bucket_groundwater_struct_destroy(this)

        ! pointer allocation guard should be set if data structures where properly initalized
        if (this%pointer_allocation_guard  ) then
            !check each pointer then deallocate

            ! check the output structure
            if ( associated(this%basin)) then
                deallocate(this%basin)
            else
                write(0,*) "Error the basin structure the nhd data object was not associated"
                io_err = .true.
            end if

            this%pointer_allocation_guard = .false.
        end if

    end subroutine nhd_groundwater_struct_destroy

    !> Run physics simulation for one timestep on this object
    !! @param ground_water_data  The nhd groundwater struct contains data for simulation

    subroutine run_nhd_groundwater_timestep(ground_water_data)
        implicit none
        class(nhd_groundwater_struct), intent(inout) :: ground_water_data
        logical :: rv

        call simp_gw_buck_nhd(ground_water_data%static_data%ix_lsm, &
            ground_water_data%static_data%jx_lsm,&
            ground_water_data%static_data%ix_rt, &
            ground_water_data%static_data%jx_rt, &
            ground_water_data%basin%num_basins, &
            ground_water_data%aggregation%agg_factor, &
            ground_water_data%static_data%dt, &
            ground_water_data%aggregation%infxswgt, &
            ground_water_data%input%infxsrt, &
            ground_water_data%input%soldrain, &
            ground_water_data%properties%cell_area, &
            ground_water_data%properties%cell_area_lsm, &
            ground_water_data%properties%gw_buck_coeff, &
            ground_water_data%properties%gw_buck_exp, &
            ground_water_data%properties%z_max, &
            ground_water_data%state%z_gwsubbas, &
            ground_water_data%output%qout_gwsubbas, &
            ground_water_data%input%qin_gwsubbas, &
            ground_water_data%static_data%gwbaseswcrt, &
            ground_water_data%static_data%ovrtswcrt, &
#ifdef MPP_LAND
            ground_water_data%basin%lnlinksl, &
#else
            ground_water_data%basin%num_basins, &
#endif
            ground_water_data%basin%basin_area, &
            ground_water_data%basin%nhd_bucket_mask, &
            ground_water_data%basin%channel_bucket_only)

    end subroutine

    !> physics code for the nhd groundwater bucket
    !! @param ix  The size of the x dimension of the LSM grid
    !! @param jx  The size of the y dimension of the LSM grid
    !! @param ixrt  The size of the x dimension of the Routing grid
    !! @param jxrt  The size of the y dimension of the Routing grid
    !! @param numbasns  The number of nhd basins simulated in this process
    !! @param AGGFACTRT  The aggregation factor used to move data from the LSM domain to routing domain
    !! @param DT  The size of the current time step
    !! @param INFXSWGT 2D array of aggregation weights
    !! @param runoff1x_in Input water from overland routing grid
    !! @param runoff2x_in Input water from subsurface routing grid
    !! @param cellArea  2D array containing the area of each routing grid cell
    !! @param area_lsm 2D array containing the area of each LSM grid cell
    !! @param c 1D array containing the bucket coefficient for each modeled basin
    !! @param exp 1D array containing the bucket exponent for each modeled basin
    !! @param z_mx 1D array containing the maximum value of the bucket for each modeled basin
    !! @param z_gwsubbas_tmp 1D array contain the inital water level at each basin, this value is updated to the final value after the time step
    !! @param qout_gwsubbas 1D array giving flow out of each modeled bucket
    !! @param qin_gwsubbas 1D array gving flow into each modeled bucket, only used if cb_only is set to 1
    !! @param GWBASESWCRT  Groundwater configuration option: 0 = disabled, 1 = exp bucket, 2 = passthrough.
    !! @param OVRTSWCRT  Overland routing contributes to groundwater: 0 = off, 1 = on
    !! @param lnlinksl The number of nhd links !FIX ME!
    !! @param basns_area 1D array containing the area of each local basin
    !! @param nhdBuckMask 1D array containing the bucket mask for each local basin
    !! @param cb_only flag 1 = run channel bucket only scheme, 0 = collect water from subsurface and or overland grids

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
            lnlinksl,                         &
            basns_area,                       &
            nhdBuckMask,   channelBucket_only )

        use module_UDMAP, only: LNUMRSL, LUDRSL
#ifdef MPP_LAND
        use module_mpp_land, only: left_id, down_id
        use MODULE_mpp_ReachLS, only: updateLinkV
#endif

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

        real, dimension(numbasns)                         :: net_perc
        integer, dimension(numbasns)                      :: nhdBuckMask

        integer                                           :: i,j,bas, k, m, ii,jj

        integer :: AGGFACYRT, AGGFACTRT, AGGFACXRT, IXXRT, JYYRT
        real*8,  dimension(lnlinksl) :: LQLateral

        !print *, "--DEBUG-- start of groundwater phys"
        !flush(6)

        !!!Initialize variables...
        net_perc = 0.
        qout_gwsubbas = 0.
        z_gwsubbas(1:numbasns) = z_gwsubbas_tmp(1:numbasns)

        if(channelBucket_only .eq. 0) then

            !! Initialize if not passed in
            qin_gwsubbas = 0.

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
                            else
                                runoff1x(IXXRT,JYYRT)=runoff1x_in(i,j)*area_lsm(I,J)     &
                                    *INFXSWGT(IXXRT,JYYRT)/cellArea(IXXRT,JYYRT)
                            endif

                            if( (runoff2x_in(i,j) .lt. 0) .or. (runoff2x_in(i,j) .gt. 1000) ) then
                                runoff2x(IXXRT,JYYRT) = 0
                            else
                                runoff2x(IXXRT,JYYRT)=runoff2x_in(i,j)*area_lsm(I,J)     &
                                    *INFXSWGT(IXXRT,JYYRT)/cellArea(IXXRT,JYYRT)
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
                        if(OVRTSWCRT.ne.1) then
                            LQLateral(k) = LQLateral(k)+runoff1x(ii,jj)*LUDRSL(k)%cellWeight(m)/1000 &
                                *cellArea(ii,jj)
                        endif
                        LQLateral(k) = LQLateral(k)+runoff2x(ii,jj)*LUDRSL(k)%cellWeight(m)/1000 &
                            *cellArea(ii,jj)
                    endif
                end do
            end do


#ifdef MPP_LAND
            call updateLinkV(LQLateral, net_perc)      ! m^3
#else
            net_perc = LQLateral        ! m^3
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
                    !write(6,*) "updated qin_gwsubbas", bas, net_perc(bas)
                else
                    !! If using channelBucket_only, get net_perc from the passed qin_gwsubbas
                    net_perc(bas)     = qin_gwsubbas(bas)         !units (m^3)
                end if

                ! !Adjust level of GW depth...(conceptual GW bucket units (mm))
                z_gwsubbas(bas) = z_gwsubbas(bas) + net_perc(bas) / basns_area(bas)   ! m

                ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                !Calculate baseflow as a function of GW bucket depth...
                ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                if(GWBASESWCRT.eq.1) then  !active exponential bucket for bucket model discharge type

                    !DJG...Estimation of bucket 'overflow' (qout_spill) if/when bucket gets filled...
                    qout_spill = 0.
                    z_gw_spill = 0.

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

                elseif (GWBASESWCRT.eq.2) then  !Pass through/steady-state bucket

                    ! Assuming a steady-state (inflow=outflow) model...
                    !DJG convert input and output units to cms...       qout_gwsubbas(bas) = qin_gwsubbas(bas)  !steady-state model...(m^3)
                    qout_gwsubbas(bas) = qin_gwsubbas(bas)/DT  !steady-state model...(m^3/s)

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
            else
                qout_gwsubbas(bas) = 0.0
            endif   ! the basns is masked


        END DO                 ! End loop for GW bucket calcs...

        z_gwsubbas_tmp(1:numbasns) = z_gwsubbas(1:numbasns)     ! units (meters)

        return

        !------------------------------------------------------------------------------
    End subroutine simp_gw_buck_nhd
    !------------------------------------------------------------------------------

    !> Check the contents and integrity of the data contained in a nhd groundwater struct
    !> @param nhd_data The nhd groundwater struct to query

    function nhd_data_info(nhd_data) result(rv)
        implicit none
        type (nhd_groundwater_struct) :: nhd_data
        integer :: rv
        logical, dimension(7) :: ptr_state
        logical, dimension(7) :: data_state

        rv = 0

        ! Check to see if the output structure exists
        print *, "Checking pointer association on data%input "
        if ( associated(nhd_data%input) ) then
            print *, "PASSED"
            ptr_state(1) = .true.
        else
            print *, "FAILED"
            ptr_state(1) = .false.
        end if
        print *, " "

        ! Check to see if the output structure exists
        print *, "Checking pointer association on data%output "
        if ( associated(nhd_data%output) ) then
            print *, "PASSED"
            ptr_state(2) = .true.
        else
            print *, "FAILED"
            ptr_state(2) = .false.
        end if
        print *, " "

        ! Check to see if the aggregation structure exists
        print *, "Checking pointer association on data%aggregation "
        if ( associated(nhd_data%aggregation) ) then
            print *, "PASSED"
            ptr_state(3) = .true.
        else
            print *, "FAILED"
            ptr_state(3) = .false.
        end if
        print *, " "

        ! Check to see if the state structure exists
        print *, "Checking pointer association on data%state "
        if ( associated(nhd_data%state) ) then
            print *, "PASSED"
            ptr_state(4) = .true.
        else
            print *, "FAILED"
            ptr_state(4) = .false.
        end if
        print *, " "

        ! Check to see if the properties structure exists
        print *, "Checking pointer association on data%properties "
        if ( associated(nhd_data%properties) ) then
            print *, "PASSED"
            ptr_state(5) = .true.
        else
            print *, "FAILED"
            ptr_state(5) = .false.
        end if
        print *, " "

        ! Check to see if the basin structure exists
        print *, "Checking pointer association on data%basin "
        if ( associated(nhd_data%basin) ) then
            print *, "PASSED"
            ptr_state(6) = .true.
        else
            print *, "FAILED"
            ptr_state(6) = .false.
        end if
        print *, " "

        ! Check to see if the static_data structure exists
        print *, "Checking pointer association on data%static_data "
        if ( associated(nhd_data%static_data) ) then
            print *, "PASSED"
            ptr_state(7) = .true.
        else
            print *, "FAILED"
            ptr_state(7) = .false.
        end if
        print *, " "

        ! Now check the data members of each substructure
        if ( ptr_state(1) ) then
            data_state(1) = test_input(nhd_data%input)
        end if

        if ( ptr_state(2) ) then
            data_state(2) = test_output(nhd_data%output)
        end if

        if ( ptr_state(3) ) then
            data_state(3) = test_aggregation(nhd_data%aggregation)
        end if

        if ( ptr_state(4) ) then
            data_state(4) = test_state(nhd_data%state)
        end if

        if ( ptr_state(5) ) then
            data_state(5) = test_nhd_basin(nhd_data%basin)
        end if

        if ( ptr_state(6) ) then
            data_state(6) = test_nhd_properties(nhd_data%properties)
        end if

        if ( ptr_state(7) ) then
            data_state(7) = test_nhd_static_data(nhd_data%static_data)
        end if

        if ( all(ptr_state) .and. all(data_state) ) then
            print *, "========================================================================"
            print *, "All Tests Passed"
            print *, "========================================================================"
        end if
        rv = 1

        flush(6)
    end function nhd_data_info
end module module_nhd_data
