module module_nhd_data
    use module_nhd_state
    use module_nhd_basin
    use module_nhd_properties
    !use module_groundwater_output
    !use module_groundwater_input
    use module_groundwater_static_data
    use module_groundwater_base
    use nhd_groundwater_tests
    implicit none

    type, extends(groundwater_base_data_struct) :: nhd_groundwater_struct

        type (nhd_state_interface), pointer :: state => null()
        type (nhd_properties_interface), pointer :: properties => null()
        type (nhd_basin_interface), pointer :: basin => null()
        type (groundwater_static_struct), pointer :: static_data => null()

        logical :: pointer_allocation_guard = .false.
        !integer :: gnumlinks

    contains

        procedure :: init => nhd_groundwater_struct_init
        procedure :: destroy => nhd_groundwater_struct_destroy
        procedure :: run_timestep => run_nhd_groundwater_timestep
    end type nhd_groundwater_struct
! ------------------DEBUG---------------------!
!    integer :: g_num_links
! ------------------DEBUG---------------------!
contains

    !constructor for the ground water struct
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

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write and error
                write(0,*) "Failure to allocate groundwater input structure"
            else
                ! initialize the input structure
                call this%input%init(ix,jx,numbasins)
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write and error
                write(0,*) "Failure to allocate groundwater output structure"
            else
                ! initialize the output structure
                call this%output%init(ixrt,jxrt,numbasins)
            end if

            !try to allocate aggregation
            allocate ( this%aggregation )
            if ( .not. associated(this%aggregation) ) then
                ! if the output structure could not be created write and error
                write(0,*) "Failure to allocate groundwater aggregation structure"
            else
                ! initialize the output structure
                call this%aggregation%init(ixrt,jxrt,agg_factor)
            end if

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the input structure could not be created write and error
                write(0,*) "Failure to allocate groundwater state structure"
            else
                ! initialize the state structure
                call this%state%init(numbasins)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate properties
            allocate ( this%properties )
            if ( .not. associated(this%properties) ) then
                ! if the properties structure could not be created write and error
                write(0,*) "Failure to allocate groundwater properties structure"
            else
                ! initialize the properties structure
                call this%properties%init( ix, jx, ixrt, jxrt, numbasins, cell_area, cell_area_lsm)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate basin
            allocate ( this%basin )
            if ( .not. associated(this%basin) ) then
                ! if the basin structure could not be created write and error
                write(0,*) "Failure to allocate groundwater properties structure"
            else
                ! initialize the basin structure
                call this%basin%init(ix, jx, numbasins, num_local_links, cb_only, agg_factor)
            end if

            ! try to allocate static data
            allocate ( this%static_data )
            if ( .not. associated(this%static_data) ) then
                ! if the static data structure could not be created write and error
                write(0,*) "Failure to allocate groundwater properties structure"
            else
                ! initialize the static data structure
                call this%static_data%init(ix, jx, ixrt, jxrt, dt, gwbaseswcrt, ovrtswcrt)
            end if

            this%pointer_allocation_guard = .true.
        end if

    end subroutine nhd_groundwater_struct_init

    ! destructor for the ground water struct
    subroutine nhd_groundwater_struct_destroy(this)
        implicit none
        class (nhd_groundwater_struct), intent(inout) :: this ! object being destroyed
    end subroutine nhd_groundwater_struct_destroy

    subroutine run_nhd_groundwater_timestep(ground_water_data)
        implicit none
        class(nhd_groundwater_struct), intent(inout) :: ground_water_data
        logical :: rv

        !rv = nhd_data_info(ground_water_data)

! ------------------DEBUG---------------------!
        !call dump_float_1d_reach(ground_water_data%input%qin_gwsubbas,"can_before_bucket_qin_gwsubbas.txt",ground_water_data%gnumlinks)
        !g_num_links = ground_water_data%gnumlinks
        !write (6,*) "CHANNEL_BUCKET_ONLY = ", ground_water_data%basin%channel_bucket_only

        !write (6,*) "INFXSRT = ", ground_water_data%input%infxsrt
        !write (6,*) "SOIL DRAIN = ", ground_water_data%input%soldrain
        !write (6,*) "Cell Area Lsm = ", ground_water_data%properties%cell_area_lsm
! ------------------DEBUG---------------------!

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

! ------------------DEBUG---------------------!
        !call dump_float_1d_reach(ground_water_data%input%qin_gwsubbas,"can_after_bucket_qin_gwsubbas.txt",ground_water_data%gnumlinks)
! ------------------DEBUG---------------------!

        !print *, "--DEBUG-- after call to bucket"
        flush(6)

    end subroutine


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
        real*8,  dimension(LNLINKSL) :: LQLateral

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

            !print *, "--DEBUG-- start of agg loop"
            !flush(6)
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
            !print *, "--DEBUG-- end of agg loop"
            !flush(6)


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
                            !write (6,*) "LQLateral2 = ", LQLateral(k), runoff1x(ii,jj), LUDRSL(k)%cellWeight(m)/1000, cellArea(ii,jj)
                        endif
                        LQLateral(k) = LQLateral(k)+runoff2x(ii,jj)*LUDRSL(k)%cellWeight(m)/1000 &
                            *cellArea(ii,jj)
                        !write (6,*) "LQLateral = ", LQLateral(k), runoff2x(ii,jj), LUDRSL(k)%cellWeight(m)/1000, cellArea(ii,jj)
                    endif
                end do
            end do
            !print *, "--DEBUG-- end of land grid accumulation loop"
            !flush(6)


#ifdef MPP_LAND
            call updateLinkV(LQLateral, net_perc)      ! m^3
#else
            net_perc = LQLateral        ! m^3
#endif
            !print *, "--DEBUG-- after update to net_perc"
            !flush(6)

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
        !print *, "--DEBUG-- end of groundwater phys"
        !flush(6)

        ! ------------------DEBUG---------------------!
        !call dump_float_1d_reach(qin_gwsubbas,"can_end_of_bucket_qin_gwsubbas.txt",g_num_links)
        ! ------------------DEBUG---------------------!

        return

        !------------------------------------------------------------------------------
    End subroutine simp_gw_buck_nhd
    !------------------------------------------------------------------------------

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
