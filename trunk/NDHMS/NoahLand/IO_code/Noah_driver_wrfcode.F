!program drive rtland
!
!    call drive_rtland(ite-its+1,jte-jts+1, dtbl& 
!          t_phy(its:ite,1,jts:jte), &
!          qv_curr(its:ite,1,jts:jte),         &
!          u_phy(its:ite,1,jts:jte),            &
!          v_phy(its:ite,1,jts:jte),            &
!          p8w(its:ite,1,jts:jte),         &
!          GLW(its:ite,jts:jte)*EMISS(its:ite,jts:jte), & !  XLONG
!          GSW(its:ite,jts:jte)/(1.0-albedo(its:ite,jts:jte)), & ! SHORT
!          rainbl(its:ite,jts:jte)/dtbl               , &    ! PRCP1
!          SNOW(its:ite,jts:jte)*0.001 ,         & ! WEASD
!          SNOWH(its:ite,jts:jte),               & ! SNODEP
!          tsk(its:ite,jts:jte), &
!          hfx(its:ite,jts:jte), &
!          qfx(its:ite,jts:jte), &
!          qsfc(its:ite,jts:jte), &
!          GRDFLX(its:ite,jts:jte), &
!          snowc(its:ite,jts:jte), &
!          canwat(its:ite,jts:jte), &
!          znt(its:ite,jts:jte) &
!       )

        program main
        use  module_rtland
        implicit none
        real cpl_land_dt
        integer ntime

        call rtland_init(ntime)

        call run_rt_land(ntime,cpl_land_dt)

        call land_finish()  
     end
