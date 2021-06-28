!  Program Name: module_river_temperature main function
!  Author(s)/Contact(s): Reza Abdi (rabdi@ucar.edu)
!  Abstract:
!  History Log: First attept, Apr. 21


PROGRAM Island
        use  module_lsm_forcing
        use  module_channel_routing
        use  module_sf_noahmpdrv
        use  disaggregateWeatherModule
        use  module_river_temperature

	implicit none

        type(weather_2D_inputs)          :: weather2D            ! From module_lsf_forcing
        type(temperature_sim_H_inputs)   :: hydraulicsInputs     ! From module_channel_routing
        type(vectorizing_tools)          :: vectorizingInputs    ! From module_channel_routing
        type(NoahMP_vectorizing_tools)   :: noahMPInputs         ! From module_sf_noahmpdrv   
	type(fineWeatherData)		 :: weatherDissag        ! From disaggregateWeatherModule
        type(river_temperature)          :: physicalModel        ! From module_river_temperature

        ! Predefinig the variables here
	INTEGER, parameter                :: IX_test                  = 3		                ! coarse grid i,j dims
        INTEGER, parameter                :: JX_test                  = 3		                ! coarse grid i,j dims
	INTEGER, parameter                :: NLINKS_test              = 12
        INTEGER, parameter                :: reach_number_test        = 9999		! coarse grid i,j dims
	INTEGER, parameter                :: IXRT_test                = 6
        INTEGER, parameter                :: JXRT_test 	              = 6	! fine grid i,j dims
        INTEGER, parameter                :: ims_test                 = 1 
	INTEGER, parameter                :: ime_test                 = 3
	INTEGER, parameter                :: jms_test                 = 1
	INTEGER, parameter                :: jme_test                 = 3	
        INTEGER, parameter                :: nsoil_test               = 2

        REAL,      DIMENSION(IX_test,JX_test)                   :: T2_test	                        ! Air temperature
	REAL,      DIMENSION(IX_test,JX_test)                   :: q2x_test	                        ! Humidity
	REAL,      DIMENSION(IX_test,JX_test)                   :: u_test	                        ! Near surface wind in the u-component
	REAL,      DIMENSION(IX_test,JX_test)                   :: short_test	                ! Shortwave radiation
	! -- for vectorizing
	INTEGER,   DIMENSION(IX_test,JX_test)                   :: CH_NETRT_test, LAKE_MSKRT_test
	INTEGER,   DIMENSION(NLINKS_test)                       :: CHANXI_test, CHANYJ_test 
	character(len=20)                                       :: Reach_number_string_test
	! -- for the stuff that are coming from NoahMP
	!INTEGER                                     :: nsoil_test, ims_test, ime_test, jms_test, jme_test
	REAL,      DIMENSION(ims_test:ime_test,jms_test:jme_test)                   :: TMN_test
	REAL,      DIMENSION(ims_test:ime_test,jms_test:jme_test)                   :: TSK_test
	REAL,      DIMENSION(ims_test:ime_test,nsoil_test,jms_test:jme_test)                 :: TSLB_test
	REAL,      DIMENSION(NLINKS_test)                       :: Tair_fine_1D_test, RelH_fine_1D_test, windSpd_fine_1D_test
 	REAL,      DIMENSION(NLINKS_test)                       :: SolarRad_fine_1D_test, TMN_1D_fine_test, TSK_1D_fine_test, TSLB_1D_fine_test
        
        REAL,      DIMENSION(NLINKS_test)          :: ChanCondConst_test
        REAL,      DIMENSION(IXRT_test,JXRT_test)  :: QSTRMVOLRT_test
        REAL,      DIMENSION(IXRT_test,JXRT_test)  :: QINFLOWBASE_test
        REAL,      DIMENSION(NLINKS_test)          :: QSUM_test              !- m
        REAL,      DIMENSION(NLINKS_test)          :: XS_Peremeter_test      !- m
        REAL,      DIMENSION(NLINKS_test)          :: Bw_test		     !- m
        REAL,      DIMENSION(NLINKS_test)          :: Tw_test	             !- m
        REAL,      DIMENSION(NLINKS_test)          :: velocity_test          !- m/s
        REAL,      DIMENSION(NLINKS_test)          :: HLINK_test	     !- m
        REAL,      DIMENSION(NLINKS_test)          :: ChSSlp_test            !- m/m
        REAL,      DIMENSION(NLINKS_test)          :: ZELEV_test
        INTEGER,   DIMENSION(NLINKS_test)          :: CHANLEN_test
        INTEGER,   DIMENSION(IXRT_test,JXRT_test)  :: CH_NETLNK_test
        REAL,      DIMENSION(NLINKS_test,2)        :: QLINK_test
        REAL,      DIMENSION(NLINKS_test)          :: node_area_test
        INTEGER,   DIMENSION(NLINKS_test)          :: TO_NODE_test
        INTEGER,   DIMENSION(NLINKS_test)          :: TYPEL_test
        INTEGER                                    :: DT_STEPS_test
        REAL                                       :: DTCT_test
        !-- Final product
        REAL,      DIMENSION(NLINKS_test)          :: T_QSUM_test
        !REAL,      DIMENSION(NLINKS_test,2         :: T_simulated

	
	!call weather2D.init(IX_test, JX_test, T2_test, q2x_test, u_test, short_test)
        !call weather2D.inputs()
        !call vectorizingInputs.init(NLINKS_test, IXRT_test, JXRT_test, CH_NETRT_test, CHANXI_test, CHANYJ_test, LAKE_MSKRT_test)
        !call vectorizingInputs.inputs_vec()
        !call noahMPInputs.init(ims_test ,ime_test, jms_test ,jme_test, TMN_test, TSK_test, TSLB_test, nsoil_test)
        !call noahMPInputs.noahMP_inputs()

        !call weatherDissag.init(IX_test, JX_test, IXRT_test, JXRT_test, T2_test, q2x_test, u_test, short_test, NLINKS_test, CH_NETRT_test, &
	!			CHANXI_test, CHANYJ_test, LAKE_MSKRT_test, TMN_test, TSK_test, TSLB_test, nsoil_test, reach_number_test, &
        !                       ims_test,ime_test, jms_test,jme_test, &
        !                       Tair_fine_1D_test, RelH_fine_1D_test, windSpd_fine_1D_test, SolarRad_fine_1D_test, TMN_1D_fine_test,&
        !                       TSK_1D_fine_test,TSLB_1D_fine_test)
        !call weatherDissag.run(Tair_fine_1D_test, RelH_fine_1D_test, windSpd_fine_1D_test, SolarRad_fine_1D_test, TMN_1D_fine_test, TSK_1D_fine_test, TSLB_1D_fine_test)

 
        call physicalModel.init(NLINKS_test, IXRT_test, JXRT_test, reach_number_test, ChanCondConst_test, QSTRMVOLRT_test,  &
			        QINFLOWBASE_test, QSUM_test, XS_Peremeter_test, Bw_test, Tw_test, velocity_test, HLINK_test, ChSSlp_test, &
			        CHANXI_test, CHANYJ_test, ZELEV_test, CHANLEN_test, CH_NETLNK_test, QLINK_test, DT_STEPS_test, DTCT_test, node_area_test, TO_NODE_test, TYPEL_test, &
                                IX_test, JX_test, T2_test, q2x_test, u_test, short_test, CH_NETRT_test, LAKE_MSKRT_test, nsoil_test, ims_test,&
                                ime_test, jms_test, jme_test, TMN_test, TSK_test, TSLB_test, &
                                Tair_fine_1D_test, RelH_fine_1D_test, windSpd_fine_1D_test, SolarRad_fine_1D_test, TMN_1D_fine_test, TSK_1D_fine_test,TSLB_1D_fine_test, T_QSUM_test)
     
	call physicalModel.run(T_QSUM_test)




	!print *, "T2_test, before allocating = ", T2_test
        !print *, "------------------------"
        !print *, "T2, after allocating = ", weatherDissag.weather_2D.T2
        !!T2_test = weather2D.T2

        print *, "------------------------"
        print *, "Tair_fine, beore allocating = ",Tair_fine_1D_test


        !print *, "from lsm IX = ", weather2D.IX
	! call module_lsm_forcing()
        !call module_channel_routing()
        !call module_sf_noahmpdrv()

	!call disaggregateWeatherModule()
	!call module_river_temperature()
        print *, "Done"


END PROGRAM Island
