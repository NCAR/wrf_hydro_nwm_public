
!  Program Name: module_river_temperature main function
!  Author(s)/Contact(s): Reza Abdi (rabdi@ucar.edu)
!  Abstract:
!  History Log: First attept, Apr. 21

module module_channel_routing

	! -- Added by Reza
	! -- two types
	! -- to module_channel_routing
	! --------------------------------------------------------------------------
        implicit none

        !integer, parameter            :: IXRT = 6
        !integer, parameter            :: JXRT = 6
        !integer, parameter            :: NLINKS = 12

	type :: temperature_sim_H_inputs
		INTEGER                                  :: NLINKS, IXRT,JXRT
		REAL, DIMENSION(:), allocatable          :: ChanCondConst
		REAL, DIMENSION(:,:),allocatable         :: QSTRMVOLRT	        ! Overland flow
		REAL, DIMENSION(:,:), allocatable        :: QINFLOWBASE	        ! Groundwater flow
		REAL, DIMENSION(:), allocatable          :: QSUM     		! Mass bal of node, flow from upstream
		REAL, DIMENSION(:), allocatable          :: XS_Peremeter	        ! wetted perimeter (m)
		REAL, DIMENSION(:), allocatable          :: Bw			! Bottom width (m)
		REAL, DIMENSION(:), allocatable          :: Tw			! top width before bankfull (m)
		!Real, DIMENSION(:),allocatable          :: velocity		! Velocity (m/s)
	        REAL, DIMENSION(:), allocatable          :: velocity
         	REAL, DIMENSION(:), allocatable          :: HLINK	        ! Water head (m)
		REAL, DIMENSION(:), allocatable          :: ChSSlp		! Channel slope (m/m)
		INTEGER, DIMENSION(:), allocatable       :: CHANXI, CHANYJ
		REAL, DIMENSION(:), allocatable          :: ZELEV	        ! elevation of nodes
		INTEGER, DIMENSION(:), allocatable       :: CHANLEN    	        ! channel lngth (m)
		INTEGER, DIMENSION(:,:), allocatable     :: CH_NETLNK	        ! River channel IDs
		REAL, DIMENSION(:,:), allocatable        :: QLINK
		INTEGER                                  :: DT_STEPS             ! number of timestep in routing
		REAL                                     :: DTCT
		REAL, DIMENSION(:),allocatable           :: node_area
		INTEGER, DIMENSION(:),allocatable        :: TO_NODE
		INTEGER, DIMENSION(:),allocatable        :: TYPEL
	   
	   
		contains
		procedure :: init => temperature_sim_H_inputs_init
		procedure :: destroy => temperature_sim_H_inputs_destroy
 
		
                ! For debugging and testing
                procedure :: inputs_hyd => vectors_and_variables_Hyd

	end type temperature_sim_H_inputs
        
        ! --------------------------------------------------------------------------
	! --------------------------------------------------------------------------
	! -- second type, to be used by the weather disaggregate	
		
	type :: vectorizing_tools
		INTEGER :: NLINKS, IXRT,JXRT
		INTEGER, DIMENSION(:,:), allocatable    :: CH_NETRT, LAKE_MSKRT
		INTEGER, DIMENSION(:), allocatable      :: CHANXI, CHANYJ
	   
		contains
		procedure :: init => vectorizing_tools_init
		procedure :: destroy => vectorizing_tools_destroy 

                ! For debugging and testing
                procedure :: inputs_vec => vectors_and_variables_Vec 
		
	end type vectorizing_tools

	! --------------------------------------------------------------------------
        Contains
	subroutine temperature_sim_H_inputs_init(this, NLINKS, IXRT, JXRT, ChanCondConst, QSTRMVOLRT,  &
						QINFLOWBASE, QSUM,  XS_Peremeter, Bw, Tw, velocity, HLINK, ChSSlp, &
                                                CHANXI, CHANYJ, ZELEV, CHANLEN, CH_NETLNK, QLINK, DT_STEPS, DTCT, node_area, TO_NODE, TYPEL)
		implicit none
		class(temperature_sim_H_inputs), intent(inout) :: this             ! the type object being initialized
		INTEGER, INTENT(in) :: NLINKS, IXRT,JXRT
		REAL, INTENT(in),  DIMENSION(NLINKS)             :: ChanCondConst
                REAL,INTENT(in),   DIMENSION(IXRT,JXRT)          :: QSTRMVOLRT     ! to be called like: QSTRMVOLRT(CHANXI(i),CHANYJ(i)); where i:1 to NLINKS
		REAL, INTENT(in),  DIMENSION(IXRT,JXRT)          :: QINFLOWBASE	
		REAL, INTENT(in),  DIMENSION(NLINKS)             :: QSUM     			
		REAL, INTENT(in),  DIMENSION(NLINKS)             :: XS_Peremeter	
		REAL, INTENT(in),  DIMENSION(NLINKS)             :: Bw			
		REAL, INTENT(in),  DIMENSION(NLINKS)             :: Tw			
		!REAL, INTENT(in),  DIMENSION(:),allocatable	 :: velocity		
		REAL, INTENT(in),  DIMENSION(NLINKS)             :: velocity
                REAL, INTENT(in),  DIMENSION(NLINKS)             :: HLINK			
		REAL, INTENT(in),  DIMENSION(NLINKS)             :: ChSSlp	
		INTEGER, INTENT(in),  DIMENSION(NLINKS)          :: CHANXI, CHANYJ	
		REAL, INTENT(in),     DIMENSION(NLINKS)          :: ZELEV
		INTEGER, INTENT(in), DIMENSION(NLINKS)           :: CHANLEN
		INTEGER, INTENT(in), DIMENSION(IXRT,JXRT)        :: CH_NETLNK
		REAL, INTENT(in),  DIMENSION(IXRT,JXRT)          :: QLINK
		
		INTEGER, INTENT(in)                              :: DT_STEPS               !-- number of timestep in routing
		REAL,    INTENT(in)                              :: DTCT
		REAL ,   INTENT(in), DIMENSION(NLINKS)           :: node_area
		INTEGER, INTENT(in), DIMENSION(NLINKS)           :: TO_NODE
		INTEGER, INTENT(IN), DIMENSION(NLINKS)           :: TYPEL                  !--link
		
		! Assign the values passed in to a particular 
		
		this%NLINKS            = NLINKS
		this%IXRT              = IXRT
		this%JXRT              = JXRT
		this%ChanCondConst     = ChanCondConst
		this%QSTRMVOLRT        = QSTRMVOLRT
		this%QINFLOWBASE       = QINFLOWBASE
		this%QSUM              = QSUM 
		this%XS_Peremeter      = XS_Peremeter
		this%Bw                = Bw
		this%Tw                = Tw
		this%velocity          = velocity
		this%HLINK             = HLINK
		this%ChSSlp            = ChSSlp
		this%CHANXI            = CHANXI
		this%CHANYJ            = CHANYJ
		this%ZELEV             = ZELEV
		this%CHANLEN           = CHANLEN
		this%CH_NETLNK         = CH_NETLNK
		this%QLINK             = QLINK
		this%DT_STEPS          = DT_STEPS
		this%DTCT              = DTCT
		this%node_area         = node_area
		this%TO_NODE           = TO_NODE
		this%TYPEL             = TYPEL
		
	end subroutine temperature_sim_H_inputs_init

	! --------------------------------------------------------------------------

	!temperature_sim_H_inputs Destructor

	subroutine temperature_sim_H_inputs_destroy (this)

		implicit none
		! the type object being destroyed

		class(temperature_sim_H_inputs), intent(inout) :: this 

	end subroutine temperature_sim_H_inputs_destroy 
		
		
	subroutine vectorizing_tools_init(this, NLINKS, IXRT, JXRT, CH_NETRT, &
	                                  CHANXI, CHANYJ, LAKE_MSKRT)
		implicit none
		class(vectorizing_tools), intent(inout)         :: this             ! the type object being initialized
		INTEGER, INTENT(in)                             :: NLINKS, IXRT, JXRT
		INTEGER ,INTENT(in), DIMENSION(IXRT,JXRT)  	:: CH_NETRT, LAKE_MSKRT
		INTEGER, INTENT(in), DIMENSION(NLINKS)  	:: CHANXI, CHANYJ 
		
		this%NLINKS = NLINKS
		this%IXRT = IXRT
		this%JXRT = JXRT
		this%CH_NETRT = CH_NETRT
		this%CHANXI = CHANXI
		this%CHANYJ = CHANYJ
		this%LAKE_MSKRT = LAKE_MSKRT
		
	end subroutine vectorizing_tools_init

	subroutine vectorizing_tools_destroy(this)

		implicit none
		! the type object being destroyed
		class(vectorizing_tools), intent(inout) :: this 

	end subroutine vectorizing_tools_destroy 



	! --------------------------------------------------------------------------
	! --------------------------------------------------------------------------

       	!subroutine vectors_and_variables(ChanCondConst, QSTRMVOLRT, QINFLOWBASE, &
	!				QSUM, XS_Peremeter, Bw, Tw, velocity, HLINK, ChSSlp, &
	!				CHANXI, CHANYJ, ZELEV, CHANLEN, CH_NETLNK, QLINK, DT_STEPS, &
	!				DTCT, node_area, TO_NODE, TYPEL, CH_NETRT, LAKE_MSKRT)

        subroutine vectors_and_variables_Hyd(This)

		!NLINKS = 12
		!IXRT = 6
		!JXRT = 6
        
                ! integer            :: IXRT    = 6
                ! integer            :: JXRT    = 6
                ! integer            :: NLINKS  = 12
        

                !- ChanCondConst: m/s; the i-Tree input is mm/second
	        !!REAL, INTENT(in), DIMENSION(NLINK)                :: ChanCondConst = (/0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18/)
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: ChanCondConst
                !!- QSTRMVOLRT:    m, only for one time step
        	!REAL, INTENT(out), DIMENSION(IXRT,JXRT)            :: QSTRMVOLRT
                !!- QINFLOWBASE:   m, only for one time step
                !REAL, INTENT(out), DIMENSION(IXRT,JXRT)            :: QINFLOWBASE
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: QSUM                   !- m
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: XS_Peremeter           !- m
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: Bw		     !- m
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: Tw	             !- m
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: velocity               !- m/s
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: HLINK		     !- m
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: ChSSlp          	     !- m/m
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: ZELEV
                !INTEGER, INTENT(out), DIMENSION(NLINKS)            :: CHANLEN
                !!- CH_NETLNK: m, only for one time step
                !REAL, INTENT(out), DIMENSION(IXRT,JXRT)            :: CH_NETLNK
                !REAL, INTENT(out), DIMENSION(NLINKS,2)             :: QLINK
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: node_area
                !REAL, INTENT(out), DIMENSION(NLINKS)               :: TO_NODE
                !INTEGER, INTENT(out), DIMENSION(IXRT,JXRT)         :: CH_NETRT                 
                !INTEGER, INTENT(out), DIMENSION(IXRT,JXRT)         :: LAKE_MSKRT              
                !INTEGER, INTENT(out), DIMENSION(NLINKS)            :: TYPEL
                !INTEGER :: DT_STEPS
                !REAL    :: DTCT


                IMPLICIT NONE
                class(temperature_sim_H_inputs), intent(inout)         :: this             ! the type object being initialized

                !this.IXRT    = IXRT
                !this.JXRT    = JXRT
                !this.NLINKS  = NLINKS

                this.ChanCondConst = (/0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18/)
	         
                this.QSTRMVOLRT  = reshape ((/0.,0.1,0.1,0.1,0.,0.,  &
			                     0.,0.,0.,0.1,0.,0.,     &
			                     0.,0.,0.,0.1,0.,0.,     &
			                     0.1,0.1,0.1,0.1,0.1,0., &
			                     0.,0.,0.,0.,0.1,0.,     &
			                     0.,0.,0.,0.,0.1,0./), shape(this.QSTRMVOLRT))
 
       		this.QINFLOWBASE = reshape  ((/0.,0.05,0.05,0.05,0.,0.,   &
			                     0.,0.,0.,0.05,0.,0.,         &
			                     0.,0.,0.,0.05,0.,0.,         &
			                     0.05,0.05,0.05,0.05,0.05,0., &
			                     0.,0.,0.,0.,0.05,0.,         &
			                     0.,0.,0.,0.,0.05,0./), shape(this.QINFLOWBASE))
	
                this.QSUM = (/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0/)
              	
                this.XS_Peremeter = (/2.2,2.2,2.2,2.2,2.2,2.2,2.2,2.2,2.2,2.2,2.2,2.2/)

                this.Bw = (/2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0/)

                this.Tw = (/2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0/)

                this.velocity = (/5.,5.,5.,5.,5.,5.,5.,5.,5.,10.,10.,10./)

                this.HLINK = (/0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1/)

                this.ChSSlp = (/0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01/)

                this.CHANXI = (/2,3,4,4,4,1,2,3,4,5,5,5/)

                this.CHANYJ = (/1,1,1,2,3,4,4,4,4,4,5,6/)

                this.ZELEV = (/100.,97.5,95.,92.5,90.,87.5,85.,82.5,80.,77.5,75.,72.5/)

		this.CHANLEN = (/250,250,250,250,250,250,250,250,250,250,250,250/)

                this.CH_NETLNK = reshape ((/0,1,2,3,0,0,   &
		                            0,0,0,4,0,0,   &
			                    0,0,0,5,0,0,   &
			                    6,7,8,9,10,0,  &
			                    0,0,0,0,11,0,  &
			                    0,0,0,0,12,0/), shape(this.CH_NETLNK))
	        	
		this.QLINK =reshape ((/0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1, &
			          0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1/), shape(this.QLINK))
	
                this.DT_STEPS = 1
		this.DTCT = 0.001
		this.TYPEL =  (/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/)

                this.node_area = (/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/)

	        !- Not sure about the last variable!
         	this.TO_NODE = (/2,3,4,5,9,7,8,9,10,11,12,13/)				


                ! For debugging
                print *, "DONE: module_channel_routing.f90 ; Hydraulic data "
           
					   
	End subroutine vectors_and_variables_Hyd
        

        subroutine vectors_and_variables_Vec(this)
               
                IMPLICIT NONE
                class(vectorizing_tools), intent(inout)         :: this             ! the type object being initialized
                !this.IXRT     = IXRT
                !this.JXRT     = JXRT
                !this.NLINKS   = NLINKS

                !INTEGER, INTENT(out), DIMENSION(NLINKS)            :: CHANXI
                !INTEGER, INTENT(out), DIMENSION(NLINKS)            :: CHANYJ
                !INTEGER, INTENT(out), DIMENSION(IXRT,JXRT)         :: CH_NETRT                 
                !INTEGER, INTENT(out), DIMENSION(IXRT,JXRT)         :: LAKE_MSKRT

                this.CHANXI = (/2,3,4,4,4,1,2,3,4,5,5,5/)
                this.CHANYJ = (/1,1,1,2,3,4,4,4,4,4,5,6/)

              	this.CH_NETRT = reshape ((/-9999,0,0,0,-9999,-9999,          &
			                    -9999,-9999,-9999,0,-9999,-9999, &
			                    -9999,-9999,-9999,0,-9999,-9999, &
			                    0,0,0,0,0,-9999,                 &
			                    -9999,-9999,-9999,-9999,0,-9999, &
			                    -9999,-9999,-9999,-9999,0,-9999/), shape(this.CH_NETRT))

              	!this.CH_NETRT = reshape ((/1,2,3,4,5,6, &
		!	                   7,8,9,10,11,12, &
		!	                   13,14,15,16,17,18, &
		!	                   19,20,21,22,23,24, &
		!	                   25,26,27,28,29,30,&
		!	                   31,32,33,34,35,36/), shape(this.CH_NETRT))
	
                this.LAKE_MSKRT = reshape ((/1,-9999,-9999,-9999,1,1,         &
			                     1,1,1,-9999,1,1,                 &
			                     1,1,1,-9999,1,1,                 &
			                     -9999,-9999,-9999,-9999,-9999,1, &
			                     1,1,1,1,-9999,1,                 &
			                     1,1,1,1,-9999,1/), shape(this.LAKE_MSKRT))


                ! For debugging
                print *, "DONE: module_channel_routing.f90; Vectorzing "


        End subroutine vectors_and_variables_Vec



End module module_channel_routing

