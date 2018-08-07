module module_reservoir_routing_base



    
    
    !type, abstract :: reservoir_base_properties_struct     
    
    !end type


    !type, abstract :: reservoir_static_struct     
    
    !end type
    
    

    !!!!reservoir base??? 
    !!!!reservoir routing base
    


    type :: reservoir_base_input_struct

        real, allocatable, dimension(:,:) :: reservoir_inflow


	
        real, allocatable, dimension(:,:) :: water_elevation   !this is inout, include this???


	contains

	
	procedure :: init => reservoir_base_input_struct_init
	procedure :: destroy => reservoir_base_input_struct_destroy


    end type





    type :: reservoir_base_output_struct

        real, allocatable, dimension(:,:) :: reservoir_outflow


	
        real, allocatable, dimension(:,:) :: water_elevation   !this is inout, include this???


	contains

	
	procedure :: init => reservoir_base_output_struct_init
	procedure :: destroy => reservoir_base_output_struct_destroy


    end type








    type :: levelpool_base_struct     
    
    end type

    
    type, abstract :: levelpool_parameters_struct     
    
    end type
    
    
    type, abstract :: machine_learning_base_struct     
    
    end type
    

    type, abstract :: machine_learning_parameters_struct     
    
    end type
    

    
    type, abstract :: reservoir_routing_base_struct 
      

	type(reservoir_base_input_struct), pointer :: input
	type(reservoir_base_output_struct), pointer :: output     
    
	
	contains

	
	procedure (run_reservoir_release_interface), deferred :: run_release
	

    end type
    


    abstract interface
        subroutine run_reservoir_release_interface(reservoir_data)    
            import :: reservoir_routing_base_struct
            class(reservoir_routing_base_struct), intent(inout) :: reservoir_data
        end subroutine
    end interface
    
    

    !constructor for the reservoir input struct
    subroutine reservoir_base_input_struct_init(this)      !I don't think I need to pass any parameters here?? 	
	implicit none
    	class (reservoir_base_input_struct), intent(inout) :: this ! object being initialized


        logical :: allocation_error = .false.


        ! allocate reservoir_inflow
        if ( .not. allocated(this%reservoir_inflow) ) then
            allocate(this%reservoir_inflow())             !Is this correct???
            this%reservoir_inflow = 0.0
        else
            allocation_error = .true.
        end if



        ! allocate water_elevation
        if ( .not. allocated(this%water_elevation) ) then
            allocate(this%water_elevation())             !Is this correct???
            this%water_elevation = 0.0
        else
            allocation_error = .true.
        end if


   
    end subroutine reservoir_base_input_struct_init





    ! destructor for the reservoir input struct
    subroutine reservoir_base_input_struct_destroy(this)
        implicit none
        class (reservoir_base_input_struct), intent(inout) :: this ! object being destroyed

        logical :: allocation_error = .false.

        ! deallocate reservoir_inflow
        if (  allocated(this%reservoir_inflow) ) then
            deallocate(this%reservoir_inflow)
        else
            allocation_error = .true.
        end if


        ! deallocate water_elevation
        if (  allocated(this%water_elevation) ) then
            deallocate(this%water_elevation)
        else
            allocation_error = .true.
        end if

    
    end subroutine reservoir_base_input_struct_destroy






    !constructor for the reservoir output struct
    subroutine reservoir_base_output_struct_init(this)      !I don't think I need to pass any parameters here?? 	
	implicit none
    	class (reservoir_base_output_struct), intent(inout) :: this ! object being initialized


        logical :: allocation_error = .false.


        ! allocate reservoir_outflow
        if ( .not. allocated(this%reservoir_outflow) ) then
            allocate(this%reservoir_outflow())             !Is this correct???
            this%reservoir_outflow = 0.0
        else
            allocation_error = .true.
        end if



        ! allocate water_elevation
        if ( .not. allocated(this%water_elevation) ) then
            allocate(this%water_elevation())             !Is this correct???  Allocating within the input and the output???
            this%water_elevation = 0.0
        else
            allocation_error = .true.
        end if


   
    end subroutine reservoir_base_output_struct_init





    ! destructor for the reservoir output struct
    subroutine reservoir_base_output_struct_destroy(this)
        implicit none
        class (reservoir_base_output_struct), intent(inout) :: this ! object being destroyed

        logical :: allocation_error = .false.

        ! deallocate reservoir_outflow
        if (  allocated(this%reservoir_outflow) ) then
            deallocate(this%reservoir_outflow)
        else
            allocation_error = .true.
        end if


        ! deallocate water_elevation
        if (  allocated(this%water_elevation) ) then
            deallocate(this%water_elevation)
        else
            allocation_error = .true.
        end if

    
    end subroutine reservoir_base_output_struct_destroy



    
end module module_reservoir_routing_base
