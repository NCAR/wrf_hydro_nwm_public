module groundwater_base
  implicit none

  type, abstract :: Groundwater_
     
   contains

     procedure (run_groundwater_time_step_interface), deferred ::run_timestep

  end type Groundwater_

  abstract interface
     subroutine run_groundwater_time_step_interface(groundwater_data)
       class(groundwater_base_data_struct) :: groundwater_data
     end subroutine run_groundwater_time_step_interface
  end interface

end module groundwater_base
