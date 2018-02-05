! module overland_routing_properties_data.F
! Purpose: This module contains the overland_control_struct class. This types holds
! the mass blance variables used in the overland routing code
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson

module overland_mass_balance

   ! holds variables used for mass balance
   type overland_mass_balance_struct
      ! mass balance
      real(kind=8) :: dsmctot     ! NEED VARIABLE INFO
      !real(kind=8) :: <dsmctot>                          ! NEED VARIABLE INFO
      real(kind=8) :: smctot1     ! NEED VARIABLE INFO
      !real(kind=8) :: pre_soil_moisture_content        ! Pre time step soil moisture content accumulator  (<units>)
      real(kind=8) :: smctot2     ! NEED VARIABLE INFO
      !real(kind=8) :: post_soil_moisture_content       ! Post time step soil moisture content accumulator (<units>)
      real(kind=8) :: suminfxsrt  ! NEED VARIABLE INFO
      !real(kind=8) :: pre_infiltration_excess          ! Pre time step infiltration excess accumulator    (<units>)
      real(kind=8) :: suminfxs1  ! NEED VARIABLE INFO
      !real(kind=8) :: post_infiltration_excess         ! Post time step infiltration excess accumulator   (<units>)
      contains
        procedure :: init => overland_mass_balance_init
        procedure :: destroy => overland_mass_balance_destroy
    end type overland_mass_balance_struct

    contains

! initalize the mass balance variables
subroutine overland_mass_balance_init(this)
    class(overland_mass_balance_struct), intent(inout) :: this ! the type object being initalized
    this%dsmctot = 0.0
    this%smctot1 = 0.0
    this%smctot2 = 0.0
    this%suminfxsrt = 0.0
    this%suminfxs1 = 0.0
end subroutine overland_mass_balance_init

! none of the mass balance variables are dynamicly allocated if such are added
! they should be deallocated here
subroutine overland_mass_balance_destroy(this)
    class(overland_mass_balance_struct), intent(inout) :: this ! the type object being destroyed
end subroutine overland_mass_balance_destroy


end module overland_mass_balance
