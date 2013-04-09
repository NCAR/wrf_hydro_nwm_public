module module_input_data_structure
  use module_llxy
  type input_data_type
     integer                       :: nx     ! Grid Dimension -- X direction
     integer                       :: ny     ! Grid Dimension -- Y direction
     type(proj_info)               :: proj   ! Map Projection information
     character(len=24)             :: hdate  ! Date (YYYY-MM-DD_hh:mm:ss.ffff)
     character(len=9)              :: field  ! Field name
     character(len=25)             :: units  ! Field units
     character(len=46)             :: desc   ! Field description
     real                          :: layer1 ! Top of a layer definition
     real                          :: layer2 ! Bottom of a layer definition
     real, pointer, dimension(:,:) :: data   ! The data array
  end type input_data_type

end module module_input_data_structure
