module io_manager_base
  use netcdf_layer_base
  implicit none
  
  type :: IOManager_
     logical :: parallel = .false.
     class(NetCDF_layer_),allocatable :: netcdf_layer
   contains
     procedure (write_restart_signature), pass(object), deferred :: write_rt
  end type IOManager_

  interface IOManager_
     module procedure IOManager_init
  end interface IOManager_
    
contains

  type(IOManager_) function IOManager_init(parallel)
    implicit none

    logical, optional, intent(in) :: parallel

    if(.not.present(parallel) .or. (present(parallel) .and. parallel .eqv. .false.)) then
       IOManager_init%parallel = .false.
       allocate(NetCDF_serial_ :: IOManager_init%netcdf_layer)
    else
       IOManager_init%parallel = .true.
       allocate(NetCDF_parallel_ :: IOManager_init%netcdf_layer)
    end if
    
  end function IOManager_init
  
end module io_manager_base
