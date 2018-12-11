module io_manager_base
  implicit none
  
  type :: IOManager_
     class(NetCDF_layer_) :: netcdf_layer
   contains
     procedure (write_restart_signature), pass(object), deferred :: write_rt
  end type IOManager_
  
  abstract interface
     subroutine write_restart_signature(object, in_buff, out_buff)
       use iso_fortran_env
       import IOManager_
       class(IOManager_), intent(in) :: object
     end subroutine write_restart_signature
  end interface

  ! This type should go in a different file
  type, extends(IOManager_) :: IOManager_serial_
     
   contains
     procedure, pass(object) :: write_rt => write_rt_serial
     
  end type IOManager_serial_
  
contains

  subroutine write_rt_serial(object)
    implicit none
    class(IOManager_serial) :: object

    write(*,*) 'Writing the restart file in serial'

  end subroutine write_restart_serial
  
end module io_manager_base
