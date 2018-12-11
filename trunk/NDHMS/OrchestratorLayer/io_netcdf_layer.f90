module netcdf_layer_base
  use netcdf
  implicit none
  
  type, abstract :: NetCDF_layer_
     procedure (nf90_open), pointer, nopass :: open_file => nf90_open
     procedure (nf90_put_att), pointer, nopass :: put_att => nf90_put_att
     procedure (nf90_def_dim), pointer, nopass :: def_dim => nf90_def_dim
     procedure (nf90_def_var), pointer, nopass :: def_var => nf90_def_var
     procedure (nf90_inq_varid), pointer, nopass :: inq_varid => nf90_inq_varid
   contains
     procedure (create_file_signature), pass(object), deferred :: create_file
  end type NetCDF_layer_

  abstract interface

     function create_file_signature(path, cmode, initialsize, chunksize, ncid) result(nf90_create)
       character (len = *), intent(in   ) :: path
       integer,             intent(in   ) :: cmode
       integer, optional,   intent(in   ) :: initialsize
       integer, optional,   intent(inout) :: chunksize
       integer,             intent(  out) :: ncid
       integer                            :: nf90_create            
     end function create_file_signature
     
  end interface

  type, extends(NetCDF_layer_) :: NetCDF_serial_
   contains
     procedure, pass(object) :: create_file => create_file_serial
  end type NetCDF_serial_

  type, extends(NetCDF_layer_) :: NetCDF_parallel_
     integer :: MPI_communicator
     integer, parameter :: default_info = MPI_INFO_NULL
   contains
     procedure, pass(object) :: create_file => create_file_parallel
  end type NetCDF_parallel_

contains

  function create_file_serial (object, path, cmode, initialsize, chunksize, ncid) result(nf90_create)
    class(NetCDF_serial_),  intent(in) :: object
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
    integer, optional,   intent(in   ) :: initialsize
    integer, optional,   intent(inout) :: chunksize
    integer,             intent(  out) :: ncid
    integer                            :: nf90_create
    
    nf90_create = nf90_create(path = path, cmode = cmode, ncid = ncid)
    
  end function create_file_serial
  
  function create_file_parallel(object, path, cmode, initialsize, chunksize, ncid) result(nf90_create)
    class(NetCDF_parallel_),intent(in) :: object
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
    integer, optional,   intent(in   ) :: initialsize
    integer, optional,   intent(inout) :: chunksize
    integer,             intent(  out) :: ncid
    integer                            :: nf90_create
    
    nf90_create = nf90_create(path = path, cmode = cmode, ncid = ncid, &
         &  comm = object%mpi_communicator, info = object%default_info)
    
  end function create_file_parallel

end module netcdf_layer_base
