module netcdf_layer_base
  use netcdf
  implicit none
  
  type, abstract :: NetCDF_layer_
     character(len=255) :: name
   contains
     
     procedure (create_file_signature), pass(object), deferred :: create_file
     procedure (open_file_signature), pass(object), deferred :: open_file
     procedure, pass(object) :: put_att
     procedure, pass(object) :: def_dim
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

     function open_file_signature(path, mode, ncid, chunksize) result(nf90_open)
        character (len = *), intent(in   ) :: path
        integer,             intent(in   ) :: mode
        integer,             intent(  out) :: ncid
        integer, optional,   intent(inout) :: chunksize
        integer                            :: nf90_open
      end function open_file_signature
     
  end interface

  type, extends(NetCDF_layer_) :: NetCDF_serial_
   contains
     procedure, pass(object) :: create_file => create_file_serial
     procedure, pass(object) :: open_file => open_file_serial
     procedure, pass(object) :: put_att => put_att_serial
     procedure, pass(object) :: def_dim => def_dim_serial
     procedure, pass(object) :: inq_varid => inq_varid_serial
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

  function open_file_serial (object, path, mode, ncid, chunksize) result(nf90_open)
    class(NetCDF_serial_),  intent(in) :: object
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: mode
    integer,             intent(  out) :: ncid
    integer, optional,   intent(inout) :: chunksize
    integer                            :: nf90_open

    nf90_open = nf90_open(path, mode, ncid=ncid)

  end function open_file_serial

  function put_att_serial (object, ncid, varid, name, values) result(nf90_put_att)
    class(NetCDF_serial_),  intent(in) :: object
    integer,            intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    character(len = *), intent( in) :: values
    integer                         :: nf90_put_att
    
    nf90_put_att = nf90_put_att(ncid, varid, name, values)

  end function put_att_serial

  function def_dim_serial (object, ncid, name, len, dimid) result(nf90_def_dim)
    class(NetCDF_serial_),  intent(in) :: object
    integer,             intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer,             intent( in) :: len
    integer,             intent(out) :: dimid
    integer                          :: nf90_def_dim

    nf90_def_dim = nf90_def_dim(ncid,name,len,dimid)

  end function def_dim_serial

  function def_var_serial(object, ncid, name, xtype, dimids, varid) result(nf90_def_var)
    class(NetCDF_serial_),  intent(in) :: object
    integer,                         intent( in) :: ncid
    character (len = *),             intent( in) :: name
    integer,                         intent( in) :: xtype
    integer, dimension(:), optional, intent( in) :: dimids
    integer,                         intent(out) :: varid
    integer                                      :: nf90_def_var
    
    if(present(dimids)) then
       nf90_def_var = nf90_def_var(ncid, name, xtype, dimids, varid)
    else
       nf90_def_var = nf90_def_var(ncid, name, xtype, varid)
    endif

  end function def_var_serial


  function inq_varid_serial (object, ncid, name, varid) result(nf90_inq_varid)
    class(NetCDF_serial_),  intent(in) :: object
    integer,             intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer,             intent(out) :: varid
    integer                          :: nf90_inq_varid
    
    nf90_inq_varid = nf90_inq_varid(ncid, name, varid)

  end function inq_varid_serial
  
  function create_file_parallel(object, path, cmode, initialsize, chunksize, ncid) result(nf90_create)
    class(NetCDF_parallel_),intent(in) :: object
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
    integer, optional,   intent(in   ) :: initialsize
    integer, optional,   intent(inout) :: chunksize
    integer,             intent(  out) :: ncid
    integer                            :: nf90_create
    
    nf90_create = nf90_create(path = path, cmode = cmode, ncid = ncid, &
         &  comm = object%mpi_communicator, info = object&default_info)
    
  end function create_file_parallel

end module netcdf_layer_base
