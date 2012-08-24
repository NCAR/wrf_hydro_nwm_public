  
module module_ndhms_io
  implicit none

#include <netcdf.inc>

     contains

     subroutine get_file_dimension(wrfsi_static_flnm, ix,jx)
            implicit none
            character(len=*) wrfsi_static_flnm
            integer ncid , iret, ix,jx, dimid

            iret = nf_open(wrfsi_static_flnm, NF_NOWRITE, ncid)
            if (iret /= 0) then
               write(*,'("Problem opening wrfsi_static file: ''", A, "''")') &
                    trim(wrfsi_static_flnm)
               stop
            endif
        
            iret = nf_inq_dimid(ncid, "west_east", dimid)
        
            if (iret /= 0) then
               stop "nf_inq_dimid:  west_east"
                    endif
        
            iret = nf_inq_dimlen(ncid, dimid, ix)
            if (iret /= 0) then
               stop "nf_inq_dimlen:  west_east"
            endif
        
            iret = nf_inq_dimid(ncid, "south_north", dimid)
            if (iret /= 0) then
                       stop "nf_inq_dimid:  south_north"
            endif
        
            iret = nf_inq_dimlen(ncid, dimid, jx)
            if (iret /= 0) then
               stop "nf_inq_dimlen:  south_north"
            endif
            iret = nf_close(ncid)

     end subroutine get_file_dimension


end module module_ndhms_io
