module module_diag_field
   IMPLICIT NONE
   INTEGER, PARAMETER :: max_domain=5
#include "diag_field_include.inc"
   type(diag_field), dimension(max_domain) :: lsm_diag

   contains

   subroutine allocate_diag(lsm_d,ix,jx)
      implicit none
      integer ix,jx
      type(diag_field) :: lsm_d

      ! allocate land model variables
       allocate( lsm_d%soilmx (ix,jx) )
       allocate( lsm_d%runoff1x (ix,jx) )
       allocate( lsm_d%runoff2x (ix,jx) )
       allocate( lsm_d%runoff3x (ix,jx) )
       allocate( lsm_d%etax (ix,jx) )
       allocate( lsm_d%ecx (ix,jx) )
       allocate( lsm_d%edirx (ix,jx) )
       allocate( lsm_d%ettx (ix,jx) )
       allocate( lsm_d%acrain (ix,jx) )
       allocate( lsm_d%acsnom (ix,jx) )
       allocate( lsm_d%esnow2d (ix,jx) )
       allocate( lsm_d%drip2d (ix,jx) )
       allocate( lsm_d%dewfall (ix,jx) )
       allocate( lsm_d%fpar (ix,jx) )
       allocate( lsm_d%qfx (ix,jx) )
       allocate( lsm_d%prcp_out (ix,jx) )
       allocate( lsm_d%etpndx (ix,jx) )

        
        lsm_d%soilmx  = 0
        lsm_d%runoff1x  = 0
        lsm_d%runoff2x  = 0
        lsm_d%runoff3x  = 0
        lsm_d%etax  = 0
        lsm_d%ecx  = 0
        lsm_d%edirx  = 0
        lsm_d%ettx  = 0
        lsm_d%acrain  = 0
        lsm_d%acsnom  = 0
        lsm_d%esnow2d  = 0
        lsm_d%drip2d  = 0
        lsm_d%dewfall  = 0
        lsm_d%fpar  = 0
        lsm_d%qfx  = 0
        lsm_d%prcp_out  = 0
        lsm_d%etpndx  = 0

   end subroutine allocate_diag

end module module_diag_field

