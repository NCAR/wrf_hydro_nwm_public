Module module_gw_gw2d_data
   IMPLICIT NONE
   INTEGER, PARAMETER :: max_domain=5

#include "gw_field_include.inc"
      type (gw_field) :: gw2d(max_domain)
      save gw2d

end module module_gw_gw2d_data
