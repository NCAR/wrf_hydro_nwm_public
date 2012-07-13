 TYPE diag_field
     ! land model domain
      real, allocatable, dimension(:,:) :: soilmx, runoff1x, &
            runoff2x, runoff3x, etax, &
            ecx, edirx, ettx, acrain, acsnom, esnow2d, drip2d, &
            dewfall, fpar, &
            qfx, prcp_out, etpndx
 end TYPE diag_field
