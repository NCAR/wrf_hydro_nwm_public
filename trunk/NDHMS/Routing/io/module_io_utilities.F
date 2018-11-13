module module_io_utilities

#ifdef MPP_LAND
     use module_mpp_land
#endif

   use netcdf
   implicit none

   contains

            !subroutine w_rst_crt_nc1_lake(ncid,n,inVar,varName,nodelist)



            ! Originally from HYDRO_io.F
            subroutine w_rst_crt_nc1_lake(ncid,n,inVar,varName &
#ifdef MPP_LAND
                     ,nodelist     &
#endif
                      )

            !subroutine w_rst_crt_nc1_lake(ncid,n,inVar,varName,nodelist)

               implicit none
               integer:: ncid,n,varid , iret
               character(len=*) varName
               real inVar(n)
#ifdef MPP_LAND
               integer:: nodelist(n)
               if(n .eq. 0) return

               call write_lake_real(inVar,nodelist,n)
               if(my_id .eq. IO_id) then
#endif
                  iret = nf90_inq_varid(ncid,varName, varid)
                  !iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
                  iret = nf90_put_var(ncid, varid, inVar, (/1/), (/n/))


#ifdef MPP_LAND
               endif
#endif
               return
            end subroutine w_rst_crt_nc1_lake


end module module_io_utilities
