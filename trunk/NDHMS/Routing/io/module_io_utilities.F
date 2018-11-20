module module_io_utilities

#ifdef MPP_LAND
     use module_mpp_land
#endif

   use netcdf
   implicit none

   contains

            subroutine test123()
                print *, 'in subroutine test123'
            end subroutine test123


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

               !call write_lake_real(inVar,nodelist,n)
               if(my_id .eq. IO_id) then
#endif
                  iret = nf90_inq_varid(ncid,varName, varid)


                print *, 'inq_varid'

                if(iret /= nf90_NoErr) call handle_err2(iret)



                  !iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)
                  iret = nf90_put_var(ncid, varid, inVar, (/1/), (/n/))

                print *, 'put var'
                if(iret /= nf90_NoErr) call handle_err2(iret)


print *, 'ncid'
print *,ncid
print *,'n'
print *,n
print *,'inVar'
print *, inVar
print *,'varName'
print *,varName
print *,'nodelist'
print *,nodelist


#ifdef MPP_LAND
            print *, 'yes mpp'
#else
            print *, 'no mpp'
#endif



#ifdef MPP_LAND
               endif
#endif
               return
            end subroutine w_rst_crt_nc1_lake



    subroutine handle_err2(status)
        implicit none
        integer, intent ( in) :: status
        character :: nf90_strerror
        integer  :: nf90_noerr
        if(status /= nf90_noerr) then
            print *, trim(nf90_strerror(status))
            print *, nf90_strerror(status) !FIXME this line deactivated, but we need to check on it!
            print *, status
            !stop "Stopped"
        end if

    end subroutine handle_err2


end module module_io_utilities


function nf90_strerror(ncerr)
integer, intent( in) :: ncerr
character(len = 80) :: nf90_strerror

end function nf90_strerror
