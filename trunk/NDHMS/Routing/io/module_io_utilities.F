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
            subroutine w_rst_crt_nc1_lake(ncid,n,inVar, array_index, varName &
                     ,nodelist)




            !subroutine w_rst_crt_nc1_lake(ncid,n,inVar,varName,nodelist)

               implicit none
               integer:: ncid,n,varid , iret, array_index
               character(len=*) varName
               real inVar(n)
               integer:: nodelist(n)
               if(n .eq. 0) return


                print *, 'varid before'
                print *, varid

               iret = nf90_inq_varid(ncid,varName, varid)


                print *, 'varid after'
                print *, varid


                print *, 'array_index'
                print *, array_index

                print *, 'inq_varid'

                if(iret /= nf90_NoErr) call handle_err2(iret)

                !call flush(6)

                  !iret = nf_put_vara_real(ncid, varid, (/1/), (/n/),inVar)



                    ! prints all
                  !iret = nf90_put_var(ncid, varid, inVar, (/1/), (/n/))


                !prints first
                !iret = nf90_put_var(ncid, varid, inVar, (/1/), (/1/))



                !prints very large and very small vals
                !iret = nf90_put_var(ncid, varid, inVar, (/1/), count = (/array_index/))


                !prints second outflow in first slot when entire outflow array passed
                !iret = nf90_put_var(ncid, varid, (/inVar(2)/), (/1/), (/1/))




                !prints second outflow in second slot when entire array passed
                !iret = nf90_put_var(ncid, varid, (/inVar(2)/), (/2/), (/1/))



                !prints a given outflow array of size 1 passed in and places in slot 2
                !iret = nf90_put_var(ncid, varid, inVar, (/2/), (/1/))


                !iret = nf90_var_par_access(ncid, varid, nf90_collective)


                iret = nf90_put_var(ncid, varid, inVar, (/array_index/), (/1/))


                !prints very large numbers
                !iret = nf90_put_var(ncid, varid, (/inVar(2)/), (/1/), (/15/))



                !pass in all outputs and prints nothing
                !iret = nf90_put_var(ncid, varid, inVar, (/15/), (/n/))



                  !iret = nf90_put_var(ncid, varid, inVar, (/1/), (/1/))








                  !iret = nf90_put_var(ncid, varid, inVar, (/array_index/), (/n/))


                  !iret = nf90_put_var(ncid, varid, inVar, (/array_index/))


                    !iret = nf90_put_var(ncid, varid, inVar, (/1/), (/1/))


                !iret = nf90_put_var(ncid, varid, inVar, (/1/))


                !iret = nf90_put_var(ncid, varid, inVar(1), start = (/1/))



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


               return
            end subroutine w_rst_crt_nc1_lake




      subroutine read_rst_crt_nc(ncid,var,n,varStr)
         implicit none
         integer ::  ireg, ncid, varid, n, iret
         real,dimension(n) ::  var
         character(len=*) :: varStr

         if( n .le. 0)  return
#ifdef MPP_LAND
         if(my_id .eq. IO_id) &
#endif
            iret = nf90_inq_varid(ncid,  trim(varStr),  varid)
#ifdef MPP_LAND
         call mpp_land_bcast_int1(iret)
#endif
            if (iret /= 0) then
#ifdef HYDRO_D
               print*, 'variable not found: name = "', trim(varStr)//'"'
#endif
               return
            endif
#ifdef HYDRO_D
         print*, "read restart variable ", varStr
#endif
#ifdef MPP_LAND
         if(my_id .eq. IO_id) then
#endif
            !iret = nf90_get_var_real(ncid, varid, var)
            iret = nf90_get_var(ncid, varid, var)

#ifdef MPP_LAND
         endif
         if(n .gt. 0) then
             call mpp_land_bcast_real(n,var)
         endif
#endif
         return
      end subroutine read_rst_crt_nc















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
