!2345678
       subroutine wrf_drv_WRHHYDRO(grid,its,ite,jts,jte)
          use module_wrf_HYDRO, only: wrf_cpl_HYDRO
          USE module_domain, ONLY : domain 
          implicit none
           integer:: its,ite,jts,jte
          TYPE ( domain ), INTENT(INOUT) :: grid
!         return

          if(grid%num_nests .lt. 1) then
             call wrf_cpl_HYDRO(grid,its,ite,jts,jte)  ! do not run the off-line lsm
          endif
       end subroutine wrf_drv_WRHHYDRO

