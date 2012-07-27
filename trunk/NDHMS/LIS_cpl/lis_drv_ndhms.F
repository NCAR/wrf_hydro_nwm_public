!2345678
       subroutine lis_drv_ndhms(n)
          use module_lis_ndhms, only: lis_cpl_ndhms
          implicit none
          integer n
          write(6,*) "calling lis_cpl_ndhms "
!         stop 888
          call lis_cpl_ndhms(n)
       end subroutine lis_drv_ndhms
