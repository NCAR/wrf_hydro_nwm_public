!2345678
       subroutine hrldas_drv_ndhms(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)
          use module_hrldas_ndhms, only: hrldas_cpl_ndhms
          implicit none
          integer:: ii,jj,kk
          real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
          real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain

             call hrldas_cpl_ndhms(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)  ! do not run the off-line lsm
       end subroutine hrldas_drv_ndhms
