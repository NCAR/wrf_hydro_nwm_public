!2345678
       subroutine hrldas_drv_ndhms(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)
          use module_hrldas_ndhms, only: hrldas_cpl_ndhms
          implicit none
          integer:: ii,jj,kk
          real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
          real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain
             call hrldas_cpl_ndhms(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)  
       end subroutine hrldas_drv_ndhms

       subroutine hrldas_drv_ndhms_ini(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt)
          use module_hrldas_ndhms, only: hrldas_cpl_ndhms_ini
          implicit none
          integer:: ii,jj,kk, kt
          real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
          real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain
             call hrldas_cpl_ndhms_ini(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt)  
       end subroutine hrldas_drv_ndhms_ini
