Module module_LSM_wrf_data
   IMPLICIT NONE
   INTEGER, PARAMETER :: max_domain=5

! define Land Surface model data
#include "lsm_wrf_field.inc"
    TYPE(LSM_WRF) , dimension(max_domain) :: lsm 
    save lsm

CONTAINS

    subroutine lsm_allocate(lsm,ix,jx,nsoil)     
       implicit none
       integer:: ix,jx,nsoil
       TYPE(LSM_WRF) :: lsm
       if(lsm%ix .gt. 1 ) return
       lsm%ix = ix
       lsm%jx = jx


       allocate(lsm%cmc (ix,jx) )
       allocate(lsm%T1 (ix,jx) )
       allocate(lsm%WEASD (ix,jx) )
       allocate(lsm%SNODEP (ix,jx) )
       allocate(lsm%T2 (ix,jx) )
       allocate(lsm%Q2X (ix,jx) )
       allocate(lsm%U (ix,jx) )
       allocate(lsm%V (ix,jx) )
       allocate(lsm%PRES (ix,jx) )
       allocate(lsm%XLONG (ix,jx) )
       allocate(lsm%SHORT (ix,jx) )
       allocate(lsm%PRCP1 (ix,jx) )
       allocate(lsm%HX (ix,jx) )
       allocate(lsm%EtAKIN (ix,jx) )
       allocate(lsm%QSFC (ix,jx) )
       allocate(lsm%SOILHX (ix,jx) )
       allocate(lsm%snowc (ix,jx) )
       allocate(lsm%znt (ix,jx) )
       allocate(lsm%LH (ix,jx) )
       allocate(lsm%ALBEDX (ix,jx) )
       allocate(lsm%SOLTYP (ix,jx) )
       allocate(lsm%BASIN_MSK (ix,jx) )
       allocate(lsm%GREENFRAC (ix,jx) )
       allocate(lsm%ALBEDO12M (ix,jx) )
!      allocate(lsm%latitude (ix,jx) )
!      allocate(lsm%longitude (ix,jx) )
!      allocate(lsm%dist (ix,jx,9) )
! tmp arra 
       allocate(lsm%ETPX (ix,jx) )
#ifdef COUPLED
       allocate(lsm%tbot_wrf (ix,jx) )
       allocate(lsm%zlvl_wrf (ix,jx) )
       allocate(lsm%z0_wrf (ix,jx) )
#endif

!       lsm%dist = -999

       lsm%cmc = 0.000025
       lsm%weasd = 0
       lsm%snodep = 0
       lsm%t1 =282.0 
       lsm%ETPX = 0.0
    end subroutine lsm_allocate

end module module_LSM_wrf_data
