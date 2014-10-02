!*******************************************************************************
!Subroutine - rapid_routing
!*******************************************************************************
subroutine rapid_routing(ZV_C1,ZV_C2,ZV_C3,ZV_Qext,                            &
                         ZV_QoutinitR,ZV_VinitR,                               &
                         ZV_QoutR,ZV_QoutbarR,ZV_VR,ZV_VbarR)

!PURPOSE
!Performs flow calculation in each reach of a river network using the Muskingum
!method (McCarthy 1938).  Also calculates the volume of each reach using a
!simple first order approximation
!Author: Cedric H. David, 2008 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use netcdf
use rapid_var, only :                                                          &
                   ZS_dtR,IS_R,JS_R,                                           &
                   ZM_Net,                                                     &
                   ZV_b,ZV_babsmax,                                            &
                   ZV_QoutprevR,ZV_VprevR,ZV_QoutRabsmin,ZV_QoutRabsmax,       &
                   ZV_VoutR,ZV_Vext,                                           &
                   ierr,ksp,                                                   &
                   ZS_one,IS_ksp_iter,IS_ksp_iter_max,                         &
                   vecscat,ZV_SeqZero,ZV_pointer,rank,                         &
                   IS_nc_status,IS_nc_id_fil_Qout,IS_nc_id_var_Qout,           &
                   IV_nc_start,IV_nc_count2,                                   &
                   IS_reachbas,JS_reachbas,IM_index_up,                        &
                   IS_opt_routing,IV_nbup,IV_basin_index,                      &
                   BS_opt_influence


implicit none


!*******************************************************************************
!Includes
!*******************************************************************************
#include "finclude/petscsys.h"       
!base PETSc routines
#include "finclude/petscvec.h"  
#include "finclude/petscvec.h90"
!vectors, and vectors in Fortran90 
#include "finclude/petscmat.h"    
!matrices
#include "finclude/petscksp.h"    
!Krylov subspace methods
#include "finclude/petscpc.h"     
!preconditioners
#include "finclude/petscviewer.h"
!viewers (allows writing results in file for example)


!*******************************************************************************
!Intent (in/out), and local variables 
!*******************************************************************************
Vec, intent(in)    :: ZV_C1,ZV_C2,ZV_C3,ZV_Qext,                               &
                      ZV_QoutinitR,ZV_VinitR 
Vec, intent(out)   :: ZV_QoutR,ZV_QoutbarR
Vec                :: ZV_VR,ZV_VbarR

PetscInt :: IS_localsize,JS_localsize
PetscScalar, pointer :: ZV_QoutR_p(:),ZV_QoutprevR_p(:),ZV_QoutinitR_p(:),     &
                        ZV_QoutbarR_p(:),ZV_Qext_p(:),ZV_C1_p(:),ZV_C2_p(:),   &
                        ZV_C3_p(:),ZV_b_p(:),                                  &
                        ZV_babsmax_p(:),ZV_QoutRabsmin_p(:),ZV_QoutRabsmax_p(:)


!*******************************************************************************
!Get local sizes for vectors
!*******************************************************************************
call VecGetLocalSize(ZV_QoutR,IS_localsize,ierr)


!*******************************************************************************
!Set mean values to zero initialize QoutprevR with QoutinitR
!*******************************************************************************
call VecSet(ZV_QoutbarR,0*ZS_one,ierr)                     !Qoutbar=0 
!call VecSet(ZV_VbarR,0*ZS_one,ierr)                        !Vbar=0 
!set the means to zero at beginning of iterations over routing time step

call VecCopy(ZV_QoutinitR,ZV_QoutprevR,ierr)               !QoutprevR=QoutinitR
!call VecCopy(ZV_VinitR,ZV_VprevR,ierr)                     !VprevR=VinitR
!set the previous value to the initial value given as input to subroutine


!*******************************************************************************
!Temporal loop 
!*******************************************************************************
call VecGetArrayF90(ZV_C1,ZV_C1_p,ierr)
call VecGetArrayF90(ZV_C2,ZV_C2_p,ierr)
call VecGetArrayF90(ZV_C3,ZV_C3_p,ierr)
call VecGetArrayF90(ZV_Qext,ZV_Qext_p,ierr)

do JS_R=1,IS_R  !*************LPR the routing is done every 15min----------------
!-------------------------------------------------------------------------------
!Update mean
!-------------------------------------------------------------------------------
call VecAXPY(ZV_QoutbarR,ZS_one/IS_R,ZV_QoutprevR,ierr) 
!Qoutbar=Qoutbar+Qoutprev/IS_R

!call VecAXPY(ZV_VbarR,ZS_one/IS_R,ZV_VprevR,ierr)       
!Vbar=Vbar+Vprev/IS_R

!-------------------------------------------------------------------------------
!Calculation of the right hand size, b
!-------------------------------------------------------------------------------
call MatMult(ZM_Net,ZV_QoutprevR,ZV_b,ierr)                !b2=Net*Qoutprev

call VecGetArrayF90(ZV_b,ZV_b_p,ierr)
call VecGetArrayF90(ZV_QoutprevR,ZV_QoutprevR_p,ierr)

do JS_localsize=1,IS_localsize
     ZV_b_p(JS_localsize)=ZV_b_p(JS_localsize)*ZV_C2_p(JS_localsize)           &
                         +(ZV_C1_p(JS_localsize)+ZV_C2_p(JS_localsize))        &
                         *ZV_Qext_p(JS_localsize)                              &
                         +ZV_C3_p(JS_localsize)*ZV_QoutprevR_p(JS_localsize)
end do

call VecRestoreArrayF90(ZV_QoutprevR,ZV_QoutprevR_p,ierr)
call VecRestoreArrayF90(ZV_b,ZV_b_p,ierr)

!-------------------------------------------------------------------------------
!Routing with PETSc using a matrix method
!-------------------------------------------------------------------------------
if (IS_opt_routing==1) then

call KSPSolve(ksp,ZV_b,ZV_QoutR,ierr)                      !solves A*Qout=b
call KSPGetIterationNumber(ksp,IS_ksp_iter,ierr)
if (IS_ksp_iter>IS_ksp_iter_max) IS_ksp_iter_max=IS_ksp_iter

end if

!-------------------------------------------------------------------------------
!Routing with Fortran using the traditional Muskingum method
!-------------------------------------------------------------------------------
if (IS_opt_routing==2) then

call VecGetArrayF90(ZV_QoutR,ZV_QoutR_p,ierr)
call VecGetArrayF90(ZV_QoutprevR,ZV_QoutprevR_p,ierr)
call VecGetArrayF90(ZV_b,ZV_b_p,ierr)

do JS_reachbas=1,IS_reachbas
     ZV_QoutR_p(JS_reachbas)=ZV_b_p(JS_reachbas)                               &
                            +sum(ZV_C1_p(JS_reachbas)                          &
                                  *ZV_QoutR_p(IM_index_up(JS_reachbas,1:       &
                                   IV_nbup(IV_basin_index(JS_reachbas))))) 
end do
!Taking into account the knowledge of how many upstream locations exist.
!Similar to exact preallocation of network matrix

call VecRestoreArrayF90(ZV_QoutR,ZV_QoutR_p,ierr)
call VecRestoreArrayF90(ZV_QoutprevR,ZV_QoutprevR_p,ierr)
call VecRestoreArrayF90(ZV_b,ZV_b_p,ierr)
end if

!-------------------------------------------------------------------------------
!Calculation of babsmax, QoutRabsmin and QoutRabsmax
!-------------------------------------------------------------------------------
if (BS_opt_influence) then

call VecGetArrayF90(ZV_b,ZV_b_p,ierr)
call VecGetArrayF90(ZV_babsmax,ZV_babsmax_p,ierr)
do JS_localsize=1,IS_localsize
     if (ZV_babsmax_p(JS_localsize)<=abs(ZV_b_p(JS_localsize))) then
         ZV_babsmax_p(JS_localsize) =abs(ZV_b_p(JS_localsize))
     end if
end do
call VecRestoreArrayF90(ZV_b,ZV_b_p,ierr)
call VecRestoreArrayF90(ZV_babsmax,ZV_babsmax_p,ierr)

call VecGetArrayF90(ZV_QoutR,ZV_QoutR_p,ierr)
call VecGetArrayF90(ZV_QoutRabsmin,ZV_QoutRabsmin_p,ierr)
call VecGetArrayF90(ZV_QoutRabsmax,ZV_QoutRabsmax_p,ierr)
do JS_localsize=1,IS_localsize
     if (ZV_QoutRabsmin_p(JS_localsize)>=abs(ZV_QoutR_p(JS_localsize))) then
         ZV_QoutRabsmin_p(JS_localsize) =abs(ZV_QoutR_p(JS_localsize))
     end if
     if (ZV_QoutRabsmax_p(JS_localsize)<=abs(ZV_QoutR_p(JS_localsize))) then
         ZV_QoutRabsmax_p(JS_localsize) =abs(ZV_QoutR_p(JS_localsize))
     end if
end do
call VecRestoreArrayF90(ZV_QoutR,ZV_QoutR_p,ierr)
call VecRestoreArrayF90(ZV_QoutRabsmin,ZV_QoutRabsmin_p,ierr)
call VecRestoreArrayF90(ZV_QoutRabsmax,ZV_QoutRabsmax_p,ierr)

end if

!-------------------------------------------------------------------------------
!Calculation of V (this part can be commented to accelerate parameter 
!estimation in calibration mode)
!-------------------------------------------------------------------------------
!call VecCopy(ZV_QoutR,ZV_VoutR,ierr)                      !Vout=Qout
!call VecScale(ZV_VoutR,ZS_dtR,ierr)                       !Vout=Vout*dt
!!result Vout=Qout*dt
!
!call VecCopy(ZV_Qext,ZV_Vext,ierr)                        !Vext=Qext
!call VecScale(ZV_Vext,ZS_dtR,ierr)                        !Vext=Vext*dt
!!result Vext=Qext*dt
!
!call MatMult(ZM_Net,ZV_VoutR,ZV_VR,ierr)                  !V=Net*Vout
!call VecAXPY(ZV_VR,ZS_one,ZV_Vext,ierr)                   !V=V+Vext
!call VecAXPY(ZV_VR,-ZS_one,ZV_VoutR,ierr)                 !V=V-Vout
!call VecAXPY(ZV_VR,ZS_one,ZV_VprevR,ierr)                 !V=V+Vprev
!!result V=Vprev+(Net*Vout+Vext)-Vout


!-------------------------------------------------------------------------------
!Reset previous
!-------------------------------------------------------------------------------
call VecCopy(ZV_QoutR,ZV_QoutprevR,ierr)              !Qoutprev=Qout
!call VecCopy(ZV_VR,ZV_VprevR,ierr)                    !Vprev=V
!reset previous 


!-------------------------------------------------------------------------------
!optional write outputs
!-------------------------------------------------------------------------------
!call VecScatterBegin(vecscat,ZV_QoutR,ZV_SeqZero,                              &
!                     INSERT_VALUES,SCATTER_FORWARD,ierr)
!call VecScatterEnd(vecscat,ZV_QoutR,ZV_SeqZero,                                &
!                        INSERT_VALUES,SCATTER_FORWARD,ierr)
!call VecGetArrayF90(ZV_SeqZero,ZV_pointer,ierr)
!!if (rank==0) write (99,'(10e10.3)') ZV_pointer
!if (rank==0) IS_nc_status=NF90_PUT_VAR(IS_nc_id_fil_Qout,IS_nc_id_var_Qout,    &
!                                       ZV_pointer,                             &
!                     [IV_nc_start(1),(IV_nc_start(2)-1)*IS_R+JS_R],IV_nc_count2)
!call VecRestoreArrayF90(ZV_SeqZero,ZV_pointer,ierr)


!-------------------------------------------------------------------------------
!End temporal loop
!-------------------------------------------------------------------------------
end do

call VecRestoreArrayF90(ZV_C1,ZV_C1_p,ierr)
call VecRestoreArrayF90(ZV_C2,ZV_C2_p,ierr)
call VecRestoreArrayF90(ZV_C3,ZV_C3_p,ierr)
call VecRestoreArrayF90(ZV_Qext,ZV_Qext_p,ierr)


!*******************************************************************************
!End
!*******************************************************************************
end subroutine rapid_routing
