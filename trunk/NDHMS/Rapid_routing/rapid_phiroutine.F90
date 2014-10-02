!*******************************************************************************
!Subroutine phiroutine
!*******************************************************************************
#ifndef NO_TAO
subroutine rapid_phiroutine(taoapp,ZV_pnorm,ZS_phi,ierr)

!PURPOSE
!calculates a cost function phi as a function of model parameters, using means
!over a given period of time.  The cost function represents the square error
!between calculated flows and observed flows where observations are available
!Author: Cedric H. David, 2008 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use netcdf
use rapid_var, only :                                                          &
                   IS_reachbas,                                                &
                   IV_basin_index,IV_basin_loc,                                &
                   m3_nc_file,Qobs_file,Qfor_file,                             &
                   JS_O,IS_O,JS_RpO,IS_RpO,ZS_TauR,                            &
                   ZM_Obs,ZV_Qobs,                                             &
                   ZV_temp1,ZV_temp2,ZS_phitemp,ZS_phifac,ZV_kfac,             &
                   IS_reachtot,IS_forcingbas,IV_forcing_index,IV_forcing_loc,  &
                   IS_gagebas,IV_gage_index,IV_gage_loc,                       &
                   ZS_knorm,ZS_xnorm,ZV_k,ZV_x,ZS_kfac,ZS_xfac,                &
                   ZV_1stIndex,ZV_2ndIndex,                                    &
                   ZV_C1,ZV_C2,ZV_C3,ZM_A,                                     &
                   ZV_QoutinitO,ZV_QoutinitR,                                  &
                   ZV_QoutbarO,ZV_VinitR,ZV_VR,ZV_VbarR,                       &
                   ZV_QoutR,ZV_QoutbarR,                                       &
                   ZV_Vlat,ZV_Qlat,ZV_Qfor,ZV_Qext,                            &
                   ZV_Qobsbarrec,                                              &
                   ksp,                                                        &
                   ZS_one,temp_char,rank,                                      &
                   ZV_read_reachtot,ZV_read_forcingtot,ZV_read_gagetot,        &
                   IS_nc_status,IS_nc_id_fil_m3,IS_nc_id_var_m3,IV_nc_start,   &
                   IV_nc_count,                                                &
                   IS_opt_phi,BS_opt_forcing,IS_strt_opt


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
#include "finclude/tao_solver.h" 
!TAO solver


!*******************************************************************************
!Intent (in/out), and local variables 
!*******************************************************************************
Vec, intent(in) :: ZV_pnorm
TAO_APPLICATION, intent(inout)  :: taoapp
PetscErrorCode, intent(out) :: ierr
PetscScalar, intent(out):: ZS_phi


!*******************************************************************************
!Set linear system corresponding to current ZV_pnorm and set initial flowrates  
!*******************************************************************************
ZS_phi=0
!initialize phi to zero

call VecDot(ZV_pnorm,ZV_1stIndex,ZS_knorm,ierr)
call VecDot(ZV_pnorm,ZV_2ndIndex,ZS_xnorm,ierr)
call VecCopy(ZV_kfac,ZV_k,ierr)
call VecScale(ZV_k,ZS_knorm,ierr)
call VecSet(ZV_x,ZS_xfac,ierr)
call VecScale(ZV_x,ZS_xnorm,ierr)
!compute ZV_k and ZV_x based on ZV_pnorm and ZV_kfac

call rapid_routing_param(ZV_k,ZV_x,ZV_C1,ZV_C2,ZV_C3,ZM_A)
!calculate Muskingum parameters and matrix ZM_A

call KSPSetOperators(ksp,ZM_A,ZM_A,DIFFERENT_NONZERO_PATTERN,ierr)
call KSPSetType(ksp,KSPRICHARDSON,ierr)                    !default=richardson
call KSPSetFromOptions(ksp,ierr)                           !if runtime options
!Set KSP to use matrix ZM_A

call VecCopy(ZV_QoutinitO,ZV_QoutinitR,ierr)
!copy initial optimization variables into initial routing variables


!*******************************************************************************
!Calculate objective function for the whole period ZS_TauO
!*******************************************************************************

!-------------------------------------------------------------------------------
!Open files
!-------------------------------------------------------------------------------
if (rank==0) then
open(99,file=m3_nc_file,status='old')
close(99)
IS_nc_status=NF90_OPEN(m3_nc_file,NF90_NOWRITE,IS_nc_id_fil_m3)
IS_nc_status=NF90_INQ_VARID(IS_nc_id_fil_m3,'m3_riv',IS_nc_id_var_m3)
end if
if (BS_opt_forcing) open(34,file=Qfor_file,status='old')
open(33,file=Qobs_file,status='old')

!-------------------------------------------------------------------------------
!Read and compute
!-------------------------------------------------------------------------------
IV_nc_start=(/1,IS_strt_opt/)
IV_nc_count=(/IS_reachtot,1/)


do JS_O=1,IS_O

!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
!Read/set upstream forcing
!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
if (BS_opt_forcing .and. IS_forcingbas>0) then
read(34,*) ZV_read_forcingtot
call VecSetValues(ZV_Qfor,IS_forcingbas,IV_forcing_loc,                        &
                  ZV_read_forcingtot(IV_forcing_index),INSERT_VALUES,ierr)
                  !here we only look at the forcing within the basin studied 
call VecAssemblyBegin(ZV_Qfor,ierr)
call VecAssemblyEnd(ZV_Qfor,ierr)           !set Qfor based on reading a file
!Qfor is only available everyday, this is why it's not included in the following 
!loop.
!call PetscPrintf(PETSC_COMM_WORLD,'ZV_Qfor' // char(10),ierr)
!call VecView(ZV_Qfor,PETSC_VIEWER_STDOUT_WORLD,ierr)
end if 

!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
!calculate mean daily flow
!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
call VecSet(ZV_QoutbarO,0*ZS_one,ierr)                 !QoutbarO=0

do JS_RpO=1,IS_RpO   !loop needed since there forcing more frequent than obs

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!Read/set surface and subsurface volumes
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (rank==0) then
IS_nc_status=NF90_GET_VAR(IS_nc_id_fil_m3,IS_nc_id_var_m3,ZV_read_reachtot,    &
                          IV_nc_start,IV_nc_count)
IV_nc_start(2)=IV_nc_start(2)+1
call VecSetValues(ZV_Vlat,IS_reachbas,IV_basin_loc,                            &
                  ZV_read_reachtot(IV_basin_index),INSERT_VALUES,ierr)
end if
call VecAssemblyBegin(ZV_Vlat,ierr)
call VecAssemblyEnd(ZV_Vlat,ierr)           !set Vlat based on reading a file

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!calculation of Q based on V (here first order explicit approx) 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call VecCopy(ZV_Vlat,ZV_Qlat,ierr)            !Qlat=Vlat
call VecScale(ZV_Qlat,1/ZS_TauR,ierr)         !Qlat=Qlat/TauR

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!calculation of Qext 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call VecWAXPY(ZV_Qext,ZS_one,ZV_Qlat,ZV_Qfor,ierr)           !Qext=1*Qlat+Qfor
!Result: Qext=Qlat+Qfor

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!Routing procedure
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call rapid_routing(ZV_C1,ZV_C2,ZV_C3,ZV_Qext,                                  &
                   ZV_QoutinitR,ZV_VinitR,                                     &
                   ZV_QoutR,ZV_QoutbarR,ZV_VR,ZV_VbarR)

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!Update variables
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call VecCopy(ZV_QoutR,ZV_QoutinitR,ierr)

call VecAXPY(ZV_QoutbarO,ZS_one/IS_RpO,ZV_QoutbarR,ierr)
!Qoutbar=QoutbarO+QoutbarR/IS_RpO

enddo                !end of loop to account for forcing more frequent than obs

!-------------------------------------------------------------------------------
!Calculate objective function for current day
!-------------------------------------------------------------------------------
read(33,*) ZV_read_gagetot
call VecSetValues(ZV_Qobs,IS_gagebas,IV_gage_loc,                              &
                  ZV_read_gagetot(IV_gage_index),INSERT_VALUES,ierr)
                  !here we only look at the observations within the basin
                  !studied
call VecAssemblyBegin(ZV_Qobs,ierr)
call VecAssemblyEnd(ZV_Qobs,ierr)           !set Qobs based on reading a file

!-------------------------------------------------------------------------------
!Objective function #1 - for current day - square error
!-------------------------------------------------------------------------------
if (IS_opt_phi==1) then
call VecWAXPY(ZV_temp1,-ZS_one,ZV_Qobs,ZV_QoutbarO,ierr)  !temp1=Qoutbar-Qobs
call VecScale(ZV_temp1,ZS_phifac,ierr)                    !if phi too big      
call MatMult(ZM_Obs,ZV_temp1,ZV_temp2,ierr)               !temp2=Obs*temp1
call VecDot(ZV_temp1,ZV_temp2,ZS_phitemp,ierr)            !phitemp=temp1.temp2
!result phitemp=(Qoutbar-Qobs)^T*Obs*(Qoutbar-Qobs)
end if

!-------------------------------------------------------------------------------
!Objective function #2 - for current day - square error normalized by avg flow
!-------------------------------------------------------------------------------
if (IS_opt_phi==2) then
call VecWAXPY(ZV_temp1,-ZS_one,ZV_Qobs,ZV_QoutbarO,ierr)  !temp1=Qoutbar-Qobs
call VecPointWiseMult(ZV_temp1,ZV_temp1,ZV_Qobsbarrec,ierr)!temp1=temp1.*Qobsbarrec
call MatMult(ZM_Obs,ZV_temp1,ZV_temp2,ierr)               !temp2=Obs*temp1
call VecDot(ZV_temp1,ZV_temp2,ZS_phitemp,ierr)            !phitemp=temp1.temp2
!result phitemp=[(Qoutbar-Qobs).*Qobsbarrec]^T*Obs*[(Qoutbar-Qobs).*Qobsbarrec]
end if

!-------------------------------------------------------------------------------
!adds daily objective function to total objective function
!-------------------------------------------------------------------------------
ZS_phi=ZS_phi+ZS_phitemp
!increments phi for each time step during the desired period of optimization

enddo

!-------------------------------------------------------------------------------
!Close files 
!-------------------------------------------------------------------------------
if (rank==0) IS_nc_status=NF90_CLOSE(IS_nc_id_fil_m3)
close(34)
close(33)


!*******************************************************************************
!Write outputs (parameters and calculated objective function)
!*******************************************************************************
call PetscPrintf(PETSC_COMM_WORLD,'current normalized p=(k,x)',ierr)
call PetscPrintf(PETSC_COMM_WORLD,char(10),ierr)
call VecView(ZV_pnorm,PETSC_VIEWER_STDOUT_WORLD,ierr)
call PetscPrintf(PETSC_COMM_WORLD,'corresponding value of phi',ierr)
call PetscPrintf(PETSC_COMM_WORLD,char(10),ierr)
write(temp_char,'(f10.3)') ZS_phi
call PetscPrintf(PETSC_COMM_WORLD,temp_char // char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)


end subroutine rapid_phiroutine
#endif
