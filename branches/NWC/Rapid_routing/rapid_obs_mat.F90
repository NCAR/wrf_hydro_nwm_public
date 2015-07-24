subroutine rapid_obs_mat

!PURPOSE
!Creates a kronecker-type diagonal sparse matrix.  "1" is recorded at the line 
!and column where observations are available.  Calculates IS_gagebas.  Also 
!creates vectors IV_gage_index and IV_gage_loc
!Author: Cedric H. David, 2008 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   IS_reachbas,JS_reachbas,                                    &
                   IS_gagetot,JS_gagetot,IS_gageuse,JS_gageuse,                &
                   IS_gagebas,JS_gagebas,                                      &
                   basin_id_file,                                              &
                   gagetot_id_file,gageuse_id_file,                            &
                   IV_basin_id,IV_gagetot_id,IV_gageuse_id,                    & 
                   IV_gage_index,IV_gage_loc,                                  &
                   ZM_Obs,ZS_norm,                                             &
                   ierr,                                                       &
                   IS_one,ZS_one,temp_char   


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
!Read data files
!*******************************************************************************
open(14,file=basin_id_file,status='old')
!read(14,'(i12)') IV_basin_id
read(14,*) IV_basin_id
close(14)

open(15,file=gagetot_id_file,status='old')
!read(15,'(i12)') IV_gagetot_id
read(15,*) IV_gagetot_id
close(15)

open(16,file=gageuse_id_file,status='old')
!read(16,'(i12)') IV_gageuse_id
read(16,*) IV_gageuse_id
close(16)


!*******************************************************************************
!Calculates IS_gagebas, creates the vectors IV_gage_index and IV_gage_loc
!*******************************************************************************
!-------------------------------------------------------------------------------
!Calculates IS_gagebas
!-------------------------------------------------------------------------------
write(temp_char,'(i10)') IS_gagetot
call PetscPrintf(PETSC_COMM_WORLD,'Number of gages in gagetot_file '       //  &
                 '                :' // temp_char // char(10),ierr)
write(temp_char,'(i10)') IS_gageuse
call PetscPrintf(PETSC_COMM_WORLD,'Number of gages in gageuse_file '       //  &
                 '                :' // temp_char // char(10),ierr)

IS_gagebas=0
!initialize to zero

do JS_gageuse=1,IS_gageuse
     do JS_reachbas=1,IS_reachbas
          if (IV_gageuse_id(JS_gageuse)==IV_basin_id(JS_reachbas)) then
               IS_gagebas=IS_gagebas+1
          end if 
     end do
end do

write(temp_char,'(i10)') IS_gagebas
call PetscPrintf(PETSC_COMM_WORLD,'Number of gages in gageuse_file '       //  &
                 'located in basin:' // temp_char // char(10),ierr)


!-------------------------------------------------------------------------------
!Allocates and populates the vectors IV_gage_index and IV_gage_loc
!-------------------------------------------------------------------------------
allocate(IV_gage_index(IS_gagebas))
allocate(IV_gage_loc(IS_gagebas))
!allocate vector size

do JS_gagebas=1,IS_gagebas
     IV_gage_index(JS_gagebas)=0
     IV_gage_loc(JS_gagebas)=0
end do
!Initialize both vectors to zero

JS_gagebas=1
do JS_gageuse=1,IS_gageuse
do JS_reachbas=1,IS_reachbas
     if (IV_gageuse_id(JS_gageuse)==IV_basin_id(JS_reachbas)) then
          do JS_gagetot=1,IS_gagetot
               if (IV_gageuse_id(JS_gageuse)==IV_gagetot_id(JS_gagetot)) then
                    IV_gage_index(JS_gagebas)=JS_gagetot
               end if
          end do
          IV_gage_loc(JS_gagebas)=JS_reachbas-1
          JS_gagebas=JS_gagebas+1
     end if
end do
end do
!Creates vector IV_gage_index and IV_gage_loc

!print *, 'IV_gage_index=', IV_gage_index 
!print *, 'IV_gage_loc  =', IV_gage_loc 


!*******************************************************************************
!Creation of the observation matrix
!*******************************************************************************
do JS_reachbas=1,IS_reachbas
     do JS_gagebas=1,IS_gagebas
          if (IV_gagetot_id(IV_gage_index(JS_gagebas))==IV_basin_id(JS_reachbas)) then
          call MatSetValues(ZM_Obs,IS_one,JS_reachbas-1,IS_one,JS_reachbas-1,  &
                            ZS_one,INSERT_VALUES,ierr)
!          print *, 'JS_gagebas                           =', JS_gagebas
!          print *, 'IV_gagetot_id(IV_gage_index(JS_gagebas))=', IV_gagetot_id(IV_gage_index(JS_gagebas))
!          print *, 'JS_reachbas                          =', JS_reachbas
!          print *, 'IV_basin_id(JS_reachbas)             =', IV_basin_id(JS_reachbas)
          else
          endif
     enddo 
enddo

call MatAssemblyBegin(ZM_Obs,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_Obs,MAT_FINAL_ASSEMBLY,ierr)
!sparse matrices need be assembled once their elements have been filled


!*******************************************************************************
!Optional: calculation of number of gaging stations used in subbasin
!*******************************************************************************
call MatNorm(ZM_Obs,NORM_FROBENIUS,ZS_norm,ierr)
ZS_norm=ZS_norm*ZS_norm
write(temp_char,'(f5.1)') ZS_norm
call PetscPrintf(PETSC_COMM_WORLD,'Number of gaging stations used (based on norm): '           &
                                  // temp_char // char(10),ierr)


!*******************************************************************************
!End
!*******************************************************************************
call PetscPrintf(PETSC_COMM_WORLD,'Observation matrix created'//char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)

!call PetscPrintf(PETSC_COMM_WORLD,'ZM_Obs:'//char(10),ierr)
!call MatView(ZM_Obs,PETSC_VIEWER_STDOUT_WORLD,ierr)

end subroutine rapid_obs_mat
