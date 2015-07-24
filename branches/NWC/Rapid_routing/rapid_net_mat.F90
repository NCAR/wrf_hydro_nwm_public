subroutine rapid_net_mat

!PURPOSE
!This subroutine is specific for RAPID connectivity tables.  
!Creates a sparse network matrix.  "1" is recorded at Net(i,j) if the reach 
!in column j flows into the reach in line i. If some connection are missing
!between the subbasin and the entire domain, gives warnings.  Also creates four 
!Fortran vectors that are useful for PETSc programming within this river routing 
!model (IV_basin_index,IV_basin_loc,IV_forcing_index,IV_forcing_loc).  
!If forcing is used, the Network matrix is modified to break connections with
!the reach upstream of forcing location
!Author: Cedric H. David, 2008 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   IS_reachtot,IS_reachbas,                                    &
                   JS_reachtot,JS_reachbas,JS_reachbas2,                       &
                   IV_basin_id,IV_basin_index,IV_basin_loc,                    &
                   modcou_connect_file,basin_id_file,                          &
                   forcingtot_id_file,forcinguse_id_file,                      &
                   ZM_Net,ZM_A,BS_logical,IV_connect_id,                       &
                   IV_down,IV_nbup,IM_up,JS_up,IM_index_up,                    &
                   IS_forcingtot,JS_forcingtot,IS_forcingbas,JS_forcingbas,    &
                   IS_forcinguse,JS_forcinguse,IV_forcingtot_id,               &
                   IV_forcing_index,IV_forcing_loc,IV_forcinguse_id,           &
                   ierr,rank,                                                  &
                   IS_one,ZS_one,temp_char,IV_nz,IV_dnz,IV_onz,                &
                   IS_ownfirst,IS_ownlast,                                     &
                   BS_opt_forcing

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
open(10,file=modcou_connect_file,status='old')
do JS_reachtot=1,IS_reachtot
!     read(10,'(7i7)') IV_connect_id(JS_reachtot), IV_down(JS_reachtot),        &
     read(10,*) IV_connect_id(JS_reachtot), IV_down(JS_reachtot),              &
                IV_nbup(JS_reachtot), IM_up(JS_reachtot,:)
enddo
close(10)

open(14,file=basin_id_file,status='old')
read(14,*) IV_basin_id
close(14)

if (BS_opt_forcing) then
     open(17,file=forcingtot_id_file,status='old')
     read(17,*) IV_forcingtot_id
     close(17)

     open(19,file=forcinguse_id_file,status='old')
     read(19,*) IV_forcinguse_id
     close(19)
end if


!*******************************************************************************
!Creates vectors with indexes for basin considered
!*******************************************************************************
do JS_reachbas=1,IS_reachbas
     IV_basin_loc(JS_reachbas)=JS_reachbas-1
enddo
!vector with zero-base index corresponding to one-base index


do JS_reachbas=1,IS_reachbas
do JS_reachtot=1,IS_reachtot
     if (IV_basin_id(JS_reachbas)==IV_connect_id(JS_reachtot)) then
          IV_basin_index(JS_reachbas)=JS_reachtot
     end if 
end do
end do 
!vector with (Fortran, 1-based) indexes corresponding to reaches of basin 
!within whole network
!IV_basin_index has two advantages.  1) it is needed in order to read forcing 
!data (Vlat for ex).  2) It allows to avoid one other nested loop in the 
!following, which reduces tremendously the computation time.

!print *, IV_basin_loc 
!print *, IV_basin_index 


!*******************************************************************************
!Matrix preallocation
!*******************************************************************************
IS_ownfirst=0
IS_ownlast=0
do JS_reachbas=1,IS_reachbas
     IV_nz(JS_reachbas)=0
     IV_dnz(JS_reachbas)=0
     IV_onz(JS_reachbas)=0
end do
!Initialize to zero

call MatGetOwnerShipRange(ZM_Net,IS_ownfirst,IS_ownlast,ierr)

do JS_reachbas=1,IS_reachbas
do JS_reachbas2=1,IS_reachbas
do JS_up=1,IV_nbup(IV_basin_index(JS_reachbas2))

if (IV_connect_id(IV_basin_index(JS_reachbas))==                               &
    IM_up(IV_basin_index(JS_reachbas2),JS_up)) then

     !Here JS_reachbas is determined upstream of JS_reachbas2
     IV_nz(JS_reachbas2)=IV_nz(JS_reachbas2)+1 
     if (JS_reachbas>=IS_ownfirst+1 .and. JS_reachbas < IS_ownlast+1) then
          IV_dnz(JS_reachbas2)=IV_dnz(JS_reachbas2)+1 
     else
          IV_onz(JS_reachbas2)=IV_onz(JS_reachbas2)+1 
     end if
     !both IS_reachbas2 and IS_reachbas are used here because the location
     !of nonzeros depends on row and column in an parallel matrix

     IM_index_up(JS_reachbas2,JS_up)=JS_reachbas
     !used for traditional Muskingum method

end if 

end do
end do
end do

call MatSeqAIJSetPreallocation(ZM_Net,PETSC_NULL_INTEGER,IV_nz,ierr)
call MatMPIAIJSetPreallocation(ZM_Net,                                         &
                               PETSC_NULL_INTEGER,                             &
                               IV_dnz(IS_ownfirst+1:IS_ownlast),               &
                               PETSC_NULL_INTEGER,                             &
                               IV_onz(IS_ownfirst+1:IS_ownlast),ierr)
call MatSeqAIJSetPreallocation(ZM_A,PETSC_NULL_INTEGER,IV_nz+1,ierr)
call MatMPIAIJSetPreallocation(ZM_A,                                           &
                               PETSC_NULL_INTEGER,                             &
                               IV_dnz(IS_ownfirst+1:IS_ownlast)+1,             &
                               PETSC_NULL_INTEGER,                             &
                               IV_onz(IS_ownfirst+1:IS_ownlast),ierr)

call PetscPrintf(PETSC_COMM_WORLD,'Network matrix preallocated'//char(10),ierr)


!*******************************************************************************
!Creates network matrix
!*******************************************************************************
if (rank==0) then
!only first processor sets values
do JS_reachbas=1,IS_reachbas
     if (IV_nbup(IV_basin_index(JS_reachbas))/=0) then
          do JS_up=1,IV_nbup(IV_basin_index(JS_reachbas))
               do JS_reachbas2=1,IS_reachbas

     if (  IM_up(IV_basin_index(JS_reachbas),JS_up)==                          &
           IV_connect_id(IV_basin_index(JS_reachbas2))  ) then
          call MatSetValues(ZM_Net,IS_one,JS_reachbas-1,IS_one,JS_reachbas2-1, &
                            ZS_one,INSERT_VALUES,ierr)
          CHKERRQ(ierr)
          call MatSetValues(ZM_A  ,IS_one,JS_reachbas-1,IS_one,JS_reachbas2-1, &
                            ZS_one,INSERT_VALUES,ierr)
          CHKERRQ(ierr)
     end if

                end do
          enddo
     end if
call MatSetValues(ZM_A  ,IS_one,JS_reachbas-1,IS_one,JS_reachbas-1, &
                  0*ZS_one,INSERT_VALUES,ierr)
CHKERRQ(ierr)
enddo
end if

call MatAssemblyBegin(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyBegin(ZM_A  ,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_A  ,MAT_FINAL_ASSEMBLY,ierr)
!sparse matrices need be assembled once their elements have been filled
call PetscPrintf(PETSC_COMM_WORLD,'Network matrix created'//char(10),ierr)


!*******************************************************************************
!Checks for missing connections and gives warning
!*******************************************************************************
do JS_reachbas=1,IS_reachbas
     do JS_reachtot=1,IS_reachtot
          if (IV_down(JS_reachtot)==                                           &
              IV_connect_id(IV_basin_index(JS_reachbas))) then             
          !Within connectivity table, index JS_reachtot has been determined as
          !Flowing into reach located at index JS_reachbas.  The following is 
          !to check that the reach corresponding to JS_reachtot is within the 
          !basin too. If not, gives a warning.
          BS_logical=.false.
          do JS_reachbas2=1,IS_reachbas
          BS_logical=( BS_logical .or.                                         &
                       (IV_connect_id(JS_reachtot)==IV_basin_id(JS_reachbas2)) )
          end do 
          if (.not. BS_logical) then
          write(temp_char,'(i10)') IV_connect_id(JS_reachtot)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           'WARNING: reach ID' // temp_char,ierr)
          write(temp_char,'(i10)') IV_basin_id(JS_reachbas)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           ' should be connected upstream   of reach ID'       &
                           // temp_char // char(10),ierr)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           '         Make sure upstream forcing is available'  &
                           // char(10),ierr)
          end if 
          end if     

          if (IV_down(IV_basin_index(JS_reachbas))==                           &
              IV_connect_id(JS_reachtot)) then             
          !Within connectivity table, index JS_reachtot has been determined as
          !Flowing out of reach located at index JS_reachbas.  The following is 
          !to check that the reach corresponding to JS_reachtot is within the 
          !basin too. If not, gives a warning.
          BS_logical=.false.
          do JS_reachbas2=1,IS_reachbas
          BS_logical=( BS_logical .or.                                         &
                       (IV_connect_id(JS_reachtot)==IV_basin_id(JS_reachbas2)) )
          end do 
          if (.not. BS_logical) then
          write(temp_char,'(i10)') IV_connect_id(JS_reachtot)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           'WARNING: reach ID' // temp_char,ierr)
          write(temp_char,'(i10)') IV_basin_id(JS_reachbas)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           ' should be connected downstream of reach ID'       &
                           // temp_char // char(10),ierr)
          end if 
               
     end if
end do
end do 
call PetscPrintf(PETSC_COMM_WORLD,'Checked for missing connections between '// &
                 'basin studied and rest of domain'//char(10),ierr)


!*******************************************************************************
!If forcing is used
!*******************************************************************************
if (BS_opt_forcing) then
!-------------------------------------------------------------------------------
!Breaks matrix connectivity in case forcing used is inside basin studied
!-------------------------------------------------------------------------------
write(temp_char,'(i10)') IS_forcinguse
call PetscPrintf(PETSC_COMM_WORLD,'WARNING: Might break '//temp_char//         &
                 ' connections (max) in network matrix if forcing is within' //&
                 ' basin. If so, details on each are below'//char(10),ierr)

if (rank==0) then
!only first processor sets values
do JS_forcinguse=1,IS_forcinguse
     do JS_reachbas=1,IS_reachbas
          if (IV_forcinguse_id(JS_forcinguse)==IV_basin_id(JS_reachbas)) then

     do JS_reachbas2=1,IS_reachbas
          if (IV_down(IV_basin_index(JS_reachbas))==IV_basin_id(JS_reachbas2)) then
          !here JS_reachbas2 is determined as directly downstream of JS_reachbas
          !and the connection between both needs be broken

          call MatSetValues(ZM_Net,IS_one,JS_reachbas2-1,IS_one,JS_reachbas-1, &
                            0*ZS_one,INSERT_VALUES,ierr)
          CHKERRQ(ierr)
          
          write(temp_char,'(i10)') IV_basin_id(JS_reachbas)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           '         connection broken downstream of reach ID' &
                            // temp_char,ierr)
          write(temp_char,'(i10)') IV_basin_id(JS_reachbas2)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           ' forcing data will be used for reach ID'           &
                           // temp_char // char(10),ierr)
          call PetscPrintf(PETSC_COMM_WORLD,'         Network matrix modified' &
                           //char(10),ierr)
          end if
     end do 

          end if
     end do
end do
end if
call MatAssemblyBegin(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
!sparse matrices need be assembled once their elements have been filled
call PetscPrintf(PETSC_COMM_WORLD,'Broke potential connections in network '//  &
                 'matrix if forcing is within basin studied'//char(10),ierr)


!-------------------------------------------------------------------------------
!Calculates IS_forcingbas
!-------------------------------------------------------------------------------
write(temp_char,'(i10)') IS_forcinguse
call PetscPrintf(PETSC_COMM_WORLD,'Total number of forcing gages in forcinguse'&
                 //' file :'// temp_char // char(10),ierr)

IS_forcingbas=0
!initialize to zero

do JS_forcinguse=1,IS_forcinguse
     do JS_reachtot=1,IS_reachtot
          if (IV_forcinguse_id(JS_forcinguse)==IV_connect_id(JS_reachtot)) then

     do JS_reachbas=1,IS_reachbas
          if (IV_down(JS_reachtot)==IV_basin_id(JS_reachbas)) then 
               IS_forcingbas=IS_forcingbas+1
          end if
     end do

          end if 
     end do
end do

write(temp_char,'(i10)') IS_forcingbas
call PetscPrintf(PETSC_COMM_WORLD,'Total number of forcing gages flowing in '//&
                 'basin   :'// temp_char // char(10),ierr)


!-------------------------------------------------------------------------------
!Allocates and populates the vectors IV_forcing_index and IV_forcing_loc
!-------------------------------------------------------------------------------
allocate(IV_forcing_index(IS_forcingbas))
allocate(IV_forcing_loc(IS_forcingbas))
!allocate vector size

do JS_forcingbas=1,IS_forcingbas
     IV_forcing_index(JS_forcingbas)=0
     IV_forcing_loc(JS_forcingbas)=0
end do
!Initialize both vectors to zero

if (IS_forcingbas>0) then
JS_forcingbas=0
do JS_forcinguse=1,IS_forcinguse
     do JS_reachtot=1,IS_reachtot
          if (IV_forcinguse_id(JS_forcinguse)==IV_connect_id(JS_reachtot)) then
               !JS_reachtot leads to the same ID as JS_forcinguse
               !print *, JS_forcinguse,JS_reachtot

     do JS_reachbas=1,IS_reachbas
          if (IV_down(JS_reachtot)==IV_basin_id(JS_reachbas)) then
               !JS_reachbas is downstream of JS_reachtot which corresponds
               !to JS_forcinguse 
               !--> JS_forcinguse flows into a reach located in basin
               do JS_forcingtot=1,IS_forcingtot

     if (IV_forcingtot_id(JS_forcingtot)==IV_connect_id(JS_reachtot)) then
          JS_forcingbas=JS_forcingbas+1
          IV_forcing_index(JS_forcingbas)=JS_forcingtot!IV_basin_id(JS_reachbas)
          !print *, JS_forcingtot,JS_reachtot,IV_basin_id(JS_reachbas),        &
          !         IV_basin_loc(JS_reachbas)
     end if

               end do
          end if 
     end do

          end if
     end do
end do
end if
!Populate IV_forcing_index

if (IS_forcingbas>0) then
JS_forcingbas=0
do JS_forcinguse=1,IS_forcinguse
     do JS_reachtot=1,IS_reachtot
          if (IV_forcinguse_id(JS_forcinguse)==IV_connect_id(JS_reachtot)) then
               !JS_reachtot leads to the same ID as JS_forcinguse
               !print *, JS_forcinguse,JS_reachtot

     do JS_reachbas=1,IS_reachbas
          if (IV_down(JS_reachtot)==IV_basin_id(JS_reachbas)) then
               !JS_reachbas is downstream of JS_reachtot which corresponds
               !to JS_forcinguse
               !print *, JS_forcinguse,JS_reachtot,IV_basin_id(JS_reachbas),   &
               !         IV_basin_loc(JS_reachbas)
               JS_forcingbas=JS_forcingbas+1
               IV_forcing_loc(JS_forcingbas)=IV_basin_loc(JS_reachbas)
          end if 
     end do

          end if
     end do
end do
end if
!Populates IV_forcing_index

!print *, 'IV_forcinguse_id   ', IV_forcinguse_id 
!print *, 'IV_forcing_index   ', IV_forcing_index
!print *, 'IV_forcing_loc     ', IV_forcing_loc
!-------------------------------------------------------------------------------
!End if forcing is used
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!End
!*******************************************************************************
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)
!call PetscPrintf(PETSC_COMM_WORLD,'ZM_Net'//char(10),ierr)
!call MatView(ZM_Net,PETSC_VIEWER_STDOUT_WORLD,ierr)


end subroutine rapid_net_mat
