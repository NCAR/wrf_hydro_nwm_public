!*******************************************************************************
!Subroutine rapid_init 
!*******************************************************************************
subroutine rapid_init(initialized)

!PURPOSE
!This subroutine allows to initialize RAPID for both regular runs and 
!optimization runs, by performing slightly different tasks depending on what 
!option is chosen.  
!Initialization tasks common to all RAPID options:
!     -Read namelist file (sizes of domain, duration, file names, options, etc.)  
!     -Compute number of time steps based on durations
!     -Allocate Fortran arrays
!     -Create all PETSc and TAO objects 
!     -Print information and warnings
!     -Determine IDs for various computing cores
!     -Compute the network matrix
!     -Initialize values of flow and volume for main procedure
!Initialization tasks specific to Option 1
!     -Copy main initial flow and vol to routing initial flow and vol
!     -Read k and x 
!     -Compute linear system matrix
!Initialization tasks specific to Option 2
!     -Copy main initial flow to optimization initial flow
!     -Compute the observation matrix
!     -Read kfac and Qobsbarrec
!     -Set initial values for the vector pnorm
!Author: Cedric H. David, 2012 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
!use netcdf
use rapid_var, only :                                                          &
                   IS_reachtot,IS_reachbas,                                    &
                   IV_basin_id,IV_basin_index,IV_basin_loc,IV_connect_id,      &
                   IV_down,IV_nbup,IM_up,IM_index_up,IS_max_up,                &
                   IV_nz,IV_dnz,IV_onz,                                        &
                   BS_opt_Qinit,BS_opt_Qfinal,BS_opt_forcing,BS_opt_influence, &
                   IS_opt_run,IS_opt_routing,IS_opt_phi,                       &
                   ZV_read_reachtot,ZV_read_forcingtot,ZV_read_gagetot,        &
                   ZS_TauM,ZS_TauO,ZS_TauR,ZS_dtO,ZS_dtR,ZS_dtM,               &
                   IS_gagetot,IS_gageuse,IS_gagebas,                           &
                   IV_gagetot_id,IV_gageuse_id,                                &
                   IV_gage_index,IV_gage_loc,                                  &
                   IS_forcingtot,IS_forcinguse,IS_forcingbas,                  &
                   IV_forcingtot_id,IV_forcinguse_id,                          &
                   IV_forcing_index,IV_forcing_loc,                            &
                   ZV_QoutinitM,ZV_QoutinitO,ZV_QoutinitR,                     &
                   ZV_VinitM,ZV_VinitR,                                        &
                   ZV_babsmax,ZV_QoutRabsmin,ZV_QoutRabsmax,                   &
                   IS_M,IS_O,IS_R,IS_RpO,IS_RpM,                               &
                   kfac_file,xfac_file,x_file,k_file,m3_nc_file,Qinit_file,    &
                   Qobsbarrec_file,                                            &
                   ZS_Qout0,ZS_V0,                                             &
                   ZV_Qobsbarrec,                                              &
                   ZV_k,ZV_x,ZV_kfac,ZV_p,ZV_pnorm,ZV_pfac,                    &
                   ZS_knorm_init,ZS_xnorm_init,ZS_kfac,ZS_xfac,                &
                   ZV_C1,ZV_C2,ZV_C3,ZM_A,                                     &
                   ierr,ksp,pc,rank,IS_one,ZS_one


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
#include "finclude/petsclog.h" 
!PETSc log

logical :: initialized

!*******************************************************************************
!Initialization procedure common to all options
!*******************************************************************************

!-------------------------------------------------------------------------------
!Read name list, allocate Fortran arrays
!-------------------------------------------------------------------------------
if(initialized) return
initialized = .True.
print *,'First time allocation of variables..........'

call rapid_read_namelist
print *,'******PASS CALL rapid_read_namelist********************'

allocate(IV_basin_id(IS_reachbas))
allocate(IV_basin_index(IS_reachbas))
allocate(IV_basin_loc(IS_reachbas))

allocate(IV_connect_id(IS_reachtot))
allocate(IV_down(IS_reachtot))
allocate(IV_nbup(IS_reachtot))
allocate(IM_up(IS_reachtot,IS_max_up))
allocate(IM_index_up(IS_reachtot,IS_max_up))

allocate(IV_nz(IS_reachbas))
allocate(IV_dnz(IS_reachbas))
allocate(IV_onz(IS_reachbas))

allocate(ZV_read_reachtot(IS_reachtot))

if (IS_opt_run==2) then
     allocate(IV_gagetot_id(IS_gagetot))
     allocate(IV_gageuse_id(IS_gageuse))
     allocate(ZV_read_gagetot(IS_gagetot))
end if

if (BS_opt_forcing) then
     allocate(IV_forcingtot_id(IS_forcingtot))
     allocate(IV_forcinguse_id(IS_forcinguse))
     allocate(ZV_read_forcingtot(IS_forcingtot))
end if

!-------------------------------------------------------------------------------
!Compute number of time steps
!-------------------------------------------------------------------------------
IS_M=int(ZS_TauM/ZS_dtM)
IS_O=int(ZS_TauO/ZS_dtO)
IS_R=int(ZS_TauR/ZS_dtR)
IS_RpO=int(ZS_dtO/ZS_TauR)
IS_RpM=int(ZS_dtM/ZS_TauR)

!-------------------------------------------------------------------------------
!Initialize libraries and create objects common to all options
!-------------------------------------------------------------------------------
call rapid_create_obj
!Initialize libraries and create PETSc and TAO objects (Mat,Vec,taoapp...)
print *,'****PASS CALL rapid_create_obj********'


call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
!Determine number associated with each processor
print *,'****PASS CALL MPI_Comm_rank********'

!-------------------------------------------------------------------------------
!Prints information about current model run based on info from namelist
!-------------------------------------------------------------------------------
if (rank==0 .and. .not. BS_opt_Qinit)                      print '(a70)',      &
       'Not reading initial flows from a file                                  '
if (rank==0 .and. BS_opt_Qinit)                            print '(a70)',      &
       'Reading initial flows from a file                                      '
if (rank==0 .and. .not. BS_opt_Qfinal .and. IS_opt_run==1) print '(a70)',      &
       'Not writing final flows into a file                                    '
if (rank==0 .and. BS_opt_Qfinal .and. IS_opt_run==1)       print '(a70)',      &
       'Writing final flows into a file                                        '
if (rank==0 .and. .not. BS_opt_forcing)                    print '(a70)',      &
       'Not using forcing                                                      '
if (rank==0 .and. BS_opt_forcing)                          print '(a70)',      &
       'Using forcing                                                          '
if (rank==0 .and. IS_opt_routing==1)                       print '(a70)',      &
       'Routing with matrix-based Muskingum method                             '
if (rank==0 .and. IS_opt_routing==2)                       print '(a70)',      &
       'Routing with traditional Muskingum method                              '
if (rank==0 .and. IS_opt_run==1)                           print '(a70)',      &
       'RAPID mode: computing flowrates                                        '
if (rank==0 .and. IS_opt_run==2 .and. IS_opt_phi==1)       print '(a70)',      &
       'RAPID mode: optimizing parameters, using phi1                          ' 
if (rank==0 .and. IS_opt_run==2 .and. IS_opt_phi==2)       print '(a70)',      &
       'RAPID mode: optimizing parameters, using phi2                          ' 
if (rank==0)                                               print '(a10,a60)',  &
       'Using    :', m3_nc_file 
if (rank==0 .and. IS_opt_run==1)                           print '(a10,a60)',  &
       'Using    :',k_file 
if (rank==0 .and. IS_opt_run==1)                           print '(a10,a60)',  &
       'Using    :',x_file 
if (rank==0 .and. IS_opt_run==2)                           print '(a10,a60)',  &
       'Using    :',kfac_file 
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)

!-------------------------------------------------------------------------------
!Calculate Network matrix
!-------------------------------------------------------------------------------
!LPR add 2014-07-27
!if(initialized .ne. .True.) then
!print *,'***LPR*** First time initialization: need to initialize river matrix'
  call rapid_net_mat
!Create network matrix
!end if

!-------------------------------------------------------------------------------
!Print warning when forcing is used (needs be after rapid_net_mat)
!-------------------------------------------------------------------------------
if (BS_opt_forcing) then
if (rank==0) print *, 'IS_forcingbas      =', IS_forcingbas
if (rank==0 .and. IS_forcingbas>0) then
     call PetscPrintf(PETSC_COMM_WORLD,'WARNING: Forcing option used: '        &
                 //'measured flow replaced computed flows '                    &
                 //'for stations located on reach ID:'//char(10),ierr)
     !print *, 'IV_forcingtot_id   =', IV_forcingtot_id
     print *, 'IV_forcinguse_id   =', IV_forcinguse_id
     print *, 'IS_forcingbas      =', IS_forcingbas
     print *, 'IV_forcing_index   =', IV_forcing_index
     print *, 'IV_forcing_loc     =', IV_forcing_loc
end if
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)
end if
!Warning about forcing downstream basins

!-------------------------------------------------------------------------------
!calculates or set initial flows and volumes
!-------------------------------------------------------------------------------
if (.not. BS_opt_Qinit) then
call VecSet(ZV_QoutinitM,ZS_Qout0,ierr)
end if

if (BS_opt_Qinit) then
open(30,file=Qinit_file,status='old')
read(30,*) ZV_read_reachtot
close(30)
call VecSetValues(ZV_QoutinitM,IS_reachbas,IV_basin_loc,                       &
                  ZV_read_reachtot(IV_basin_index),INSERT_VALUES,ierr)
                  !here we use the output of a simulation as the intitial 
                  !flow rates.  The simulation has to be made on the entire
                  !domain, the initial value is taken only for the considered
                  !basin thanks to the vector IV_basin_index
call VecAssemblyBegin(ZV_QoutinitM,ierr)
call VecAssemblyEnd(ZV_QoutinitM,ierr)  
end if

call VecSet(ZV_VinitM,ZS_V0,ierr)
!Set initial volumes for Main procedure

!-------------------------------------------------------------------------------
!Initialize default values for ZV_QoutRabsmin, ZV_QoutRabsmax and ZV_babsmax
!-------------------------------------------------------------------------------
if (BS_opt_influence) then
call VecSet(ZV_babsmax    ,ZS_one*0        ,ierr)
call VecSet(ZV_QoutRabsmin,ZS_one*999999999,ierr)
call VecSet(ZV_QoutRabsmax,ZS_one*0        ,ierr)
end if

!*******************************************************************************
!Initialization procedure for OPTION 1
!*******************************************************************************
if (IS_opt_run==1) then

!-------------------------------------------------------------------------------
!copy main initial values into routing initial values 
!-------------------------------------------------------------------------------
call VecCopy(ZV_QoutinitM,ZV_QoutinitR,ierr)
call VecCopy(ZV_VinitM,ZV_VinitR,ierr)

!-------------------------------------------------------------------------------
!Read/set k and x
!-------------------------------------------------------------------------------
open(20,file=k_file,status='old')
read(20,*) ZV_read_reachtot
call VecSetValues(ZV_k,IS_reachbas,IV_basin_loc,                               &
                  ZV_read_reachtot(IV_basin_index),INSERT_VALUES,ierr)
call VecAssemblyBegin(ZV_k,ierr)
call VecAssemblyEnd(ZV_k,ierr)
close(20)
!get values for k in a file and create the corresponding ZV_k vector

open(21,file=x_file,status='old')
read(21,*) ZV_read_reachtot
call VecSetValues(ZV_x,IS_reachbas,IV_basin_loc,                               &
                  ZV_read_reachtot(IV_basin_index),INSERT_VALUES,ierr)
call VecAssemblyBegin(ZV_x,ierr)
call VecAssemblyEnd(ZV_x,ierr)
close(21)
!get values for x in a file and create the corresponding ZV_x vector

!-------------------------------------------------------------------------------
!Compute routing parameters and linear system matrix
!-------------------------------------------------------------------------------
call rapid_routing_param(ZV_k,ZV_x,ZV_C1,ZV_C2,ZV_C3,ZM_A)
!calculate Muskingum parameters and matrix ZM_A

call KSPSetOperators(ksp,ZM_A,ZM_A,DIFFERENT_NONZERO_PATTERN,ierr)
call KSPSetType(ksp,KSPRICHARDSON,ierr)                    !default=richardson
!call KSPSetInitialGuessNonZero(ksp,PETSC_TRUE,ierr)
!call KSPSetInitialGuessKnoll(ksp,PETSC_TRUE,ierr)
call KSPSetFromOptions(ksp,ierr)                           !if runtime options

!-------------------------------------------------------------------------------
!End of initialization procedure for OPTION 1
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!Initialization procedure for OPTION 2
!*******************************************************************************
if (IS_opt_run==2) then
#ifndef NO_TAO

!-------------------------------------------------------------------------------
!Create observation matrix
!-------------------------------------------------------------------------------
call rapid_obs_mat
!Create observation matrix

!-------------------------------------------------------------------------------
!copy main initial values into optimization initial values 
!-------------------------------------------------------------------------------
call VecCopy(ZV_QoutinitM,ZV_QoutinitO,ierr)
!copy initial main variables into initial optimization variables

!-------------------------------------------------------------------------------
!Read/set kfac, xfac and Qobsbarrec
!-------------------------------------------------------------------------------
open(22,file=kfac_file,status='old')
read(22,*) ZV_read_reachtot
close(22)
call VecSetValues(ZV_kfac,IS_reachbas,IV_basin_loc,                            &
                  ZV_read_reachtot(IV_basin_index),INSERT_VALUES,ierr)
                  !only looking at basin, doesn't have to be whole domain here 
call VecAssemblyBegin(ZV_kfac,ierr)
call VecAssemblyEnd(ZV_kfac,ierr)  
!reads kfac and assigns to ZV_kfac

if (IS_opt_phi==2) then
open(35,file=Qobsbarrec_file,status='old')
read(35,*) ZV_read_gagetot
close(35)
call VecSetValues(ZV_Qobsbarrec,IS_gagebas,IV_gage_loc,                        &
                  ZV_read_gagetot(IV_gage_index),INSERT_VALUES,ierr)
                  !here we only look at the observations within the basin
                  !studied
call VecAssemblyBegin(ZV_Qobsbarrec,ierr)
call VecAssemblyEnd(ZV_Qobsbarrec,ierr)  
!reads Qobsbarrec and assigns to ZV_Qobsbarrec
end if

!-------------------------------------------------------------------------------
!Set pnorm, pfac and p
!-------------------------------------------------------------------------------
call VecSetValues(ZV_pnorm,IS_one,IS_one-1,ZS_knorm_init,INSERT_VALUES,ierr)
call VecSetValues(ZV_pnorm,IS_one,IS_one,ZS_xnorm_init,INSERT_VALUES,ierr)
call VecAssemblyBegin(ZV_pnorm,ierr)
call VecAssemblyEnd(ZV_pnorm,ierr)
!set pnorm to pnorm=(knorm,xnorm)

!call VecSetValues(ZV_pfac,IS_one,IS_one-1,ZS_kfac,INSERT_VALUES,ierr)
!call VecSetValues(ZV_pfac,IS_one,IS_one,ZS_xfac,INSERT_VALUES,ierr)
!call VecAssemblyBegin(ZV_pnorm,ierr)
!call VecAssemblyEnd(ZV_pnorm,ierr)
!!set pfac to pfac=(kfac,xfac)

!call VecPointWiseMult(ZV_p,ZV_pfac,ZV_pnorm,ierr)
!!set p to p=pfac.*pnorm

!-------------------------------------------------------------------------------
!End of OPTION 2
!-------------------------------------------------------------------------------
#endif
end if

!*******************************************************************************
!End of subroutine
!*******************************************************************************
end subroutine rapid_init
