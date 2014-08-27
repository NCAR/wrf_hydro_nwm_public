!*******************************************************************************
!RAPID main program
!*******************************************************************************
module rapid_main

!PURPOSE
!Allows to route water through a river network, and to estimate optimal 
!parameters using the inverse method 
!Author: Cedric H. David, 2008 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   namelist_file,                                              &
                   IS_reachbas,                                                &
                   IV_basin_id,IV_basin_index,IV_basin_loc,                    &
                   m3_nc_file,Qfor_file,                                       &
                   Qout_file,V_file,Qout_nc_file,                              &
                   IS_M,JS_M,JS_RpM,IS_RpM,                                    &
                   ZS_TauR,                                                    &
                   ZV_pnorm,                                                   &
                   ZV_C1,ZV_C2,ZV_C3,                                          &
                   ZV_Qext,ZV_Qfor,ZV_Qlat,                                    &
                   ZV_Vlat,                                                    &
                   ZV_QoutR,ZV_QoutinitR,ZV_QoutbarR,                          &
                   ZV_VR,ZV_VinitR,ZV_VbarR,                                   &
                   ZS_phi,                                                     &
                   ierr,vecscat,rank,stage,temp_char,temp_char2,               &
                   ZV_pointer,ZS_one,                                          &
                   ZV_read_reachtot,ZV_read_forcingtot,                        &
                   ZV_SeqZero,                                                 &
                   IS_reachtot,IS_forcingbas,                                  &
                   IV_forcing_index,IV_forcing_loc,                            &
                   ZS_time1,ZS_time2,ZS_time3,                                 &
                   IS_nc_status,IS_nc_id_fil_m3,IS_nc_id_fil_Qout,             &
                   IS_nc_id_var_m3,IS_nc_id_var_Qout,IS_nc_id_var_comid,       &
                   IS_nc_id_dim_time,IS_nc_id_dim_comid,IV_nc_id_dim,          &
                   IV_nc_start,IV_nc_count,IV_nc_count2,                       &
                   BS_opt_forcing,IS_opt_run,								   &
                   IS_reachtot,JS_reachtot,IS_reachbas,                        &
            	   ierr,rank,                                                  &
            	   IV_basin_id,IV_basin_index,IV_basin_loc,                    &
			       ZV_read_reachtot

#ifndef NO_TAO
use rapid_var, only :                                                          &
                   tao,taoapp
#endif

implicit none


external rapid_phiroutine
!because the subroutine is called by a function

!public :: RAPID_exe, rapid_runoff_to_inflow, rapid_ini, rapid_main_exe


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

#ifndef NO_TAO
#include "finclude/tao_solver.h" 
!TAO solver
#endif

!Vec :: ZV_Vlat  ! LPR a public variable: pass from COUPLER to MAIN
integer cnt_rapid_run
integer cnt_rapid_final
character(len=100) :: str
character(len=100) :: Qout_nc_dir
logical initialized 

CONTAINS

!--------------Dummy subroutine to pass runoff information to RAPID-----------------
!  subroutine RAPID_exe(runoff,ii,jj)
!        implicit none
 
!        integer ii,jj
!  	    real,dimension(ii,jj) :: runoff
!        Vec :: ZV_Vlat
!        PetscScalar,dimension(ii,jj) :: ZM_runoff

!        ZM_runoff = runoff
!        call rapid_runoff_to_inflow(ZM_runoff,ZV_Vlat)
!  end subroutine RAPID_exe
 
!*******************************************************************************
!Initialize
!*******************************************************************************
  subroutine rapid_ini(NTIME)
     implicit none
     integer NTIME
     namelist_file='./rapid_namelist' 

print *,'!!!LPR!!!!!!!!!!!!!!!!! initialized = ',initialized
      if(initialized)  return

      call rapid_init(initialized)
      Qout_nc_dir   = Qout_nc_file  !first time finalize the Qout_nc_dir from RAPID namelist LPR
      initialized = .True.

      print *, '******** check before PetscLogStageRegister *****'
      call PetscLogStageRegister('Read Comp Write',stage,ierr)
      print *,'***LPR PASS CALL PetscLogStageRegister******'
      print *,'stage = ',stage
      print *,'ierr = ',ierr
      call PetscLogStagePush(stage,ierr)
      print *,'***LPR PASS CALL PetscLogStagePush****'

  end subroutine rapid_ini

!*********************LPR new subroutine*************************************
!****************************************************************************
subroutine rapid_main_exe(ITIME,runoff,ii,jj)
     use netcdf
implicit none
integer ITIME
integer ii,jj
real,dimension(ii,jj) :: runoff
!Vec :: ZV_Vlat  !no need to declare again, already in rapid_var, note uncomment this might cause PETSC SEGV error
!PetscScalar,dimension(ii,jj) :: ZM_runoff
real,dimension(ii,jj) :: ZM_runoff

ZM_runoff = runoff  ! LPR need to check if this works

!*******************************************************************************
!OPTION 1 - use to calculate flows and volumes and generate output data 
!*******************************************************************************
if (IS_opt_run==1) then

!print '(a20)', 'Rank     :',Qout_nc_file
!Qout_nc_file = Qout_nc_file+cnt_rapid_run

!if(cnt_rapid_run .eq. 0) then !first time running RAPID
  cnt_rapid_run = cnt_rapid_run + 1

!LPR edit RAPID output filenames
  if (cnt_rapid_run < 10) then
      write(str,100) cnt_rapid_run
100   format('000',i1)
  else if (cnt_rapid_run < 100) then
      write(str,200) cnt_rapid_run
200   format('00',i2)
  else if (cnt_rapid_run < 1000) then
      write(str,300) cnt_rapid_run
300   format('0',i3)
  else
      write(str,'(i4)') cnt_rapid_run
  end if
!print *,'***LPR*** str = ',str
  Qout_nc_file = trim(Qout_nc_dir)//'RAPID.with.WRF_hydro.'//trim(str)//'.nc'
!else
  
!endif

print *,'***LPR*** Qout_nc_file = ',trim(Qout_nc_file)
!-------------------------------------------------------------------------------
!Create Qout file
!-------------------------------------------------------------------------------
if (rank==0) then 
IS_nc_status=NF90_CREATE(Qout_nc_file,NF90_CLOBBER,IS_nc_id_fil_Qout)
IS_nc_status=NF90_DEF_DIM(IS_nc_id_fil_Qout,'Time',NF90_UNLIMITED,             &
                          IS_nc_id_dim_time)
IS_nc_status=NF90_DEF_DIM(IS_nc_id_fil_Qout,'COMID',IS_reachbas,               &
                          IS_nc_id_dim_comid)
IS_nc_status=NF90_DEF_VAR(IS_nc_id_fil_Qout,'COMID',NF90_INT,                  &
                          IS_nc_id_dim_comid,IS_nc_id_var_comid)
IV_nc_id_dim(1)=IS_nc_id_dim_comid
IV_nc_id_dim(2)=IS_nc_id_dim_time
IS_nc_status=NF90_DEF_VAR(IS_nc_id_fil_Qout,'Qout',NF90_REAL,                  &
                          IV_nc_id_dim,IS_nc_id_var_Qout)
IS_nc_status=NF90_ENDDEF(IS_nc_id_fil_Qout)
IS_nc_status=NF90_PUT_VAR(IS_nc_id_fil_Qout,IS_nc_id_var_comid,IV_basin_id)
if (rank==0) IS_nc_status=NF90_CLOSE(IS_nc_id_fil_Qout)
end if

!-------------------------------------------------------------------------------
!Open files          
!-------------------------------------------------------------------------------
!LPR 2014-07-22 No need to read in the m3_nc_file, instead ZV_Vlat will be passed
!if (rank==0) then 
!open(99,file=m3_nc_file,status='old')
!close(99)
!IS_nc_status=NF90_OPEN(m3_nc_file,NF90_NOWRITE,IS_nc_id_fil_m3)
!IS_nc_status=NF90_INQ_VARID(IS_nc_id_fil_m3,'m3_riv',IS_nc_id_var_m3)
!end if
if (rank==0) then 
open(99,file=Qout_nc_file,status='old')
close(99)
IS_nc_status=NF90_OPEN(Qout_nc_file,NF90_WRITE,IS_nc_id_fil_Qout)
IS_nc_status=NF90_INQ_VARID(IS_nc_id_fil_Qout,'Qout',IS_nc_id_var_Qout)
end if
if (BS_opt_forcing) open(34,file=Qfor_file,status='old')

!-------------------------------------------------------------------------------
!Read, compute and write          
!-------------------------------------------------------------------------------
!stage = cnt_rapid_run

! print *, '******** check before PetscLogStageRegister *****'
! call PetscLogStageRegister('Read Comp Write',stage,ierr)
! print *,'***LPR PASS CALL PetscLogStageRegister******'
! print *,'stage = ',stage
! print *,'ierr = ',ierr
! call PetscLogStagePush(stage,ierr)
! print *,'***LPR PASS CALL PetscLogStagePush****'
ZS_time3=0

IV_nc_start=(/1,1/)
IV_nc_count=(/IS_reachtot,1/)
IV_nc_count2=(/IS_reachbas,1/)

!do JS_M=1,IS_M  !LPR discard the loop for days

!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - +  
!Read/set upstream forcing
!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - +  
if (BS_opt_forcing .and. IS_forcingbas > 0) then 
read(34,*) ZV_read_forcingtot
call VecSetValues(ZV_Qfor,IS_forcingbas,IV_forcing_loc,                        &
                  ZV_read_forcingtot(IV_forcing_index),INSERT_VALUES,ierr)
                  !here we only look at the forcing within the basin studied 
call VecAssemblyBegin(ZV_Qfor,ierr)
call VecAssemblyEnd(ZV_Qfor,ierr)           !set Qfor based on reading a file

end if

!*******RAPID COUPLING*******************************
!print *,'********************************************************************'
!print *,'********RAPID model is running**************************************'
!print *,'********************************************************************'
!read(31,*) ZV_read_reachtot
!if (rank==0) then
!IS_nc_status=NF90_GET_VAR(IS_nc_id_fil_m3,IS_nc_id_var_m3,                     &
!                          ZV_read_reachtot,IV_nc_start,IV_nc_count)
!end if
call rapid_runoff_to_inflow(ZM_runoff,ZV_Vlat)
! LPR get ZV_Vlat value each time
!*******RAPID COUPLING END****************************

!do JS_RpM=1,IS_RpM  !LPR discard loop for every day 8 times, 3-hrly time step 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!Read/set surface and subsurface volumes 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!read(31,*) ZV_read_reachtot
! LPR 2014-07-21 note that this step has already been written in the new coupling file
!if (rank==0) then
!IS_nc_status=NF90_GET_VAR(IS_nc_id_fil_m3,IS_nc_id_var_m3,                     &
!                          ZV_read_reachtot,IV_nc_start,IV_nc_count)
!call VecSetValues(ZV_Vlat,IS_reachbas,IV_basin_loc,                            &
!                  ZV_read_reachtot(IV_basin_index),INSERT_VALUES,ierr)
!end if
!call VecAssemblyBegin(ZV_Vlat,ierr)
!call VecAssemblyEnd(ZV_Vlat,ierr)           !set Vlat based on reading a file

!****LPR above is no need bcuz ZV_Vlat is already calculated through COUPLER

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!calculation of Q based on V (here first order explicit approx) 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! LPR 2014-07-22 retain this bcuz it calculates the volume
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
call PetscGetTime(ZS_time1,ierr)

!LPR the routing is further discretized into 15min for 3-hrly Noah-MP input
call rapid_routing(ZV_C1,ZV_C2,ZV_C3,ZV_Qext,                                  &
                   ZV_QoutinitR,ZV_VinitR,                                     &
                   ZV_QoutR,ZV_QoutbarR,ZV_VR,ZV_VbarR)

call PetscGetTime(ZS_time2,ierr)
ZS_time3=ZS_time3+ZS_time2-ZS_time1

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!Update variables
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
call VecCopy(ZV_QoutR,ZV_QoutinitR,ierr)
call VecCopy(ZV_VR,ZV_VinitR,ierr)
     
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!write outputs         
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
call VecScatterBegin(vecscat,ZV_QoutbarR,ZV_SeqZero,                           &
                     INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterEnd(vecscat,ZV_QoutbarR,ZV_SeqZero,                             &
                        INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecGetArrayF90(ZV_SeqZero,ZV_pointer,ierr)
if (rank==0) IS_nc_status=NF90_PUT_VAR(IS_nc_id_fil_Qout,IS_nc_id_var_Qout,    &
                                       ZV_pointer,IV_nc_start,IV_nc_count2)
call VecRestoreArrayF90(ZV_SeqZero,ZV_pointer,ierr)

if (rank==0) IV_nc_start(2)=IV_nc_start(2)+1

!end do
!end do

!-------------------------------------------------------------------------------
!Performance statistics
!-------------------------------------------------------------------------------
!yw call PetscPrintf(PETSC_COMM_WORLD,'Cumulative time for routing only'           &
!yw                                   //char(10),ierr)
!print '(a10,i7,a2,a10,f7.2)', 'Rank     :',rank,', ','Time     :',ZS_time3
!yw write(temp_char ,'(i10)')   rank
!yw write(temp_char2,'(f10.2)') ZS_time3
!yw call PetscSynchronizedPrintf(PETSC_COMM_WORLD,'Rank     :'//temp_char //', '// &
!yw                                               'Time     :'//temp_char2//       &
!yw                                                char(10),ierr)
!yw call PetscSynchronizedFlush(PETSC_COMM_WORLD,ierr)

!yw call PetscLogStagePop(ierr)
!yw call PetscPrintf(PETSC_COMM_WORLD,'Output data created'//char(10),ierr)


!-------------------------------------------------------------------------------
!Close files          
!-------------------------------------------------------------------------------
!if (rank==0) IS_nc_status=NF90_CLOSE(IS_nc_id_fil_m3)
if (rank==0) IS_nc_status=NF90_CLOSE(IS_nc_id_fil_Qout)
if (BS_opt_forcing) close(34)

!-------------------------------------------------------------------------------
!End of OPTION 1
!-------------------------------------------------------------------------------
end if

!*******************************************************************************
!OPTION 2 - Optimization 
!*******************************************************************************
if (IS_opt_run==2) then
#ifndef NO_TAO

!-------------------------------------------------------------------------------
!Only one computation of phi - For testing purposes only
!-------------------------------------------------------------------------------
!call PetscLogStageRegister('One comp of phi',stage,ierr)
!call PetscLogStagePush(stage,ierr)
!!do JS_M=1,5
!call rapid_phiroutine(taoapp,ZV_pnorm,ZS_phi,ierr)
!!enddo
!call PetscLogStagePop(ierr)

!-------------------------------------------------------------------------------
!Optimization procedure
!-------------------------------------------------------------------------------
call PetscLogStageRegister('Optimization   ',stage,ierr)
call PetscLogStagePush(stage,ierr)
call TaoAppSetObjectiveAndGradientRo(taoapp,rapid_phiroutine,TAO_NULL_OBJECT,  &
                                     ierr)
call TaoAppSetInitialSolutionVec(taoapp,ZV_pnorm,ierr)
call TaoSetTolerances(tao,1.0d-4,1.0d-4,TAO_NULL_OBJECT,TAO_NULL_OBJECT,ierr)
call TaoSetOptions(taoapp,tao,ierr)

call TaoSolveApplication(taoapp,tao,ierr)

call TaoView(tao,ierr)
call PetscPrintf(PETSC_COMM_WORLD,'final normalized p=(k,x)'//char(10),ierr)
call VecView(ZV_pnorm,PETSC_VIEWER_STDOUT_WORLD,ierr)
call PetscLogStagePop(ierr)

!-------------------------------------------------------------------------------
!End of OPTION 2
!-------------------------------------------------------------------------------
#else
if (rank==0)                                         print '(a70)',            &
        'ERROR: The optimization mode requires RAPID to be compiled with TAO   '
#endif
end if


!*******************************************************************************
!Finalize
!*******************************************************************************

!call rapid_final



end subroutine rapid_main_exe

 !--------------RAPID coupler: gridded runoff to vector runoff-----------------------  
  subroutine rapid_runoff_to_inflow(ZM_runoff,ZV_Vlat) 
        implicit none

!        PetscScalar, dimension(:,:), intent(in) :: ZM_runoff
        real, dimension(:,:), intent(in) :: ZM_runoff
        Vec, intent(out) :: ZV_Vlat

        integer, dimension(IS_reachtot) :: IV_i_index
        integer, dimension(IS_reachtot) :: IV_j_index
	real, dimension(IS_reachtot) :: ZV_areakm
        integer :: JS_lon,JS_lat
        character(len=100) :: coupling_file='./rapid_input_tx/RAPID_coupling_new_4km_WRF_hydro.csv'
        !Unit 10

        !----------Read coupling files---------------
        open(10,file=coupling_file,status='old')
        do JS_reachtot=1,IS_reachtot
                 read(10,*) IV_basin_id(JS_reachtot),ZV_areakm(JS_reachtot),               &
                            IV_i_index(JS_reachtot),IV_j_index(JS_reachtot)
        end do
        close(10)
print *,'****LPR RAPID read coupling file successfully************'
        !print *,IV_basin_id(IS_reachtot),ZV_areakm(IS_reachtot),                      &
        !        IV_i_index(IS_reachtot),IV_j_index(IS_reachtot) 

	!----------tease out weird runoff values-----------
        if (rank==0) then
        if (maxval(ZM_runoff)>1000) stop 'Runoff exceeds 1000'
        if (minval(ZM_runoff)<0) stop 'Negative runoff'
        !print *, 'Maximum value for ZM_runoff is:', maxval(ZM_runoff)
        end if

!print *,'***************************************************************'
!print *,'********************RAPID actual coupling.....*****************'
!print *,'***************************************************************'
	!----------COUPLING START----------------------------
        if (rank==0) then
        do JS_reachtot=1,IS_reachtot
            ZV_read_reachtot(JS_reachtot) = 0.
        end do

        do JS_reachtot=1,IS_reachtot
            JS_lon=IV_i_index(JS_reachtot)
            JS_lat=IV_j_index(JS_reachtot)
            ZV_read_reachtot(JS_reachtot)=ZM_runoff(JS_lon,JS_lat)               &
                  *ZV_areakm(JS_reachtot)*1000
          !with runoff in kg/m2=mm and area in km2
!print *,'Reach: ',JS_reachtot
!print *,'Lon = ',JS_lon,' Lat = ',JS_lat
!print *,'***LPR CHECK*** value = ',ZV_read_reachtot(JS_reachtot)
        end do
!print *,'***LPR CHECK*** ZV_read_reachtot(1000) = ',ZV_read_reachtot(1000)
     	!print *, maxval(ZV_Vol)
        end if

print *, '************************************************************'
print *, '***** LPR: RAPID coupling successful! **********************'
print *, '************************************************************'
	
	!------write to PETSC vector---------------------------
	if (rank==0) then
print *,'LPR: IS_reachbas = ',IS_reachbas
print *,'LPR: IV_basin_loc = ',size(IV_basin_loc)
print *,'LPR: IV_basin_index = ',size(IV_basin_index) !,' ZV_read_reachtot(IV_basin_index) = ',ZV_read_reachtot(IV_basin_index)
        call VecSetValues(ZV_Vlat,IS_reachbas,IV_basin_loc,                       &
             ZV_read_reachtot(IV_basin_index),INSERT_VALUES,ierr)
print *, 'PASS Call VecSetValue'
	end if

	call VecAssemblyBegin(ZV_Vlat,ierr)
print *, 'PASS Call VecAssemblyBegin'
	call VecAssemblyEnd(ZV_Vlat,ierr)
print *, 'PASS Call VecAssemblyEnd'

  end subroutine rapid_runoff_to_inflow
  

!*******************************************************************************
!End
!*******************************************************************************
end module rapid_main
