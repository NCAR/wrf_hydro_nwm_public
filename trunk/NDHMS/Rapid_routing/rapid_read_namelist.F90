!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

subroutine rapid_read_namelist

!PURPOSE
!This subroutine allows to read the RAPID namelist and hence to run the model
!multiple times without ever have to recompile.  Some information on the options
!used is also printed in the stdout
!Author: Cedric H. David, 2011 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                     NL_namelist,namelist_file


implicit none


!*******************************************************************************
!Read namelist file 
!*******************************************************************************
open(88,file=namelist_file,status='old',form='formatted')
read(88, NL_namelist)
close(88)


!*******************************************************************************
!Optional prints what was read 
!*******************************************************************************
!print *, namelist_file


end subroutine rapid_read_namelist
