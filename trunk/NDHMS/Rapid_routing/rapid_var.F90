!*******************************************************************************
!Module to declare variables
!*******************************************************************************
module rapid_var

!PURPOSE
!Module where all the variables are defined 
!Author: Cedric H. David, 2008 


implicit none


!*******************************************************************************
!Includes
!*******************************************************************************
#include "finclude/petscsys.h"       
!base PETSc routines
#include "finclude/petscvec.h"  
#include "finclude/petscvec.h90"
!vectors, and Fortran90-specific vectors 
#include "finclude/petscmat.h"    
!matrices
#include "finclude/petscksp.h"    
!Krylov subspace methods
#include "finclude/petscpc.h"     
!preconditioners
#include "finclude/petscviewer.h"
!viewers (allows writing results in file for example)
!#include "finclude/petsclog.h" 
!Profiling log

#ifndef NO_TAO
#include "finclude/tao_solver.h" 
!TAO solver
#endif


!*******************************************************************************
!Declaration of variables - Basin variables and number of observed reaches
!*******************************************************************************
PetscInt :: IS_reachtot
!total number of river reaches, corresponds to the size of the table Ficvid
PetscInt :: IS_reachbas
!size of the matrix and the vectors in this basin, corresponds to the number of
!reaches in the basin
PetscInt :: JS_reachtot,JS_reachbas
PetscInt :: JS_reachtot2,JS_reachbas2
!JS_reachtot is index for all network and JS_reachbas for all river reaches in 
!considered basin only

PetscInt, dimension(:),allocatable :: IV_basin_id
!unique IDs in basin_id_file, of length IS_reachbas
integer*8, dimension(:), allocatable :: IV_basin_index
!indexes (Fortran, 1-based) of the reaches in the basin within the whole network
!size IS_reachbas
PetscInt,dimension(:), allocatable :: IV_basin_loc
!vector giving the zero-base index corresponding to the river reaches within 
!the basin studied only, to be used in VecSetValues. size IS_reachbas

PetscInt :: IS_gagetot
!total number of reaches that have observations (gaged reaches), corresponds to
!the number of lines in gagetot_id_file 
PetscInt :: JS_gagetot
!loop index corresponding to the number of gaged reaches (IS_gagetot)

PetscInt :: IS_forcingtot
!total number of reaches that where forcing is available, corresponds to the 
!number of lines in forcingtot_id_file 
PetscInt :: JS_forcingtot
!loop index corresponding to the number of gaged reaches (IS_forcingtot)
PetscInt :: IS_forcinguse
!total number of reaches where forcing will be used if in basin considered
PetscInt :: JS_forcinguse


!*******************************************************************************
!Declaration of variables - input and output files
!*******************************************************************************
character(len=100) :: modcou_connect_file
!unit 10 - file with connectivity information following MODCOU notations
character(len=100) :: basin_id_file
!unit 14 - file with all the IDs of the reaches used in subbasin considered
character(len=100) :: gagetot_id_file
!unit 15 - file with all the IDs of the reaches that have gage measurements
character(len=100) :: gageuse_id_file
!unit 16 - file with all the IDs of the reaches that have gage measurements
character(len=100) :: forcingtot_id_file
!unit 17 - file with the IDs where flows can be used as forcing to their 
!corresponding downstream reach  
character(len=100) :: forcingtot_tp_file 
!unit 18 - file in which the type of forcing can be stored 
character(len=100) :: forcinguse_id_file
!unit 19 - file with the IDs of the reaches with forcing used 

character(len=100) :: k_file
!unit 20 - file with values for k (possibly from previous param. estim.)
character(len=100) :: x_file
!unit 21 - file with values for x (possibly from previous param. estim.)
character(len=100) :: kfac_file  
!unit 22 - file with kfac for all reaches of the domain
character(len=100) :: xfac_file
!unit 23 - file with xfac for all reaches of the domain

character(len=100) :: Qinit_file
!unit 30 - file where initial flowrates can be stored to run the model with them
character(len=100) :: Qfinal_file
!unit 31 - file where final flowrates can be stored at the end of model run 
character(len=100) :: m3_nc_file

character(len=100) :: Qobs_file
!unit 33 - file where the flowrates observations are given
character(len=100) :: Qfor_file
!unit 34 - file where forcing flowrates are stored.  Forcing is taken as the
!flow coming from upstream reach.
character(len=100) :: Qobsbarrec_file
!unit 35 - file where the reciprocal (1/xi) of the average forcing are stored.

character(len=100) :: Qout_file
!unit 40 - file where model-calculated flows are stored
character(len=100) :: V_file
!unit 41 - file where model-calculated volumes are stored
character(len=100) :: babsmax_file
!unit 42 - file where the maximum of the absolute values of the right-hand-side
!are stored
character(len=100) :: QoutRabsmin_file
!unit 43 - file where the minimum of the absolute values of the instantaneous 
!flows are stored 
character(len=100) :: QoutRabsmax_file
!unit 44 - file where the maximum of the absolute values of the instantaneous 
!flows are stored 
character(len=100) :: Qout_nc_file


!*******************************************************************************
!Declaration of variables - routing parameters and initial values 
!*******************************************************************************
PetscScalar :: ZS_knorm, ZS_xnorm
!constants (k,x) in Muskingum expression, normalized
PetscScalar :: ZS_knorm_init, ZS_xnorm_init
!constants (k,x) in Muskingum expression, normalized, initial values for opt.
PetscScalar, parameter :: ZS_kfac=3600,ZS_xfac=0.1
!corresponding factors, k in seconds, x has no dimension
PetscScalar :: ZS_k,ZS_x
!constants (k,x) in Muskingum expression.  k in seconds, x has no dimension

PetscScalar :: ZS_V0=10000,ZS_Qout0=0
!values to be used in the intitial state of V and Qout for river routing
!initial volume for each reach (m^3), initial outflow for each reach (m^3/s)


!*******************************************************************************
!Declaration of variables - temporal parameters
!*******************************************************************************
PetscScalar :: ZS_TauM
!Duration of main procedure, in seconds
PetscScalar :: ZS_dtM
!Time step of main procedure, in seconds
PetscInt :: IS_M
!Number of time steps within the main precedure
PetscInt :: JS_M
!Index of main procedure 

PetscScalar :: ZS_TauO
!Duration of optimization procedure, in seconds
PetscScalar :: ZS_dtO
!Time step of optimization procedure, in seconds
PetscInt :: IS_O
!Number of time steps within the optimization precedure
PetscInt :: JS_O
!Index of optimization procedure 

PetscScalar :: ZS_TauR
!Duration of river routing procedure, in seconds
PetscScalar :: ZS_dtR  
!Time step of river routing procedure, in seconds  
PetscInt :: IS_R
!Number of time steps within the river routing procedure
PetscInt :: JS_R
!Index of river routing procedure

PetscInt :: IS_RpO
!Number routing procedures needed per optimization time step 
PetscInt :: JS_RpO
!Index 

PetscInt :: IS_RpM
!Number routing procedures needed per optimization time step 
PetscInt :: JS_RpM
!Index 


!*******************************************************************************
!Declaration of variables - Network matrix variables
!*******************************************************************************
Mat :: ZM_Net
!Network matrix
Logical :: BS_logical
!Boolean used during network matrix creation to give warnings if connectivity pb

PetscInt, dimension(:), allocatable :: IV_connect_id
!unique IDs of reaches in modcou_connect_file
PetscInt, dimension(:), allocatable :: IV_down
!vector of the downstream river reach of each river reach (corresponds to ipere)
PetscInt, dimension(:), allocatable :: IV_nbup
!vector of the number of direct upstream river reach of each river reach 
!(corresponds to nbfils)
PetscInt :: IS_max_up
!maximum number of upstream river reaches for each river reach
PetscInt, dimension(:,:), allocatable :: IM_up
!matrix with the ID of the upstream river reaches of each river reach
PetscInt :: JS_up
!JS_up for the corresponding upstream reaches
PetscInt :: IS_row,IS_col
!index of rows and columns used to fill up the network matrix
PetscInt,dimension (:,:), allocatable :: IM_index_up
!matrix with the index of the upstream river reaches of each river reach
!index goes from 1 to IS_reachbas 


!*******************************************************************************
!Declaration of variables - Observation matrix and optimization variables
!*******************************************************************************
Mat :: ZM_Obs
!Observation matrix
Vec :: ZV_Qobs
!Observation vector
PetscScalar :: ZS_norm
!norm of matrix ZM_Obs, used to calculate the number of gaging stations used

PetscInt :: IS_gageuse
!Number of gages available in gageuse_id_file
PetscInt :: JS_gageuse
!Corresponding index
PetscInt :: IS_gagebas
!Number of gages within basin studied.  Will be calculated based on 
!gagetot_id_file, gageuse_id_file and basin_id_file
PetscInt :: JS_gagebas
!Corresponding index

PetscInt, dimension(:), allocatable :: IV_gagetot_id
!vector were are stored the river ID of each gage available
PetscInt, dimension(:), allocatable :: IV_gageuse_id
!vector were are stored the river ID of each gage used in current run
PetscInt, allocatable, dimension(:) :: IV_gage_index
!vector where the Fortran 1-based indexes of the gages within the Qobs_file. 
!Will be allocated size IS_gagebas
PetscInt, allocatable, dimension(:) :: IV_gage_loc
!vector where the C (0-based) vector indexes of where gages are. This is 
!within the basin only, not all domain. Will be used in VecSet.  Will be 
!allocated size IS_gagebas

PetscScalar :: ZS_phi,ZS_phitemp
!cost function
PetscInt :: IS_Iter
!number of iterations needed for optimization procedure to end
Vec :: ZV_temp1,ZV_temp2
!temporary vectors, used for calculations
PetscScalar :: ZS_phifac
PetscInt :: IS_strt_opt
!first time step at which m3_riv data is read during optimization

Vec :: ZV_kfac
!Vector of size IS_reachbas a multiplication factor for k for all river reaches
!in basin
Vec :: ZV_Qobsbarrec
!Vector with the reciprocal (1/xi) of the average observations
 

!*******************************************************************************
!Declaration of variables - Forcing variables
!*******************************************************************************
PetscInt :: IS_forcingbas
!number of reaches forced by observations, within basin. Calculated on the fly
!from forcing_id_file and basin_id_file
PetscInt :: JS_forcingbas
!corresponding index
PetscInt, dimension(:), allocatable :: IV_forcingtot_id
!IDs of the reaches where forcing data are available
PetscInt, dimension(:), allocatable :: IV_forcinguse_id
!IDs of the reaches where forcing date will be used 
PetscInt, allocatable, dimension(:) :: IV_forcing_index
!vector where the Fortran 1-based indexes of the forcing needed is stored 
!(useful for disconnected network or forcing by gage measurement).  used to 
!read forcing_file, the index is within the whole domain.  This is upstream
!of the missing connection
PetscInt, allocatable, dimension(:) :: IV_forcing_loc
!vector where the C (0-based) vector indexes of where the above forcing is going
!to be applied (calculated thanks to connectivity and basin_id tables).  This is 
!within the basin only, not all domain. Will be used in VecSet


!*******************************************************************************
!Declaration of variables - Routing matrices and vectors
!*******************************************************************************
Mat :: ZM_A
!Matrix used to solve linear system 
Vec :: ZV_k,ZV_x
!Muskingum expression constants vectors, k in seconds, x has no dimension
Vec :: ZV_p, ZV_pnorm,ZV_pfac
!vector of the problem parameters, p=(k,x).  normalized version and 
!corresponding factors p=pnorm*pfac
Vec :: ZV_C1,ZV_C2,ZV_C3,ZV_Cdenom 
!Muskingum method constants (last is the common denominator, for calculations)
Vec :: ZV_b,ZV_babsmax
!Used for linear system A*Qout=b

!Input variables (contribution)
Vec :: ZV_Qext,ZV_Qfor,ZV_Qlat
!flowrates Qext is the sum of forced and lateral
Vec :: ZV_Vext,ZV_Vfor,ZV_Vlat 
!volumes (same as above)

!Main only variables
Vec :: ZV_QoutM,ZV_QoutinitM,ZV_QoutprevM,ZV_QoutbarM
Vec :: ZV_VM,ZV_VinitM,ZV_VprevM,ZV_VbarM

!Optimization only variables
Vec :: ZV_QoutO,ZV_QoutinitO,ZV_QoutprevO,ZV_QoutbarO
Vec :: ZV_VO,ZV_VinitO,ZV_VprevO,ZV_VbarO

!Routing only variables
Vec :: ZV_QoutR,ZV_QoutinitR,ZV_QoutprevR,ZV_QoutbarR
Vec :: ZV_QoutRabsmin,ZV_QoutRabsmax
Vec :: ZV_VR,ZV_VinitR,ZV_VprevR,ZV_VbarR
Vec :: ZV_VoutR


!*******************************************************************************
!Declaration of variables - PETSc specific objects and variables
!*******************************************************************************
PetscErrorCode :: ierr
!needed for error check of PETSc functions
KSP :: ksp
!object used for linear system solver
PC :: pc
!preconditioner object
PetscMPIInt :: rank
!integer where the number of each processor is stored, 0 will be main processor 
VecScatter :: vecscat
!Allows for scattering and gathering vectors from in parallel environement
PetscLogEvent :: stage
!Stage for investigating performance

PetscInt :: IS_ksp_iter, IS_ksp_iter_max
!integer where the number of iterations in KSP is solved
PetscInt :: IS_one=1
!integer of value 1.  to be used in MatSetValues and VecSet. Directly using 
!the value 1 in the functions crashes PETSc
PetscScalar :: ZS_one=1
!Scalars of values 1 and 0, same remark as above
Vec :: ZV_one
!vector with only ones, useful for creation of matrices here
Vec :: ZV_SeqZero
!Sequential vector of size IS_reachbas, allows for gathering data on zeroth 
!precessor before writing in file

PetscScalar,dimension(:), allocatable :: ZV_read_reachtot
!temp vector that stores information from a 'read', before setting the value
!in the object, this vector has the size of the total number of reaches
PetscScalar,dimension(:), allocatable :: ZV_read_gagetot
!same as previous, with size IS_gagetot
PetscScalar,dimension(:), allocatable :: ZV_read_forcingtot
!same as previous, with size IS_forcingtot
PetscScalar :: ZS_time1, ZS_time2, ZS_time3
!to estimate computing time

PetscScalar, pointer :: ZV_pointer(:)
!used to point to a PETSc vector and to output formatted as needed in a file
character(len=10) :: temp_char,temp_char2
!usefull to print variables on output.  write a variable in this character and
!then use PetscPrintf

PetscInt, dimension(:), allocatable :: IV_nz, IV_dnz, IV_onz
!number of nonzero elements per row for network matrix.  nz for sequential, dnz 
!and onz for distributed matrix (diagonal and off-diagonal elements)
PetscInt :: IS_ownfirst, IS_ownlast
!Ownership of each processor


!*******************************************************************************
!Declaration of variables - TAO specific objects and variables
!*******************************************************************************
#ifndef NO_TAO
TAO_SOLVER :: tao
!TAO solver object
TAO_APPLICATION :: taoapp
!TAO application object
TaoTerminateReason :: reason
!TAO terminate reason object
Vec :: ZV_1stIndex, ZV_2ndIndex
!ZV_1stIndex=[1;0], ZV_2ndIndex=[0,1].  Used with VecDot to extract first and 
!second indexes of the vector of parameter
#endif

!*******************************************************************************
!Declaration of variables - netCDF variables
!*******************************************************************************
PetscInt :: IS_nc_status
PetscInt :: IS_nc_id_fil_m3,IS_nc_id_fil_Qout
PetscInt :: IS_nc_id_var_m3,IS_nc_id_var_Qout,IS_nc_id_var_comid
PetscInt :: IS_nc_id_dim_comid,IS_nc_id_dim_time
PetscInt, parameter :: IS_nc_ndim=2
PetscInt, dimension(IS_nc_ndim) :: IV_nc_id_dim, IV_nc_start, IV_nc_count,     &
                                   IV_nc_count2


!*******************************************************************************
!Declaration of variables - runtime options
!*******************************************************************************
logical :: BS_opt_Qinit
!.false. --> no read initial flow    .true. --> read initial flow
logical :: BS_opt_Qfinal
!.false. --> no write final flow     .true. --> write final flow 
logical :: BS_opt_forcing
!.false. --> no forcing              .true. --> forcing
logical :: BS_opt_influence
!.false. --> no output influence     .true. --> output influence
PetscInt :: IS_opt_routing
!1       --> matrix-based Muskingum  2      --> traditional Muskingum
PetscInt :: IS_opt_run
!1       --> regular run             2      --> parameter optimization
PetscInt :: IS_opt_phi
!1       --> phi1                    2      --> phi2


!*******************************************************************************
!Namelist
!*******************************************************************************
namelist /NL_namelist/                                                         &
                       BS_opt_Qinit,BS_opt_Qfinal,                             &
                       BS_opt_forcing,BS_opt_influence,                        &
                       IS_opt_routing,IS_opt_run,IS_opt_phi,                   &
                       IS_reachtot,modcou_connect_file,m3_nc_file,IS_max_up,   &
                       IS_reachbas,basin_id_file,                              &
                       Qinit_file,Qfinal_file,                                 &
                       IS_forcingtot,forcingtot_id_file,forcingtot_tp_file,    &
                       Qfor_file,                                              &
                       IS_forcinguse,forcinguse_id_file,                       &
                       babsmax_file,QoutRabsmin_file,QoutRabsmax_file,         &
                       k_file,x_file,Qout_nc_file,                             &
                       kfac_file,xfac_file,ZS_knorm_init,ZS_xnorm_init,        &
                       IS_gagetot,gagetot_id_file,IS_gageuse,gageuse_id_file,  &
                       Qobs_file,Qobsbarrec_file,                              &
                       ZS_TauM,ZS_dtM,ZS_TauO,ZS_dtO,ZS_TauR,ZS_dtR,           &
                       ZS_phifac,IS_strt_opt
 
character(len=100) :: namelist_file
!unit 88 - Namelist


end module rapid_var
