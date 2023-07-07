!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_TRIDIAG_GROUND_SNOWCRO
!
INTERFACE TRIDIAG_GROUND_SNOWCRO
!
SUBROUTINE TRIDIAG_GROUND_SNOWCRO_1D(PA,PB,PC,PY,PX,KNLVLS_USE,KDIFLOOP)
REAL,    DIMENSION(:,:), INTENT(IN)  :: PA  ! lower diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PB  ! main  diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PC  ! upper diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PY  ! r.h.s. term
!
REAL,    DIMENSION(:,:), INTENT(OUT) :: PX  ! solution of A.X = Y
!
INTEGER,    DIMENSION(:), INTENT(IN) :: KNLVLS_USE ! number of effective layers 
!
INTEGER, INTENT(IN) :: KDIFLOOP       ! shift in control loops: 0 or 1
END SUBROUTINE TRIDIAG_GROUND_SNOWCRO_1D
!
SUBROUTINE TRIDIAG_GROUND_SNOWCRO_2D(PA,PB,PC,PY,PX,KNLVLS_USE,KDIFLOOP)
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PA  ! lower diag. elements of A matrix
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PB  ! main  diag. elements of A matrix
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PC  ! upper diag. elements of A matrix
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PY  ! r.h.s. term
!
REAL,    DIMENSION(:,:,:), INTENT(OUT) :: PX  ! solution of A.X = Y
!
INTEGER,    DIMENSION(:,:), INTENT(IN) :: KNLVLS_USE ! number of effective layers 
!
INTEGER, INTENT(IN) :: KDIFLOOP       ! shift in control loops: 0 or 1
END SUBROUTINE TRIDIAG_GROUND_SNOWCRO_2D
!
END INTERFACE TRIDIAG_GROUND_SNOWCRO
!
END MODULE MODI_TRIDIAG_GROUND_SNOWCRO
!
!###################################################################
       SUBROUTINE TRIDIAG_GROUND_SNOWCRO_1D(PA,PB,PC,PY,PX,KNLVLS_USE,KDIFLOOP)
!      #########################################
!
!
!!****   *TRIDIAG_GROUND* - routine to solve a time implicit scheme
!!
!!
!!     PURPOSE
!!     -------
!        The purpose of this routine is to resolve the linear system:
!
!       A.X = Y
!
!      where A is a tridiagonal matrix, and X and Y two vertical vectors.
!     However, the computations are performed at the same time for all
!     the verticals where an inversion of the system is necessary.
!     This explain the dimansion of the input variables.
!
!!**   METHOD
!!     ------
!!
!!        Then, the classical tridiagonal algorithm is used to invert the 
!!     implicit operator. Its matrix is given by:
!!
!!     (  b(1)      c(1)      0        0        0         0        0        0  )
!!     (  a(2)      b(2)     c(2)      0  ...    0        0        0        0  ) 
!!     (   0        a(3)     b(3)     c(3)       0        0        0        0  ) 
!!      .......................................................................
!!     (   0   ...   0      a(k)      b(k)     c(k)       0   ...  0        0  ) 
!!      .......................................................................
!!     (   0         0        0        0        0 ...  a(n-1)   b(n-1)   c(n-1))
!!     (   0         0        0        0        0 ...     0      a(n)     b(n) )
!!
!!
!!       All these computations are purely vertical and vectorizations are 
!!     easely achieved by processing all the verticals in parallel.
!!
!!     EXTERNAL
!!     --------
!!
!!       NONE
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!
!!     REFERENCE
!!     ---------
!!
!!     AUTHOR
!!     ------
!!       V. Masson
!!
!!     MODIFICATIONS
!!     -------------
!!       Original        May 13, 1998
!!       05/2011: Brun  Special treatment to tackle the variable number
!!                      of snow layers
!!                      In case of second call, a shift of 1 snow layer
!!                      is applied in the control loops.
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
!USE YOMHOOK , ONLY : LHOOK,   DR_HOOK
!USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1 declarations of arguments
!
REAL,    DIMENSION(:,:), INTENT(IN)  :: PA  ! lower diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PB  ! main  diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PC  ! upper diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PY  ! r.h.s. term
!
REAL,    DIMENSION(:,:), INTENT(OUT) :: PX  ! solution of A.X = Y
!
INTEGER,    DIMENSION(:), INTENT(IN) :: KNLVLS_USE ! number of effective layers 
!
INTEGER, INTENT(IN) :: KDIFLOOP       ! shift in control loops: 0 or 1
!
!*       0.2 declarations of local variables
!
REAL, DIMENSION(SIZE(PA,1),SIZE(PA,2)) :: ZW   ! work array
REAL, DIMENSION(SIZE(PA,1)           ) :: ZDET ! work array
!
!
INTEGER           :: JL, JJ            ! vertical loop control
INTEGER           :: INL               ! number of vertical levels
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
! ---------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('TRIDIAG_GROUND_SNOWCRO_1D',0,ZHOOK_HANDLE)
!
INL = SIZE(PX,2)
!
!*       1.  levels going up
!            ---------------
!
!*       1.1 first level
!            -----------
!
ZDET(:) = PB(:,1)
!
PX(:,1) = PY(:,1) / ZDET(:)
!
!*       1.2 other levels
!            ------------
!
DO JL = 2,INL
  !
  DO JJ = 1,SIZE(PX,1)
    !
    IF ( JL<=KNLVLS_USE(JJ)-KDIFLOOP ) THEN
      !
      ZW(JJ,JL) = PC(JJ,JL-1)/ZDET(JJ)
      ZDET(JJ)  = PB(JJ,JL  ) - PA(JJ,JL)*ZW(JJ,JL)
      !
      PX(JJ,JL) = ( PY(JJ,JL) - PA(JJ,JL)*PX(JJ,JL-1) ) / ZDET(JJ)
      !
    ENDIF
    !
  ENDDO
  !
END DO
!
!-------------------------------------------------------------------------------
!
!*       2.  levels going down
!            -----------------
!
DO JL = INL-1,1,-1
  !
  DO JJ = 1,SIZE(PX,1)
    !
    IF ( JL<=KNLVLS_USE(JJ)-1-KDIFLOOP ) THEN
      !
      PX(JJ,JL) = PX(JJ,JL) - ZW(JJ,JL+1)*PX(JJ,JL+1)
      !
    ENDIF
    !
  ENDDO
  !
END DO
!
!IF (LHOOK) CALL DR_HOOK('TRIDIAG_GROUND_SNOWCRO_1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIDIAG_GROUND_SNOWCRO_1D
!
!###################################################################
       SUBROUTINE TRIDIAG_GROUND_SNOWCRO_2D(PA,PB,PC,PY,PX,KNLVLS_USE,KDIFLOOP)
!      #########################################
!
!
!!****   *TRIDIAG_GROUND* - routine to solve a time implicit scheme
!!
!!
!!     PURPOSE
!!     -------
!        The purpose of this routine is to resolve the linear system:
!
!       A.X = Y
!
!      where A is a tridiagonal matrix, and X and Y two vertical vectors.
!     However, the computations are performed at the same time for all
!     the verticals where an inversion of the system is necessary.
!     This explain the dimansion of the input variables.
!
!!**   METHOD
!!     ------
!!
!!        Then, the classical tridiagonal algorithm is used to invert the 
!!     implicit operator. Its matrix is given by:
!!
!!     (  b(1)      c(1)      0        0        0         0        0        0  )
!!     (  a(2)      b(2)     c(2)      0  ...    0        0        0        0  ) 
!!     (   0        a(3)     b(3)     c(3)       0        0        0        0  ) 
!!      .......................................................................
!!     (   0   ...   0      a(k)      b(k)     c(k)       0   ...  0        0  ) 
!!      .......................................................................
!!     (   0         0        0        0        0 ...  a(n-1)   b(n-1)   c(n-1))
!!     (   0         0        0        0        0 ...     0      a(n)     b(n) )
!!
!!
!!       All these computations are purely vertical and vectorizations are 
!!     easely achieved by processing all the verticals in parallel.
!!
!!     EXTERNAL
!!     --------
!!
!!       NONE
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!
!!     REFERENCE
!!     ---------
!!
!!     AUTHOR
!!     ------
!!       V. Masson
!!
!!     MODIFICATIONS
!!     -------------
!!       Original        May 13, 1998
!!       05/2011: Brun  Special treatment to tackle the variable number
!!                      of snow layers
!!                      In case of second call, a shift of 1 snow layer
!!                      is applied in the control loops.
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
!USE YOMHOOK , ONLY : LHOOK,   DR_HOOK
!USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1 declarations of arguments
!
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PA  ! lower diag. elements of A matrix
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PB  ! main  diag. elements of A matrix
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PC  ! upper diag. elements of A matrix
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PY  ! r.h.s. term
!
REAL,    DIMENSION(:,:,:), INTENT(OUT) :: PX  ! solution of A.X = Y
!
INTEGER,    DIMENSION(:,:), INTENT(IN) :: KNLVLS_USE ! number of effective layers 
!
INTEGER, INTENT(IN) :: KDIFLOOP       ! shift in control loops: 0 or 1
!
!*       0.2 declarations of local variables
!
REAL, DIMENSION(SIZE(PA,2)) :: ZW   ! work array
REAL :: ZDET ! work array
!
!
INTEGER           :: JL, JJ, JB     ! vertical loop control
INTEGER           :: INL, INB            ! number of vertical levels
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
! ---------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('TRIDIAG_GROUND_SNOWCRO_2D',0,ZHOOK_HANDLE)
!
INL = SIZE(PX,2)
INB = SIZE(PX,3)
!
!*       1.  levels going up
!            ---------------
!
!*       1.1 first level
!            -----------
!
DO JB = 1,INB
  !
  DO JJ = 1,SIZE(PX,1)
    !
    ZDET = PB(JJ,1,JB)
    !
    PX(JJ,1,JB) = PY(JJ,1,JB) / ZDET
    !
    !*       1.2 other levels
    !            ------------
    !
    DO JL = 2,INL
      !
      IF ( JL<=KNLVLS_USE(JJ,JB)-KDIFLOOP ) THEN
        !
        ZW(JL) = PC(JJ,JL-1,JB)  /ZDET
        ZDET   = PB(JJ,JL  ,JB) - PA(JJ,JL,JB) * ZW(JL)
        !
        PX(JJ,JL,JB) = ( PY(JJ,JL,JB) - PA(JJ,JL,JB) * PX(JJ,JL-1,JB) ) / ZDET
        !
      ENDIF
      !
    ENDDO
    !
    !-------------------------------------------------------------------------------
    !
    !*       2.  levels going down
    !            -----------------
    !
    DO JL = INL-1,1,-1
      !
      IF ( JL<=KNLVLS_USE(JJ,JB)-1-KDIFLOOP ) THEN
        !
        PX(JJ,JL,JB) = PX(JJ,JL,JB) - ZW(JL+1) * PX(JJ,JL+1,JB)
        !
      ENDIF
      !
    ENDDO
    !
  END DO
  !
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('TRIDIAG_GROUND_SNOWCRO_2D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIDIAG_GROUND_SNOWCRO_2D
