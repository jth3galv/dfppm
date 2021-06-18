!
!     SCHIT. 224
!     N=2, M=8 (box=4)
!

SUBROUTINE N(i)
    INTEGER, INTENT(OUT) :: i
    i = 2
END SUBROUTINE

SUBROUTINE M(i)
    INTEGER, INTENT(OUT) :: i
    i = 8
END SUBROUTINE

SUBROUTINE XSTAR(X)
    IMPLICIT NONE
    REAL(kind = 8), INTENT(OUT) :: X(2)

    X(1) = 4d0
    X(2) = 4d0

    RETURN
END

SUBROUTINE XINIZ(N, X)
    IMPLICIT NONE
    INTEGER N, I
    REAL(kind = 8) X(N)

    X(1) = 0.1d0
    X(2) = 0.1d0

    RETURN
END

SUBROUTINE FUNCTF(N, X, F)
    IMPLICIT NONE

    INTEGER N
    REAL(kind = 8) X(N)
    REAL(kind = 8), INTENT(OUT) :: F

    F = 2.D0 * X(1)**2 + X(2)**2 - 48.D0 * X(1) - 40.D0 * X(2)

    RETURN
END
!
!     GRADIENTE FUNZIONE OBIET.
!
!      SUBROUTINE GRADF(N,X,GF)
!      IMPLICIT NONE
!      INTEGER N
!      REAL(kind=8) X(N),GF(N),COST

!      COST=27.D0*DSQRT(3.D0)

!       GF(1) =2.D0*(X(1)-3.D0)*X(2)**3/COST

!       GF(2)= 3.D0*((X(1)-3.D0)**2-9.D0)*X(2)**2/COST

!      RETURN
!      END

!
!     HESSIANO FUNZIONE OBIET.
!

!      SUBROUTINE HESSF(N,X,H)
!      IMPLICIT NONE
!      INTEGER N
!      REAL(kind=8) X(N),H(N,N),COST

!      COST=27.D0*DSQRT(3.D0)
!      H=0.D0

!      H(1,1)=2.D0*X(2)**3/COST
!      H(2,2)=6.D0*((X(1)-3.D0)**2-9)*X(2)/COST
!      H(1,2)=6.D0*(X(1)-3.D0)*X(2)**2/COST
!      H(2,1)=H(1,2)

!      RETURN
!      END

!
!     VINCOLI DI BOUND
!

!      SUBROUTINE BOUNDS(N,LOWER,UPPER)
!      IMPLICIT NONE
!      INTEGER N
!      REAL(kind=8) LOWER(N),UPPER(N)

!      LOWER=0.D0
!      UPPER=1.D+5

!      RETURN
!      END

!
!     VINCOLI DISUGUAGLIANZA GENERALI
!
SUBROUTINE FUNCTVD(N, NQ, X, VD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8)  X(N), VD(NQ)

    VD(1) = -X(1) - 3.D0 * X(2)
    VD(2) = -18.D0 + X(1) + 3.D0 * X(2)
    VD(3) = -X(1) - X(2)
    VD(4) = -8.D0 + X(1) + X(2)
    VD(5) = -X(1)
    VD(6) = -X(2)
    VD(7) = X(1) - 6.D0
    VD(8) = X(2) - 6.D0

    RETURN
END

!
!     GRADIENTI DEI VINCOLI DISUGUAGLIANZA
!
SUBROUTINE GRADVD(N, NQ, X, GVD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8)  X(N), GVD(N, NQ)

    GVD(1, 1) = -1.D0
    GVD(2, 1) = -3.D0

    GVD(1, 2) = 1.D0
    GVD(2, 2) = 3.D0

    GVD(1, 3) = -1.D0
    GVD(2, 3) = -1.D0

    GVD(1, 4) = 1.D0
    GVD(2, 4) = 1.D0

    GVD(1, 5) = -1.D0
    GVD(2, 5) = 0.D0

    GVD(1, 6) = 0.D0
    GVD(2, 6) = -1.D0

    GVD(1, 7) = 1.D0
    GVD(2, 7) = 0.D0

    GVD(1, 8) = 0.D0
    GVD(2, 8) = 1.D0

    RETURN
END

!
!     HESSIANI VINCOLI DISUGUAGLIANZA
!
!      SUBROUTINE HESSVD(N,NQ,X,HVD)
!      IMPLICIT NONE
!      INTEGER N,NQ,I,J,K
!      REAL(kind=8) X(N),HVD(N,N,NQ)
!
!	HVD=0.D0
!      RETURN
!      END




