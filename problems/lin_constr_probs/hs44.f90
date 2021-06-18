!
!     SCHIT. 67 (Betts)
!     N=4, M=10 (box=4)
!


SUBROUTINE N(i)
    INTEGER, INTENT(OUT) :: i
    i = 4
END SUBROUTINE N

SUBROUTINE M(i)
    INTEGER, INTENT(OUT) :: i
    i = 10
END SUBROUTINE M


SUBROUTINE XSTAR(X)
    IMPLICIT NONE
    REAL(kind = 8), INTENT(OUT) :: X(4)

    X(1) = 0d0
    X(2) = 3.d0
    X(3) = 0d0
    X(4) = 4d0

    RETURN
END

SUBROUTINE XINIZ(N, X)
    IMPLICIT NONE
    INTEGER N, I
    REAL(kind = 8) X(N)

    X(1) = 0.d0
    X(2) = 0.d0
    X(3) = 0.d0
    X(4) = 0.d0

    RETURN
END

!
SUBROUTINE FUNCTF(N, X, F)
    IMPLICIT NONE

    INTEGER N
    REAL(kind = 8) X(N)
    REAL (kind = 8), INTENT(OUT) :: F

    F = X(1) - X(2) - X(3) - X(1) * X(3) + X(1) * X(4)&
            + X(2) * X(3) - X(2) * X(4)

    RETURN
END
!
!     GRADIENTE FUNZIONE OBIET.
!
!      SUBROUTINE GRADF(N,X,GF)
!      IMPLICIT NONE
!      INTEGER N
!      REAL(kind=8) X(N),GF(N)

!       GF(1) =
!       GF(2)= 

!      RETURN
!      END

!
!     HESSIANO FUNZIONE OBIET.
!

!      SUBROUTINE HESSF(N,X,H)
!      IMPLICIT NONE
!      INTEGER N
!      REAL(kind=8) X(N),H(N,N)

!      H(1,1)=

!      RETURN
!      END

!
!     VINCOLI DI BOUND
!

!
!     VINCOLI DISUGUAGLIANZA GENERALI
!
SUBROUTINE FUNCTVD(N, NQ, X, VD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8)  X(N), VD(NQ)

    VD(1) = -8.D0 + X(1) + 2.D0 * X(2)
    VD(2) = -12.D0 + 4.D0 * X(1) + X(2)
    VD(3) = -12.D0 + 3.D0 * X(1) + 4.D0 * X(2)
    VD(4) = -8.D0 + 2.D0 * X(3) + X(4)
    VD(5) = -8.D0 + X(3) + 2.D0 * X(4)
    VD(6) = -5.D0 + X(3) + X(4)
    VD(7) = -X(1)
    VD(8) = -X(2)
    VD(9) = -X(3)
    VD(10) = -X(4)
    RETURN
END

!
!     GRADIENTI DEI VINCOLI DISUGUAGLIANZA
!
SUBROUTINE GRADVD(N, NQ, X, GVD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8)  X(N), GVD(N, NQ)

    GVD(1, 1) = 1.D0
    GVD(2, 1) = 2.D0
    GVD(3, 1) = 0.D0
    GVD(4, 1) = 0.D0

    GVD(1, 2) = 4.D0
    GVD(2, 2) = 1.D0
    GVD(3, 2) = 0.D0
    GVD(4, 2) = 0.D0

    GVD(1, 3) = 3.D0
    GVD(2, 3) = 4.D0
    GVD(3, 3) = 0.D0
    GVD(4, 3) = 0.D0

    GVD(1, 4) = 0.D0
    GVD(2, 4) = 0.D0
    GVD(3, 4) = 2.D0
    GVD(4, 4) = 1.D0

    GVD(1, 5) = 0.D0
    GVD(2, 5) = 0.D0
    GVD(3, 5) = 1.D0
    GVD(4, 5) = 2.D0

    GVD(1, 6) = 0.D0
    GVD(2, 6) = 0.D0
    GVD(3, 6) = 1.D0
    GVD(4, 6) = 1.D0

    GVD(1, 7) = -1.D0
    GVD(2, 7) = 0.D0
    GVD(3, 7) = 0.D0
    GVD(4, 7) = 0.D0

    GVD(1, 8) = 0.D0
    GVD(2, 8) = -1.D0
    GVD(3, 8) = 0.D0
    GVD(4, 8) = 0.D0

    GVD(1, 9) = 0.D0
    GVD(2, 9) = 0.D0
    GVD(3, 9) = -1.D0
    GVD(4, 9) = 0.D0

    GVD(1, 10) = 0.D0
    GVD(2, 10) = 0.D0
    GVD(3, 10) = 0.D0
    GVD(4, 10) = -1.D0

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




