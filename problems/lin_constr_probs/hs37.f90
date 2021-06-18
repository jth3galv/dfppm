!
!     SCHIT. 37 (Betts- Box)
!     N=3,M=2
!


SUBROUTINE N(i)
    INTEGER, INTENT(OUT) :: i
    i = 3
END SUBROUTINE

SUBROUTINE M(i)
    INTEGER, INTENT(OUT) :: i
    i = 8
END SUBROUTINE

SUBROUTINE XSTAR(X)
    IMPLICIT NONE
    REAL(kind = 8), INTENT(OUT) :: X(3)

    X(1) = 24d0
    X(2) = 12d0
    X(3) = 12d0

    RETURN
END

SUBROUTINE XINIZ(N, X)
    IMPLICIT NONE
    INTEGER N
    REAL(kind = 8)  X(N)

    X(1) = 10.D0
    X(2) = 10.D0
    X(3) = 10.D0

    RETURN
END
!
!     PROBLEMA TEST HOCK SCHI 37
!
SUBROUTINE FUNCTF(N, X, F)
    IMPLICIT NONE
    INTEGER N
    REAL(kind = 8)  X(N)
    REAL, INTENT(OUT) :: F
    F = -X(1) * X(2) * X(3)
    RETURN
END
!
!     GRADIENTE FUNZIONE OBIET.
!
SUBROUTINE GRADF(N, X, GF)
    IMPLICIT NONE
    INTEGER N
    REAL(kind = 8) X(N), GF(N)

    GF(1) = -X(2) * X(3)
    GF(2) = -X(1) * X(3)
    GF(3) = -X(1) * X(2)

    RETURN
END
!
!     HESSIANO FUNZIONE OBIET.
!
SUBROUTINE HESSF(N, X, H)
    IMPLICIT NONE
    INTEGER N
    REAL(kind = 8)  X(N), H(N, N)
    H(1, 1) = 0.D0
    H(1, 2) = -X(3)
    H(1, 3) = -X(2)
    H(2, 1) = -X(3)
    H(2, 2) = 0.D0
    H(2, 3) = -X(1)
    H(3, 1) = -X(2)
    H(3, 2) = -X(1)
    H(3, 3) = 0.D0
    RETURN
END


!
!     VINCOLI BOX
!
SUBROUTINE BOUNDS(N, LOWER, UPPER)
    IMPLICIT NONE
    INTEGER N
    REAL(kind = 8) LOWER(N), UPPER(N)
    LOWER = 0.D0
    UPPER = 42.D0
    RETURN
END


!
!     VINCOLI DISUGUAGLIANZA GENERALI
!
SUBROUTINE FUNCTVD(N, NQ, X, VD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8)  X(N), VD(NQ)
    VD(1) = X(1) + 2.D0 * X(2) + 2.d0 * X(3) - 72.D0
    VD(2) = -X(1) - 2.D0 * X(2) - 2.D0 * X(3)
    VD(3) = -X(1)
    VD(4) = X(1) - 42.D0
    VD(5) = -X(2)
    VD(6) = X(2) - 42.D0
    VD(7) = -X(3)
    VD(8) = X(3) - 42.D0
    RETURN
END

!
!     GRADIENTI DEI VINCOLI DISUGUAGLIANZA
!
SUBROUTINE GRADVD(N, NQ, X, GVD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8) X(N), GVD(N, NQ)
    GVD(1, 1) = 1.D0
    GVD(2, 1) = 2.D0
    GVD(3, 1) = 2.D0

    GVD(1, 2) = -1.D0
    GVD(2, 2) = -2.D0
    GVD(3, 2) = -2.D0

    GVD(1, 3) = -1.D0
    GVD(2, 3) = 0.D0
    GVD(3, 3) = 0.D0
    !
    GVD(1, 4) = 1.D0
    GVD(2, 4) = 0.D0
    GVD(3, 4) = 0.D0
    !
    GVD(1, 5) = 0.D0
    GVD(2, 5) = -1.D0
    GVD(3, 5) = 0.D0
    !
    GVD(1, 6) = 0.D0
    GVD(2, 6) = 1.D0
    GVD(3, 6) = 0.D0

    GVD(1, 7) = 0.D0
    GVD(2, 7) = 0.D0
    GVD(3, 7) = -1.D0
    !
    GVD(1, 8) = 0.D0
    GVD(2, 8) = 0.D0
    GVD(3, 8) = 1.D0

    RETURN
END

!
!     HESSIANI VINCOLI DISUGUAGLIANZA 
!
SUBROUTINE HESSVD(N, NQ, X, HVD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8) X(N), HVD(N, N, NQ)
    HVD = 0.D0
    RETURN
END





























