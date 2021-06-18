!----------------------------------
!     PROBLEMA TEST DI HOCK 77
!----------------------------------

SUBROUTINE N(i)
    INTEGER, INTENT(OUT) :: i
    i = 2
END SUBROUTINE

SUBROUTINE M(i)
    INTEGER, INTENT(OUT) :: i
    i = 2
END SUBROUTINE

SUBROUTINE XSTAR(X)
    IMPLICIT NONE
    REAL(kind = 8), INTENT(OUT) :: X(2)

    X(1) = 1d0
    X(2) = 1d0

    RETURN
END


SUBROUTINE XINIZ(N, X)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N)

    X(1) = -1.2d0
    X(2) = 1.d0

    RETURN
END

!------------------------------------




SUBROUTINE FUNCTF(N, X, F)
    IMPLICIT DOUBLE PRECISION (A, B, X)
    DIMENSION X(N)
    REAL(kind = 8), INTENT(OUT) :: F

    A = X(2) - X(1) * X(1)
    B = 1 - X(1)

    F = 100.d0 * A * A + B * B

    RETURN
END
!----------------------------------
!     GRADIENTE FUNZIONE OBIET.
!----------------------------------
SUBROUTINE GRADF(N, X, GF)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N), GF(N)

    A = X(2) - X(1) * x(1)
    B = 1.d0 - X(1)

    GF(1) = -400.D0 * X(1) * A - 2.d0 * B
    GF(2) = 200.D0 * A

    RETURN
END
!----------------------------------
!     HESSIANO FUNZIONE OBIET.
!----------------------------------
SUBROUTINE HESSF(N, X, H)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N), H(N, N)
    H(1, 1) = -400.d0 * (X(2) - X(1) * X(1)) + 800.d0 * X(1) * X(1) + 2.d0
    H(1, 2) = -400.d0 * X(1)
    H(2, 1) = -400.d0 * X(1)
    H(2, 2) = 200.d0

    RETURN
END
!----------------------------
!     VINCOLI UGUAGLIANZA
!----------------------------
SUBROUTINE FUNCTVE(N, NQ, X, VE)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N), VE(NQ)
    RETURN
END
!------------------------------------------
!     GRADIENTI DEI VINCOLI UGUAGLIANZA
!------------------------------------------
SUBROUTINE GRADVE(N, NQ, X, GVE)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N), GVE(N, NQ)
    RETURN
END
!-------------------------------------
!     HESSIANI VINCOLI UGUAGLIANZA
!-------------------------------------
SUBROUTINE HESSVE(N, NQ, X, HVE)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N), HVE(N, N, NQ)
    RETURN
END

SUBROUTINE FUNCTVD(N, NM, X, VD)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N), VD(NM)
    terzo = 1.d0 / 3.d0
    VD(1) = -terzo * X(1) - X(2) - 0.1d0
    VD(2) = terzo * X(1) - X(2) - 0.1d0

    RETURN
END
!------------------------------------------
!     GRADIENTI DEI VINCOLI UGUAGLIANZA
!------------------------------------------
SUBROUTINE GRADVD(N, NM, X, GVD)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N), GVD(N, NM)
    !
    terzo = 1.d0 / 3.d0
    GVD(1, 1) = -terzo
    GVD(2, 1) = -1.d0
    GVD(1, 2) = terzo
    GVD(2, 2) = -1.d0
    RETURN
END
!-------------------------------------
!     HESSIANI VINCOLI UGUAGLIANZA
!-------------------------------------
SUBROUTINE HESSVD(N, NM, X, HVD)
    IMPLICIT DOUBLE PRECISION (A-H, O-Z)
    DIMENSION X(N), HVD(N, N, NM)
    !
    DO I = 1, N
        DO J = 1, n
            DO K = 1, NM
                HVD(I, J, K) = 0.d0
            ENDDO
        ENDDO
    ENDDO
    RETURN
END

