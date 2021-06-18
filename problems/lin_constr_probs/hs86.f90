!
!     SCHIT. 86 (Conville n.1)
!     N=5, M=10 (box=5)
!

SUBROUTINE N(i)
    INTEGER, INTENT(OUT) :: i
    i = 5
END SUBROUTINE N

SUBROUTINE M(i)
    INTEGER, INTENT(OUT) :: i
    i = 15
END SUBROUTINE M

SUBROUTINE XSTAR(X)
    IMPLICIT NONE
    REAL(kind = 8), INTENT(OUT) :: X(5)

    X(1) = .3d0
    X(2) = .33346761d0
    X(3) = .4d0
    X(4) = .42831010d0
    X(5) = .22396487d0

    RETURN
END

SUBROUTINE XINIZ(N, X)
    IMPLICIT NONE
    INTEGER N, I
    REAL(kind = 8) X(N)

    X(1) = 0.0d0
    X(2) = 0.0d0
    X(3) = 0.1d0
    !X(3) = 0.0d0
    X(4) = 0.0d0
    X(5) = 1.0d0

    RETURN
END

SUBROUTINE FUNCTF(N, X, F)
    IMPLICIT NONE

    INTEGER N, I, J
    REAL(kind = 8) X(N), D(5, 5), VC(5), VD(5)
    REAL (kind = 8), INTENT(OUT) :: F

    VC(1) = -15.D0
    VC(2) = -27.D0
    VC(3) = -36.D0
    VC(4) = -18.D0
    VC(5) = -12.D0

    D(1, 1) = 30.D0
    D(1, 2) = -20.D0
    D(1, 3) = -10.D0
    D(1, 4) = 32.D0
    D(1, 5) = -10.D0

    D(2, 1) = -20.D0
    D(2, 2) = 39.D0
    D(2, 3) = -6.D0
    D(2, 4) = -31.D0
    D(2, 5) = 32.D0

    D(3, 1) = -10.D0
    D(3, 2) = -6.D0
    D(3, 3) = 10.D0
    D(3, 4) = -6.D0
    D(3, 5) = -10.D0

    D(4, 1) = 32.D0
    D(4, 2) = -31.D0
    D(4, 3) = -6.D0
    D(4, 4) = 39.D0
    D(4, 5) = -20.D0

    D(5, 1) = -10.D0
    D(5, 2) = 32.D0
    D(5, 3) = -10.D0
    D(5, 4) = -20.D0
    D(5, 5) = 30.D0

    VD(1) = 4.D0
    VD(2) = 8.D0
    VD(3) = 10.D0
    VD(4) = 6.D0
    VD(5) = 2.D0

    F = 0.D0
    DO I = 1, 5
        F = F + VC(I) * X(I) + VD(I) * X(I) * X(I) * X(I)
        DO J = 1, 5
            F = F + D(I, J) * X(I) * X(J)
        ENDDO
    END DO

    RETURN
END
!
!     GRADIENTE FUNZIONE OBIET.
!
!      SUBROUTINE GRADF(N,X,GF)
!      IMPLICIT NONE
!      INTEGER N
!      REAL(kind=8) X(N),GF(N),COST
!
!      COST=27.D0*DSQRT(3.D0)
!
!       GF(1) =2.D0*(X(1)-3.D0)*X(2)**3/COST
!
!       GF(2)= 3.D0*((X(1)-3.D0)**2-9.D0)*X(2)**2/COST
!
!      RETURN
!      END

!
!     HESSIANO FUNZIONE OBIET.
!

!      SUBROUTINE HESSF(N,X,H)
!      IMPLICIT NONE
!      INTEGER N
!      REAL(kind=8) X(N),H(N,N),COST
!
!      COST=27.D0*DSQRT(3.D0)
!      H=0.D0
!
!      H(1,1)=2.D0*X(2)**3/COST
!      H(2,2)=6.D0*((X(1)-3.D0)**2-9)*X(2)/COST
!      H(1,2)=6.D0*(X(1)-3.D0)*X(2)**2/COST
!      H(2,1)=H(1,2)
!
!      RETURN
!      END


!
!     VINCOLI DISUGUAGLIANZA GENERALI
!
SUBROUTINE FUNCTVD(N, NQ, X, VD)
    IMPLICIT NONE
    INTEGER N, NQ, I, J
    REAL(kind = 8)  X(N), VD(NQ), A(10, 5), B(10)

    B(1) = -40.D0
    B(2) = -2.D0
    B(3) = -.25D0
    B(4) = -4.D0
    B(5) = -4.D0
    B(6) = -1.D0
    B(7) = -40.D0
    B(8) = -60.D0
    B(9) = 5.D0
    B(10) = 1.D0

    A(1, 1) = -16.D0
    A(1, 2) = 2.D0
    A(1, 3) = 0.D0
    A(1, 4) = 1.D0
    A(1, 5) = 0.D0

    A(2, 1) = 0.D0
    A(2, 2) = -2.D0
    A(2, 3) = 0.D0
    A(2, 4) = 4.D0
    A(2, 5) = 2.D0

    A(3, 1) = -3.5D0
    A(3, 2) = 0.D0
    A(3, 3) = 2.D0
    A(3, 4) = 0.D0
    A(3, 5) = 0.D0

    A(4, 1) = 0.D0
    A(4, 2) = -2.D0
    A(4, 3) = 0.D0
    A(4, 4) = -4.D0
    A(4, 5) = -1.D0

    A(5, 1) = 0.D0
    A(5, 2) = -9.D0
    A(5, 3) = -2.D0
    A(5, 4) = 1.D0
    A(5, 5) = -2.8D0

    A(6, 1) = 2.D0
    A(6, 2) = 0.D0
    A(6, 3) = -4.D0
    A(6, 4) = 0.D0
    A(6, 5) = 0.D0

    A(7, 1) = -1.D0
    A(7, 2) = -1.D0
    A(7, 3) = -1.D0
    A(7, 4) = -1.D0
    A(7, 5) = -1.D0

    A(8, 1) = -1.D0
    A(8, 2) = -2.D0
    A(8, 3) = -3.D0
    A(8, 4) = -2.D0
    A(8, 5) = -1.D0

    A(9, 1) = 1.D0
    A(9, 2) = 2.D0
    A(9, 3) = 3.D0
    A(9, 4) = 4.D0
    A(9, 5) = 5.D0

    A(10, 1) = 1.D0
    A(10, 2) = 1.D0
    A(10, 3) = 1.D0
    A(10, 4) = 1.D0
    A(10, 5) = 1.D0

    DO I = 1, 10
        VD(I) = B(I)
        DO J = 1, 5
            VD(I) = VD(I) - A(I, J) * X(J)
        ENDDO
    ENDDO

    VD(11) = -X(1)
    VD(12) = -X(2)
    VD(13) = -X(3)
    VD(14) = -X(4)
    VD(15) = -X(5)

    RETURN
END

!
!     GRADIENTI DEI VINCOLI DISUGUAGLIANZA
!
SUBROUTINE GRADVD(N, NQ, X, GVD)
    IMPLICIT NONE
    INTEGER N, NQ, I, J
    REAL(kind = 8)  X(N), GVD(N, NQ), A(10, 5)

    A(1, 1) = -16.D0
    A(1, 2) = 2.D0
    A(1, 3) = 0.D0
    A(1, 4) = 1.D0
    A(1, 5) = 0.D0

    A(2, 1) = 0.D0
    A(2, 2) = -2.D0
    A(2, 3) = 0.D0
    A(2, 4) = 4.D0
    A(2, 5) = 2.D0

    A(3, 1) = -3.5D0
    A(3, 2) = 0.D0
    A(3, 3) = 2.D0
    A(3, 4) = 0.D0
    A(3, 5) = 0.D0

    A(4, 1) = 0.D0
    A(4, 2) = -2.D0
    A(4, 3) = 0.D0
    A(4, 4) = -4.D0
    A(4, 5) = -1.D0

    A(5, 1) = 0.D0
    A(5, 2) = -9.D0
    A(5, 3) = -2.D0
    A(5, 4) = 1.D0
    A(5, 5) = -2.8D0

    A(6, 1) = 2.D0
    A(6, 2) = 0.D0
    A(6, 3) = -4.D0
    A(6, 4) = 0.D0
    A(6, 5) = 0.D0

    A(7, 1) = -1.D0
    A(7, 2) = -1.D0
    A(7, 3) = -1.D0
    A(7, 4) = -1.D0
    A(7, 5) = -1.D0

    A(8, 1) = -1.D0
    A(8, 2) = -2.D0
    A(8, 3) = -3.D0
    A(8, 4) = -2.D0
    A(8, 5) = -1.D0

    A(9, 1) = 1.D0
    A(9, 2) = 2.D0
    A(9, 3) = 3.D0
    A(9, 4) = 4.D0
    A(9, 5) = 5.D0

    A(10, 1) = 1.D0
    A(10, 2) = 1.D0
    A(10, 3) = 1.D0
    A(10, 4) = 1.D0
    A(10, 5) = 1.D0

    DO I = 1, 10
        DO J = 1, 5
            GVD(J, I) = -A(I, J)
        ENDDO
    ENDDO

    GVD(1, 11) = -1.D0
    GVD(2, 11) = 0.D0
    GVD(3, 11) = 0.D0
    GVD(4, 11) = 0.D0
    GVD(5, 11) = 0.D0

    GVD(1, 12) = 0.D0
    GVD(2, 12) = -1.D0
    GVD(3, 12) = 0.D0
    GVD(4, 12) = 0.D0
    GVD(5, 12) = 0.D0

    GVD(1, 13) = 0.D0
    GVD(2, 13) = 0.D0
    GVD(3, 13) = -1.D0
    GVD(4, 13) = 0.D0
    GVD(5, 13) = 0.D0

    GVD(1, 14) = 0.D0
    GVD(2, 14) = 0.D0
    GVD(3, 14) = 0.D0
    GVD(4, 14) = -1.D0
    GVD(5, 14) = 0.D0

    GVD(1, 15) = 0.D0
    GVD(2, 15) = 0.D0
    GVD(3, 15) = 0.D0
    GVD(4, 15) = 0.D0
    GVD(5, 15) = -1.D0

    RETURN
END



