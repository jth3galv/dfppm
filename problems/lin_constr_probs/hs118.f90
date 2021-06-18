!------------------------------------
!     FUNZIONE HOCK-SCHITTKOWSKI 118
!------------------------------------


SUBROUTINE N(i)
    INTEGER, INTENT(OUT) :: i
    i = 15
END SUBROUTINE N

SUBROUTINE M(i)
    INTEGER, INTENT(OUT) :: i
    i = 59
END SUBROUTINE M

SUBROUTINE XSTAR(X)
    IMPLICIT NONE
    REAL(kind = 8), INTENT(OUT) :: X(15)

    X(1) = 8d0
    X(2) = 49d0
    X(3) = 3d0
    X(4) = 1d0
    X(5) = 56d0
    X(6) = 0d0
    X(7) = 1d0
    X(8) = 63d0
    X(9) = 6d0
    X(10) = 3d0
    X(11) = 70d0
    X(12) = 12d0
    X(13) = 5d0
    x(14) = 77d0
    X(15) = 18d0

    RETURN
END


SUBROUTINE XINIZ(N, X)
    IMPLICIT NONE
    INTEGER N, I
    REAL(kind = 8) X(N)

    X(1) = 20.d0
    X(2) = 55.d0
    X(3) = 15.d0
    X(4) = 20.d0
    X(5) = 60.d0
    X(6) = 20.d0
    X(7) = 20.d0
    X(8) = 60.d0
    X(9) = 20.d0
    X(10) = 20.d0
    X(11) = 60.d0
    X(12) = 20.d0
    X(13) = 20.d0
    X(14) = 60.d0
    X(15) = 20.d0

    RETURN
END

!------------------------------------
!     FUNZIONE 
!------------------------------------

SUBROUTINE FUNCTF(N, X, F)
    IMPLICIT NONE

    INTEGER N, K
    REAL(kind = 8) X(N)
    REAL(kind = 8), INTENT(OUT) :: F

    F = 2.3D0 * X(1) + 0.0001D0 * X(1) * X(1) + 1.7D0 * X(2) + 0.0001D0 * X(2) * X(2)
    F = F + 2.2D0 * X(3) + 0.00015D0 * X(3) * X(3)
    DO K = 1, 4
        F = F + 2.3D0 * X(3 * K + 1) + 0.0001D0 * X(3 * K + 1) * X(1) + 1.7D0 * X(3 * K + 2) + &
                0.0001D0 * X(3 * K + 2) * X(3 * K + 2)
        F = F + 2.2D0 * X(3 * K + 3) + 0.00015D0 * X(3 * K + 3) * X(3 * K + 3)
    END DO

    RETURN
END

!------------------------------------
!     VINCOLI 
!------------------------------------
SUBROUTINE FUNCTVD(N, NQ, X, VD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8)  X(N), VD(NQ)

    VD(1) = -13.D0 + X(4) - X(1) + 7.D0
    VD(2) = -13.D0 + X(6) - X(3) + 7.D0
    VD(3) = -14.D0 + X(5) - X(2) + 7.D0
    VD(4) = -13.D0 + X(7) - X(4) + 7.D0
    VD(5) = -13.D0 + X(9) - X(6) + 7.D0
    VD(6) = -14.D0 + X(8) - X(5) + 7.D0
    VD(7) = -13.D0 + X(10) - X(7) + 7.D0
    VD(8) = -13.D0 + X(12) - X(9) + 7.D0
    VD(9) = -14.D0 + X(11) - X(8) + 7.D0
    VD(10) = -13.D0 + X(13) - X(10) + 7.D0
    VD(11) = -13.D0 + X(15) - X(12) + 7.D0
    VD(12) = -14.D0 + X(14) - X(11) + 7.D0

    VD(13) = -(X(4) - X(1) + 7.D0)
    VD(14) = -(X(6) - X(3) + 7.D0)
    VD(15) = -(X(5) - X(2) + 7.D0)
    VD(16) = -(X(7) - X(4) + 7.D0)
    VD(17) = -(X(9) - X(6) + 7.D0)
    VD(18) = -(X(8) - X(5) + 7.D0)
    VD(19) = -(X(10) - X(7) + 7.D0)
    VD(20) = -(X(12) - X(9) + 7.D0)
    VD(21) = -(X(11) - X(8) + 7.D0)
    VD(22) = -(X(13) - X(10) + 7.D0)
    VD(23) = -(X(15) - X(12) + 7.D0)
    VD(24) = -(X(14) - X(11) + 7.D0)

    VD(25) = -(X(1) + X(2) + X(3) - 60.D0)
    VD(26) = -(X(7) + X(8) + X(9) - 70.D0)
    VD(27) = -(X(13) + X(14) + X(15) - 100.D0)
    VD(28) = -(X(4) + X(5) + X(6) - 50.D0)
    VD(29) = -(X(10) + X(11) + X(12) - 85.D0)

    VD(30) = -21.D0 + X(1)
    VD(31) = -57.D0 + X(2)
    VD(32) = -16.D0 + X(3)
    VD(33) = -X(1) + 8.D0
    VD(34) = -X(2) + 43.D0
    VD(35) = -X(3) + 3.D0

    VD(36) = -(90.D0 - X(4))
    VD(37) = -(120.D0 - X(5))
    VD(38) = -(60.D0 - X(6))
    VD(39) = -X(4)
    VD(40) = -X(5)
    VD(41) = -X(6)

    VD(42) = -(90.D0 - X(7))
    VD(43) = -(120.D0 - X(8))
    VD(44) = -(60.D0 - X(9))
    VD(45) = -X(7)
    VD(46) = -X(8)
    VD(47) = -X(9)

    VD(48) = -(90.D0 - X(10))
    VD(49) = -(120.D0 - X(11))
    VD(50) = -(60.D0 - X(12))
    VD(51) = -X(10)
    VD(52) = -X(11)
    VD(53) = -X(12)

    VD(54) = -(90.D0 - X(13))
    VD(55) = -(120.D0 - X(14))
    VD(56) = -(60.D0 - X(15))
    VD(57) = -X(13)
    VD(58) = -X(14)
    VD(59) = -X(15)

    RETURN
END

!-----------------------------------------
!     GRADIENTI DEI VINCOLI DISUGUAGLIANZA
!-----------------------------------------
SUBROUTINE GRADVD(N, NQ, X, GVD)
    IMPLICIT NONE
    INTEGER N, NQ, I, J
    REAL(kind = 8)  X(N), GVD(N, NQ)

    DO I = 1, N
        DO J = 1, NQ
            GVD(I, J) = 0.D0
        END DO
    END DO

    GVD(1, 1) = -1.D0
    GVD(4, 1) = 1.D0
    GVD(3, 2) = -1.D0
    GVD(6, 2) = 1.D0
    GVD(2, 3) = -1.D0
    GVD(5, 3) = 1.D0
    GVD(4, 4) = -1.D0
    GVD(7, 4) = 1.D0
    GVD(6, 5) = -1.D0
    GVD(9, 5) = 1.D0
    GVD(5, 6) = -1.D0
    GVD(8, 6) = 1.D0
    GVD(7, 7) = -1.D0
    GVD(10, 7) = 1.D0
    GVD(9, 8) = -1.D0
    GVD(12, 8) = 1.D0
    GVD(8, 9) = -1.D0
    GVD(11, 9) = 1.D0
    GVD(10, 10) = -1.D0
    GVD(13, 10) = 1.D0
    GVD(12, 11) = -1.D0
    GVD(15, 11) = 1.D0
    GVD(11, 12) = -1.D0
    GVD(14, 12) = 1.D0

    GVD(1, 13) = 1.D0
    GVD(4, 13) = -1.D0
    GVD(3, 14) = 1.D0
    GVD(6, 14) = -1.D0
    GVD(2, 15) = 1.D0
    GVD(5, 15) = -1.D0
    GVD(4, 16) = 1.D0
    GVD(7, 16) = -1.D0
    GVD(6, 17) = 1.D0
    GVD(9, 17) = -1.D0
    GVD(5, 18) = 1.D0
    GVD(8, 18) = -1.D0
    GVD(7, 19) = 1.D0
    GVD(10, 19) = -1.D0
    GVD(9, 20) = 1.D0
    GVD(12, 20) = -1.D0
    GVD(8, 21) = 1.D0
    GVD(11, 21) = -1.D0
    GVD(10, 22) = 1.D0
    GVD(13, 22) = -1.D0
    GVD(12, 23) = 1.D0
    GVD(15, 23) = -1.D0
    GVD(11, 24) = 1.D0
    GVD(14, 24) = -1.D0

    GVD(1, 25) = -1.D0
    GVD(2, 25) = -1.D0
    GVD(3, 25) = -1.D0
    GVD(7, 26) = -1.D0
    GVD(8, 26) = -1.D0
    GVD(9, 26) = -1.D0
    GVD(13, 27) = -1.D0
    GVD(14, 27) = -1.D0
    GVD(15, 27) = -1.D0
    GVD(4, 28) = -1.D0
    GVD(5, 28) = -1.D0
    GVD(6, 28) = -1.D0
    GVD(10, 29) = -1.D0
    GVD(11, 29) = -1.D0
    GVD(12, 29) = -1.D0

    GVD(1, 30) = 1.D0
    GVD(2, 31) = 1.D0
    GVD(3, 32) = 1.D0
    GVD(1, 33) = -1.D0
    GVD(2, 34) = -1.D0
    GVD(3, 35) = -1.D0

    GVD(4, 36) = 1.D0
    GVD(5, 37) = 1.D0
    GVD(6, 38) = 1.D0
    GVD(4, 39) = -1.D0
    GVD(5, 40) = -1.D0
    GVD(6, 41) = -1.D0

    GVD(7, 42) = 1.D0
    GVD(8, 43) = 1.D0
    GVD(9, 44) = 1.D0
    GVD(7, 45) = -1.D0
    GVD(8, 46) = -1.D0
    GVD(9, 47) = -1.D0

    GVD(10, 48) = 1.D0
    GVD(11, 49) = 1.D0
    GVD(12, 50) = 1.D0
    GVD(10, 51) = -1.D0
    GVD(11, 52) = -1.D0
    GVD(12, 53) = -1.D0

    GVD(13, 54) = 1.D0
    GVD(14, 55) = 1.D0
    GVD(15, 56) = 1.D0
    GVD(13, 57) = -1.D0
    GVD(14, 58) = -1.D0
    GVD(15, 59) = -1.D0
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





SUBROUTINE GRAD(N, X, G)
    IMPLICIT NONE

    INTEGER N, K, I
    REAL(kind = 8) X(N), G(N)

    DO I = 1, N
        G(I) = 0.D0
    END DO
    G(1) = 2.3D0 + 0.0001D0 * X(1)
    G(2) = 1.7D0 + 0.0001D0 * X(2)
    G(3) = 2.2D0 + 0.00015D0 * X(3)
    DO K = 1, 4
        G(3 * K + 1) = G(3 * K + 1) + 2.3D0 + 0.0001D0 * X(3 * K + 1)
        G(3 * K + 2) = G(3 * K + 2) + 1.7D0 + 0.0001D0 * X(3 * K + 2)
        G(3 * K + 3) = G(3 * K + 3) + 2.2D0 + 0.00015D0 * X(3 * K + 3)
    END DO

    RETURN
END