!     SCHIT. 392
!

SUBROUTINE N(i)
    INTEGER, INTENT(OUT) :: i
    i = 30
END SUBROUTINE N

SUBROUTINE M(i)
    INTEGER, INTENT(OUT) :: i
    i = 75
END SUBROUTINE M


SUBROUTINE XSTAR(X)
    IMPLICIT NONE
    REAL(kind = 8), INTENT(OUT) :: X(30)

    X(1) = 99.99d0
    X(2) = 142.22d0
    X(3) = 519.88d0
    X(4) = 136.74d0
    X(5) = 103.47d0
    X(6) = 399.99d0
    X(7) = 191.70d0
    X(8) = 1.56d0
    X(9) = 500d0
    X(10) = 143.43d0
    X(11) = 82.39d0
    X(12) = 629.82d0
    X(13) = 99.92d0
    X(14) = 125.22d0
    X(15) = 600d0
    X(16) = 101.85d0
    X(17) = 142.25d0
    X(18) = 519.88d0
    X(19) = 144.58d0
    X(20) = 105.73d0
    X(21) = 409.59d0
    X(22) = 182.01d0
    X(23) = 29.34d0
    X(24) = 490.52d0
    X(25) = 143.43d0
    X(26) = 52.43d0
    X(27) = 629.70d0
    X(28) = 99.92d0
    X(29) = 125.12d0
    X(30) = 600d0

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
    X(1) = 80.D+0
    X(2) = 100.D+0
    X(3) = 400.D+0
    X(4) = 100.D+0
    X(5) = 200.D+0
    X(6) = 200.D+0
    X(7) = 100.D+0
    X(8) = 250.D+0
    X(9) = 400.D+0
    X(10) = 50.D+0
    X(11) = 200.D+0
    X(12) = 500.D+0
    X(13) = 50.D+0
    X(14) = 200.D+0
    X(15) = 500.D+0
    X(16) = 100.D+0
    X(17) = 120.D+0
    X(18) = 410.D+0
    X(19) = 120.D+0
    X(20) = 250.D+0
    X(21) = 250.D+0
    X(22) = 150.D+0
    X(23) = 300.D+0
    X(24) = 410.D+0
    X(25) = 600.D+0
    X(26) = 250.D+0
    X(27) = 510.D+0
    X(28) = 100.D+0
    X(29) = 250.D+0
    X(30) = 510.D+0

    !    !     NUOVO PUNTO INIZIALE
    !    X(1) = 100.000000000000000
    !    X(2) = 126.308888886808347
    !    X(3) = 520.000000000000000
    !    X(4) = 95.9999999986891481
    !    X(5) = 199.691111115157923
    !    X(6) = 400.000000000000000
    !    X(7) = 34.2624434412935059
    !    X(8) = 217.076923058194154
    !    X(9) = 500.000000000000000
    !    X(10) = 54.6349924668466258
    !    X(11) = 217.076923079595616
    !    X(12) = 630.000000000000000
    !    X(13) = 49.8974359211830887
    !    X(14) = 200.153846118225403
    !    X(15) = 600.000000000000000
    !    X(16) = 112.426666669122156
    !    X(17) = 126.308888886808347
    !    X(18) = 520.204444438033647
    !    X(19) = 83.5733333295669922
    !    X(20) = 199.691111115157952
    !    X(21) = 399.795555561966239
    !    X(22) = 34.2624434412935130
    !    X(23) = 248.606334838059723
    !    X(24) = 500.000000000000000
    !    X(25) = 54.6349924668466116
    !    X(26) = 185.547511299730047
    !    X(27) = 630.000000000000000
    !    X(28) = 49.8974359211830887
    !    X(29) = 200.153846118225374
    !    X(30) = 600.000000000000000

    !    DO I = 1, N
    !        READ(86, *) X(I)
    !    END DO

    RETURN
END

SUBROUTINE FUNCTF(N, X, F)

    IMPLICIT NONE
    INTEGER N
    DOUBLE PRECISION X(N)
    DOUBLE PRECISION, INTENT(OUT) :: F
    INTEGER MODE, I, J, K, L

    DOUBLE PRECISION SUM, SUM1, R1(3, 5), R2(3, 5), KA(3, 5), &
            K1(3, 5), &
            KP(3, 5), K3(3, 5), KL1(3, 5), KL2(3, 5), H(3, 5), B(3, 5), &
            T(3, 3)
    SAVE R1, R2, KA, K1, KP, K3, KL1, KL2, H, B, T
    DATA R1/1.D+3, 5.2D+2, 9.1D+2, 1.D+3, 5.2D+2, 9.1D+2, 1.D+3, &
            5.2D+2, &
            1.D+3, 1.1D+3, 6.D+2, 1.D+3, 1.1D+3, 6.D+2, 1.D+3/
    DATA R2/0.3D+0, 0.1D+0, 0.2D+0, 0.3D+0, 0.1D+0, 0.2D+0, &
            0.3D+0, 0.1D+0, &
            0.2D+0, 0.3D+0, 0.1D+0, 0.2D+0, 0.3D+0, 0.1D+0, 0.2D+0/
    DATA KA/1.2D+2, 6.5D+1, 1.05D+2, 1.5D+2, 6.5D+1, 1.05D+2, &
            1.5D+2, 8.D+1, &
            1.2D+2, 1.7D+2, 8.D+1, 1.2D+2, 1.7D+2, 8.D+1, 1.2D+2/
    DATA K1/1.5D+2, 7.5D+1, 1.4D+2, 1.5D+2, 7.5D+1, 1.4D+2, &
            1.5D+2, 7.5D+1, &
            1.4D+2, 1.7D+2, 9.D+1, 1.5D+2, 1.7D+2, 9.D+1, 1.5D+2/
    DATA KP/1.6D+2, 7.5D+1, 1.4D+2, 1.6D+2, 7.5D+1, 1.4D+2, &
            1.6D+2, 7.5D+1, &
            1.4D+2, 1.8D+2, 9.D+1, 1.5D+2, 1.8D+2, 9.D+1, 1.5D+2/
    DATA K3/.2D-1, .1D-1, .15D-1, .2D+0, .1D+0, .15D+0, .25D+0, &
            .1D+0, .15D+0, &
            .25D+0, 2*.15D+0, .25D+0, 2*.15D+0/
    DATA KL1/3*.5D-2, 3*.5D-1, 9*.6D-1/
    DATA KL2/8.D+1, 4.5D+1, 7.5D+1, 8.D+1, 4.5D+1, 7.5D+1, 1.D+2, &
            4.5D+1, &
            9.D+1, 1.D+2, 5.D+1, 9.D+1, 1.D+2, 5.D+1, 9.D+1/
    DATA H/1.D+2, 2.8D+2, 5.2D+2, 1.8D+2, 2*4.D+2, 2.2D+2, 4.5D+2, &
            5.D+2, &
            1.5D+2, 4.5D+2, 6.3D+2, 1.D+2, 4.D+2, 6.D+2/
    DATA T/.6D+0, .3D+0, .36D+0, .4D+0, .1D+0, .8D-1, .1D+0, .12D+0, &
            .6D-1/
    DATA B/2*1.7D+2, 1.8D+2, 2*1.7D+2, 1.8D+2, 2*1.7D+2, 1.8D+2, &
            2*1.7D+2, &
            1.8D+2, 2*1.7D+2, 1.8D+2/

    F = 0.D+0
    DO I = 1, 5
        SUM = 0.D+0
        DO J = 1, 3
            SUM1 = 0.D+0
            DO K = 1, I
                SUM1 = SUM1 + X(12 + J + 3 * K) - X(J - 3 + 3 * K)
            end do
            SUM = SUM + X(3 * (I - 1) + J) * (R1(J, I) - KA(J, I))&
                    - X(3 * (I - 1) + J)**2 * R2(J, I)&
                    - X(12 + 3 * I + J) * (K1(J, I) + KP(J, I)) - (X(12 + 3 * I + J)&
                    - X(J + 3 * I - 3))**2&
                    * (K3(J, I) + KL1(J, I)) - KL2(J, I) * SUM1
        end do
        F = F - SUM
    end do

    RETURN
END

!
!     VINCOLI DISUGUAGLIANZA GENERALI
!
SUBROUTINE FUNCTVD(N, NQ, X, VD)
    IMPLICIT NONE
    INTEGER N, NQ
    REAL(kind = 8)  X(N), VD(NQ)
    INTEGER MODE, I, J, K, L

    DOUBLE PRECISION SUM, SUM1, R1(3, 5), R2(3, 5), KA(3, 5), &
            K1(3, 5), &
            KP(3, 5), K3(3, 5), KL1(3, 5), KL2(3, 5), H(3, 5), B(3, 5), &
            T(3, 3)
    SAVE H, B, T
    DATA H/1.D+2, 2.8D+2, 5.2D+2, 1.8D+2, 2*4.D+2, 2.2D+2, 4.5D+2, &
            5.D+2, &
            1.5D+2, 4.5D+2, 6.3D+2, 1.D+2, 4.D+2, 6.D+2/
    DATA T/.6D+0, .3D+0, .36D+0, .4D+0, .1D+0, .8D-1, .1D+0, .12D+0, &
            .6D-1/
    DATA B/2*1.7D+2, 1.8D+2, 2*1.7D+2, 1.8D+2, 2*1.7D+2, 1.8D+2, &
            2*1.7D+2, &
            1.8D+2, 2*1.7D+2, 1.8D+2/

    4     DO I = 1, 5
        DO J = 1, 3
            L = 3 * (I - 1) + J
            VD(L) = H(J, I) - X(L)
        end do
    end do
    DO I = 1, 5
        DO J = 1, 3
            L = 3 * (I - 1) + J + 15

            VD(L) = B(J, I)
            DO K = 1, 3
                VD(L) = VD(L) - T(J, K) * X(12 + 3 * I + K)
            end do
        end do
    end do
    DO I = 1, 5
        DO J = 1, 3
            L = 3 * (I - 1) + J + 30

            VD(L) = 0.D+0
            DO K = 1, I
                VD(L) = VD(L) + X(12 + 3 * K + J) - X(J - 3 + 3 * K)
            end do
        end do
    end do

    DO I = 1, 45
        VD(I) = -VD(I)
    ENDDO
    DO I = 1, 30
        VD(45 + I) = -X(I)
    ENDDO
    RETURN
END

!
!     GRADIENTI DEI VINCOLI DISUGUAGLIANZA
!
SUBROUTINE GRADVD(N, NQ, X, GVD)
    IMPLICIT NONE
    INTEGER N, NQ, I, J, L
    REAL(kind = 8)  X(N), GVD(N, NQ)
    INTEGER  K

    DOUBLE PRECISION SUM, SUM1, R1(3, 5), R2(3, 5), KA(3, 5), &
            K1(3, 5), &
            KP(3, 5), K3(3, 5), KL1(3, 5), KL2(3, 5), H(3, 5), B(3, 5), &
            T(3, 3)
    SAVE H, B, T
    DATA H/1.D+2, 2.8D+2, 5.2D+2, 1.8D+2, 2*4.D+2, 2.2D+2, 4.5D+2, &
            5.D+2, &
            1.5D+2, 4.5D+2, 6.3D+2, 1.D+2, 4.D+2, 6.D+2/
    DATA T/.6D+0, .3D+0, .36D+0, .4D+0, .1D+0, .8D-1, .1D+0, .12D+0, &
            .6D-1/
    DATA B/2*1.7D+2, 1.8D+2, 2*1.7D+2, 1.8D+2, 2*1.7D+2, 1.8D+2, &
            2*1.7D+2, &
            1.8D+2, 2*1.7D+2, 1.8D+2/

    DO I = 1, N
        DO J = 1, NQ
            GVD(I, J) = 0.D0
        END DO
    END DO

    DO L = 1, 15
        GVD(L, L) = 1.D0
    ENDDO

    DO I = 1, 5
        DO J = 1, 3
            L = 3 * (I - 1) + J + 15

            DO K = 1, 3
                GVD(12 + 3 * I + K, L) = T(J, K)
            end do
        end do
    end do

    DO I = 1, 5
        DO J = 1, 3
            L = 3 * (I - 1) + J + 30

            DO K = 1, I
                GVD(12 + 3 * K + J, L) = -1.D0
                GVD(J - 3 + 3 * K, L) = 1.D0
            end do
        end do
    end do

    DO I = 1, 30
        GVD(I, 45 + I) = -1.D0
    ENDDO
    RETURN
END   
                          







