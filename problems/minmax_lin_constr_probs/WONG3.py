import numpy as np

from problems.lin_constr_probs.LinConstrProb import LinConstrProb


class WONG3(LinConstrProb):

    def __init__(self):
        self.__x0 = np.array((2, 3, 5, 5, 1, 2, 7, 3, 6, 10, 2, 2, 6, 15, 1, 2, 1, 2, 1, 3), dtype=np.float).reshape(-1,
                                                                                                                     1)

        self.__n = 20
        self.__m = 4
        self.__b = np.array((105, 0, 12, 0), dtype=np.float).reshape(-1, 1)

        self.__A = np.array(((4, 5, 0, 0, 0, 0, -3, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                             (10, -8, 0, 0, 0, 0, -17, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                             (-8, 2, 0, 0, 0, 0, 0, 0, 5, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                             (1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 4, -21, 0, 0, 0, 0, 0, 0, 0, 0)),
                            dtype=np.float)

        self.__ub = np.ones_like(self.__x0) * 1e10
        self.__lb = - self.__ub

    @property
    def n(self):
        return self.__n

    @property
    def m(self):
        return self.__m

    def f(self, x):
        f1 = x[0] ** 2 + x[1] ** 2 + x[0] * x[1] - 14 * x[0] \
             - 16 * x[1] + (x[2] - 10) ** 2 + 4 * (x[3] - 5) ** 2 \
             + (x[4] - 3) ** 2 \
             + 2 * (x[5] - 1) ** 2 + 5 * x[6] ** 2 + 7 * (x[7] - 11) ** 2 \
             + 2 * (x[8] - 10) ** 2 + (x[9] - 7) ** 2 \
             + (x[10] - 9) ** 2 + 10 * (x[11] - 1) ** 2 + 5 * (x[12] - 7) ** 2 + 4 * (x[13] - 14) ** 2 \
             + 27 * (x[14] - 1) ** 2 \
             + x[15] ** 4 + (x[16] - 2) ** 2 + 13 * (x[17] - 2) ** 2 + (x[18] - 3) ** 2 + x[19] ** 2 + 95
        return np.max((f1,
                       f1 + 10 * (3 * (x[0] - 2) ** 2 + 4 * (x[1] - 3) ** 2 + 2 * x[2] ** 2 - 7 * x[3] - 120),
                       f1 + 10 * (5 * x[0] ** 2 + 8 * x[1] + (x[2] - 6) ** 2 - 2 * x[3] - 40),
                       f1 + 10 * (0.5 * (x[0] - 8) ** 2 + 2 * (x[1] - 4) ** 2 + 3 * x[4] ** 2 - x[5] - 30),
                       f1 + 10 * (x[0] ** 2 + 2 * (x[1] - 2) ** 2 - 2 * x[0] * x[1] + 14 * x[4] - 6 * x[5]),
                       f1 + 10 * (-3 * x[0] + 6 * x[1] + 12 * (x[8] - 8) ** 2 - 7 * x[9]),
                       f1 + 10 * (x[0] ** 2 + 5 * x[10] - 8 * x[11] - 28),
                       f1 + 10 * (4 * x[0] + 9 * x[1] + 5 * x[12] ** 2 - 9 * x[13] - 87),
                       f1 + 10 * (3 * x[0] + 4 * x[1] + 3 * (x[12] - 6) ** 2 - 14 * x[13] - 10),
                       f1 + 10 * (14 * x[0] ** 2 + 35 * x[14] - 79 * x[15] - 92),
                       f1 + 10 * (15 * x[1] ** 2 + 11 * x[14] - 61 * x[15] - 54),
                       f1 * 10 * (5 * x[0] ** 2 + 2 * x[1] + 9 * x[16] ** 4 - x[17] - 68),
                       f1 + 10 * (x[0] ** 2 - x[8] + 19 * x[18] - 20 * x[19] + 19),
                       f1 + 10 * (7 * x[0] ** 2 + 5 * x[1] ** 2 + x[18] ** 2 - 30 * x[19])
                       ))

    @property
    def A(self):
        return self.__A

    @property
    def b(self):
        return self.__b

    @property
    def x0(self):
        return self.__x0

    @property
    def x_star(self):
        return None

    @property
    def name(self) -> str:
        return 'WONG3'

    @property
    def fstar(self):
        return 133.72828
