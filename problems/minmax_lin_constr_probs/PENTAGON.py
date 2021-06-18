import numpy as np

from problems.lin_constr_probs.LinConstrProb import LinConstrProb


class PENTAGON(LinConstrProb):

    def __init__(self):
        self.__x0 = np.ones((6, 1), dtype=np.float)

        self.__n = 6
        self.__m = 15
        self.__b = np.ones((self.__m, 1))
        self.__A = np.zeros((self.__m, self.__n))

        count = 0
        for i in [1, 3, 5]:
            for j in range(5):
                self.__A[count][i - 1] = np.cos(2. * np.pi * float(j) / 5.)
                self.__A[count][i] = np.sin(2. * np.pi * float(j) / 5.)
                count += 1

    @property
    def n(self):
        return self.__n

    @property
    def m(self):
        return self.__m

    def f(self, x):
        return np.max(
            (-np.sqrt((x[0] - x[2]) ** 2 + (x[1] - x[3]) ** 2),
             -np.sqrt((x[2] - x[4]) ** 2 + (x[3] - x[5]) ** 2),
             -np.sqrt((x[4] - x[0]) ** 2 + (x[5] - x[1]) ** 2)))

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
        return 'PENTAGON'

    @property
    def fstar(self):
        return -1.85961870
