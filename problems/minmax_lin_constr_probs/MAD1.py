import numpy as np

from problems.lin_constr_probs.LinConstrProb import LinConstrProb


class MAD1(LinConstrProb):

    def __init__(self):
        self.__x0 = np.array((1, 2), dtype=np.float).reshape(-1, 1)

        self.__n = 2
        self.__m = 1
        self.__b = np.array((-0.5,), dtype=np.float).reshape(-1, 1)
        self.__A = np.zeros((self.__m, self.__n))

        self.__A[0] = np.array((-1, -1), dtype=np.float)

    @property
    def n(self):
        return self.__n

    @property
    def m(self):
        return self.__m

    def f(self, x):
        return np.max((x[0] ** 2 + x[1] ** 2 + x[0] * x[1] - 1, np.sin(x[0]), -np.cos(x[1])))

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
        return 'MAD1'

    @property
    def fstar(self):
        return -0.38965952
