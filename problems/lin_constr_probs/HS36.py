from typing import Union
import numpy as np

from problems.lin_constr_probs.LinConstrProb import LinConstrProb


class HS36(LinConstrProb):
    """
    represents the problem HS36
     min_x - x1x2x3
      s. t. - 72 + x1 + 2x2 + 2x3 <= 0
            0 <= x1 <= 20
            0 <= x2 <= 11
            0 <= x2 <= 42
    """

    def __init__(self):
        self.__x0 = np.array((10, 10., 10.)).reshape(-1, 1)
        self.__xstar = np.array((20, 11., 15.)).reshape(-1, 1)

        self.__m = 7
        self.__n = 3

        self.__b = np.array((72, 20, 11, 42, 0, 0, 0), dtype=np.float).reshape(-1, 1)
        self.__A = np.zeros((self.__m, self.__n))

        self.__A[0, :] = np.array((1, 2, 2))

        # box constraints
        for i in range(self.__n):
            self.__A[i + 1, i] = 1
        for i in range(self.__n):
            self.__A[i + 1 + + self.__n, i] = -1

        self.__ub = np.ones_like(self.__x0) * 1e10
        self.__lb = - self.__ub

    @property
    def name(self) -> str:
        return "HS36"

    @property
    def n(self):
        return self.__n

    @property
    def m(self):
        return self.__m

    @property
    def x0(self):
        return self.__x0

    def f(self, x) -> float:
        return float(-x[0] * x[1] * x[2])

    @property
    def A(self):
        return self.__A

    @property
    def b(self):
        return self.__b

    @property
    def lb(self):
        return self.__lb

    @property
    def ub(self):
        return self.__ub

    @property
    def x_star(self) -> np.ndarray:
        return self.__xstar

    @property
    def fstar(self) -> Union[None, float]:
        return self.f(self.x_star)


if __name__ == '__main__':
    prob = HS36()
    print(prob)

    x_star = prob.x_star

    print('\nf*:', prob.f(x_star.squeeze()))
    print('x_star: ', x_star.squeeze())

    x0 = prob.x0
    print('\nf0:', prob.f(x0.squeeze()))
    print('x0: ', x0.squeeze())
