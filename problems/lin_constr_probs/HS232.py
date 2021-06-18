from typing import Union
import numpy as np

from problems.lin_constr_probs.LinConstrProb import LinConstrProb


class HS232(LinConstrProb):
    """
    represents the problem HS232
     min_x - (9 - (x1 - 3)^2)(x2^3 * sqrt(3)/27)
      s. t. -  sqrt(3)x1  + x2  <= 0
            - x1 - sqrt(3)x2    <=0
            - 6 +x1 + sqrt(3)x2 <=0
            0 <= x1
            0 <= x2
    """

    def __init__(self):
        self.__x0 = np.array((2, 0.5)).reshape(-1, 1)
        self.__xstar = np.array((3, 1.732)).reshape(-1, 1)

        self.__m = 5
        self.__n = 2

        self.__b = np.array((0, 0, 6, 0, 0), dtype=np.float).reshape(-1, 1)
        self.__A = np.zeros((self.__m, self.__n))

        self.__A[0] = np.array((-1. / np.sqrt(3), 1))
        self.__A[1] = np.array((-1, -np.sqrt(3)))
        self.__A[2] = np.array((1, np.sqrt(3)))

        # box constraints LB (only)
        for i in range(self.__n):
            self.__A[i + 3, i] = -1

        self.__ub = np.ones_like(self.__x0) * 1e10
        self.__lb = - self.__ub

    @property
    def name(self) -> str:
        return "HS232"

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
        return float(-(9 - (x[0] - 3) ** 2) * (x[1] ** 3 / np.sqrt(3) / 27))

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
    prob = HS232()
    print(prob)

    x_star = prob.x_star

    print('\nf*:', prob.f(x_star.squeeze()))
    print('x_star: ', x_star.squeeze())

    x0 = prob.x0
    print('\nf0:', prob.f(x0.squeeze()))
    print('x0: ', x0.squeeze())
