from typing import Union
import numpy as np

from problems.lin_constr_probs.LinConstrProb import LinConstrProb


class AS6(LinConstrProb):
    """
    represents the problem
     min_x sum_{i=1}^{n} (xi − 1)**2
      s. t.
    x1 − 2x2 − 2x3 −···− 2xn−1 − 2xn <= 0
    −2x1 + x2 − 2x3 −···− 2xn−1 − 2xn <= 0
    .
    .
    .
    −2x1 − 2x2 − 2x3 −···− 2xn−1 + xn <= 0
    xi ≥ 0, i = 1, 2, . . . , n.
    """

    def __init__(self, n: int):
        self.__n = n
        self.__x0 = np.zeros((self.n, 1))
        self.__xstar = np.ones_like(self.__x0)

        self.__m = 2 * n

        self.__b = np.zeros((self.__m, 1))
        self.__A = np.zeros((self.__m, self.__n))

        for i in range(n):
            self.__A[i] = np.ones_like(self.n) * 2
            self.__A[i, i] = -1
        self.__A *= -1

        # box constraints
        for i in range(n):
            self.__A[i + n, i] = -1

    @property
    def name(self) -> str:
        return "AS6(n={})".format(self.__n)

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
        return float(np.sum((x.squeeze() - 1) ** 2))

    @property
    def A(self):
        return self.__A

    @property
    def b(self):
        return self.__b

    @property
    def x_star(self):
        return self.__xstar

    @property
    def fstar(self) -> Union[None, float]:
        return self.f(self.x_star)


if __name__ == '__main__':
    prob = AS6(6)
    print(prob)

    x_star = prob.x_star

    print('f*:', prob.f(x_star.squeeze()))
    print('x_star: ', x_star.squeeze())

    x0 = prob.x0
    print('f0:', prob.f(x0.squeeze()))
    print('x0: ', x0.squeeze())
