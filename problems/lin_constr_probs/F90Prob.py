import importlib
from typing import Union
import numpy as np
import sys

from problems.lin_constr_probs.LinConstrProb import LinConstrProb


class F90Prob(LinConstrProb):
    """represents the problem
        min   f(x)
         s.t. Ax <= b
              lb <= x <= ub """

    def __init__(self, name: str):
        self.__name = name

        p = '/'.join(__file__.split('/')[:-1])
        sys.path.append(p)
        self.__flib = importlib.import_module('{}'.format(name.lower()))

        # print(self.__flib.__doc__)
        self.__n = self.__flib.n()
        self.__x0 = np.zeros((self.n,), order='F')
        self.__flib.xiniz(self.__x0)

        use_bounds = False

        if use_bounds:
            # self.__flib.bounds(self.__lb, self.__ub)

            # know term (b) of the linear constraint Ax <= b
            # obtained as - g(0) where g = Ax-b is the fortran 'functvd' function
            # in the fortran code there are m+n entries (bound contraints are included at the end of the vector)
            b_tmp = np.zeros((2 * self.n + self.m))
            self.__flib.functvd(np.zeros_like(self.__x0), b_tmp)
            self.__b = - b_tmp[:self.m]

            # A matrix of the linear constraints
            A_tmp = np.zeros((self.n, self.n + self.m), order='F')
            self.__flib.gradvd(self.x0, A_tmp)
            self.__A = np.ascontiguousarray(A_tmp[:, :self.m]).T

            self.__ub = - b_tmp[self.m:self.m + self.n]
            self.__lb = b_tmp[self.m + self.n:self.m + 2 * self.n]

            self.__m = self.__flib.m()
        else:

            self.__m = self.__flib.m()

            b_tmp = np.zeros((self.__m,))
            self.__flib.functvd(np.zeros_like(self.__x0), b_tmp)
            self.__b = - b_tmp

            # A matrix of the linear constraints
            A_tmp = np.zeros((self.n, self.__m), order='F')
            self.__flib.gradvd(self.x0, A_tmp)
            self.__A = np.ascontiguousarray(A_tmp).T

            self.__ub = np.ones_like(self.__x0) * 1e10
            self.__lb = - self.__ub

            self.__lb = self.__lb.reshape(-1, 1)
            self.__ub = self.__ub.reshape(-1, 1)
            self.__x0 = self.__x0.reshape(-1, 1)
            self.__b = self.__b.reshape(-1, 1)

    @property
    def n(self):
        return self.__n

    @property
    def m(self):
        return self.__m

    @property
    def x0(self):
        return self.__x0

    def f(self, x):
        return self.__flib.functf(x)

    @property
    def A(self):
        return self.__A

    @property
    def b(self):
        return self.__b

    @property
    def x_star(self):
        return self.__flib.xstar().reshape(-1, 1)

    @property
    def fstar(self) -> Union[None, float]:
        return self.f(self.x_star)

    @property
    def name(self) -> str:
        return self.__name


if __name__ == '__main__':
    prob = F90Prob('hs224')
    print(prob)

    x_star = prob.x_star

    # x_star = np.array((0.6175, 0.1039))

    print('f*:', prob.f(x_star.squeeze()))
    print('x_star: ', x_star.squeeze())

    x0 = prob.x0
    print('f0:', prob.f(x0.squeeze()))
    print('x0: ', x0.squeeze())
