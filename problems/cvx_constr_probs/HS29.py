import numpy as np
from typing import Union

from problems.cvx_constr_probs.CvxConstrProb import CvxConstrProb


class HS29(CvxConstrProb):

    def __init__(self):
        self.__x0 = np.array((1., 1., 1.)).reshape(-1, 1)
        self.__x_star = np.array((4., 2. * np.sqrt(2), 2)).reshape(-1, 1)

    def f(self, x) -> float:
        return float(- x[0] * x[1] * x[2])

    def g(self, x):
        return np.array(x[0] ** 2 + 2 * x[1] ** 2 + 4 * x[2] ** 2 - 48, dtype=np.float).reshape(-1, 1)

    def nabla_g(self, x):
        return np.array((2 * x[0], 4 * x[1], 8 * x[2]), dtype=np.float).reshape(1, -1)

    def nabla2_g(self, x):
        ng2 = np.zeros((3, 3, 1), dtype=np.float)
        ng2[:, :, 0] = np.diag((2., 4., 8.))
        return ng2

    @property
    def x0(self):
        return self.__x0

    @property
    def x_star(self):
        return self.__x_star

    @property
    def fstar(self) -> Union[None, float]:
        return self.f(self.x_star)

    @property
    def name(self):
        return "HS29"
