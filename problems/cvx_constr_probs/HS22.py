from typing import Union
import numpy as np

from problems.cvx_constr_probs.CvxConstrProb import CvxConstrProb


class HS22(CvxConstrProb):

    def __init__(self):
        self.__x0 = np.array((2., 2.)).reshape(-1, 1)
        self.__x_star = np.array((1., 1.)).reshape(-1, 1)

    def f(self, x) -> float:
        return float((x[0] - 2) ** 2 + (x[1] - 1) ** 2)

    def g(self, x):
        return np.array((x[0] + x[1] - 2, x[0] ** 2 - x[1]), dtype=np.float).reshape(-1, 1)

    def nabla_g(self, x):
        return np.array(((1, 1), (2 * x[0], -1)), dtype=np.float)

    def nabla2_g(self, x):
        g1 = np.zeros((len(x), len(x)))
        g2 = np.array(((2, 0), (0, 0)), dtype=np.float)
        g1 = np.expand_dims(g1, 2)
        g2 = np.expand_dims(g2, 2)
        return np.concatenate((g1, g2), axis=2)

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
        return "HS22"
