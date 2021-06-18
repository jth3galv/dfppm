import numpy as np
from typing import Union

from problems.cvx_constr_probs.CvxConstrProb import CvxConstrProb


class HS66(CvxConstrProb):
    """
    represents the problem HS66
     min_x 0.2x3 - 0.8x1

      s. t.  -x2 + exp(x1)  <= 0
             -x3 + exp(x2)  <= 0
            0 <= x1 <= 100
            0 <= x2 <= 100
            0 <= x3 <= 10
    """

    def __init__(self):
        self.__x0 = np.array((0, 1.05, 2.9), dtype=np.float).reshape(-1, 1)
        self.__x_star = np.array((0.1841264879, 1.202167873, 3.327322322)
                                 ).reshape(-1, 1)

    def f(self, x):
        return float(0.2 * x[2] - 0.8 * x[0])

    def g(self, x):
        return np.array(
            (-x[1] + np.exp(x[0]),
             -x[2] + np.exp(x[1]),
             -x[0] - 0,
             -x[1] - 0,
             -x[2] - 0,
             x[0] - 100,
             x[1] - 100,
             x[2] - 10)
        ).reshape(-1, 1)

    def nabla_g(self, x):
        return np.array(
            ((np.exp(x[0]), -1, 0),
             (0, np.exp(x[1]), -1),
             (-1, 0, 0),
             (0, -1, 0),
             (0, 0, -1),
             (1, 0, 0),
             (0, 1, 0),
             (0, 0, 1)
             ), dtype=np.float)

    def nabla2_g(self, x):
        ng2 = np.zeros((3, 3, 8), dtype=np.float)  # n x n x m
        ng2[0, 0, 0] = np.exp(x[0])
        ng2[1, 1, 1] = np.exp(x[1])
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
        return "HS66"
