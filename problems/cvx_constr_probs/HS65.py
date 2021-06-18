import numpy as np
from typing import Union

from problems.cvx_constr_probs.CvxConstrProb import CvxConstrProb


class HS65(CvxConstrProb):
    """
    represents the problem HS65
     min_x (x1 - x2)^2 + (x1 + x2 -10)^2/9 + (x3 -5)^2

      s. t. -48 + x1^2 + x2^2 + x3^2  <= 0
            -4.5 <= x1 <= 4.5
            -4.5 <= x2 <= 4.5
            -5 <= x3 <= 5
    """

    def __init__(self):
        self.__x0 = np.array((-5, 5, 0), dtype=np.float).reshape(-1, 1)
        self.__x_star = np.array((3.650461821, 3.65046168, 4.6204170507)).reshape(-1, 1)

    def f(self, x):
        return float((x[0] - x[1]) ** 2 + ((x[0] + x[1] - 10) ** 2) / 9 + (x[2] - 5) ** 2
                     )

    def g(self, x):
        return np.array(
            (-48 + x[0] ** 2 + x[1] ** 2 + x[2] ** 2,
             -x[0] - 4.5,
             -x[1] - 4.5,
             -x[2] - 5,
             x[0] - 4.5,
             x[1] - 4.5,
             x[2] - 5)
        ).reshape(-1, 1)

    def nabla_g(self, x):
        return np.array(
            ((2 * x[0], 2 * x[1], 2 * x[2]),
             (-1, 0, 0),
             (0, -1, 0),
             (0, 0, -1),
             (1, 0, 0),
             (0, 1, 0),
             (0, 0, 1)
             ), dtype=np.float)

    def nabla2_g(self, x):
        ng2 = np.zeros((3, 3, 7), dtype=np.float)  # n x n x m
        ng2[:, :, 0] = np.diag((2, 2, 2))
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
        return "HS65"
