import numpy as np
from typing import Union

from problems.cvx_constr_probs.CvxConstrProb import CvxConstrProb


class HS270(CvxConstrProb):
    """
    represents the problem HS270
     min_x - x0x1x2x3x4 -3x1x2x4 -4x1x2x3 +12x1x2 -x2x3x4 +3x2x4
                +3x2x3 -12x2 -2x1x3x4 +6x1x4 +8x1x3 -24x1 +2x3x4 -6x4
                -8x3 +24 +1.5x5^4 -5.75x3^3 +5.25x5^2

      s. t. - 34 + x1^2 +x2^2 +x3^3 +x4^2 +x5^2  <= 0
            1 <= x1
            2 <= x2
            3 <= x2
            4 <= x3
    """

    def __init__(self):
        self.__x0 = np.array((1.1, 2.1, 3.1, 4.1, -1), dtype=np.float).reshape(-1, 1)
        self.__x_star = np.array((1., 2., 3., 4., 2.)).reshape(-1, 1)

    def f(self, x):
        return float(
            x[0] * x[1] * x[2] * x[3] - 3 * x[0] * x[1] * x[3] - 4 * x[0] * x[1] * x[2] + 12 * x[0] * x[1]
            - x[1] * x[2] * x[3] + 3 * x[1] * x[3] + 4 * x[1] * x[2] - 12 * x[1] - 2 * x[0] * x[2] * x[3]
            + 6 * x[0] * x[3] + 8 * x[0] * x[2] - 24 * x[0] + 2 * x[2] * x[3] - 6 * x[3] - 8 * x[2] + 24
            + 1.5 * x[4] ** 4 - 5.75 * x[4] ** 3 + 5.25 * x[4] ** 2
        )

    def g(self, x):
        return np.array(
            (-34 + x[0] ** 2 + x[1] ** 2 + x[2] ** 2 + x[3] ** 2 + x[4] ** 2,
             -x[0] + 1,
             -x[1] + 2,
             -x[2] + 3,
             -x[3] + 4)
        ).reshape(-1, 1)

    def nabla_g(self, x):
        return np.array(
            ((2 * x[0], 2 * x[1], 2 * x[2], 2 * x[3], 2 * x[4]),
             (-1, 0, 0, 0, 0),
             (0, -1, 0, 0, 0),
             (0, 0, -1, 0, 0),
             (0, 0, 0, -1, 0)
             ), dtype=np.float)

    def nabla2_g(self, x):
        ng2 = np.zeros((5, 5, 5), dtype=np.float)  # n x n x m
        ng2[:, :, 0] = np.diag((2, 2, 2, 2, 2))
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
        return "HS270"
