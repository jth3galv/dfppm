import numpy as np
from typing import Union

from problems.cvx_constr_probs.CvxConstrProb import CvxConstrProb


class HS43(CvxConstrProb):

    def __init__(self):
        self.__x0 = np.zeros((4, 1), dtype=np.float)
        self.__x_star = np.array((0., 1., 2., -1.)).reshape(-1, 1)

    def f(self, x):
        return float(x[0] ** 2 + x[1] ** 2 + 2 * x[2] ** 2 + x[3] ** 2 - 5 * x[0] - 5 * x[1] - 21 * x[2] + 7 * x[3])

    def g(self, x):
        return np.array(
            (-8 + x[0] ** 2 + x[1] ** 2 + x[2] ** 2 + x[3] ** 2 + x[0] - x[1] + x[2] - x[3],
             -10 + x[0] ** 2 + 2 * x[1] ** 2 + x[2] ** 2 + 2 * x[3] ** 2 - x[0] - x[3],
             -5 + 2 * x[0] ** 2 + x[1] ** 2 + x[2] ** 2 + 2 * x[0] - x[1] - x[3])
        ).reshape(-1, 1)

    def nabla_g(self, x):
        return np.array(
            ((2 * x[0] + 1, 2 * x[1] - 1, 2 * x[2] + 1, 2 * x[3] - 1),
             (2 * x[0] - 1, 4 * x[1], 2 * x[2], 4 * x[3] - 1),
             (4 * x[0] + 2, 2 * x[1] - 1, 2 * x[2], -1)
             ), dtype=np.float
        )

    def nabla2_g(self, x):
        ng2 = np.zeros((4, 4, 3), dtype=np.float)
        ng2[:, :, 0] = np.diag((2, 2, 2, 2))
        ng2[:, :, 1] = np.diag((2, 4, 2, 4))
        ng2[:, :, 2] = np.diag((4, 2, 2, 0))
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
        return "HS43"
