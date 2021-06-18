from abc import ABC, abstractmethod
import numpy as np
from typing import Union


class LinConstrProb(ABC):
    """encodes the problem
           min   f(x)
            s.t. Ax <= b
    """

    def g(self, x) -> np.ndarray:
        """:returns Ax-b"""
        return self.A.dot(x) - self.b

    @property
    @abstractmethod
    def n(self) -> int:
        """:returns the number of variables"""

    @property
    @abstractmethod
    def m(self) -> int:
        """:returns the number of constraints"""

    @abstractmethod
    def f(self, x) -> float:
        """:returns the value of f at x"""

    @property
    @abstractmethod
    def A(self):
        """:returns the A matrix"""

    @property
    @abstractmethod
    def b(self):
        """:returns the known terms b"""

    @property
    @abstractmethod
    def x0(self):
        """:returns the initial point"""

    @property
    @abstractmethod
    def x_star(self) -> Union[None, np.ndarray]:
        """:returns the optimal solution (if available)"""

    @property
    @abstractmethod
    def fstar(self) -> Union[None, float]:
        """:return the optimimum function values (if available)"""

    @property
    @abstractmethod
    def name(self) -> str:
        """:returns the name of the problem"""

    def __repr__(self):
        r = "== Problem: {} ==".format(self.name)
        r += "\n\tn: {}, m: {}".format(self.n, self.m)
        r += "\n\tx0 = {}".format(self.x0.squeeze())
        for i in range(self.m):
            r += '\n\t'
            for j in range(self.n):
                if self.A[i, j] != 0:
                    r += "{:+} x_{} ".format(self.A[i, j], j)
            r += "<= {}".format(float(self.b[i]))
        # for i in range(self.n):
        #     r += '\n\t{} <= x_{} <= {}'.format(self.__lb[i], i, self.__ub[i])

        return r
