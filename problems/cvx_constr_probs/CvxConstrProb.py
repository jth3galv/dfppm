from abc import ABC, abstractmethod
from typing import Union


class CvxConstrProb(ABC):
    """
    encodes the problem
    min f(x)
     s.t.
     g(x) <= 0,
     Ax = b
    """

    @abstractmethod
    def f(self, x):
        """:returns the value of f at x"""

    @abstractmethod
    def g(self, x):
        """
        :returns: the evaluation of the constraints g at x as a m by 1 array
        """

    @abstractmethod
    def nabla_g(self, x):
        """
        :returns: the Jacobian of g at x as a m by n array
        """

    @abstractmethod
    def nabla2_g(self, x):
        """:returns the hessian of the constraints g_i as a n by n by m array"""

    @property
    @abstractmethod
    def x0(self):
        """:returns the initial point"""

    @property
    @abstractmethod
    def x_star(self):
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
        return self.name
