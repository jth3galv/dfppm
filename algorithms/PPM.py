from abc import ABC, abstractmethod
from typing import Callable, Union
from time import time
import numpy as np

from linesearch.DFLineSearchCoords import DFLineSearchCoords
from linesearch.DFLinesearch import DFLineSearch
from utils.History import History, Counter
from utils.SobolGenerator import SobolGenerator


class FProj:
    """Implementation of the modified function f~(x) = f(pi(x)) + eps |x-pi(x)|"""

    def __init__(self, f: Callable[[np.ndarray], float], p: Callable[[np.ndarray], np.ndarray], eps: float = 0.1):
        """
        :params f: a function (R^n->R)
        :param p: a projection operation (R^n->R^n)
        :eps: epsilon parameter
        """
        self.__p = p
        self.__f = f
        self.__eps = eps

    def __call__(self, y) -> float:
        x = self.__p(y)
        return self.__f(x) + self.__eps * np.linalg.norm(y - x)

    @property
    def eps(self) -> float:
        return self.__eps

    @eps.setter
    def eps(self, eps):
        self.__eps = eps


class EpsUpdater(ABC):

    @property
    @abstractmethod
    def eps0(self) -> float:
        """
        :returns: the initial value of eps
        """

    @abstractmethod
    def __call__(self, alpha_max: float) -> float:
        """
        :param alpha_max:
        :returns: the update values for epsilon
        """

    @abstractmethod
    def __repr__(self):
        """"""


class ConstantEps(EpsUpdater):

    def __init__(self, eps0: float = 1):
        self.__eps0 = eps0

    @property
    def eps0(self) -> float:
        return self.__eps0

    def __call__(self, alpha_max: float) -> float:
        return self.__eps0

    def __repr__(self):
        return "eps={:.0e}".format(self.__eps0)


class DynamicEps(EpsUpdater):

    @property
    def eps0(self) -> float:
        return self.__eps0

    def __call__(self, alpha_max: float) -> float:
        return self.__sigma_alpha * alpha_max

    def __repr__(self):
        return "eps0={:.0e}, sigma={}".format(self.__eps0, self.__sigma_alpha)

    def __init__(self, eps0: float = 1, sigma: float = 2):
        self.__sigma_alpha = sigma
        self.__eps0 = eps0


class PPM:
    """Implementation of the projection-based penalty method.
        Solver for problems of the form
        min   f(x)
         s.t. x in X,
        where f has no derivatives, and X is a convex set
        whose projection operation is available.
        """

    def __init__(self, eps_fnc: EpsUpdater = DynamicEps(eps0=1, sigma=2), max_it: int = -1, verbosity: int = 0,
                 alpha_stop: float = 1e-6, eta: float = 0,
                 max_nf: int = -1, record_iterates: bool = False, seed: int = 12):

        self.__eps_fnc: EpsUpdater = eps_fnc  # penalty parameter update function
        self.__max_it: int = max_it
        self.__verbosity: int = verbosity
        self.__alpha_stop: float = alpha_stop
        self.__max_nf: int = max_nf
        self.__record_iterates: bool = record_iterates
        self.__eta = eta
        self.__seed = seed

    def __repr__(self):
        return "PPM({})".format(self.__eps_fnc)

    def __str__(self):
        return self.__repr__()

    def solve(self, f: Callable[[np.ndarray], float], proj_op: Callable[[np.ndarray], np.ndarray],
              x0: Union[None, np.ndarray]) -> (
            np.ndarray, History):
        """
        :param f: function to be minimized (R^n->R)
        :param proj_op:  a function which performs the projection over the feasible set (R^n->R^n)
        :param x0: initial solution
        :returns: a solution to the problem and an 'History' object
        """

        f = Counter(f, name='f')  # keep count of the number of function evaluations

        x = proj_op(x0)

        # choice of the starting step sizes along the directions
        d = np.ones_like(x)  # records the sign for each coordinate direction
        alpha_d = np.maximum(1e-3, np.minimum(np.abs(x), 1))  # records the putative steps
        alpha_max = np.max(alpha_d)
        df_linesearch = DFLineSearchCoords(verbosity=-1, delta=0.5, gamma=1e-6).linesearch_box_all_coords

        # dense initial step
        alpha_put_dense = self.__eta
        rnd = SobolGenerator(self.__seed)
        df_dense_linesearch = DFLineSearch(verbosity=-1, delta=0.5, gamma=1e-6).linesearch_box_single_coord_put

        ub = np.ones_like(x) * 1e10
        lb = - ub

        f_proj = FProj(f=f, p=proj_op, eps=self.__eps_fnc.eps0)

        history = History()

        it = 0
        fx = f_proj(x)
        stop = False

        while not stop:
            t0 = time()

            # linesearch over 2n coordinate directions
            x, alpha_max, fx, _ = df_linesearch(x, lb, ub, fx, f_proj, alpha_d, alpha_max, d)

            # dense search
            if alpha_max < self.__eta:
                d = rnd.gen(x.shape[0]).reshape(-1, 1)
                d /= np.linalg.norm(d)
                alpha_taken, alpha_put_dense, x, fx, _ = df_dense_linesearch(fnc=f_proj, lb=lb, ub=ub, x=x, f=fx, d=d,
                                                                             alpha_put=alpha_put_dense)
                alpha_max = max(alpha_max, alpha_taken)

            it += 1
            stop = self.__check_stopping_crit(it, f.count, alpha_max)

            elapsed_time = time() - t0

            f_proj.eps = self.__eps_fnc(alpha_max)

            if self.__record_iterates:
                history.add_iterate({'x': x.copy()})

            history.update(
                {'f': fx, 'alpha_max': alpha_max, 'time': elapsed_time, 'eps': f_proj.eps},
                f.lap)

            if self.__verbosity > 0:
                history.print_last()

        # final projection
        t0 = time()
        x = proj_op(x)
        fx = f_proj(x)
        history.update(
            {'f': fx, 'alpha_max': alpha_max, 'time': time() - t0, 'eps': f_proj.eps},
            f.lap)
        if self.__record_iterates:
            history.add_iterate({'x': x.copy()})

        return x, history

    def __check_stopping_crit(self, it, nf, alpha_max):

        if (0 < self.__max_it <= it) or alpha_max <= self.__alpha_stop or (0 < self.__max_nf <= nf):
            return True
        else:
            return False


# example script
if __name__ == '__main__':

    from problems.lin_constr_probs.F90Prob import F90Prob
    from utils.ProjOverLin import ProjOverLin

    prob = F90Prob('hs224')

    proj_op = ProjOverLin(prob.A, prob.b)

    np.set_printoptions(precision=3, suppress=True, linewidth=170)
    print('Problem: {}'.format(prob))
    solver = PPM(verbosity=1, eps_fnc=DynamicEps(eps0=1, sigma=2), alpha_stop=1e-6, max_it=600)
    print('\nSolving with {} ...'.format(solver))
    x_bar, history = solver.solve(prob.f, proj_op, prob.x0)

    print("num_f: ", history.dataframe['num_f'].cumsum().iloc[-1])
    print('\nx: ', x_bar.squeeze())
    print('g(x):', prob.g(x_bar).squeeze())
    print('f(x):', prob.f(x_bar))

    if prob.x_star is not None:
        print('\nx*:', prob.x_star.squeeze())
        print('g(x*):', prob.g(prob.x_star).squeeze())
        print('f(x*):', prob.f(prob.x_star))
    else:
        print('f*  :', prob.fstar)
