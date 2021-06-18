from typing import Callable, Union
from time import time
import numpy as np

from linesearch.DFLineSearchCoords import DFLineSearchCoords
from linesearch.DFLinesearch import DFLineSearch
from utils.History import History, Counter
from utils.SobolGenerator import SobolGenerator


class ExactPenaltyFnc:

    def __init__(self, f, g, eps0):
        self.__g = g
        self.__f = f
        self.__eps = eps0

    @property
    def eps(self) -> np.ndarray:
        return self.__eps

    @eps.setter
    def eps(self, eps):
        self.__eps = eps

    def __call__(self, x):
        f = self.__f(x)
        tau = (1. / self.__eps).squeeze()
        p = np.sum(tau * np.maximum(self.__g(x).squeeze(), 0))
        return f + p


class DFExactPenalty:
    """
    implementation of the CS-DFN algorithm proposed in

        Fasano, G., Liuzzi, G., Lucidi, S., Rinaldi, F.: A linesearch-based derivative-free ap-
    proach for nonsmooth constrained optimization. SIAM Journal on Optimization 24(3),
    959–992 (2014), doi: https://doi.org/10.1137/130940037

    with an exact penalty on the constraints.
    The epsilon parameter is automatically tuned with the adaptive strategy proposed in the paper.
    """

    def __init__(self, max_it: int = -1, verbosity: int = 0, alpha_stop: float = 1e-6, eta: float = 0,
                 max_nf: int = -1, record_iterates: bool = False, seed: int = 12):

        self.__max_it: int = max_it
        self.__verbosity: int = verbosity
        self.__alpha_stop: float = alpha_stop
        self.__max_nf: int = max_nf
        self.__record_iterates: bool = record_iterates
        self.__eta = eta
        self.__seed = seed

    def __repr__(self):
        return "DFExactPenalty(eps=a)"

    def __str__(self):
        return self.__repr__()

    def solve(self, f: Callable[[np.ndarray], float], g: Callable[[np.ndarray], np.ndarray],
              x0: Union[None, np.ndarray],
              lb: Union[None, np.ndarray] = None,
              ub: Union[None, np.ndarray] = None, ) -> (
            np.ndarray, History):
        """
        :param f: function to be minimized
        :param g: constraints (g(x)<=0)
        :param x0: initial solution
        :param lb: lower bound on x
        :param ub: upper bound on y
        :returns: a solution to the problem and an history object
        """

        f = Counter(f, name='f')  # keep count of the number of function evaluations

        # eps 0
        m = g(x0).shape[0]
        eps0 = np.ones(shape=(m, 1)) * 1e-1
        eps0[g(x0) < 1] = 1e-3

        # penalized function
        f_penalized = ExactPenaltyFnc(f=f, g=g, eps0=eps0)

        # choice of the starting step sizes along the directions
        d = np.ones_like(x0)  # records the sign for each coordinate direction
        alpha_d = np.maximum(1e-3, np.minimum(np.abs(x0), 1))  # records the putative steps
        alpha_max = np.max(alpha_d)
        df_linesearch = DFLineSearchCoords(verbosity=-1, delta=0.5, gamma=1e-6).linesearch_box_all_coords

        # dense initial step
        alpha_put_dense = self.__eta
        rnd = SobolGenerator(self.__seed)
        df_dense_linesearch = DFLineSearch(verbosity=-1, delta=0.5, gamma=1e-6).linesearch_box_single_coord_put

        history = History()

        it = 0
        stop = False

        x = x0.copy().reshape(-1, 1)
        fx = f_penalized(x)

        if ub is None:
            ub = np.ones_like(x) * 1e10
        if lb is None:
            lb = - ub

        while not stop:
            t0 = time()

            # linesearch over 2n coordinate directions
            x, alpha_max, fx, _ = df_linesearch(x, lb, ub, fx, f_penalized, alpha_d, alpha_max, d)

            # dense search
            if alpha_max < self.__eta:
                d = rnd.gen(x.shape[0]).reshape(-1, 1)
                d /= np.linalg.norm(d)
                alpha_taken, alpha_put_dense, x, fx, _ = df_dense_linesearch(fnc=f_penalized, lb=lb, ub=ub, x=x, f=fx,
                                                                             d=d,
                                                                             alpha_put=alpha_put_dense)
                alpha_max = max(alpha_max, alpha_taken)

            p = np.sum((1 / f_penalized.eps) * np.maximum(g(x), 0))

            # update eps
            eps_k = f_penalized.eps
            gx = g(x)
            ind = (eps_k * g(x)) > alpha_max
            eps_k[ind] *= 1e-2
            f_penalized.eps = eps_k

            it += 1
            stop = self.__check_stopping_crit(it, f.count, alpha_max)

            elapsed_time = time() - t0

            if self.__record_iterates:
                history.add_iterate({'x': x.copy()})

            history.update(
                {'f': fx - p, 'p': p, 'g': np.linalg.norm(np.maximum(gx, 0)), 'alpha_max': alpha_max,
                 'time': elapsed_time, '|eps|': np.linalg.norm(f_penalized.eps)},
                f.lap)

            if self.__verbosity > 0:
                history.print_last()

        return x, history

    def __check_stopping_crit(self, it, nf, alpha_max):

        if (0 < self.__max_it <= it) or alpha_max <= self.__alpha_stop or (0 < self.__max_nf <= nf):
            return True
        else:
            return False


# example script
if __name__ == '__main__':
    from problems.minmax_lin_constr_probs.PENTAGON import PENTAGON

    prob = PENTAGON()

    np.set_printoptions(precision=3, suppress=True, linewidth=170)
    print('Problem: {}'.format(prob))
    solver = DFExactPenalty(verbosity=1, alpha_stop=1e-16, max_it=600)
    print('\nSolving ...')
    x_bar, history = solver.solve(f=prob.f, g=prob.g, x0=prob.x0)

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
