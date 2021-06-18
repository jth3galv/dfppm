from typing import Callable

import numpy as np


class DFLineSearch:
    """
    Linesearch procedure along a given general direction.
    """

    def __init__(self, verbosity: int, gamma: float = 1.e-6, delta_exp: float = 0.5, delta: float = 0.5):

        self.__verbosity: int = verbosity
        self.__gamma = gamma
        self.__delta_exp = delta_exp
        self.__delta = delta

    def __expansion_step(self, x: np.ndarray, z: np.ndarray, d: np.ndarray, fz: float, fx: float,
                         fnc: Callable[[np.ndarray], float], alpha: float, alpha_max: float):
        """
        :param x: current iterate
        :param z: trial point
        :param d: direction
        :param fz: (z)
        :param fx: f(x)
        :param fnc: function callable
        :param alpha: step (must be != alpha_max)
        :param alpha_max: maximum step
        :param lb: lower bound
        :param ub: upper bound
        :returns: the tuple (the expanded step alpha, f(z), the number of function evaluations,
                  whether z = x + alpha d_j is on the frontier or not)
        """
        assert alpha < alpha_max

        nf = 0
        stop = False
        alpha_exp = alpha

        while not stop:

            alpha_exp /= self.__delta_exp
            if alpha_exp > alpha_max:
                alpha_exp = alpha_max
                stop = True
                if self.__verbosity >= 1:
                    print(' punto espan. sulla front.')

            z = x + alpha_exp * d

            fzdelta = fnc(z)
            nf += 1

            if self.__verbosity >= 1:
                print(' fzex=%f  alfaex=%f' % (fzdelta, alpha_exp))

            if fzdelta < fx - self.__gamma * alpha_exp ** 2:
                fz = fzdelta
                alpha = alpha_exp
            else:
                stop = True

                if self.__verbosity >= 1:
                    print(' accetta punto fz =%f   alfa =%f' % (fz, alpha))

        return alpha, z, fz, nf

    def __project_step_box(self, d, x, lb, ub) -> float:
        """:returns the maximum step that can be taken along d"""

        alpha = np.zeros_like(d)
        gt_index = d > 0
        l_index = d < 0
        alpha[gt_index] = ub[gt_index] - x[gt_index]
        alpha[l_index] = x[l_index] - lb[l_index]

        return np.max(alpha.squeeze())

    def __explore_dir(self, d, alpha_put, x, fnc, f, lb, ub):
        """computes a descend step alpha at least large as alpha_put along d.
        Return 0 if x + alpha_put * d is not a descent step"""

        nf = 0

        alfa_max_box = self.__project_step_box(d, x, lb, ub)
        alpha = min((alfa_max_box, alpha_put))
        fz = f

        z = x.copy()

        if alpha > 0:
            z = x + alpha * d

            fz = fnc(z)
            nf += 1

            if self.__verbosity >= 1:
                print(' fz: {:.2e}   alpha ={:.2e}'.format(fz, alpha))
            if self.__verbosity >= 2:
                print(' z: {}'.format(z))

            if fz < f - self.__gamma * alpha ** 2:

                # expansion step
                if alpha < alfa_max_box:
                    alpha, z, fz, nf_exp = self.__expansion_step(x, z, d, fz, f, fnc, alpha, alfa_max_box)
                    nf += nf_exp
            else:
                alpha = 0
                z = x

        return alpha, z, fz, nf

    def linesearch_box_single_coord(self, fnc: Callable[[np.ndarray], float], lb: np.ndarray, ub: np.ndarray, x, f, d,
                                    alpha_put):
        """
        :param fnc: function to be minimized
        :param lb: lower bound of box contraints
        :param ub: upper bound of box constraints
        :param x:  starting point
        :param f: function values at x
        :param d: direction to be explored. d is supposed to have unitary norm.
        :param alpha_put: putative step length
        :return:
        """

        nf = 0

        if self.__verbosity >= 1:
            print('d={}, alpha_put: {:.2e}'.format(d.squeeze(), alpha_put))

        alpha, z, fz, nf_ex = self.__explore_dir(d, alpha_put, x, fnc, f, lb, ub)
        nf += nf_ex

        if self.__verbosity >= 1 and alpha <= 0:
            print(' failure along the direction.')
        return alpha, z, fz, nf

    def linesearch_box_single_coord_put(self, fnc: Callable[[np.ndarray], float], lb: np.ndarray, ub: np.ndarray, x, f,
                                        d,
                                        alpha_put):

        alpha, z, fz, nf = self.linesearch_box_single_coord(fnc, lb, ub, x, f, d, alpha_put)

        if alpha <= 0:
            alpha_put = self.__delta * alpha
        else:
            alpha_put = alpha

        return alpha, alpha_put, z, fz, nf
