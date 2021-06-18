from typing import Callable

import numpy as np


class DFLineSearchCoords:
    """
    Linesearch procedure along the 2n coordinate directions
    """

    def __init__(self, verbosity: int, gamma: float = 1.e-6, delta_exp: float = 0.5, delta: float = 0.5):

        self.__verbosity: int = verbosity
        self.__gamma = gamma
        self.__delta_exp = delta_exp
        self.__delta = delta

    def __expansion_step(self, x: np.ndarray, z: np.ndarray, d: np.ndarray, fz: float, fx: float,
                         fnc: Callable[[np.ndarray], float], alpha: float,
                         lb: np.ndarray, ub: np.ndarray, j: int,
                         ifront: int):
        """
        :param x: current iterate
        :param z: trial point
        :param d: vectors of signs
        :param fz: (z)
        :param fx: f(x)
        :param fnc: function callable
        :param alpha: step
        :param lb: lower bound
        :param ub: upper bound
        :param j: coordinate direction axis
        :param ifront: whether on the boundary or not
        :returns: the tuple (the expanded step alpha, f(z), the number of function evaluations,
                  whether z = x + alpha d_j is on the frontier or not)
        """

        nf = 0
        stop = False

        while not stop:

            if ifront == 1:
                if self.__verbosity >= 1:
                    print(' accetta punto sulla frontiera fz =%f   alfa =%f' % (fz, alpha))
                stop = True

            if d[j - 1] > 0.0:

                if (alpha / self.__delta_exp - (ub[j - 1] - x[j - 1])) < -1.e-6:
                    alpha_exp = alpha / self.__delta_exp
                else:
                    alpha_exp = ub[j - 1] - x[j - 1]
                    ifront = 1
                    if self.__verbosity >= 1:
                        print(' punto espan. sulla front.')

            else:

                if (alpha / self.__delta_exp - (x[j - 1] - lb[j - 1])) < -1.e-6:
                    alpha_exp = alpha / self.__delta_exp
                else:
                    alpha_exp = x[j - 1] - lb[j - 1]
                    ifront = 1
                    if self.__verbosity >= 1:
                        print(' punto espan. sulla front.')

            z[j - 1] = x[j - 1] + alpha_exp * d[j - 1]

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

        return alpha, fz, nf, ifront

    def __project_step_box(self, d, alpha_d, x, lb, ub, j):
        """:returns alpha_d if x + alpha_d * d_j inside the box or the maxixum step that can be taken,
        ifront = 0 or 1 if x + alpha_d * d_j is on the boundary"""

        ifront = 0

        if d[j - 1] > 0:

            if (alpha_d[j - 1] - (ub[j - 1] - x[j - 1])) < -1.e-6:
                alfa = max(1.e-24, alpha_d[j - 1])
            else:
                alfa = ub[j - 1] - x[j - 1]
                ifront = 1
                if self.__verbosity >= 1:
                    print(' point on the boundary. *')
        else:

            if (alpha_d[j - 1] - (x[j - 1] - lb[j - 1])) < -1.e-6:
                alfa = max(1.e-24, alpha_d[j - 1])
            else:
                alfa = x[j - 1] - lb[j - 1]
                ifront = 1
                if self.__verbosity >= 1:
                    print(' point on the boundary. *')
        return float(alfa), ifront

    def __explore_dir(self, d, alfa_d, x, z, fnc, f, lb, ub, j, alfa_max):
        """compute a descend step alpha at least large as alfa_d[j] along d[j].
        Return 0 if x + alfa_d * d_j is not a descent step"""

        # XXX z get modified

        nf = 0

        alfa, ifront = self.__project_step_box(d, alfa_d, x, lb, ub, j)

        if False and abs(alfa) <= 1.e-3 * min(1.0, alfa_max):  # XXX

            if self.__verbosity >= 1:
                print(' direzione opposta per alfa piccolo')
                print(' j =%d    d(j) =%f' % (j, d[j - 1]))
                print(' alfa=%e    alfamax=%e' % (alfa, alfa_max))
            alfa = 0
            fz = f

        else:
            z[j - 1] = x[j - 1] + alfa * d[j - 1]

            fz = fnc(z)
            nf += 1

            if self.__verbosity >= 1:
                print(' fz =%f   alfa =%e' % (fz, alfa))
            if self.__verbosity >= 2:
                for i in range(0, len(x)):
                    print(' z(%d)=%f' % (i, z[i]))

            if fz < f - self.__gamma * alfa ** 2:

                # expansion step
                alfa, fz, nf_exp, ifront = self.__expansion_step(x, z, d, fz, f, fnc, alfa, lb, ub, j, ifront)
                nf += nf_exp
            else:
                alfa = 0

        return alfa, fz, nf, ifront

    def linesearch_box_single_coord(self, fnc: Callable[[np.ndarray], float], lb: np.ndarray, ub: np.ndarray, x, f, d,
                                    alfa_d,
                                    j, alfa_max):
        """
        :param fnc: function to be minimized
        :param lb: lower bound of box contraints
        :param ub: upper bound of box constraints
        :param x:  starting point
        :param f: function values at x
        :param d: vector of +1 or -1 that indicates whether the postive coord dir j or its opposite should be considered
        :param alfa_d: vector of putative steps
        :param j: number of coordinate direction
        :param alfa_max: maximum of alfa_d
        :return:
        """

        # XXX this procedure modifies d

        nf = 0
        z = np.copy(x)
        coord_dir_j_fall = 0  # XXX can it be removed?

        if self.__verbosity >= 1:
            print('j =%d    d(j) =%f alfa=%e' % (j, d[j - 1], alfa_d[j - 1]))

        alfa, fz, nf_ex, ifront = self.__explore_dir(d, alfa_d, x, z, fnc, f, lb, ub, j, alfa_max)
        nf += nf_ex

        if alfa <= 0:  # failure along d
            d[j - 1] = -d[j - 1]
            coord_dir_j_fall += 1

            if self.__verbosity >= 1:
                print(' failure along the direction + d_{}'.format(j))

            alfa, fz, nf_ex, ifront = self.__explore_dir(d, alfa_d, x, z, fnc, f, lb, ub, j, alfa_max)
            nf += nf_ex

            if self.__verbosity >= 1 and alfa <= 0:
                print(' failure along the direction - d_{}'.format(j))

        return alfa, fz, nf, coord_dir_j_fall, ifront

    def linesearch_box_all_coords(self, x, lb, ub, f, fnc, alfa_d, alfa_max, d):

        # XXX modifies alpha_d and d

        num_f_eval = 0
        n = len(x)

        for coord_dir_j in range(1, n + 1):

            # skip a coordinate dir if alpha_d is too little w.r.t. alpha_max
            if False and abs(alfa_d[coord_dir_j - 1]) <= 1.e-3 * min(1.0, alfa_max):  # XXX
                if self.__verbosity >= 1:
                    print('  alfa piccolo')
                    print(' alfa_d(j)=%e    alfamax=%e' % (alfa_d[coord_dir_j - 1], alfa_max))
            else:

                alfa, fz, num_f_eval_ls, coord_dir_j_fall, ifront = self.linesearch_box_single_coord(fnc, lb, ub,
                                                                                                     x, f,
                                                                                                     d,
                                                                                                     alfa_d,
                                                                                                     coord_dir_j,
                                                                                                     alfa_max)

                if alfa <= 0 or ifront == 1:  # failure along d or on the boundary
                    alfa_d[coord_dir_j - 1] = self.__delta * alfa_d[coord_dir_j - 1]
                else:
                    alfa_d[coord_dir_j - 1] = alfa * self.__delta

                num_f_eval += num_f_eval_ls

                if abs(alfa) >= 1.e-12:
                    x[coord_dir_j - 1] = x[coord_dir_j - 1] + alfa * d[
                        coord_dir_j - 1]  # make a step along coord_dir_j
                    f = fz

                alfa_max = np.max(alfa_d)

        return x, alfa_max, f, num_f_eval
