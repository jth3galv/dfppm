from _warnings import warn

import numpy as np
import cvxopt
from cvxopt import matrix, solvers
from cvxopt.coneprog import qp

cvxopt.solvers.options['show_progress'] = False

solvers.options['abstol'] = 1e-17
solvers.options['reltol'] = 1e-17


class ProjOverLin:

    def __init__(self, A: np.ndarray, b: np.ndarray):
        """
        Projection over the convex set defined by linear constraints Ax <= b
        :param A: constraint matrix of shape n by m
        :param b: known terms of shape m by 1
        """
        n = A.shape[1]
        assert len(b) == A.shape[0]

        # data for the qp solver
        self.__P = matrix(np.eye(n))
        self.__A = matrix(A)
        self.__b = matrix(b)

        # self.__fx = 0

    def __call__(self, x: np.ndarray):
        """
        :param x: a point in R^n
        :return: the projection of 'x' over Ax <= b
        """
        sol = qp(P=self.__P, q=matrix(-x.squeeze()), G=self.__A, h=self.__b, maxiters=500)

        status = sol['status']
        if status != 'optimal':
            warn('Non optimal solution returned by the solver')
        x_bar = np.array(sol['x']).reshape(-1, 1)  # retrieve the solution
        return x_bar
