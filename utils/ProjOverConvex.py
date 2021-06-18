from _warnings import warn

import numpy as np
from cvxopt import matrix, solvers
from cvxopt.solvers import cp
from typing import Callable, Union
import cvxopt

cvxopt.solvers.options['show_progress'] = False
solvers.options['abstol'] = 1e-17
solvers.options['reltol'] = 1e-17


# solvers.options['maxiters'] = 200


class ProjOverConvex:

    def __init__(self, A: Union[np.ndarray, None], b: Union[np.ndarray, None], g: Callable[[np.ndarray], np.ndarray],
                 nabla_g: Callable[[np.ndarray], np.ndarray], nabla2_g: Callable[[np.ndarray], np.ndarray]):
        """
        Projection over the convex set defined by linear constraints
        g(x) <= 0
        Ax <= b
        :param A: constraint matrix of shape n by m
        :param b: known terms of shape m by 1
        :param g: function that evaluates non linear constraints at given x. The output shape is m by 1
        :param nabla_g: function that evaluate the Jacobian of g at given x. The output shape is m by n
        :param nabla2_g: function that evaluate the 'Hessian' of g at given x. The output shape is m by n by n

        """
        # n = A.shape[1]
        # assert len(b) == A.shape[0]

        self.__A = matrix(A) if A is not None else None
        self.__b = matrix(b) if b is not None else None

        self.__g = g
        self.__nabla_g = nabla_g
        self.__nabla2_g = nabla2_g

        self.__m, self.__n = None, None

    def __F(self, y, x=None, z=None):

        if self.__m is None:
            t = self.__nabla_g(y)
            self.__n = t.shape[1]
            self.__m = t.shape[0]

        if x is None:
            x0 = matrix(y.reshape((-1, 1)))
            return self.__m, x0
        else:

            x = np.array(x)
            f = np.array((0.5 * np.linalg.norm(y - x) ** 2,))
            f = np.vstack((f, self.__g(x).reshape(-1, 1)))
            Df = np.array(x - y).reshape(-1, 1).T
            # ng = self.__nabla_g(x)
            Df = np.vstack((Df, self.__nabla_g(x)))
            f, Df, = matrix(f), matrix(Df)
            if z is None:
                return f, Df
            else:
                H = np.eye(self.__n).reshape(self.__n, self.__n, 1)
                # ng2 = self.__nabla2_g(x)
                H = np.concatenate((H, self.__nabla2_g(x)), axis=2)
                H = H.dot(z).squeeze()
                return f, Df, matrix(H)

    def __call__(self, x: np.ndarray):
        sol = cp(F=lambda x_=None, z_=None: self.__F(x, x_, z_), A=self.__A, b=self.__b)
        status = sol['status']
        if status != 'optimal':
            warn('Non optimal solution returned by the solver for proj op')
        x_bar = np.array(sol['x']).reshape(-1, 1)  # retrieve the solution
        return x_bar


if __name__ == '__main__':
    def g(x):
        return np.array((x[0] + x[1] - 2, x[0] ** 2 - x[1]), dtype=np.float).reshape(2, 1)


    def nabla_g(x):
        return np.array(((1, 1), (2 * x[0], -1)), dtype=np.float)


    def nabla2_g(x):
        g1 = np.zeros((len(x), len(x)))
        g2 = np.array(((2, 0), (0, 0)), dtype=np.float)
        g1 = np.expand_dims(g1, 2)
        g2 = np.expand_dims(g2, 2)
        return np.concatenate((g1, g2), axis=2)


    proj_op = ProjOverConvex(g=g, nabla_g=nabla_g, nabla2_g=nabla2_g, A=None, b=None)

    x = np.array((2, 1), dtype=np.float).reshape(-1, 1)
    x_bar = proj_op(x)

    print('x_bar: ', x_bar)
    print('g(x): ', g(x_bar))
