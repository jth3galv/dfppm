import numpy as np
from matplotlib import pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.patches as patches
from matplotlib import rc

rc('font', **{'family': 'sans-serif', 'sans-serif': ['Helvetica'], 'size': 6})
rc('text', usetex=True)

NUM_POINTS = 100
N_LEVELS = 200


def plot_feasible_set(ax, A, b, x_0, x_1):
    """plots the feasible set Ax<=b as on overlay grey area"""

    m = A.shape[0]

    ub_0 = np.ones_like(x_0) * np.max(x_0)
    lb_0 = np.ones_like(x_0) * np.min(x_0)
    ub_1 = np.ones_like(x_1) * np.max(x_1)
    lb_1 = np.ones_like(x_1) * np.min(x_0)

    for j in range(m):
        if A[j, 1] != 0:
            y = -(A[j, 0] * x_0 - b[j]) / A[j, 1]
            if A[j, 1] > 0:
                ub_1 = np.minimum(y, ub_1)
            else:
                lb_1 = np.maximum(y, lb_1)
        else:
            y = -(A[j, 1] * x_0 - b[j]) / A[j, 0]
            if A[j, 0] > 0:
                ub_0 = np.minimum(y, ub_0)
            else:
                lb_0 = np.maximum(y, lb_0)

    condition = np.logical_and(ub_1 > lb_1, x_0 <= ub_0)
    condition = np.logical_and(condition, x_0 > lb_0)
    ax.fill_between(x_0, lb_1, ub_1, where=condition, facecolor='gray', interpolate=True, alpha=0.4)


def plot_lin_prob(fig, prob, ax, f, x_0, x_1):
    assert prob.n == 2

    X, Y = np.meshgrid(x_0, x_1)
    Z = np.zeros_like(X)
    m = X.shape[0]
    for k in range(m):
        for l in range(m):
            x_0_i, x_1_i = X[k, l], Y[k, l]
            x_i = np.array((x_0_i, x_1_i)).reshape(-1, 1)
            Z[k, l] = f(x_i)

    cp = ax.contourf(X, Y, Z, levels=N_LEVELS)
    divider = make_axes_locatable(ax)
    cax = divider.append_axes("right", size="5%", pad=0.1)
    fig.colorbar(cp, cax=cax)

    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')

    return cp


def plot_iterates(ax, iterates, x0):
    for it in range(len(iterates[list(iterates.keys())[0]])):
        # V[k, l] = np.max(prob.A.dot(x_i)- prob.b)

        if 'x' in iterates:
            x_it = iterates['x'][it]
            ax.plot([x_it[0]], [x_it[1]], 'rd', markersize=0.5)

        if 'y' in iterates:
            y_it = iterates['y'][it]
            ax.plot([y_it[0]], [y_it[1]], 'r.')

        if 'delta' in iterates:
            y_1_it = iterates['y1'][it]
            delta_it = iterates['delta'][it]
            ax.plot([y_1_it[0]], [y_1_it[1]], 'g+')

            # Create a Rectangle patch
            rect = patches.Rectangle((x_it[0] - delta_it, x_it[1] - delta_it), 2 * delta_it, 2 * delta_it, linewidth=1,
                                     edgecolor='b', facecolor='none')
            ax.add_patch(rect)

    x_0_init, x_1_init = x0
    ax.plot([x_0_init], [x_1_init], 'gd', markersize=0.5)


def draw_fnc(fig, prob, f, ax, iterates=None, x0=None):
    x_0 = np.linspace(-2, 10, num=NUM_POINTS)
    x_1 = np.linspace(-2, 10, num=NUM_POINTS)

    # plot x star
    x_0_star, x_1_star = prob.x_star
    ax.plot([x_0_star], [x_1_star], 'r*', markersize=1)

    plot_lin_prob(fig, prob, ax, f, x_0, x_1)
    plot_feasible_set(ax, prob.A, prob.b, x_0, x_1)
    if iterates is not None and x0 is not None:
        plot_iterates(ax, iterates, x0=x0)

    ax.axis('scaled')
    ax.set_xlim(-2, 10)
    ax.set_ylim(-2, 10)
    plt.tight_layout()
