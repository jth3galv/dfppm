import numpy as np
from matplotlib import pyplot as plt


def conv_test(f_0, f_star, gp_0, f_x, gp_x, tau, beta):
    return (1 - beta) * (f_x - f_star) + beta * gp_x <= tau * (1 - beta) * (f_0 - f_star) + tau * beta * gp_0


def data_profiles(data, tau, beta):
    solvers = list(data.keys())
    problems = list(data[solvers[0]].keys())

    max_alpha = max([max([rr['nf'] / rr['x_bar'].shape[0] for _, rr in r.items()]) for _, r in data.items()])

    x = np.arange(0, max_alpha, 10)
    ys = {}

    for j, solver in enumerate(solvers):

        y = np.zeros_like(x, dtype=np.float)

        for problem in problems:

            data_s = data[solver][problem]
            nf = np.array(data_s['cum_num_fnc'])
            f_x = np.array(data_s['f_x'])
            f_0 = float(data_s['f_x0'])
            f_star = float(data_s['f_star'])
            gp_x = np.array(data_s['gp_x'])
            gp_0 = float(data_s['gp_0'])

            n = len(data_s['x_0'].squeeze())

            success = [None] * len(nf)
            for i in range(len(nf)):
                success[i] = conv_test(f_0, f_star, gp_0, f_x[i], gp_x[i], tau, beta)
            success = np.array(success)
            for i in range(len(x)):
                ind = (nf / (n + 1)) < x[i]
                y[i] += float(np.any(success[ind]))

        y /= len(problems)
        ys[solver] = y
    return x, ys


def data_profiles_fig(data, tau, beta, **properties):
    solvers = list(data.keys())

    figsize = (5, 4) if 'figsize' not in properties else properties['figsize']
    fig, ax = plt.subplots(1, 1, figsize=figsize)

    x, ys = data_profiles(data, tau, beta)
    for j, solver in enumerate(solvers):
        ax.plot(x, ys[solver], color=properties['colors'][j], linewidth=properties['lw'], linestyle=properties['ls'],
                marker=properties['markers'][j], markersize=properties['ms'])

    legends = properties['legends'] if 'legends' in properties else solvers
    ax.legend(legends, loc='lower right')
    ax.set_xlabel('$\\alpha$')
    ax.set_ylabel('$d_s(\\alpha)$')
    if 'scale' in properties and properties['scale'] == 'log':
        ax.set_xscale('log')

    ax.set_yticks(np.linspace(0, 1, 11))

    ax.grid(b=True, which='major', color='#666666', linestyle='dashed')
    plt.tight_layout()
    return fig
