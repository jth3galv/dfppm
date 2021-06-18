import os

from algorithms.DFExactPenalty import DFExactPenalty
from algorithms.PPM import PPM, DynamicEps, ConstantEps

from problems.cvx_constr_probs.HS22 import HS22
from problems.cvx_constr_probs.HS270 import HS270
from problems.cvx_constr_probs.HS29 import HS29
from problems.cvx_constr_probs.HS43 import HS43
from problems.cvx_constr_probs.HS65 import HS65
from problems.cvx_constr_probs.HS66 import HS66
from problems.lin_constr_probs.AS6 import AS6
from problems.lin_constr_probs.AS7 import AS7
from problems.lin_constr_probs.F90Prob import F90Prob
from problems.lin_constr_probs.HS250 import HS250
from problems.lin_constr_probs.HS36 import HS36
from problems.lin_constr_probs.HS37 import HS37
from problems.minmax_lin_constr_probs.MAD1 import MAD1
from problems.minmax_lin_constr_probs.MAD2 import MAD2
from problems.minmax_lin_constr_probs.MAD4 import MAD4
from problems.minmax_lin_constr_probs.MAD5 import MAD5
from problems.minmax_lin_constr_probs.PENTAGON import PENTAGON
from problems.minmax_lin_constr_probs.WONG2 import WONG2

from utils.ProjOverConvex import ProjOverConvex
from utils.ProjOverLin import ProjOverLin
from utils.data_profiles import data_profiles_fig
from utils.latex_utils import format_table

from tqdm import tqdm
import numpy as np

from matplotlib import rc
from matplotlib import cm
import matplotlib.pyplot as plt

from os.path import join

# font
rc('font', **{'family': 'sans-serif', 'sans-serif': ['Helvetica'], 'size': 11})
rc('text', usetex=True)

# lines styles
lw = 0.4
ls = 'dashed'
ms = 0.7

probs_ordering = ['HS36', 'HS37', 'HS44', 'HS86', 'HS224', 'HS231', 'HS232', 'HS250', 'HS331', 'AS6(n=6)', 'AS6(n=7)',
                  'AS6(n=8)', 'AS7(n=6)', 'AS7(n=7)', 'AS7(n=8)', 'HS22', 'HS29', 'HS43', 'HS65', 'HS66', 'HS270',
                  'MAD1', 'MAD2', 'MAD4', 'MAD5', 'PENTAGON', 'WONG2']


def solve_1(solver, prob, proj_op):
    return solver.solve(prob.f, proj_op, prob.x0)


def solve_2(solver, prob, proj_op):
    return solver.solve(prob.f, prob.g, prob.x0)


def get_solvers(alpha_stop, max_it, max_nf, eta):
    solvers = []
    solvers += [
        (DFExactPenalty(verbosity=0, alpha_stop=alpha_stop, max_it=max_it, record_iterates=True, max_nf=max_nf,
                        eta=eta),
         solve_2)]

    eps0_list = [0.1, 1, 10, 100]
    solvers += [(PPM(verbosity=0, alpha_stop=alpha_stop, record_iterates=True, max_it=max_it,
                     eps_fnc=ConstantEps(eps0=eps0), max_nf=max_nf), solve_1) for eps0 in eps0_list]

    solvers += [(PPM(verbosity=0, alpha_stop=alpha_stop, record_iterates=True, max_it=max_it, eta=eta,
                     eps_fnc=DynamicEps(eps0=10, sigma=2), max_nf=max_nf), solve_1)]

    return solvers


def get_problems():
    lin_fortran_probs = [F90Prob(name) for name in
                         ['hs24', 'hs44', 'hs86', 'hs224', 'hs231', 'hs232', 'hs331']]
    lin_deg_probs = [AS6(n) for n in (6, 7, 8)] + [AS7(n) for n in (6, 7, 8)]
    lin_py_probs = [HS36(), HS37(), HS250()]

    minmax_probs = [MAD1(), MAD2(), MAD4(), MAD5(), WONG2(), PENTAGON()]

    lin_probs = lin_fortran_probs + lin_py_probs + lin_deg_probs + minmax_probs

    cvx_probs = [HS22(), HS43(), HS29(), HS65(), HS66(), HS270()]

    problems = [(prob, ProjOverLin(prob.A, prob.b)) for prob in lin_probs]
    problems += [(prob, ProjOverConvex(A=None, b=None, g=prob.g, nabla_g=prob.nabla_g, nabla2_g=prob.nabla2_g)) for prob
                 in
                 cvx_probs]
    return problems


def execute_benchmark(alpha_stop, max_it, max_nf, eta):
    # list of problems for the benchmark
    problems = get_problems()

    # solvers to be compared
    solvers = get_solvers(alpha_stop=alpha_stop, max_it=max_it, max_nf=max_nf, eta=eta)

    results = {'alpha_stop': alpha_stop, 'max_nf': max_nf, 'max_it': max_it, 'solvers': dict()}

    for i, s in enumerate(solvers):
        solver = s[0]
        solve = s[1]
        solver_name = str(solver)
        print('Running benchmark for solver: {}\n'.format(solver_name))
        d = {}
        pbar = tqdm(problems)
        for (prob, proj_op) in pbar:
            pbar.set_postfix_str(f'problem: {prob.name}')
            x_bar, history = solve(solver, prob, proj_op)
            x_bar = proj_op(x_bar)

            f_star = prob.f(prob.x_star) if prob.x_star is not None else prob.fstar
            f_0 = prob.f(prob.x0)
            gp_0 = np.linalg.norm(np.maximum(prob.g(prob.x0), 0))

            iterates = history.iterates
            fx = np.array([prob.f(x) for x in iterates['x']])
            fnc_delta = (fx - f_star)  # / (np.abs(f_star) + 1)
            violation = np.array([np.linalg.norm(np.maximum(prob.g(x), 0)) for x in iterates['x']])
            num_fnc = [num_f for num_f in history.dataframe['num_f'].cumsum()]

            nf = history.dataframe['num_f'].cumsum().iloc[-1]
            d[prob.name.upper()] = {'x_star': prob.x_star, 'f_star': f_star,
                                    'x_bar': x_bar, 'f_xbar': prob.f(x_bar),
                                    'f_x': fx,
                                    'x_0': prob.x0, 'f_x0': f_0,
                                    'gp_0': gp_0,
                                    'gp_x': violation,
                                    'hist': history.dataframe, 'nf': nf, 'fnc_delta': fnc_delta,
                                    'cum_num_fnc': num_fnc,
                                    'violation': violation}

        results['solvers'][solver_name] = d
    return results


def get_marker_style(name):
    if 'PPM' in name and 'sigma' in name:
        return 'o'
    elif 'PPM' in name:
        return 'd'
    elif 'Penalty' in name:
        return '^'
    else:
        raise AttributeError('Unknown name: {} -> no marker available!'.format(name))


def get_legend(name, simple: bool = False):
    s = name.replace('DFExactPenalty(eps=a)', 'exact penalty')
    if simple and 'PPM' in name:
        s = 'PPM'
    else:
        s = s.replace('PPM', '').replace("(", "").replace(")", "")
    return s


def get_solver_to_show(experiment_name):
    if experiment_name == 'first_cmp':
        solvers_to_keep = ['DFExactPenalty(eps=a)', 'PPM(eps=1e+00)']
    elif experiment_name == 'eps_cmp':
        solvers_to_keep = ['PPM(eps=1e-01)', 'PPM(eps=1e+00)', 'PPM(eps=1e+01)', 'PPM(eps=1e+02)',
                           'PPM(eps0=1e+01, sigma=2)']
    elif experiment_name == 'final_cmp':
        solvers_to_keep = ['DFExactPenalty(eps=a)', 'PPM(eps0=1e+01, sigma=2)']
    else:
        raise ValueError(f'No plot configuration for plot: {experiment_name}')
    return solvers_to_keep


# table export params
field_names = ['$\\Delta f^*$', '$\\|g_+\\|$']
field_formats = [':.2e', ':.2e']
field_fncs = [lambda p: float(p['fnc_delta'][-1]), lambda p: float(p['violation'][-1])]


def export_results(data, outputpath, taus, betas, experiment_name, simple_legend=False):
    os.makedirs(outputpath, exist_ok=True)

    # setup colors
    if len(data.keys()) > 1:
        colors = cm.jet(np.linspace(0, 1, len(list(data.keys()))))[::-1]
    else:
        colors = ['r', 'b']

    # export latex table with final values
    table_str = format_table(data, field_names=field_names,
                             problems=probs_ordering,
                             field_formats=field_formats,
                             field_fncs=field_fncs)

    with open(join(outputpath, 'benchmark_table.tex'), 'w') as f:
        f.writelines([table_str])

    # make data profiles
    solvers = list(data.keys())
    legend_fnc = lambda name: get_legend(name, simple_legend)
    legends = [legend_fnc(s) for s in solvers]
    for tau in taus:
        for beta in betas:
            fig = data_profiles_fig(data, tau=tau, beta=beta, colors=colors, legends=legends, max_alpha=10 ** 4,
                                    scale='log',
                                    markers=[get_marker_style(str(s)) for s in solvers], ms=ms, ls=ls, lw=lw,
                                    figsize=(5, 3.5))

            fig.savefig(
                join(outputpath, f'data_profiles_{experiment_name}_t{tau:.0e}' + f'{beta:.2f}'.split('.')[1] + '.pdf')
                , transparent=True)
            plt.close(fig)
