from algorithms.PPM import PPM, FProj, ConstantEps
from utils.ProjOverLin import ProjOverLin
from utils.figure_utils import draw_fnc

import matplotlib.pyplot as plt
from matplotlib.pyplot import savefig

from os.path import join

from configs import OUTPUT_PATH

# script to reproduce Figure 3 of the paper
if __name__ == '__main__':
    from problems.lin_constr_probs.F90Prob import F90Prob

    prob = F90Prob('hs224')
    print(prob)
    proj_op = ProjOverLin(A=prob.A, b=prob.b)

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(5, 3))

    plt.subplots_adjust(hspace=0.1)

    # PLOT 1
    eps_1 = 0.1
    f_1 = FProj(eps=eps_1, p=proj_op, f=prob.f)
    solver = PPM(verbosity=1, eps_fnc=ConstantEps(eps0=eps_1), alpha_stop=1e-6, max_it=600, record_iterates=True)
    print('\nSolving ...')
    x0 = prob.x0
    _, history = solver.solve(prob.f, proj_op, x0)
    iterates = history.iterates
    draw_fnc(f=f_1, prob=prob, ax=ax1, iterates=iterates, x0=x0, fig=fig)

    # PLOT 2
    eps_2 = 10
    f_2 = FProj(eps=eps_2, p=proj_op, f=prob.f)
    solver = PPM(verbosity=1, eps_fnc=ConstantEps(eps0=eps_2), alpha_stop=1e-6, max_it=600, record_iterates=True)
    print('\nSolving ...')
    x0 = prob.x0
    _, history = solver.solve(prob.f, proj_op, x0)
    iterates = history.iterates
    draw_fnc(f=f_2, prob=prob, ax=ax2, iterates=iterates, x0=x0, fig=fig)

    plt.tight_layout(pad=0.15, w_pad=0.1, h_pad=0)
    savefig(join(OUTPUT_PATH, f'{prob.name}_eps_paths.pdf'), transparent=True, bbox_inches='tight')

    plt.show()
