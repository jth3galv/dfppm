from algorithms.PPM import FProj
from problems.lin_constr_probs.F90Prob import F90Prob
from utils.ProjOverLin import ProjOverLin
from utils.figure_utils import draw_fnc

import matplotlib.pyplot as plt
from matplotlib.pyplot import savefig

from os.path import join

from configs import OUTPUT_PATH

# script to reproduce Figure 1 of the paper
if __name__ == '__main__':
    prob = F90Prob('hs224')
    print(prob)
    proj_op = ProjOverLin(A=prob.A, b=prob.b)

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(5, 3))

    plt.subplots_adjust(hspace=0.1)

    # plot original function
    draw_fnc(f=prob.f, prob=prob, ax=ax1, fig=fig)

    # plot projection modified function
    f_mod = FProj(eps=1, p=proj_op, f=prob.f)
    draw_fnc(f=f_mod, prob=prob, ax=ax2, fig=fig)

    plt.tight_layout(pad=0.15, w_pad=0.1, h_pad=0)
    savefig(join(OUTPUT_PATH, f'{prob.name}_level_curves.pdf'), transparent=True, bbox_inches='tight')

    plt.show()
