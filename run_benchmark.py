import pickle

from os.path import join
import os
from utils.benchmark_utils import execute_benchmark, get_solver_to_show, export_results

from warnings import warn

from configs import OUTPUT_PATH

"""script to run all the experiments to reproduce Figure 2, 4, 5 of the paper"""

# OPTIMIZATION PARAMS
MAX_NF = 10**4
"""maximum number of function evaluation"""
MAX_IT = -1
"""maximum number of iterations (-1 means unlimited)"""
ALPHA_STOP = 1e-16
"""minimum values of alpha (the step in the linesearch procedure)"""
ETA = 1e-6
"""parameter eta for the linesearch procedure"""

# DATA PROFILES PARAMS
taus = [10 ** (-k) for k in [1, 3, 5, 7]]
betas = [0.9, 0.99]

if __name__ == '__main__':

    results_file = join(OUTPUT_PATH, 'benchmark_results.pkl')

    if os.path.exists(results_file):
        warn(f'Looks like experiments have already been run. '
             f'If you want to re-execute them. Please delete {results_file}')

        with open(results_file, 'rb') as f:
            results = pickle.load(f)

    else:
        results = execute_benchmark(alpha_stop=ALPHA_STOP, max_it=MAX_IT, max_nf=MAX_NF, eta=ETA)

        with open(results_file, 'wb') as f:
            pickle.dump(results, f)

    for experiment_name in ['first_cmp', 'eps_cmp', 'final_cmp']:
        print(f'Exporting figure and table for experiment {experiment_name} in {OUTPUT_PATH}...')
        solvers_to_keep = get_solver_to_show(experiment_name)
        print(f'\t solvers: {solvers_to_keep}')

        solvers_data = {}
        for s in solvers_to_keep:
            solvers_data[s] = results['solvers'][s]

        export_results(solvers_data, outputpath=join(OUTPUT_PATH, experiment_name), taus=taus, betas=betas,
                       simple_legend=False if experiment_name == 'eps_cmp' else True, experiment_name=experiment_name)
