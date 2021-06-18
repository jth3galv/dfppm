import subprocess
from tqdm import tqdm
import os
from glob import glob

# wraps fortran problems into python functions using f2py (https://numpy.org/doc/stable/f2py/)
# requires a fortran compiler
# on debian-like system it can be installed with
# $sudo apt install gfortan

if __name__ == '__main__':

    exe = 'python'
    prob_dir = './problems'
    files = glob(f'./{prob_dir}/**/*.f90', recursive=True)
    files = [os.path.abspath(f) for f in files]

    for f in tqdm(files):
        name = os.path.basename(f).split('.')[0]
        path = os.path.dirname(f)
        os.chdir(path)

        args = ['-m', 'numpy.f2py', '-c', f, '-m', name, '--f90flags=-w', '--f77flags=-w', '--quiet']
        p = subprocess.Popen([exe] + args, bufsize=0)
        p.wait()
