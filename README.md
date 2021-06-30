# Projection-based Penalty Method (PPM)

Code containing the implementation of the **PPM** method described in:
 
Galvan, G., Sciandrone, M. & Lucidi, S. A parameter-free unconstrained reformulation for nonsmooth problems with convex constraints. *Comput Optim Appl* (2021), [link](https://link.springer.com/content/pdf/10.1007/s10589-021-00296-1.pdf).

## Installation
1. you will need a python interpreter (>=3.7) and the pip package manager (or conda). Please see [the official page](https://www.python.org/downloads/) or other distributions like [anaconda](https://www.anaconda.com/products/individual) if you need instructions.

1. install a **fortran** compiler and compile (optional)

	  you will be able to use the code of the algorithm without fortran as the code is pure python. However to run the benchmark (and reproduce the results reported on the paper) you will need to install a fortran compiler as some of the problem are written in fortran.

	 for a debian-based systems install fortran as:
 
		$ sudo apt install gfortran
	
	then compile the problems:
	
		$ cd dfppm
		$ python setup_f90probs.py
	
2. install python dependencies

		$ pip install numpy pandas matplotlib sobol_seq tqdm
	
	and optionally install the **cvxopt** library. 
	
	Some of the solver contained in the library are used to compute the projection for some of the benchmark problems. Again, you can run the algorithms without the library (provided that you define your own projection operator) but you will not be able to run the benchmark otherwise. Install the library with
	
	
		$ pip install cvxopt
		
## Using the algorithm

The algorithms (both PPM and the exact-penalty) can be used in a stand-alone manner. They are pure python and require minimum dependecies. Please see the *DFExactPenalty* and *PPM* classes in the *algorithm* module.


		
	
## Running the benchmark

		$ python run_benchmark.py
		
This will run the algorithms on all the test problems and extract the data profiles and tables as reported in the paper.

## Reproducing the figures in the paper

- reproduce Figure 1 with

		$ python plot_paper_level_curves.py

- reproduce Figure 3 with

		$ python plot_paper_paths_eps.py
