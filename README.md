### Running the SAT solver

Our SAT-solver requires Python (3) to run.
We used a Jupyter Notebook for structure and overview.
A Jupyter Notebook is not runnable from the command line and with arguments by default, so it requires the following package in order to do so:

**papermill**
```sh
$ pip install papermill
```

The following required Python packages are probably installed already.
**Python packages**
- sklearn
- numpy
- json
- random
- copy
- collections
- argparse
- pandas

After installing **papermill** and in possession of the required packages, the command runs the SAT solver with required arguments:
```sh
$ python SAT.py 4 sudoku10.txt
```
which tries to find a solution to *sudoku10.txt* using heuristic 4, which is logistic regression. The options are explained when typing:
```sh
$ python SAT.py -h

usage: SAT [-h] heuristic infile

positional arguments:
  heuristic   [1] DP basic, [2] MOMs, [3] JeroSloWang, [4] Logistic regression
  infile      DIMACS file including all constraints to satisfy

optional arguments:
  -h, --help  show this help message and exit
```
The true assignments are saved to the file *solution.txt* in DIMACS format.