import papermill as pm
import argparse

parser = argparse.ArgumentParser(prog='SAT')
parser.add_argument("heuristic", type=int,
                    help="[1] DP basic, [2] MOMs, [3] JeroSloWang, [4] Logistic regression",
                    default=1)
parser.add_argument("infile", type=str,
                    help="DIMACS file including all constraints to satisfy",
                    default="constraint_problem.txt")

args = parser.parse_args()

argMapping = {1:"random", 2:"moms", 3:"jerow", 4:"logreg"}

print("Finding satisfying interpretation of", args.infile, "using", argMapping[args.heuristic], "splits...")
pm.execute_notebook(
   'SAT_book.ipynb',
   'output.ipynb',
   parameters = dict(heuristic=argMapping[args.heuristic], infile=args.infile)
)

nb = pm.read_notebook('output.ipynb')
df = nb.dataframe
solution = next(iter((df[df['name']=='solution']['value'].values)))

if solution:
    print("Saved solution to file: solution.txt")   
else:
    print("No solution found")

with open("solution.txt", "w") as outfile:
    for s in solution:
        outfile.write('' + str(s) + ' 0\n')