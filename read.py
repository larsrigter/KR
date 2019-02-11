
from collections import defaultdict
def sudokus_to_dimac_rules(filename='1000 sudokus.txt', sudokusize=9):
    sudokus = []
    with open(filename) as sudoku_input:
        for s in sudoku_input:
            sudoku = defaultdict(int)
            row = 1
            for i, content in enumerate(s):
                if i%sudokusize == 0 and i > 0:
                    row+=1
                try:
                    int(content)
                    rule = str(row) + str(i%sudokusize+1) + content
                    sudoku[rule] = True
                except:
                    # is not convertible to int
                    continue
            sudokus.append(sudoku)
    return sudokus

sudokus = sudokus_to_dimac_rules('1000 sudokus.txt')   

                



