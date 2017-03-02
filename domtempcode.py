import random
import sys

row=9
col=12
mine=10

def createBoard(row,col,mine):
	board=[]
	for r in range (row):
		board.append([])
		for c in range (col):
			board[r].append("C ")
	while mine > 0:
		rowTemp= random.randrange(0,row-1)
		colTemp= random.randrange(0,col-1)
		if board[rowTemp][colTemp]=="C ":
			board[rowTemp][colTemp]="C*"
			mine=mine-1
	print(board)
	return board


board=createBoard(row,col,mine)
