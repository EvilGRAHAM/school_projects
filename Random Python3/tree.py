#http://redd.it/1t0r09

#Takes user input.
def getInput():
	n = input("Info: ")
	size, base, body = n.split()
	size = int(size)
	return size, base, body
	
#Calculates the number of spaces needed to centre the top most row.
def calc(size):
	space = int((size-1)/2)
	return space
	
#Prints the ASCII tree
def draw(size, body, base):
	leaf = body
	for row in range (0,calc(size)+1):
#Uses the calculated space less the current row number to centre the tree.
		print(" " * (calc(size) - row), leaf)
#Leaf increases by 2 characters each iteration
		leaf = leaf + (body * 2)
#The number of spaces required for the base will always equal the 2nd highest row.
	print(" " * (calc(size) - 1), base * 3)	
	
def main():
	size, base, body = getInput()
	draw(size, body, base)
	
	
main()