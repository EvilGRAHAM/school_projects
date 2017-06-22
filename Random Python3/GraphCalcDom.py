import math

x=0
exit=True

while exit==True:
	expString=input("Please input the expression you wish to graph: ")
	for x in range (0,10):
		expRes=eval(expString)
		print(round(expRes,3))
	exit=input("Type 'f' to exit: ")
	if exit=="f":
		exit=False
	else:
		exit=True