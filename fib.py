def fib(length):
	a=0
	b=1
	for i in range (0,length+1):
		print("%d. \t%d" %(i, a))
		aTemp=b
		b=a+b
		a=aTemp

def main():
	length=int(input("Input the amount of terms you wish to calculate: "))
	fib(length)

	
main()
