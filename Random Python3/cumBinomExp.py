import math
PSum=0
prob=float(input("Probability: "))
lower=int(input("Lower Bound: "))
upper=int(input("Upper Bound: "))

for n in range(lower,upper+1,1):
	n=int(n)
	P=(math.factorial(upper)/(math.factorial(n)*math.factorial(upper-n)))*(prob**n)*((1-prob)**(upper-n))
	PSum=PSum+P

print(PSum)
