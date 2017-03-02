import math
prob=float(input("Probability: "))
numSuccess=int(input("Number of Successful Trials: "))
numTrial=int(input("Number of Trials: "))

P=(math.factorial(numTrial)/(math.factorial(numSuccess)*math.factorial(numTrial-numSuccess)))*(prob**numSuccess)*((1-prob)**(numTrial-numSuccess))

print(P)
