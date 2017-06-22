#http://redd.it/1tixzk

import math

PI = math.pi

def inputValue():
	values = input("Please enter the number of sides and circumradius: ")
#Input should be in the form of sides circumR
	sides, circumR = values.split()
	sides = int(sides)
	circumR = float(circumR)
	return sides, circumR

def calc(sides, circumR):
#Pheta is equal to a circle divided by the number of sides of the polygon giving the angle for the connection between two vertices and the centre.
	pheta = (2 * PI) / sides
#LenSide uses the law of cosines, where a==b and C==pheta
	lenSide = (2 * (circumR**2) - 2 * (circumR**2) * math.cos(pheta))**0.5
#Perimeter is equal to the length of a side * the number of sides
	perimeter = lenSide * sides
	return perimeter
	
def main():
	sides, circumR = inputValue()
	perimeter = calc(sides, circumR)
	print (perimeter)
	
main()
