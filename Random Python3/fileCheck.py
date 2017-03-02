"""
This function is designed to check if a file exists, and if not to create a blank one.
Created by Scott Graham
"""

TEXT = "Hello World!"

def fileCheck(filename):
#Try will check to see if the file exists by opening it in read mode.
	try:
		file = open(filename, "r")
		print(filename, "exists.")
#If said file doesn't exist, a new file is created using write mode.
	except IOError:
		file = open(filename, "w")
#"Hello World!" can be changed to whatever.
		file.write(TEXT)
		print ("Wrote \"%s\" to %s." %(TEXT, filename))
	finally:
		file.close()
#The following is optional and situational dependent.
#		file = open(filename, "r+")


fileCheck()