from tkinter import *

def getInput():
	x = int(input("Enter the x coordinate: "))
	y = int(input("Enter the y coordinate: "))
	return x,y
	
def GUI(x,y):
	w = Tk()
	w.title("smile.py")
	aCanvas = Canvas(w, width = 500, height = 500, bg = "light blue")
	aCanvas.create_oval(x,y,x+100,y+100, fill="yellow")
	aCanvas.create_oval(x+25,y+25,x+35,y+50, fill="black")
	aCanvas.create_oval(x+65,y+25,x+75,y+50, fill="black")
#Arc reads counter-clockwise from the 3 o'clock position
	aCanvas.create_arc(x+30,y+60,x+70,y+90, style="chord", fill="black", start=180, extent=180)
	aCanvas.pack()
	w.mainloop()
	
def main():
	x,y = getInput()
	GUI(x,y)
	
main()