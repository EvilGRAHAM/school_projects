#Name: Scott Graham
#ID: 10131323
#Tutorial: 12
#Version: Dec 5th, 2013

#November 26th, 2013:
#Initialized the gameboard.
#Did movement functions
#Did move check functions.
#Wrote fire generation code for modes 2 and 3.
#Defined when modes 2 and 3 occur.

#December 2nd, 2013:
#Cleaned up code.
#Wrote cheat menu.
#Wrote invulnerability and debug toggles.
#Did most of the debug messages.
#Wrote code to prevent damage for invulnerability.
#Wrote game ending conditions.
#Started some documentation.

#December 4th, 2013:
#The high score writing function was written.
#Some minor clean-up throughout the code.
#Did some documentation.
#Set up the game so it can be played again.
#General bug testing and debugging.
#Wrote instructions.
#Fixed an error with the movement input exception handler.

#December 5th, 2013
#More debug messages.


#Program Description:
"""This game is based off a scene in J.R.R. Tolkien's novel "The Hobbit" where Bilbo Baggins attempts to sneak into the dragon Smaug's layer to steal his treasure. This is a turnbased game where Bilbo can move 1 space in any of the four compass directions and the four sub compass directions. After each movement the appropriate changes are done to Bilbo's stats and then fire is cleared and generated. The game will also save the users score in a separate text file, scores.txt if the appropriate conditions are met. If said file does not exist, upon successful completion of the game, scores.txt will be created in the same folder as the game. 
'*" = Fire
'H' = Bilbo
'#' = Wall
't' = Treasure 
"""

#Features:
"""Player can move Bilbo using the number keys.
Bilbo can collect gold.
Bilbo can take damage.
The game will end with either Bilbo's death, or him escaping.
Game will prompt the user to play again.
Modes 2 and 3 are entered at the appropriate times.
If invulnerability is enabled, Bilbo will not recieve damage.
High scores are saved in a separate file, "scores.txt" in the form "Gold,HP" according to the rule outlined in the assignment.
If said file does not exist, the program will create it in the same folder as bilbo.py when a high score is achieved.
Debugging messages will provide the user with a variety of information regarding status messages and what happens in the background.
"""

#Limitations:
"""
Doesn't allow the user to change on the fly the location where the high scores are saved. If they choose to, they must edit the code themselves.
"""


import sys
import random
import os
from tkinter import *

#Named Constants:
BILBO = "H"
BILBO_GOLD = 1
BILBO_HP = 0
BILBO_C = 4
BILBO_R = 3
C_BORDER = "-"
C_SIZE = 32
FIRE = "*"
INVULNERABILITY = 2
MAP_EDGE = 3
R_BORDER = "|"
R_HALL = 16
R_SIZE = 32
R_SIZE_LEFT = 3
SCORES_FILE = "scores.txt"
SPACE = " "
TREASURE = "t"
WALL = "#"



#This function will create and store a list of information regarding Bilbo's health, gold, and his invulnerability state.
#Bilbo's stats wll be set to this at the start of each new game.
def charInfo():
#hp stands for hitpoints.
    hp = 20
    gold = 0
    invulnerability = "Off"
    bilboR = 2
    bilboC = 17
    bilboStats = [hp, gold, invulnerability, bilboR, bilboC]
    return bilboStats


#Displays the cheat menu, takes the users input and does the appropriate operation.
#Entering 'i' turns on invulnerability, 'd' turns on debugging mode.
#If anything else is entered, the menu is left.
def cheatMenu(debug, invuln, bilboStats):
    print ("""Cheat options:
(d)ebug mode toggle
(i)nvulnerability mode toggle""")
    cheat = input("Enter your Selection:")
    print ()
    if cheat in ("d", "D"):
#This branch will toggle the boolean value of debug, by telling it to make debug whatever debug was not before.       
        debug = not debug
#Same logic as debug.
#invuln is a boolean value, while INVULNERABILITY is a named constant, that is used to reference the invulnerabilty portion of Bilbo's stats.
    elif cheat in ("i", "I"):
        invuln = not invuln
#These branches will change the value of the invulnerability in the bilboStats list, so that it displays properly at the top.
        if invuln:
                bilboStats[INVULNERABILITY] = "On"
        else:
                bilboStats[INVULNERABILITY] = "Off"
    return debug, invuln


#Creates a debug statement that prints Bilbo's current stats in their a list, and his row/column values.
def debugDisplay(bilboStats):
    bilboStatsDebug = []
    for i in range (0,5,1):
        bilboStatsDebug.append(bilboStats[i])
#This sets the value of Bilbo's row, to align with what is displayed ont he left hand side of the board.
    bilboStatsDebug[BILBO_R] = bilboStatsDebug[BILBO_R] - 2
#This defines the space layed out by the coordinate system above the map. 
    if bilboStatsDebug[BILBO_C] in range (MAP_EDGE,C_SIZE):
#This here shifts the column debu value over so that it lines up with what is up top, then whenever the value is > 9,55 will be added and then converted to a character, because 10+55=65 which is the ASCII value of A. This can be extrapolated for the rest of the letters.
        bilboStatsDebug[BILBO_C] = bilboStatsDebug[BILBO_C] - 3
        if (bilboStatsDebug[BILBO_C] > 9):
            bilboStatsDebug[BILBO_C] = chr(bilboStatsDebug[BILBO_C] + 55)
    print (bilboStatsDebug)
    

#This function will display erebor and the following movement options to the user.
def display(erebor, bilboStats):
    erebor[bilboStats[BILBO_R]][bilboStats[BILBO_C]] = BILBO
    print()
    print ("""
Movement options
----------------
 7 8 9
 4 5 6
 1 2 3
Enter 5 to pass on movement.
Enter a negative value to quit the game.""")


#This function will calculate which squares in the main hall will be hit with fire using random number generation. What happens is that using nested for loops, each square in the lower hall will be checked to see what occupys it. If the square is empty or has Bilbo in it, a random number is generated and if it is 0 (10% chance) fire will be generated in that square.
def dragonFire2(erebor):
    for r in range (R_HALL,R_SIZE-1,1):
        for c in range (MAP_EDGE+1,C_SIZE-1,1):
#These two nested loops will map out the area of the hall not including the outer walls.
            if (erebor[r][c] == SPACE) or (erebor[r][c] == BILBO):
                fireChance = random.randrange(0,10)
#This random number generator will pick a number between 0-9 (10 unique numbers) and if that number is 0 (1 unique number/ 10 unique numbers * 100 == 10%) fire will appear in that square defined by their nested loops. Anytime this is used later on in this form, the logic is mostly the same.
                if fireChance == 0:
                    erebor[r][c] = FIRE


#This function will calculate and spread the fire along the exit tunnel as bilbo enters Mode 3.
#It relies on the fact that fireChase by default is 0 causing the first turn it is active not to spread, but after that the random number is calculated and then used on the next time through.
def dragonFire3(erebor, fireChance, fireChase, debug):
#This gives it a 50% chance of no spread, 25% for one space, 25% for two spaces.
    if fireChance in (0,1):
        fireChase = fireChase
        if debug:
            print ("Fire did not spread.")
    elif fireChance == 2:
        fireChase = fireChase - 1
        if debug:
            print ("Fire spread one square.")
    elif fireChance == 3:
        fireChase = fireChase - 2
        if debug:
            print ("Fire spread two squares.")
#This will just ensure that the fire doesn't spread beyond the bounds of the tunnel because of a bug that doesn't allow Bilbo to exit through the tunnel.
    if (erebor[fireChase][17] == SPACE):
        erebor[fireChase][17] = FIRE
    fireChance = random.randrange(0,4)
    return fireChance, fireChase


#This function will look at every square in the lower hall and if it is an emptry space or Bilbo, it will generate a random number and if that number is 0, fire will be generated in that space. 
def dragonFireClear2(erebor):
    for r in range (R_HALL,R_SIZE-1, 1):
        for c in range (MAP_EDGE+1,C_SIZE-1,1):
#These two nested loops serve the same function as dragonFire except each space is cleared to be a blank space so that new fire may appear and old fire will not persist.
            if (erebor[r][c] == FIRE):
                erebor[r][c] = SPACE


#This is the gameboard that James gave us. 
def gameBoard():
    erebor = [[' ',' ',' ','0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S'],
              [' ',' ',' ','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-'],
              [' ','0','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','1','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','2','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','3','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','4','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','5','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','6','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','7','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','8','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              [' ','9','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              ['1','0','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              ['1','1','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              ['1','2','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              ['1','3','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
              ['1','4','|','#',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','t',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ',' ',' ',' ',' ','#'],
              ['1','5','|','#',' ',' ',' ',' ',' ','#','#',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ','#',' ',' ','#','#',' ',' ',' ',' ','#'],
              ['1','6','|','#',' ',' ',' ',' ',' ','#','#',' ',' ',' ',' ',' ',' ','t',' ',' ',' ',' ','#',' ',' ','#','#',' ',' ',' ',' ','#'],
              ['1','7','|','#','t',' ',' ','t',' ',' ',' ',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ','#',' ',' ','t',' ',' ',' ',' ',' ','#'],
              ['1','8','|','#','#','#','#','#','#','#','#',' ',' ',' ',' ',' ',' ',' ',' ','t',' ',' ',' ',' ','#','#',' ',' ',' ',' ',' ','#'],
              ['1','9','|','#','#',' ',' ',' ',' ','#','#',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ',' ','#','#',' ',' ',' ',' ',' ','#'],
              ['2','0','|','#',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','t',' ',' ',' ',' ','#'],
              ['2','1','|','#',' ',' ',' ',' ',' ','#','#',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ',' ','#','#',' ',' ',' ',' ',' ','#'],
              ['2','2','|','#',' ',' ',' ',' ',' ','#','#',' ',' ',' ','t',' ',' ','#',' ',' ',' ',' ',' ',' ','#','#',' ','#','#','#','#','#'],
              ['2','3','|','#',' ',' ','t',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ',' ','#',' ','t',' ',' ',' ',' ','#'],
              ['2','4','|','#',' ',' ',' ',' ',' ','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#',' ','#'],
              ['2','5','|','#',' ',' ',' ',' ',' ','#','#',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ',' ','#','#',' ',' ',' ','t',' ','#'],
              ['2','6','|','#',' ',' ',' ',' ',' ',' ',' ',' ','t',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','#'],
              ['2','7','|','#',' ','t',' ',' ',' ','#','#',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ',' ','#','#',' ','#','#','#','#','#'],
              ['2','8','|','#',' ',' ',' ',' ',' ','#','#','t',' ',' ',' ',' ',' ',' ','t',' ',' ',' ',' ',' ','#','#','t','t','t','t','t','#'],
              ['2','9','|','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'] ]
#Erebor design courtesy of James Tam. The image the design is based off of can be found at http://pages.cpsc.ucalgary.ca/~tamj/217/assignments/assignment5/index.htm see figure 2.
#This version differs from the one given to us by James, as the row and column markers are included in the list, as it made it easier to do certain calculations.
    return erebor


#Writes the users high score to a file if applicable.
#For file scores.txt, the first number is the gold value, and the second number is the hp.(Gold,HP)
def highScore(bilboStats, debug):
#These are preset to an invalid number so that they may be assigned.
    gold = -1
    hp = -1
#This exception will check to see if scores.txt exists and if it doesn't it will be created with the line "0,0" inside it. 
    try:
        score = open(SCORES_FILE, "r")
    except IOError:
        score = open(SCORES_FILE, "w")
#The "0,0" is useful as if scores.txt doesn't exist before the game is run, the new version will have a set of values to check against causing a new high score to be written. 
        score.write("0,0")
#After the exception is dealt with, scores.txt will be closed and reopened using the read/write modifier
    finally:
        score.close()
        score = open(SCORES_FILE, "r+")
#This will take the gold and hp values from scores.txt and save them as two local variables gold, hp respectively.
    for line in score:
        gold, hp = line.split(",")
    gold = int(gold)
    hp = int(hp)
#These next set of branches will check the pre-existing values for gold and hp against the current values and determine the appropriate action.
#The first branch checks if the new value of gold is larger than the high score and if so it overrides it. Also the hp is overwritten
    if (gold < bilboStats[BILBO_GOLD]):
        gold = bilboStats[BILBO_GOLD]
        hp = bilboStats[BILBO_HP]
#This case handles if the old and new gold values are equal, so it will check if the new hp is greater than the old.
    elif (gold == bilboStats[BILBO_GOLD]) and (hp < bilboStats[BILBO_HP]):
        hp = bilboStats[BILBO_HP]
#Both gold and hp are converted to strings so that they may be written to scores.txt
    gold = str(gold)
    hp = str(hp)
#Small debug message telling the user that a new high score was written to scores.txt and what was written.
    if debug:
        print ("%s,%s was written to scores.txt for Gold and HP respectively." %(gold, hp))
#This will clear scores.txt of all information preparing it to be written in again. If no new high score is achieved, the previous high score is written in it again.
    with score:
        pass
#Since scores.txt is closed with the loop above it must be re-opened in write mode and the high score values are written in.
    score = open(SCORES_FILE, "w")
    for temp in (gold, ",", hp):
        score.write(temp)
#Closes file.
    score.close()
    

#This is the main movement mechanism used, it will evaluate a user input to a corresponding movement. Using moveCheck(), it will take the corresponding action to Bilbo's stats depending on what square he moves in to. Also within here using cheatMenu() that will be displayed if 0 is entered. Idf a negative value is entered the game exits with no high score being recorded. 
def hobbitMove(moveSelect, erebor, gameState, bilboStats, debug, invuln, gameWin, mode2):
    validMove = True
#These following branches, will group like numbers together based on the typical computer number pad and their result. the first 3 branches describe the vertical movement, while the latter 3 the horizontal movement. Also a compass direction is assigned to each movement, to be used for debugging purposes.
    if moveSelect in (1,2,3):
        bilboRNew = bilboStats[BILBO_R] + 1
        vertical = "South"
    elif moveSelect in (0,4,5,6):
        bilboRNew = bilboStats[BILBO_R]
        vertical = ""
    elif moveSelect in (7,8,9):
        bilboRNew = bilboStats[BILBO_R] - 1
        vertical = "North"
    if moveSelect in (1,4,7):
        bilboCNew = bilboStats[BILBO_C] - 1
        horizontal = "West"
    elif moveSelect in (0,2,5,8):
        bilboCNew = bilboStats[BILBO_C]
        horizontal = ""
    elif moveSelect in (3,6,9):
        bilboCNew = bilboStats[BILBO_C] + 1
        horizontal = "East"
#This is done in case Bilbo decides to stay still, 10% of the time he will take fire damage. Mode 2 has to be inititiated first as well.
    if (moveSelect == 5) and (mode2) :
        fireChance = random.randrange(0,10)
        if (fireChance == 0):
            if invuln:
#Usual damage debug messages.
                if debug:
                    print ("Bilbo took 4 damage, but his invulnerability protected him.")
            else:
                bilboStats[BILBO_HP] = bilboStats[BILBO_HP] - 4
                if debug:    
                    print ("Bilbo took 4 damage.")
        if (erebor[bilboStats[BILBO_R]][bilboStats[BILBO_C]] == FIRE):
            bilboStats[BILBO_HP] = bilboStats[BILBO_HP] - 4
#If a number < 0 is entered, the game is exited.
    if (moveSelect < 0):
        gameState = False
#This handles the cheat menu if the user selects 0.
    if (moveSelect == 0):
        debug, invuln = cheatMenu(debug, invuln, bilboStats)
    if gameState:
#This handles if the user wishes to quit.
#No movement will occur and the loop in the main function will be exited.
        validMove, gameState, gameWin= moveCheck(bilboRNew, bilboCNew, erebor, bilboStats, debug, invuln, gameWin, mode2)
        if validMove:
            erebor[bilboRNew][bilboCNew] = BILBO
#This causes an "H" to appear in the element defined by bilboRNew and bilboCNew.
            erebor[bilboStats[BILBO_R]][bilboStats[BILBO_C]] = SPACE
#This clears the previous space Bilbo was in to be an empty space.
            bilboStats[BILBO_R] = bilboRNew
            bilboStats[BILBO_C] = bilboCNew
#This debug statement handles if Bilbo moves, what compass direction he moved.
            if debug:
                print ("Bilbo moved %s %s." %(vertical, horizontal))
#These two lines above reset Bilbo's position so that when the user again is prompted to move him, he will be at his new position and any changes will occur from there.
    return gameState, debug, invuln, gameWin
    

#This function displays a background to the stroy the game is based on, and the user instructions.
def intro():
#Instructions
    print ("""
To move Bilbo Baggins ('H') use the number pad on the right hand side of the keyboard. Each key represents a direction that Bilbo can move. Bilbo can also move diagonally so please keep that in mind as you play. Once you have made your decision, press <enter> to move Bilbo. The game will end when either Bilbo runs out of HP, or he successfully exits the tunnel at the top of the screen.
Legend:
H = Bilbo
# = Wall
t = Treasure
* = Fire
You can access your high score through the file scores.txt where your score is presented in the form, "Gold, HP".
""")


#This function will essentially cause the game to look at where Bilbo is about to move before he physically moves, see what is currently on that square and take appropriate action. Bilbo can either take 4 point of damage due to fire, gain 3 gold due to treasure, or due nothing as he ran into a wall. if debug mode is on, the game will tell the user if Bilbo took damage, and if Bilbo found treasure.
def moveCheck(bilboRNew, bilboCNew, erebor, bilboStats, debug, invuln, gameWin, mode2):
#This branch checks to see if the space Bilbo moves in to is either empty, filled with fire or treasure.    
    if erebor[bilboRNew][bilboCNew] in (TREASURE, FIRE, SPACE, BILBO):
        if (erebor[bilboRNew][bilboCNew] == FIRE):
#Checks to see if invulnerability is on, and if so, no damage is taken.
            if invuln:
                if debug:
                    print ("Bilbo took 4 damage, but his invulnerability protected him.")
#4 damage is taken.
            else:
                bilboStats[BILBO_HP] = bilboStats[BILBO_HP] - 4
                if debug:
                    print("Bilbo took 4 damage.")
        elif (erebor[bilboRNew][bilboCNew] == TREASURE):
#3 Gold is added.
            bilboStats[BILBO_GOLD] = bilboStats[BILBO_GOLD] + 3
            if debug:
                print ("Bilbo found 3 pieces of Gold.")
        validMove = True
        gameState = True
    elif (erebor[bilboRNew][bilboCNew] == WALL):
#This handles if Bilbo attempts to move into a wall, and will tell hobbitMove() not to move Bilbo
        validMove = False
        gameState = True
        if mode2:
#The following allows Bilbo to take damage if the player decides to continually run into a wall.
            fireChance = random.randrange(0,10)
            if (fireChance == 0):
                if invuln:
                    if debug:
                        print ("Bilbo took 4 damage, but his invulnerability protected him.")
                else:
                    bilboStats[BILBO_HP] = bilboStats[BILBO_HP] - 4
                    if debug:    
                        print ("Bilbo took 4 damage.")
                if (erebor[bilboStats[BILBO_R]][bilboStats[BILBO_C]] == FIRE):
                    bilboStats[BILBO_HP] = bilboStats[BILBO_HP] - 4
#This branch handles when Bilbo moves outside of the game board, i.e. he extis and prints the winning message.
    else:
        print ("Congratulations on escaping with your treasure!!!")
#These cause the game to end, no movement to occur and for a high score to be written if applicable.
        validMove = False
        gameState = False
        gameWin = True
    return validMove, gameState, gameWin


#This function will take the user input, check if it is a numeric value, and if not, prompt the user again. If it is, it will check that it is a number <= 9 as those are the valid numbers, and again if no, the user will be prompted. For some reason unknown to me, if the user enters 'X' the program produces a runtime error.
def moveInput():
    invalidInput = True
    error = True
#This handles if the user doesn't enter a valid character and will prompt them again.
    while invalidInput:
        while error:
            moveSelect = input("Enter your selection:")
            try:
                invalidinput = int(moveSelect)
            except ValueError:
                print ("Please enter a number.")
                moveSelect = input("Enter your selection:")
            else:
                invalidInput = False
                error = False
                moveSelect = int(moveSelect)
#Handles if the user inputs a value > than 9
        if (moveSelect > 9):
            invalidInput = True
            error = True
        else:
            invalidInput = False
    os.system("cls")
    return moveSelect


def gui(window, aStats, aGameBoard, aStory, bilboStats, erebor):
    aStats.delete(ALL)
    aGameBoard.delete(ALL)
    aStory.delete(ALL)
#The following displays the gameboard.
    y = 30
    hobbit=aStats.create_text(350,10, text="Bilbo hit points: %d\t Gold: %d\t Invulnerability: %s" %(bilboStats[BILBO_HP] , bilboStats[BILBO_GOLD], bilboStats[INVULNERABILITY]), font="Times 12")
    for r in range(2, 32):
        x = 30
        for c in range(3, 32):
            if erebor[r][c] == WALL:
                aGameBoard.create_rectangle(x,y,x+10,y+10, fill="dark grey", outline="dark grey")
            if erebor[r][c] == BILBO:
#This makes sure that Bilbo appears to be standing on solid ground.
                aGameBoard.create_rectangle(x,y,x+10,y+10, fill="Saddle Brown", outline="Saddle Brown")
                aGameBoard.create_oval(x,y,x+10,y+10, fill="green", outline="green")
            elif erebor[r][c] == TREASURE:
#See above, but for treasure.
                aGameBoard.create_rectangle(x,y,x+10,y+10, fill="Saddle Brown", outline="Saddle Brown")
                aGameBoard.create_oval(x,y,x+10,y+10, fill="gold", outline="black")
            elif erebor[r][c] == SPACE:
                aGameBoard.create_rectangle(x,y,x+10,y+10, fill="Saddle Brown", outline="Saddle Brown")
            elif erebor[r][c] == FIRE:
#Again above.
                aGameBoard.create_rectangle(x,y,x+10,y+10, fill="Saddle Brown", outline="Saddle Brown")
                aGameBoard.create_polygon(x,y+10,x+10,y+10,x+5,y, fill="red", outline="black")
            x = x + 11
        y = y + 11
#Following the movement selection screen.
    aStory.create_text(150,210,text="\"The Hobbit is a tale of high adventure,\n undertaken by a company of dwarves,\n in search of dragon-guarded gold.\n A reluctant partner in this perilous quest\n is Bilbo Baggins,\n a comfort-loving, unambitious hobbit,\n who surprises even himself\n by his resourcefulness\n and his skill as a burglar.\nEncounters with trolls, goblins,\n dwarves, elves and giant spiders,\n conversations with the dragon,\n Smaug the Magnificent,\n and a rather unwilling presence at\n the Battle of the Five Armies\n are some of the adventures that befall Bilbo.\n But there are lighter moments as well:\n good fellowship, welcome meals and song.\"\n - J.R.R. Tolken \"The Hobbit\"", font="Times 12")
    aStats.pack(fill=BOTH, side=TOP)
    aGameBoard.pack(fill=Y, side=LEFT)
    aStory.pack(expand=TRUE, fill=BOTH, side=RIGHT)
    return window



#Main function contains all other functions.
def main():
#This is used at the end of the function to check if the user wishes to play again.
    replay = True
    while replay:
        window = Tk()
        window.title("\"The Hobbit\" The Game - By Scott Graham")
#This causes the screen to clear each time a new game is started.
		#os.system("clear")
        os.system("cls")
#These all initialze to default values each time the game is run, so that a new game is created.
#Sets debug mode to off by default.
        debug = False
#By setting fireChance to 0 from the start, allows when mode 3 is initialized for the first time, to guarentee that fire won't be spawned on the first turn.
        fireChance = 0
#This is the value for the initial row that fire appears in at mode 3.
        fireChase = 15
#Used for high score writing
        gameWin = False
#True by default so the game loop runs.
        gameState = True
#Bilbo takes damage by default.
        invuln = False    
#Modes 2 and 3 won't initialize until deemed necessary.
        mode2 = False
        mode3 = False
#Used to initiate the replay loop.
        replayCheck = True
#This is just used so that when mode 3 is first initialized, a print message appears.
        smaugCheck = True
#Initializes the playing space.
        erebor = gameBoard()
        intro()
#Initializes Bilbo's stats.
        bilboStats = charInfo()
        aStats=Canvas(window, width=700, height=20, bg="white")
        aGameBoard=Canvas(window, width=400,height=400, bg="white")
        aStory=Canvas(window, width=300, height=400, bg="white")
        
#Game Starts.
        while gameState:
            erebor[bilboStats[BILBO_R]][bilboStats[BILBO_C]] = BILBO
            if debug:
                debugDisplay(bilboStats)
 
#Mode 2
            if (bilboStats[BILBO_R] >= R_HALL) or mode2:
                mode2 = True
#If debug is enabled, the game will display what mode the user is currently in, if the user enters Mode 3, this will not display.
                if debug and not mode3:
                    print ("User in Mode 2.")
                dragonFireClear2(erebor)
                dragonFire2(erebor)

#Mode 3
                if (bilboStats[BILBO_R] < R_HALL) or  mode3:
                    mode3 = True
                    if debug:
                        print ("User in Mode 3.")
#The following only prints once when Bilbo enters Mode 3.
                    if smaugCheck:
                        print ("Smaug is moving towards the tunnel!")
                        smaugCheck = False
                    dragonFireClear2(erebor)
#This makes sure all squares leading up to the leading fire square are filled with fire each turn.
                    if (bilboStats[BILBO_R] != 15):
                        erebor[15][17] = FIRE
                    fireChance, fireChase = dragonFire3(erebor, fireChance, fireChase, debug)
                    for i in range (fireChase,15):
                        if i > 1:
                            erebor[i][17] = FIRE
            display(erebor, bilboStats)

            window = gui(window, aStats, aGameBoard, aStory, bilboStats, erebor)

            moveSelect = moveInput()
            gameState, debug, invuln, gameWin = hobbitMove(moveSelect, erebor, gameState, bilboStats, debug, invuln, gameWin, mode2)
            if (bilboStats[BILBO_HP] <= 0):
                print ("Unfortunately Bilbo was burnt to a crisp...")
                if debug:
                    print ("No Highscore was recorded.")
                gameState = False
            if gameWin:
                highScore(bilboStats, debug)
#Note replayCheck is preset to True.
        while replayCheck:
            erebor[bilboStats[BILBO_R]][bilboStats[BILBO_C]] = BILBO
            print ("Bilbo hit points: %d\t Gold: %d\t Invulnerability: %s" %(bilboStats[BILBO_HP] , bilboStats[BILBO_GOLD], bilboStats[INVULNERABILITY]))
            playAgain = input ("Would you like to play again? (y)es or (n)o: ")
            if playAgain in ("n", "N", "y", "Y"):
                replayCheck = False
#If 'n' is entered, the game exits, if 'y' is entered the game is reset from scratch.
                if playAgain in ("n", "N"):
                    replay = False
                print("Please close the Gameboard.")
                window.mainloop()


                
main()
