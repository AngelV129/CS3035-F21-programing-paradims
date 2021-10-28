""" Lab 10: Part 2: War Card Game [10 POINTS] """

"""
In this assignment, you are going to write a command line card game of War (see
https://youtu.be/G4DhzhDlXFM and/or https://en.wikipedia.org/wiki/War_(card_game) for
the game). For simiplification, assume that the deck of cards just has numbers
from 1 to 10 (no repeats) in it. Please implement the game in three steps as follows.

Note: Do NOT define ANY functions in your implementation.

Please do all of these in Python 3, NOT Python 2. Your submitted file MUST be compilable
(i.e. NO compiling errors) in Python 3, otherwise you will get ZERO.
If you have some uncompilable code, I recommend COMMENTing them.

After writing your code, please play the game to test your implementation.
The testing procedure is as follows:
-- Open a command prompt and locate to the directory/folder of THIS script/file.
-- In the command prompt, type $python THISFILENAME.py$ OR $python3 THISFILENAME.py$ to
    run the script, then you can play the game.

This video (https://calstatela.zoom.us/rec/share/uIcKncpEt8rvakCv35XKGEnMD8BLA21CpLFmYp8sJJeItDljIRdTjY06DiHveSDB.Pda6RN2vVGCY5CH0?startTime=1604017135000)
illustrates a play round of my implementation. You may take it as a reference.
"""


# Please do NOT DELETE nor CHANGE the following line.
# It is used to import the random module which pre-defines functions to generate
#   pseudo-random numbers (https://docs.python.org/3/library/random.html).
# You need it in Step 1.
import random


"""
Step 1 [2.5 POINTS]:
Create two player variable scores, and a deck of cards for each player.
Each deck has 10 cards. Shuffle the decks.

Hint: Use random.shuffle(your_deck) to shuffle in place

Note: Do NOT define ANY functions in your implementation.
"""


"""
Step 2 [5 POINTS]:
Using a while loop (while some_boolean_expression :  block of code), repeatedly
ask the user to type/enter 1 to "hit", that is, play a hand. With each hand,
check which side wins and update player scores. If there is a tie, just leave
the player scores alone and repeat the asking process.

Note: Do NOT define ANY functions in your implementation.
"""

"""
Step 3 [2.5 POINTS]:
When any of the decks are empty, print who won (or tie if it's a tie),
and end/terminate the game.

Note: Do NOT define ANY functions in your implementation.
"""