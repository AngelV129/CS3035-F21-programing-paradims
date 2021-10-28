""" Python Bootcamp 1 """


"""
Resources:
-- Python Tutorial: https://docs.python.org/3/tutorial/index.html
-- Python 3 Documentations: https://docs.python.org/3/
"""


"""
Python Comments:
-- single line comments: start by a pound/number sign # until the end of the line
    -- e.g. line 21
-- block comments: enclosed in pairs of three double or single quotes, may span multiple lines
    -- also used for docstrings (https://www.geeksforgeeks.org/python-docstrings/)
    -- e.g. lines 23-27 
    -- e.g. lines 29-33
"""

# THIS IS A SINGLE LINE COMMENT

"""
THIS IS A BLOCK COMMENT
THIS LINE CONTINUES THE BLOCK COMMENT
"""

'''
THIS IS ANOTHER BLOCK COMMENT
THIS LINE CONTINUES THE BLOCK COMMENT
'''




"""
Python Bootcamp 1


********************************************************************************
I. Introduction

Python is an object-oriented general purpose programming language. A few features:
1. Python is an interpreted language:
    -- Computers only understand binary code (1's and 0's). Programming language
    code needs to be translated by either compiling the code to a file of 1's and
    0's (compiled languages), or another program has to interpret the code as it
    runs to binary (interpreted)
2. Python is widely used in data science, artificial intelligence, web Programming,
scripting, and many other uses
3. The syntax of Python is closer to human language than most programming languages
4. Python uses INDENTATION as part of its syntax
5. Python supports object-oriented programming with classes
6. Python supports aspects of functional programming with the functools module (https://docs.python.org/3/library/functools.html)
7. Functions are FIRST-CLASS citizens in Python
8. Python is often used as glue code between programs as it can interact with
system commands
9. There are two basic versions of Python: Python 2 and Python 3. Python 2 has
remained in wide use due to the slowness of frameworks adapting to the newer Python.
We will use Python 3.

There are two basic ways to use Python:
    1. Work with the interactive prompt
        -- Open a terminal (or command prompt, hereafter understood). If python is
        installed, type either python or python3 (depending on your system) to
        run Python version 3
        -- Use quit() or Ctrl-Z plus Return to exit the interactive prompt
    2. Run python scripts
        -- Open a text editor and name it, give it a .py extension
        -- Open a terminal, navigate to where the python script is, type:
        python(3) yourscriptname.py
            -- You can also use the path to where the file is instead of navigating
            to its location
            -- You can also pass additional arguments when you run your script




********************************************************************************
II. Using Python as a Calculator (interacting with the python interpretor)

Open a terminal, type python(3), you should see this:
>>> 

Try the following
>>> 2 + 3
>>> 5 * 6
>>> 3.4 * 2
>>> 3 / 2   # normal division
>>> 3 / 3   # normal division
>>> 3 // 2  # integer or floor division
>>> 3 // 3  # integer or floor division
>>> 3 ** 2  # exponents
>>> 17 % 3  # modulus

Here we see two data types: int, and float.
-- int, or integers, are whole numbers that can be positive, negative, or 0
-- floats are floating-point numbers
-- Usually languages limit the amount of space an integer or float can take
    -- They have to be stored as bits somewhere in your computer's memory
-- Python does not limit the size of integers, but does floats

There are also booleans: True or False
(only the first letter must be capitalized)

Logical operations: and, or, not (all letters must be in lower-case)
>>> True and False
>>> True or False
>>> not True

Comparison operators: >, <, >=, <=, !=, ==, is, is not
-- take two arguments and evaluate to a boolean
-- == vs. is (!= vs. is not)
    -- The == and != operators compares the VALUE or equality of two objects 
    -- The is and is not operators compare the IDENTITY of two objects
        -- whether two variables point to the same object in memory
    -- https://realpython.com/python-is-identity-vs-equality/
>>> 6 < 7
>>> 6 == 7
>>> 6 is 7  # warning
>>> 6 != 7
>>> 6 is not 7  # warning




********************************************************************************
III. Variables

Variables are names we give to a piece of memory. We can store (assign) values
(e.g., numbers) in that memory like this
>>> a = 5

Then we can use it later
>>> a + 6

We can store ANY value of ANY type in a variable
-- Python is not *strongly typed* nor *statically typed* like Java is
>>> a * 1.5
>>> a = 4.3




********************************************************************************
IV. Strings

Strings are sequences of chars (characters). They are denoted with enclosing
quotes (either single or double).
-- e.g., "Hello World!"
-- \ can be used to escape special characters
    -- e.g. 'Hello \"World\"!'  # \" is the double quote character
    -- e.g. "Hello \nWorld!"    # \n is the new line character

To print to console, type print("your string")
>>> print("Hello World!")
>>> print('Hello \"World\"!')
>>> print("Hello \nWorld!")

print() is a *function*: it does something or outputs something or both.
Functions sometimes take *arguments*, values you place in the parentheses.

Use triple quotes for multiline
>>> print('''
Usage: thingy [OPTIONS]
    -h                        Display this usage message
    -H hostname               Hostname to connect to
''')

When typed out "like this", strings are called *string literals*

Strings can be concatenated with a + sign
>>> print("Hello " + "World")

The built in function len() tells how many characters are in the strings
>>> len("Hello World!")
>>> len('Hello \"World\"!')
>>> len("Hello \nWorld!")




********************************************************************************
V. Lists

*list* is another data type that stores ordered sequences.
-- Strings are lists of characters.

Lists can be of any data type: ints, floats, Strings
List literals can be made like this:
>>> [1,2,3,4]

You can MIX types in a lists
-- NOT allowed in Haskell
>>> [1, "hello"]

Store them in a variable
>>> a = [1,2,3,4]

Lists are *indexed* by ints, the *index* tells you the position of an element
in a list. They start with the number 0.
-- index:   0  1  2  3
-- list:   [6, 8, 2, 3]

To get the value at an index of a list, use [] after the list's names
>>> a = [1,2,3]
>>> a[1]

You can also use NEGATIVE indices: these count backward from the end of the lists
-- index:  -4  -3  -2  -1
-- list:   [6,  8,  2,  3]

>>> a = [6, 8, 2, 3, 4]
>>> a[-1]
>>> a[-3]

Index -1 is often used to get the last element of a list

Lists can also be sliced:
>>> a[1:3]
This gives elements at indices 1 up to, but NOT including, 3

Try:
>>> a[1:]
>>> a[:3]
>>> a[-1:]
>>> a[:-1]
Python substitutes beginning of list and end of list when no value is added

You can also step through a list at different intervals
>>> a = [1,2,3,4,5,6,7,8,9]
>>> a[1:7:2]    # 2 is the step
>>> a[::-1]     # reverse
>>> a[::-2]

Lists also support concatenation with a + sign
>>> b = [100, 10000]
>>> c = a + b

A difference between most lists and strings:
-- strings are *immutable*, which means
    -- they cannot be altered
    -- When you do alter one, Python creates a new string and destroys the old one.
-- Lists, on the other hand can be modified, are *mutable*

Lists can also be nested
>>> [[1,2,3], [4,5,6]]

There is a way to generate sequences of numbers in a list
>>> list(range(12))

range() is a function that produces a range object. That range object can be converted to a list.
The above code makes the range object, then converts it to a list

range() can also have a start, stop (NOT included), and step, just like slices
>>> list(range(0, 11, 3))
>>> list(range(0, 12, 3))   # same result with previous one




*******************************************************************************
VI. Control Flow

Let's write a script! This is a file that contains a program, which is
a series of instructions

Open a new file in your text editor.
Name it whateveryouwant.py, save it wherever you like

Write the following (also in print.py)
"""
# print("Hello World!")
# a = list(range(100))
# print(a)

"""
The above code just executes the instructions in order
Note: NO semicolon after every statement


A. if Statements

What if we want the program to do something if a condition (an expression that
evaluates to a boolean, that is, is either True or False) holds, or do something
else if another holds.

The syntax is
# if expression:
# 	statement
# elif expression: # else-if
# 	statement
# elif expression: # else-if
# 	statement
# ......
# else:
# 	statement

Before that, let's get user input. Use input
Uncomment out the following: (in most editors, select the code, then hit control /)
(also in if.py)

"""
# a = input("Enter a number, negative numbers always invalid: \n")
#
# if not a.isdigit(): #only works on non-negative numbers
#     print("invalid input")
# else:
#     print("You entered: " + a)
#     a = int(a)
#     if a < 4:
#         print("number too low")
#     elif a > 100:
#         print("number too high")
#     else:
#         print("good number")
#         if a < 5:
#             print("number almost too low")

"""
notice the code included under the if statements is indented: INDENTATION indicates blocks/scopes


B. for statements (loops)

loop example in Java:
for(int i = 0; i < 100; i++){
    System.out.println(i);
}

See code below (also in for.py)

"""
# for i in range(100):
#     print(str(i))

"""
above code:
-- range is a container that can be looped over, i is each value in the container
-- for loops can be used with lists, and many other things, as long a they are *iterable*

Challenge: code up fizzbuzz (see fizzbuzz.py)
-- for ints from 1 to 100, print "Fizz!" if the number is a multiple of 3, "Buzz!"
    if the number is a multiple of 5, "Fizzbuzz!" (note, not "Fizz!Buzz!") if the number
    is divisible by both, otherwise print the number

Another example: (also in words.py)
"""
# words = ["at", "to", "howdy", "opie", "total", "wimp", "gong"]
# more_than_two = []
# for i, w in enumerate(words):
#     print(w + " found at index " + str(i))
#     if len(w) > 2:
#         more_than_two.append(w)
#
# print(more_than_two)

"""
Another way: (also in words.py)
"""
# for i in range(len(words)):
#     print(words[i])

"""
if you want both the index and value (also in words.py)
"""
# for i, w in enumerate(words):
#     print((i, w))   #(i, w) is a tuple, an immutable collection

"""
Backwards: (also in words.py)
"""
# for w in reversed(words):
#     print(w)

"""
Another way of backwards: (also in words.py)
"""
# for w in words[::-1]:
#     print(w)

"""
break is a keyword that breaks out of a loop
continue skips the rest of the code in the current iteration and goes to the next

For example: (also in breakcontinue.py)
"""
# for n in range(2, 10):
#      for x in range(2, n):
#          if n % x == 0:
#              print(n, 'equals', x, '*', n//x)
#              break
#          # else runs if no break occurs
#          else:
#              # loop fell through without finding a factor
#              print(n, 'is a prime number')

# for num in range(2, 10):
#      if num % 2 == 0:
#          print("Found an even number", num)
#          continue
#      print("Found a number", num)


"""
C. while statements

*while* continues as long as condition is True

See code below (also in while.py)
"""
# x = 0
# user_input = input("enter a number: \n")
# while x < int(user_input):
#     print("number is positive")
#     user_input = input("enter a number: \n")

"""
If you use one of the above statements, or any statement with a : at the end, Python
expect an indented block, otherwise you'll get a syntax error involving indentation.
If you want an EMPTY block use pass, e.g. (also in emptyblock.py)
"""
# while True:
#    pass
"""
This code will run forever, or until Control C is hit




**************************************************************************************
VII. Defining Functions

Sometimes you want reusable code: create a shortcut with a function.

The syntax is
# def yourfunctionname(arguments):
#     stuff your function does
#     return a value if you want

See code below (also in function.py)
"""

# def fib_rec(n):
#     if n == 0: return 0
#     if n == 1: return 1
#     return fib_rec(n-2) + fib_rec(n-1)

# def fib(n):
#     """ This is docstring of the fib() function. """
#     if n == 0: return 0
#     if n == 1: return 1
#     cache = [0] * (n+1)
#     cache[1] = 1
#     for i in range(2, len(cache)):
#         cache[i] = cache[i - 1] + cache[i - 2]
#     return cache[-1]

# print(fib_rec(10))
# print(fib(2000))

"""
some functions return values
"""
# def return_true():
#     return True

"""
some functions don't return values
"""
# def print_sometin():
#     print("l;djkof")
# print_sometin()

# a = return_true()
# print(a)
# print(fib.__doc__)
"""
by the way, this is an example of *dynamic programming*, another topic the comment
at the top of the function is a docstring for inline documentation (some ide's) support it

functions are first-class citizens: they can be assigned to variables, and then used
by adding a () at the end of the variable name
"""
# f = fib
# print(f(10))

"""
You can set default values
"""
# def ask_ok(prompt, retries=4, reminder='Please try again!'):
# # keyword arguments can be set to a default
#     while True:
#         ok = input(prompt)
#         if ok in ('y', 'ye', 'yes'): # in asks whether ok is in the tuple
#             return True
#         if ok in ('n', 'no', 'nop', 'nope'):
#             return False
#         retries = retries - 1
#         if retries < 0:
#             raise ValueError('invalid user response') # throws an exception and stops program
#         print(reminder)
#
# if ask_ok("Continue? (y/n)"):
#     print("Awesome!")
# else:
#     print("No problem")
