{- Lecture 05-2: Getting Started -}


{-
Resource/Textbook:
-- Learn You a Haskell for Great Good
-- http://learnyouahaskell.com/
-- Chapter 2: Starting Out

Good Haskell Programming Style:
-- https://www.seas.upenn.edu/~cis194/fall16/style.html

INDENTATION is important in Haskell programs, because Haskell relies on
indentation to reduce the verbosity of your code. Some syntax ERRORS may
be caused by indentation issues.
-- Indentation Rules: https://en.wikibooks.org/wiki/Haskell/Indentation
-}


{-
Use of Glasgow Haskell Compiler (GHC)

GHC can take a Haskell script (a *.hs file) and compile it.
    -- Compile Haskell script: open a command prompt, locate to the folder
        of the script, type $ghc -o hello hello.hs$.
    -- Run the generated executable file in the command prompt:
        -- For Windows operating systems, type $hello.exe$.
        -- For macOS and Linux operating systems, type $./hello$.

GHC also has an INTERACTIVE mode which allows you to interact with scripts
    and see the results immediately.
    -- Enter the interactive mode: open a command prompt and type $ghci$
    -- Evaluate an expression: type the expression, e.g. $a = 1 + 2$
    -- Load a script: type $:l hello.hs$ or $:l hello$ (locate to the folder of hello.hs
        BEFORE entering the interactive mode)
    -- Call a function defined in LOADED scripts: type function name,
        e.g. $main$ (the script defining the function MUST be loaded first)
    -- Re-Load a script: after modifying the script, type $:l hello$ or $:r$
        to load the latest script
    -- Exit the interactive mode: type $:q$ or $:quit$
-}


{- Arithmetic Expressions

Arithmetic Operators:
    -- addition +
    -- subtraction -
    -- multiplication *
    -- division /

Try the following arithmetic expressions:
2 + 15   
49 * 100
1892 - 1472
5 / 2   -- NOT integer division
(50 * 100) - 4999  
50 * 100 - 4999  
50 * (100 - 4999) 

Try the following arithmetic expressions with negative numbers: 
5 * -3  -- ERROR
5 * (-3)
-3 * 5
(-3) * 5

Suggestion: ALWAYS surround negative numbers with parentheses
-}


{- Boolean Algebra

Boolean Values: True, False (ONLY "T" and "F" MUST be capitalized)

Boolean Operators: 
    -- and &&
    -- or ||
    -- not (ALL lower-case letters)

Try the following boolean expressions:
True && False  
True && True  
False || True  
not False
not (True && True)
not True || True
not (True || True)
-}


{- Comparison

Comparison Operators:
    -- equal to ==
    -- not equal to /=
    -- less than <
    -- less than or equal to <=
    -- greater than >
    -- greater than or equal to >=

Try the following boolean expressions:
5 == 5  
1 == 0  
5 /= 5  
5 /= 4  
"hello" == "hello"
"hello" > "he"
"hello" >= "he"
"hello" < "hello world"
"hello" <= "hello world"
"aaa" > "b"
[1, 2, 3] > [2]
-}


{- Type Mismatch/Errors

Try the following expression: 5 + "hello"
ERROR: No instance for (Num [Char]) arising from a use of ‘+’
-- The error means that "hello" is not a number and so the compiler doesn't know
    how to add "hello" to 5.
-- Other examples with the same error: 5 + "four", 5 + "4", 5 + '4'
-- "hello", "four", and "4" are strings (enclosed in double quotes)
-- '4' is character (enclosed in single quotes)
-- A string is just a list of characters.

How about the following expression: 5 == True
ERROR: No instance for (Num Bool) arising from the literal ‘5’
    -- The error means that the types don't match.

ALL arithmetic operators expect both left and right sides to be NUMBERs.
ALL comparison operators expect both left and right sides to be the SAME type.

How about the following expression: 5 + 4.0
-- NO ERROR
-- 5 can act as an integer or a floating-point number.
-- 4.0 can only act as a floating-point number
-- the compiler adapts 5 as a floating-point number 5.0 and results 9.0
-}


{- Function Basics

infix VS prefix functions
-- infix functions: called by sandwiching function names between parameters
    -- e.g. arithmetic operators, comparison operators, &&, ||
    -- e.g. operator * is a function that takes two numbers and multiplies them
-- prefix functions: called by writing the function name, a space, and then
    the parameters that are separated by spaces
    -- Most functions that aren't used with numbers are prefix functions.

Some pre-defined functions:
-- succ: takes anything that has a defined successor and returns that successor
    -- e.g. succ 8
    -- e.g. succ 4.5
    -- e.g. succ 'a'
    -- e.g.  succ "a"   -- ERROR
-- min: take two things that can be put in an order (like numbers) and
    returns the one that's lesser
    -- e.g. min 9 10  
    -- e.g. min 3.4 3.2 
-- max: take two things that can be put in an order (like numbers) and
    returns the one that's greater
    -- e.g. max 100 101
    -- e.g. max "hello" "he"
    -- e.g. max [1, 2, 3] [2]
-- div: takes two integers and does INTEGER division between them
    -- e.g. div 92 10
    -- e.g. div 92 10.0 -- ERROR

Function application (calling a function) has the HIGHEST precedence.
-- e.g. the following two expressions are EQUIVALENT
    -- succ 9 + max 5 4 + 1  
    -- (succ 9) + (max 5 4) + 1
-- e.g. the following two expressions are NOT EQUIVALENT
    -- succ 9 * 10
    -- succ (9 * 10)

If a function takes TWO parameters, we can also call it as an INFIX function
by surrounding it with BACKTICKs.
    -- e.g. 92 `div` 10
    -- has clearer physical meaning: 92 is divided by 10

Common Mistakes: use parentheses to denote function application

In Haskell, DO NOT put parameters inside parentheses.
    -- e.g. bar (bar 3)
    -- It means that we first call the function $bar$ with 3 as the parameter
        to get some number, and then we call $bar$ again with that number.
-}


{- Define Our Own Functions -}

-- Write a function with name $doubleMe$ to take a number and multiplie it by two.
doubleMe x = x + x
doubleMe' x = x * 2
{-
Functions are defined in the following format:
    -- function name is followed by parameters seperated by spaces
    -- then there is a equal sign (=) 
    -- after = we define what the function does

Test: in GHCI, load this script and test $doubleMe$ with following cases
doubleMe 9
doubleMe 8.3

Analysis: Because + and * work on anything that can be considered as a number,
our $doubleMe$ function also works on any number.
-}

-- Write a function with name $doubleUs$ to take two numbers, multiplies each
--  by two, and then adds them together.
doubleUs x y = x * 2 + y * 2   
doubleUs' x y = x + x + y + y
{-
Test: in GHCI, load this script and test $doubleUs$ with following cases
doubleUs 4 9  
doubleUs 2.3 34.2  
doubleUs 28 88 + doubleMe 123
-}

doubleUs'' x y = doubleMe x + doubleMe y
{-
The $doubleUs''$ function definition calls our own $doubleMe$ function.

Common Pattern of Haskell Programms:
Making BASIC functions that are obviously correct and then COMBINing them
into more COMPLEX functions.
    -- This way, we can avoid repetition.

Function definitions in Haskell do NOT have to be in any particular order,
so we can define $doubleMe$ BEFORE or AFTER the definition of $doubleUs''$.
    -- show example: $doubleMe$ after $doubleUs''$

Test: in GHCI, load this script and test $doubleUs''$ with following cases
doubleUs'' 4 9  
doubleUs'' 2.3 34.2  
doubleUs'' 28 88 + doubleMe 123
-}

-- Write a function with name $doubleSmallNumber$ to take a number and
--  multiplie it by two only if that number is smaller than or equal to 100.
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2
doubleSmallNumber' x = 
                    if x > 100
                    then x
                    else x * 2
{-
Haskell if statement: it is an EXPRESSION
    -- An expression is basically a piece of code that returns a VALUE.
        -- e.g. $5$, $4 + 8$, $x + y$
    -- An $if$ statement will ALWAYS return something.
    -- The $else$ part is MANDATORY, because every expression and
        function MUST return something.

Test: in GHCI, load this script and test $doubleSmallNumber$ with following cases
doubleSmallNumber 90
doubleSmallNumber 100
doubleSmallNumber 110
-}

doubleSmallNumber'' x = (if x > 100 then x else x * 2) + 1
doubleSmallNumber''' x = if x > 100 then x else x * 2 + 1
{-
doubleSmallNumber'': add 1 to every number that's produced by $doubleSmallNumber$

doubleSmallNumber''': add 1 to the number that's produced by $doubleSmallNumber$
    ONLY IF $x$ is not greater than 100

Rules of function names:
    -- MUST begin with lowercase letters or underscore
    -- can NOT begin with uppercase letters
    -- can NOT begin with '

Note that we usually use ' to either denote a strict version of
    a function (one that isn't lazy) or a slightly modified version
    of a function.
-}

{-
A function WITHOUT any parameters is usually called a definition (or a name).
    -- e.g. $a = 4$ gives a function with name $a$, the $a$ function takes NO
        parameter and ALWAYS returns number 4.
        (Haskell do NOT think $a$ as a variable)
-}