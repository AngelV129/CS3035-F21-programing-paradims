{- Lecture 05-1: Introduction to Haskell -}


{-
Resource/Textbook:
-- Learn You a Haskell for Great Good
-- http://learnyouahaskell.com/
-- Chapter 1: Introduction

Good Haskell Programming Style:
-- https://www.seas.upenn.edu/~cis194/fall16/style.html

INDENTATION is important in Haskell programs, because Haskell relies on
indentation to reduce the verbosity of your code. Some syntax ERRORS may
be caused by indentation issues.
-- Indentation Rules: https://en.wikibooks.org/wiki/Haskell/Indentation
-}


{-
Haskell Comments:
-- single line comments: start by two hyphens, until the end of the line
    -- e.g. line 28
-- block comments: enclosed in curly brace/hyphen pairs, may span multiple lines
    -- e.g. lines 29-32
-}

-- THIS IS A SINGLE LINE COMMENT
{-
THIS IS A BLOCK COMMENT
THIS LINE CONTINUES THE BLOCK COMMENT
-}


{-
Haskell is a purely FUNCTIONAL programming language.

-- Imperative Languages: tell HOW to do (give a sequence of tasks)
    -- executing tasks can change state
        -- e.g. set variable $a$* to 5, do some stuff, set $a$ to 10
    -- have control flow structures
        -- e.g. if-else, switch, while, for, etc.

-- Functional Languages: describe WHAT stuff is, NOT how to do
    -- express stuff  in the form of FUNCTIONs
        -- e.g. the factorial of a number is the product of
            all the numbers from 1 to that number:q
            ghc -o hello hello.hs
        -- e.g. the sum of a list of numbers is the first number
            plus the sum of all the other numbers
    -- a function does NOT have side-effects (such as change state)
        -- e.g. can NOT set variable $a$ to 5, and then set $a$ to 10 later
    -- The ONLY thing a function can do is CALCULATE something and
        RETURN it as a RESULT.
    -- Referential Transparency: if a function is called twice with
        the SAME PARAMETERs, it's guaranteed to return the SAME RESULT
        -- allow the compiler to reason about the program's behavior
        -- allow programmers to easily deduce (and even prove) that
            a function is correct
        -- allow programmers to build more complex functions by
            gluing simple functions together


* In lecture notes, within text descriptions (in Haskell comments),
we put variable/function names and source code inside $ signs
to differentiate them with text descriptions. Note that this is
JUST MY style, NOT Haskell style.  
-}

{-
Haskell is LAZY.

LAZY EVALUATION means that Haskell will NOT execute functions and
calculate things until it's really FORCED to show a result.
    -- go well with referential transparency
    -- think programs as a series of TRANSFORMATIONS ON DATA
        -- first take some initial data, then efficiently transform and mend data,
            finally resemble what you want at the end
    -- allow INFINITE data structures
        -- show examples when introducing list
-}

{-
Haskell is STATICALLY typed.

When you compile your program, the compiler knows which piece of code
is a number, which is a string, and so on. Hence, a lot of possible
errors can be caught at the compiling time. For instance, you will
get a compiling error when adding a number and a string.

Haskell uses a very good type system that has TYPE INFERENCE.
    -- You do NOT have to explicitly label every piece of code with a type,
        the type system can intelligently figure it out.
        -- e.g. If you say $a = 5 + 4$, you don't have to tell Haskell that
            $a$ is a number, it can figure that out by itself.
    -- Type inference also allows your code to be more GENERAL.
        -- e.g. If a function takes two parameters and adds them together,
         and you don't explicitly state their type, the function will work 
         on any two parameters that act like numbers (such as Int, Integer,
         Float, Double).
-}

{-
Haskell is elegant and concise.

Using a lot of high level concepts, Haskell programs are usually
shorter than their imperative equivalents.
Shorter programs are easier to maintain than longer ones and have less bugs.
-}