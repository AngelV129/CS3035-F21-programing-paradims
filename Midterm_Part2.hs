{- Midterm Exam Part 2: Haskell Programming [10 POINTS] -}

{-
Please solve the problems with the following steps:
-- (1) REPLACE each function's pre-implementation (i.e. the existing code between
    the line "-- YOUR CODE START HERE" and the line "-- YOUR CODE END HERE")
    with your own implementation/code.
    -- BEFORE writting your code, you MUST DELETE these pre-implementations, otherwise
        you will get compiling ERRORs.
    -- Note that the pre-implementations JUST ensures no compiling errors if you
        MISS any function's implementation.
-- (2) Submit the MODIFIED file in Canvas. Please do NOT change the file name.

Please ONLY TOUCH/CHANGE the code between the line "-- YOUR CODE START HERE"
and the line "-- YOUR CODE END HERE" after each problem description.

Please do NOT change the name and type signature of the functions in each problem.
We will use a grading program to automatically load and grade your submission.
If you CHANGE the function name and/or type signature, the grading program can
NOT load your submission and you will get ZERO for this assignment.

Your submitted file MUST be compilable (i.e. NO compiling errors),
otherwise you will get ZERO.
If you have some uncompilable code, I recommend COMMENTing them. 

After implementing these functions, you should figure out some test cases
and TEST your implementation thoroughly. The testing procedure is as follows:
-- Open a command prompt and locate to the folder of your Haskell script
-- Enter the GHC interactive environment by typing $ghci$
-- Load your Haskell script in GHCI by typing $:l YOURSCRIPTFILENAME$
    -- after modifying your script, type $:r$ in GHCI to re-load the latest version
-- Test your implementation by calling corresponding functions with your test cases
-}


-- Please do NOT DELETE nor CHANGE the following line.
-- It is used by the grading program to load your submission.
module Midterm_Part2 where


{-
Q1 [2.5 POINTS]:
Write a function named $doubleTuple$ (with type $Int -> [(Int, Int)]$) that
uses a LIST COMPREHENSION to produce a list from 1 to n (n is the input parameter)
of tuples of two Ints where the first value is twice the second value.
Assume n is greater than 1.

REQUIREMENTS:
-- MUST use LIST COMPREHENSION, otherwise you will get ZERO for this question

e.g. $doubleTuple 3$ returns $[(2,1),(4,2),(6,3)]$
e.g. $doubleTuple 5$ returns $[(2,1),(4,2),(6,3),(8,4),(10,5)]$
-}
doubleTuple :: Int -> [(Int, Int)]
-- YOUR CODE START HERE
doubleTuple n = [( 2*x, n) | (x,n) <- zip [1..] [1..n] ]
-- YOUR CODE END HERE


{-
Q2 [2.5 POINTS]:
Write a function named $phrases$ (with type $[[Char]]$) that
uses a LIST COMPREHENSION to generate all the possible pairs of
["smart", "funny", "hungry"] and ["cat", "dog",  "lizard"].
These phrases can be in any order. Do NOT hard-code the result, 
use list comprehension to generate it.

The result should ALWAYS be (with spaces between two words):
["smart cat", "funny cat", "hungry cat", "smart dog", "funny dog", "hungry dog",
"smart lizard", "funny lizard",  "hungry lizard"].

REQUIREMENTS:
-- MUST use LIST COMPREHENSION, otherwise you will get ZERO for this question

Note: the function does NOT take any parameters
-}
phrases :: [[Char]]
-- YOUR CODE START HERE
phrases = [left++ " " ++ right | left <- ["smart", "funny", "hungry"], right <- ["cat", "dog",  "lizard"]]
-- YOUR CODE END HERE


{-
Q3 [2.5 POINTS]:
Using RECURSION, write a function named $sumOfMultiples$ (with type $[Int] -> [Int] -> Int$) that
takes two lists of Int and sums the multiples of the corresponding elements in the list.
If one list is longer than the other, discard the unpaired elements of the longer list.
The $sumOfMultiples$ of empty list is defined as 0.

REQUIREMENTS:
-- MUST use RECURSION, otherwise you will get ZERO for this question
-- do NOT use $zip$ function, otherwise you will get ZERO for this question
-- do NOT use LIST COMPREHENSION, otherwise you will get ZERO for this question

e.g. $sumOfMultiples [1,2] [3,4]$ returns $11$ (1*3 + 2*4 = 11)
e.g. $sumOfMultiples [1,2] [3,4,5]$ returns $11$ (1*3 + 2*4 = 11)
e.g. $sumOfMultiples [1,2] []$ returns $0$



-}
sumOfMultiples :: [Int] -> [Int] -> Int
-- YOUR CODE START HERE
sumOfMultiples [] [] = 0
sumOfMultiples xs [] = 0
sumOfMultiples [] ys = 0

sumOfMultiples (x:xs) (y:ys) = (x * y) + (sumOfMultiples xs ys)
-- YOUR CODE END HERE


{-
Q4 [2.5 POINTS]:
Using PATTERNs or GUARDs, write a function named $romanNums$ (with type $Int -> String$) that
takes an Int. If the input number is between 1 and 5 (inclusive), the function produces a string
that contains the corresponding roman numeral. For all other numbers, the function returns the
input number itself as a STRING (hint: $show$ function may be relevant).

REQUIREMENTS:
-- MUST use PATTERNs or GUARDs, otherwise you will get ZERO for this question

e.g. $romanNums 1$ returns "I"
e.g. $romanNums 2$ returns "II"
e.g. $romanNums 3$ returns "III"
e.g. $romanNums 4$ returns "IV"
e.g. $romanNums 5$ returns "V"
e.g. $romanNums 0$ returns "0"
e.g. $romanNums (-1)$ returns "-1"
e.g. $romanNums 6$ returns "6"
-}
romanNums :: Int -> String
-- YOUR CODE START HERE
romanNums n  
    | n == 1 = "I"
    | n == 2 = "II"
    | n == 3 = "III"
    | n == 4 = "IV"
    | n == 5 = "V"
    | otherwise = show n
-- YOUR CODE END HERE
