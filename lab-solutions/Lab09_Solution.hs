{- Lab 09: Functions and Recursions [15 POINTS] -}

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
module Lab09_Solution where




{-
Q1 [2 POINTS]:
Using GUARDs to write a function named $generations$ (with type $(Integral a) => a -> String$)
that takes an $age$, and returns which generation they belong to.
The generations are as follows:
-- If the person was born after 1995, they are "Generation Z".
-- Else if the person was born after 1980, they are "Millennial".
-- Else if they were born after 1965 they are "Generation X".
-- Else if they were born after 1945 they are "Baby Boomer".
-- Else if they were born after 1933 they are "The Silent Generation".
-- Otherwise they are "The Greatest Generation".

REQUIREMENTS:
-- Use $where$ to convert $age$ to birth year and use it.

e.g. $generations 17$ returns "Generation Z"
e.g. $generations 32$ returns "Millennial"
-}
generations :: (Integral a) => a -> String
-- YOUR CODE START HERE
generations age
    | year >= 1995 = "Generation Z"
    | year >= 1980 = "Millenial"
    | year >= 1965 = "Generation X"
    | year >= 1945 = "Baby Boomer"
    | year >= 1933 = "The Silent Generation"
    | otherwise    = "The Greatest Generation"   
    where year = 2021 - age
-- YOUR CODE END HERE


{-
Q2 [2 POINTS]:
Write a function named $byPi$ (with type $Float -> [Float] -> [Float]$) that
takes a Float $a$ and a list of Floats. Use LIST COMPREHENSION to multiply each
value in the list by $pi * a$, where $a$ is the Float passed in.

REQUIREMENTS:
-- Use $let$ to get $pi * a$

e.g. $byPi 0.5 [1.0, 2.0, 3.0]$ returns $[1.5707963267948966,3.141592653589793,4.71238898038469]$
Notes:
-- The floating-point numbers' precision is machine dependent.
-- Hence, your running result may NOT be $[1.5707963267948966,3.141592653589793,4.71238898038469]$
    exactly, but with a small error.
-}
byPi :: Float -> [Float] -> [Float]
-- YOUR CODE START HERE
byPi a xs = [x * factor | x <- xs, let factor = pi * a]
-- YOUR CODE END HERE


{-
Q3 [3 POINTS]:
Implement a search algorithm (function $findPrev :: Int -> [Int] -> Int$) that
searches through a list for Int $n$ and, returns the value in the list
TWO positions before $n$. If there is no such value (either because $n$ isn't
in the list, or there is no number two positions before $n$), return $-1$.

e.g. $findPrev 5 [1, 2, 3, 4, 5, 6]$ returns $3$
e.g. $findPrev 5 [1, 2, 3]$ returns $-1$
e.g. $findPrev 5 [0, 5, 20, 30]$ returns $-1$
-}
findPrev :: Int -> [Int] -> Int
-- YOUR CODE START HERE
findPrev _ [] = -1
findPrev _ [x] = -1
findPrev _ (x:y:[]) = -1
findPrev n (x:y:z:xs)
    | n == z    = x
    | otherwise = findPrev n (y:z:xs)
-- YOUR CODE END HERE


{-
Q4 [4 POINTS]:
Implement the INSERTION SORT algorithm (https://www.geeksforgeeks.org/insertion-sort/).
In particular, write a function $insertionSort :: [Int] -> [Int]$ to sort a
list of Int in the DESCENDING order.

REQUIREMENTS:
-- Please do NOT use the $reverse$ function pre-defined in Haskell library
    NOR the $reverse'$ function in the lecture note.

e.g. $insertionSort [3,1,5,1]$ returns $[5,3,1,1]$
e.g. $insertionSort []$ returns $[]$

Hints:
-- Think the indertion sort FUNCTIONALLY and RECURSIVELY
-- You can create/use any helper functions you need, provide that you wrote it
    yourself, or it's built into Haskell.
-}
insertionSort :: [Int] -> [Int]
-- YOUR CODE START HERE
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x >= y     = x:y:ys
    | otherwise = y : (insert x ys)
-- YOUR CODE END HERE


{-
Q5 [4 POINTS]:
Implement the SELECTION SORT algorithm (https://www.geeksforgeeks.org/selection-sort/).
In particular, write a function $selectionSort :: [Int] -> [Int]$ to sort a
list of Int in the ASCENDING order.

Here is a short description of the selection sort:
-- find the minimum Int and put it at the beginning of the list
-- then recursively sort the remainder of the list

e.g. $selectionSort [3,1,5,1]$ returns $[1,1,3,5]$
e.g. $selectionSort []$ returns $[]$

Hints:
-- Think the selection sort FUNCTIONALLY and RECURSIVELY
-- You can create/use any helper functions you need, provide that you wrote it
    yourself, or it's built into Haskell.
-}
selectionSort :: [Int] -> [Int]
-- YOUR CODE START HERE
selectionSort [] = []
selectionSort xs = minimum xs : (selectionSort (removeMin xs))

removeMin :: [Int] -> [Int]
removeMin [] = []
removeMin [x] = []
removeMin (x:xs)
    | x == smallest = xs
    | otherwise = x : (removeMin xs)
    where smallest = minimum (x:xs)
-- YOUR CODE END HERE

