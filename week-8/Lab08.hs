{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- Lab 08: Types and Functions (Pattern Matching) [10 POINTS] -}

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
module Lab08 where




{-
Q1 [2 POINTS]:
Write a function named $percent$ (with type $Int -> Int -> Float$) that
takes two integers $m$ and $n$ and returns the percentage of $m/n$.

e.g. $percent 3 4$ returns $75.0$
-}
percent :: Int -> Int -> Float
-- YOUR CODE START HERE
percent m n = fromIntegral m / fromIntegral n * 100
-- YOUR CODE END HERE


{-
Q2 [2 POINTS]:
Write a function named $positions$ (with type $Eq a => a -> [a] -> [Int]$) that
takes one value with type $a$ and a list of $a$ type elements, where tyep $a$
is a member of $Eq$ typeclass. The function returns a list of positions in which
the input value appears in the input list. If the input value does not appear in
the input list, the function returns an empty list.

Note that the element position starts at 1 in a list.

e.g. $positions 3 [1, 2, 3, 4, 3]$ returns $[3, 5]$
e.g. $positions 7 [1, 2, 3, 4, 3]$ returns $[]$
e.g. $positions 3 []$ returns $[]$
e.g. $positions 'c' "abcd"$ returns $[3]$

Hints:
-- you may use list comprehension
-- you may also use $zip$ and pattern matching inside list comprehension
-}
positions :: Eq a => a -> [a] -> [Int]
-- YOUR CODE START HERE
positions _ [] = []
positions x xs = [index | (index,value) <- zip [1..] xs, value == x]
-- YOUR CODE END HERE


{-
Q3 [2 POINTS]:
Write a function named $search$ (with type $Int -> [Int] -> Bool$) that takes
an Int and a list of Ints. The function searches the list for the input Int and
returns a boolean that indicates whether the Int is in the list.

REQUIREMENTS:
-- MUST use pattern matching and recursion
-- do NOT use the pre-defined $elem$ function

e.g. $search 3 [1, 2, 3, 4, 5]$ returns $True$
e.g. $search 3 []$ returns $False$

Hints:
-- you may use if statement in the function body
-}
search :: Int -> [Int] -> Bool
-- YOUR CODE START HERE
search _ [] = False
search x (h:xs) = (h == x) || search x xs

-- YOUR CODE END HERE


{-
Q4 [2 POINTS]:
Write a function named $removeDups$ (with type $[Int] -> [Int]$) that takes
a SORTED list of Ints and removes duplicates from the SORTED list.

REQUIREMENTS:
-- MUST use pattern matching and recursion

e.g. $removeDups [1,1,2,3,3,4,5]$ returns $[1,2,3,4,5]$
e.g. $removeDups [1]$ returns $[1]$
e.g. $removeDups []$ returns $[]$

Hints:
-- the input list is SORTED, using SORTED features make the problem easier
-- you may use if statement in the function body
-}
removeDups :: [Int] -> [Int]
-- YOUR CODE START HERE
removeDups [] = []
removeDups [x] = [x]
-- if first two are the same then ignore the first and compare 2nd with the following.
-- else add the two to the new list.
removeDups (x:y:xs) = if x == y then removeDups (y:xs)
                        else x : y : removeDups xs
-- YOUR CODE END HERE


{-
Q5 [2 POINTS]:
Write a function named $unpack$ (with type $[[Int]] -> [Int]$) that takes
a list of lists of Ints and returns a list of Ints, in which the elements
of the inner lists are "unpacked" and placed into the list you are returning.

REQUIREMENTS:
-- MUST use pattern matching and recursion

e.g. $unpack [[1,2],[3,4]]$ returns $[1,2,3,4]$
e.g. $unpack [[1,2], [3,4,5], [4]]$ returns $[1,2,3,4,5,4]$
e.g. $unpack [[1,2], [], [4]]$ returns $[1,2,4]$
e.g. $unpack [[]]$ returns $[]$
e.g. $unpack []$ returns $[]$

Hints:
-- you may use the ++ operator
-}
unpack :: [[Int]] -> [Int]
-- YOUR CODE START HERE
unpack [] = []
unpack [[]] = []
unpack xs = head xs ++ unpack (drop 1 xs)  
-- YOUR CODE END HERE
