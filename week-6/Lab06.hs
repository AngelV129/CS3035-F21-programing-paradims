{- Lab 06: Haskell List Basics [20 POINTS] -}

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
module Lab06 where




{-
Q1 [2 POINTS]:
Write a function named $greet$ (with type $String -> String$ ) that takes a
person's name and says "Hi " to that person. If the name provided is an empty
string, the $greet$ function should return "Hi there".

e.g. $greet ""$ returns "Hi there"
e.g. $greet "Haskell"$ returns "Hi Haskell"
Note that the Haskell language is named after a logician, Haskell Curry.

Hint:
To test a string for emptiness, use the $null$ function.
-}
greet :: String -> String
-- YOUR CODE START HERE
greet s = if null s then "Hi there"
            else "Hi " ++ s
-- YOUR CODE END HERE


{-
Q2 [2 POINTS]:
Write a function named $palindrome$ (with type $String -> Bool$) that takes a
string and checks whether the string is a palindrome or not. (A palindrome is
a word, like "level" or "racecar", that reads the same forward or backward.)

e.g. $palindrome "level"$ returns $True$
e.g. $palindrome "hello"$ returns $False$

Hint:
The $reverse$ function is helpful.
-}
palindrome :: String -> Bool
-- YOUR CODE START HERE
palindrome s = s == reverse s
-- YOUR CODE END HERE


{-
Q3 [2 POINTS]:
Write a function named $range$ (with type $Int -> Int -> [Int]$) that takes
two integers and creates a list containing all integers between the two given
integers. The list's head is the first given integer, and the list's last element
is the second given integer.

e.g. $range 4 9$ returns $[4,5,6,7,8,9]$
e.g. $range 9 4$ returns $[9,8,7,6,5,4]$
e.g. $range 4 4$ returns $[4]$
-}
range :: Int -> Int -> [Int]
-- YOUR CODE START HERE
range x y = if x < y then [x..y]
            else reverse ([y..x])
-- YOUR CODE END HERE


{-
Q4 [2 POINTS]:
Write a function named $butLast$ (with type $[a] -> a$) that takes a list
of numbers $xs$ and returns the last but one element of the list $xs$
(i.e., the second last element).

Assumption:
(1) the input list $xs$ contains at least two numbers

e.g. $butLast [1,2,3,4]$ returns $3$

-}
butLast :: Num a => [a] -> a
-- YOUR CODE START HERE
butLast xs = init xs !! (length (init xs) -1)
-- YOUR CODE END HERE


{-
Q5 [2 POINTS]:
Write a function named $elementAt$ (with type $[a] -> Int -> a$) that takes
a list of numbers $xs$ and an integer $k$ (representing element position).
The $elementAt$ function returns the k'th element of the list $xs$ (i.e., the
element at position $k$).

Note that the element position starts at 1.

Assumption:
(1) the input list $xs$ contains at least one number
 

e.g. $elementAt [1,2,3] 2$ returns $2$
-}
elementAt :: Num a => [a] -> Int -> a
-- YOUR CODE START HERE
elementAt xs k = xs !! (k - 1)
-- YOUR CODE END HERE


{-
Q6 [3 POINTS]:
Write a function named $slice$ (with type $[a] -> Int -> Int -> [a]$) that takes
a list $xs$ and two integers $i$ and $k$ (representing element position).
The $slice$ function returns a list containing the elements between the i'th
and k'th element of the original list $xs$ (both limits included). 

Note that the element position starts at 1.

Assumption:
(1) the input list $xs$ contains at least one elements
(2) the input integers $i$ and $k$ are valid,
    i.e. 1 <= i <= list_length && 1 <= k <= list_length && i <= k

e.g. $slice [1,2,3,4,5,6,7,8,9,10] 3 7$ returns $[3,4,5,6,7]$
e.g. $slice "haskell" 2 5$ returns "aske"
e.g. $slice [1,2,3,4,5] 3 3$ returns $[3]$
e.g. $slice [1] 1 1$ returns $[1]$

Hint:
The $take$ and $drop$ functions are helpful.
-}
slice :: [a] -> Int -> Int -> [a]
-- YOUR CODE START HERE
slice xs i k = if length xs == 1 then xs else drop (i - 1) (take k xs)
-- YOUR CODE END HERE


{-
Q7 [3 POINTS]:
Write a function named $removeAt$ (with type $[a] -> Int -> [a]$) that takes
a list $xs$ and an integer $k$ (representing element position).
The $removeAt$ function removes the k'th element from the list $xs$.

Note that the element position starts at 1.

Assumption:
(1) the input list $xs$ contains at least one elements
(2) the input integer $k$ is valid, i.e. 1 <= k <= list_length

e.g. $removeAt [1,2,3,4,5] 3$ returns $[1,2,4,5]$
e.g. $removeAt "haskell" 2$ returns "hskell"
e.g. $removeAt [1] 1$ returns $[]$

Hint:
The $take$ and $drop$ functions are helpful.
-}
removeAt :: [a] -> Int -> [a]
-- YOUR CODE START HERE
removeAt xs k = if length xs == 1 then xs else init (take k xs) ++ drop k (xs)
-- YOUR CODE END HERE


{-
Q8 [4 POINTS]:
Write a function named $insertAt$ (with type $[a] -> a -> Int -> [a]$) that takes
a list $xs$, an element $x$, and an integer $k$ (representing element position).
The $insertAt$ function inserts the element $x$ at position $k$ into the list $xs$ 
(i.e., after insertion, the k's element of the new list is $x$).

Note that the element position starts at 1.

Assumption:
(1) the element $x$ is valid, i.e. $x$ has the same type with
    existing elements in $xs$
(2) the input integer $k$ is valid, i.e. 1 <= k <= (list_length + 1)

e.g. $insertAt [1,2,3,4,5] 7 6$ returns $[1,2,3,4,5,7]$
e.g. $insertAt "hskell" 'a' 2$ returns "haskell"
e.g. $insertAt [] 3 1$ returns $[3]$
e.g. $insertAt [] 'h' 1$ returns "h"

Hint:
The $take$ and $drop$ functions are helpful.
-}
insertAt :: [a] -> a -> Int -> [a]
-- YOUR CODE START HERE
insertAt xs x k = []
-- YOUR CODE END HERE