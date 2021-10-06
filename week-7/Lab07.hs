{- Lab 07: Haskell List Comprehensions and Tuples [10 POINTS] -}

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
module Lab07 where




{-
Q1 [2 POINTS]:
Write a function named $doubleIt$ (with type $[Double] -> [Double]$) that
takes a list of Double and returns a list of Double in which each number is
doubled.

e.g. $doubleIt [1.0, 2.5]$ returns $[2.0, 5.0]$
-}
doubleIt :: [Double] -> [Double]
-- YOUR CODE START HERE
doubleIt xs = []
-- YOUR CODE END HERE


{-
Q2 [2 POINTS]:
Write a function named $doubleOdd$ (with type $[Int] -> [Int]$) that takes
a list of Int and returns a list of Int that doubles all the odd numbers.
If a number isn't odd, we throw it out of the list.

e.g. $doubleOdd [1, 2, 3, 4, 5]$ returns $[2, 6, 10]$
e.g. $doubleOdd [2, 4, 6]$ returns $[]$
-}
doubleOdd :: [Int] -> [Int]
-- YOUR CODE START HERE
doubleOdd xs = []
-- YOUR CODE END HERE


{-
Q3 [2 POINTS]:
Write a function named $diffSquared$ (with type $[Double] -> [Double] -> Double$)
that takes two lists of Double and returns a Double that is
$(sum of all (n1 - n2)^2) / M$, where $n1$ is an element from the first list,
$n2$ is an element from the second list such that $n1$ and $n2$ have the
same index, and $M$ is the length of the shorter list.

e.g. $diffSquared [1, 2, 3, 4, 5] [7, 5, 2]$ returns $15.333333333333334$
Notes:
-- The aucual results of the example is $46.0/3$ which contains INFINITE 3s
    in the fractional part.
-- The floating-point numbers' precision is machine dependent.
-- Hence, your running result may NOT be $15.333333333333334$ exactly, but with
    a small error.

Hints:
(1) You may use the $length$ function to calculate $M$;
(2) The $length$ returns value belongs to $Integral$ typeclass, but the division
    operator / requires that both operands are values belonging to $Num$ typeclass.
    (We will introduce more details about typeclass in the next lecture).
(3) Hence, you need to convert $M$ from any $Integral$ type into any $Num$ type
    using the function $fromIntegral$.
    -- e.g. $let x = length [1,2,3]$    -- x :: Int
    -- e.g. $let y = fromIntegral (length [1,2,3])$ -- y :: Num b => b
    -- e.g. $(sum [1,2,3]) / (length [1,2,3])$  -- ERROR
    -- e.g. $(sum [1,2,3]) / (fromIntegral (length [1,2,3]))$  -- result is 2.0
-}
diffSquared :: [Double] -> [Double] -> Double
-- YOUR CODE START HERE
diffSquared xs ys = 0.0
-- YOUR CODE END HERE


{-
Q4 [2 POINTS]:
Write a function named $largers$ (with type $[Int] -> [Int] -> [Int]$) that
takes two lists of Int and returns a list of Int which has the larger of the
corresponding elements (with the same index) from the two lists. If the two
corresponding elements are equal, choose either. The length of the new list
is the same with the shorter input list.

e.g. $largers [1, 5, 3] [2, 3, 2]$ returns $[2, 5, 3]$
e.g. $largers [1, 5, 3] [2, 3, 2, 20]$ returns $[2, 5, 3]$
-}
largers :: [Int] -> [Int] -> [Int]
-- YOUR CODE START HERE
largers xs ys = []
-- YOUR CODE END HERE


{-
Q5 [2 POINTS]:
Write a function named $twoAhead$ (with type $[Int] -> [Int]$) that takes
a list of Int and returns a list of Int which consists of the difference
between an element $x$ of the input list and the element that is 2-position
ahead of $x$. If there is no element 2-position ahead, then don't put
anything in the new list.

e.g. $twoAhead [1, 3, 2, 5, 6]$ returns $[1, 2, 4]$
-}
twoAhead :: [Int] -> [Int]
-- YOUR CODE START HERE
twoAhead xs = []
-- YOUR CODE END HERE
