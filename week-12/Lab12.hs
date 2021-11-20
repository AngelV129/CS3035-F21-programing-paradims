{- Lab 12: Higher-Order Functions [10 POINTS] -}

{-
You MUST solve each problem using "map", "filter", "foldr", or "fold",
otherwise you will get ZERO.

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
module Lab12 where




{-
You MUST solve each problem using "map", "filter", "foldr", or "fold",
otherwise you will get ZERO.
-}

{-
Q1 [2.5 POINTS]:
Implement the "limitRange" function.

limitRange : takes a list of numbers and a pair of numbers,
              returns a list of numbers which are in the input list
              and within the range (inclusive) given by the input
              pair.
              The elements in the output list must be in the same order
              they appeared in the input list.

If the input pair is (n1,n2), we assume that n1 is the LOWER bound
of the range and n2 is the UPPER bound of the range.

Examples:
            limitRange [1,3,4,5,2] (1,3) == [1,3,2]
            limitRange [1,3,4,5,2] (3,1) == []
-}

limitRange :: Ord a => [a] -> (a, a) -> [a]
-- YOUR CODE START HERE
limitRange xs (a,b) = filter (<= b) $ filter (>=a) (xs)
-- YOUR CODE END HERE

{-
Test your implementation: all following expressions shoud return "True"

limitRange [1,3,4,5,2] (1,3) == [1,3,2]
limitRange [1,3,4,5,2] (3,1) == []
limitRange [1,2,3,4,5] (2,4) == [2,3,4]
limitRange [1,2,3,4,5] (4,2) == []
limitRange [] (0,0) == []
limitRange [(-1),0,1,2,3] ((-1),1) == [(-1),0,1]
limitRange [1.1,2.2,3.3,4.4,5.5] (2.1,5.2) == [2.2,3.3,4.4]
limitRange [1.1,2.2,3.3,4.4,5.5] (5.2,2.1) == []
-}




{-
Q2 [2.5 POINTS]:

For Q2 (min2), you can use fold to return a pair (smallestNumber, secondSmallestNumber), i.e., 
the accumulator is (smallestNumber, secondSmallestNumber). Then just get the second component of the pair.

To get the second-smallest elements, we must also know/remember the smallest element. Without the smallest, we 
cannot say which is the second-smallest.


Implement the "min2" function.

min2: takes a list of numbers,
      returns the second-smallest number of the input list          

Note that if a list contains DUPLICATES, the second-smallest 
number and the smallest number may be identical; 
your code should return it.

We also assume that all input lists contain at least TWO numbers.

Examples:
            min2 [2110, 4820, 3110, 4120] == 3110
            min2 [2110, 4820, 2110, 4120] == 2110
-}

min2 :: Ord a => [a] -> a
-- YOUR CODE START HERE

-- YOUR CODE END HERE

{-
Test your implementation: all following expressions shoud return "True"

min2 [2110, 4820, 3110, 4120] == 3110
min2 [2110, 4820, 2110, 4120] == 2110
min2 [1.2, 2.3, 1.2, 1.2] == 1.2
-}



{-
Q3 [2.5 POINTS]:
Implement the "flatten" function.

flatten: takes a list of lists,
         returns a one-dimensional list with all of
         the elements in the order they appeared in
         the input list.

Examples:
            flatten [[1,2,3], [], [0], [4,5]] == [1,2,3,0,4,5]
-}

flatten :: [[a]] -> [a]
-- YOUR CODE START HERE
flatten xs = foldl ( ++ ) [] xs
-- YOUR CODE END HERE

{-
Test your implementation: all following expressions shoud return "True"

flatten [[]] == []
flatten [[2,3]] == [2,3]
flatten [[1,2,3], [], [0], [4,5]] == [1,2,3,0,4,5]
flatten [[1,2,3], [], [4], [5,6]] == [1,2,3,4,5,6]
flatten [[], ['e','d'], ['a','b','c']] == ['e','d','a','b','c']
flatten [[1,2,3], [4,5], [6], [], [7,8,9], []] == [1,2,3,4,5,6,7,8,9]
-}




{-
Q4 [2.5 POINTS]:
Implement the "sumRows" function.

sumRows: takes a list of number lists ((call an internal list a "row")),
         returns a one-dimensional list of numbers with each number
         eaual to the sum of the corresponding row in the input list.

Examples:
            sumRows [[1,2], [3,4]] == [3, 7]
            sumRows [[],[],[1]] == [0,0,1]
            sumRows [[]] == [0]
-}

sumRows :: Num a => [[a]] -> [a]
-- YOUR CODE START HERE
sumRows = map sum
-- sumRows xs = map sum xs
-- sumRows xs = map (\ x -> sum x) xs
-- sumRows xs = foldr (\x acc -> sum x : acc ) [] xs
-- YOUR CODE END HERE

{-
Test your implementation: all following expressions shoud return "True"

sumRows [[1,2], [3,4]] == [3, 7]
sumRows [[],[],[1]] == [0,0,1]
sumRows [[]] == [0]
sumRows [[1,2,3],[2,2,2],[3,3,3]] == [6,6,9]
-}