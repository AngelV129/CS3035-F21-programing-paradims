{- Higher-Order Functions 1: Understanding -}

{-
What is a higher order function?
A higher order function either take functions as parameters
or return functions as results.

Why are higher-order functions useful/important?
-- necessary when we define computations by specifying
    what stuff is instead of how to calculate (imperative programming)
-- a really powerful way of solving problems and thinking about programs.
-}




{- 1. Curried Functions -}

{-
Feature/Fact of Haskell Function:
Every function in Haskell officially ONLY takes ONE parameter.

However, we have defined and used functions with MORE THAN ONE
parameters previously.
e.g. "max 4 5" looks like taking two parameters

Curried Functions (Syntatic Sugar):
-- functions that accept several parameters
-- can be partially applied
-}

{-
Example:
max 4 5

Type of "max":
max :: (Ord a) => a -> a -> a
i.e., "max" LOOKs LIKE taking two "a"s and returning an "a"
OR
max :: (Ord a) => a -> (a -> a)
i.e. "max" takes an "a" and returns a FUNCTION
which takes an "a" and returns an "a"

Evaluation Steps of "max 4 5":
(1) apply "max" with "4" and creates a function "max4 = max 4"
which takes ONE parameter
(2) apply function "max4" with "5"
(Note: "max4" is just a SAMPLE function name)

Type of "max4" and "max 4":
max4 :: (Ord a, Num a) => a -> a
max 4 :: (Ord a, Num a) => a -> a
i.e., "max4" and "max 4" take an "a" and returns an "a"

Hence, the following two calls are equivalent:
max 4 5
(max 4) 5
-}

{-
Another Example:
-}
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z

{-
Type of "multThree":
multThree :: (Num a) => a -> (a -> (a -> a))
i.e., "multThree" takes an "a" and returns a "(Num a) => a -> (a -> a)" function (suppose we call it "multTwo")
multTwo :: (Num a) => a -> (a -> a)
i.e., "multTwo" takes an "a" and returns a "(Num a) => a -> a" function (suppose we call it "multOne")
multOne :: (Num a) => a -> a
i.e., "multOne" takes an "a" and returns an "a"

"multThree 3 5 9" is equivalent with "((multThree 3) 5) 9"
-}

{-
Evaluation Steps of "((multThree 3) 5) 9":
(1) apply "multThree" with "3" and creates a function which
takes one parameter and returns a function
-}
multTwo = multThree 3
{-
(2) apply "multTwo" with "5" and creates a function which
takes one parameter and returns a value
-}
multOne = multTwo 5
multOne' = multThree 3 5
{-
(3) apply "multOne" with "9" and returns the result 135
"multOne 9"
(Note: "multTwo" and "multOne" are just SAMPLE function names)
-}

{-
Partial Application:
-- call functions with few desired parameters (NOT all desired parameters)
-- create new functions on the fly

Example: create a function that takes a number and compares it with 100
-}
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x  

compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100  
{-
Note: The type declarations of "compareWithHundred" and "compareWithHundred'"
stay the same, because "compare 100" returns a function
-}

{-
Partial Application of Infix Functions:
-- surround it with parentheses
-- only supply a parameter on one side
-- create a function which takes one parameter and
    applies it to the side that's missing an operand
-}
divideByTen :: (Floating a) => a -> a  
divideByTen = (/ 10)

{-
The following three calls are equivalent:
200 / 10
divideByTen 200
(/10) 200
-}

{-
If we would like to partially apply the minus operation
we should use "subtract" instead of "-".

(-4) means negtive number -4
-}
minus4 :: (Num a) => a -> a 
minus4 = (subtract 4)

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z']) 

{-
Note: in GHCI, we can NOT partially apply a function without
binding it to a (function) name or passing it to another function.

just "multThree 3 4" will give an error
-}




{- 2. Some higher-orderism is in order -}

{-
we're going to make a function that takes a function
and then applies it twice to something!
-}
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 
{-
Note: parentheses in the type declaration HERE are MANDATORY,
because they indicate that the first parameter is a function.

Examples:
applyTwice (+3) 10
applyTwice (++ " HAHA") "HEY"
applyTwice ("HAHA " ++) "HEY"
applyTwice (multThree 2 2) 9
applyTwice (3:) [1]
-}
f :: (Num a) => a -> a
f x = x - 1
{-
applyTwice f 5
-}


{-
Use higher-order programming to re-implement the "zipWith" function.
It takes a function and two lists as parameters and then joins the two
lists by applying the function between corresponding elements.

zipWith (+) [4,2,5,6] [2,6,2,3]
zipWith max [6,3,2,1] [7,3,1,5]
zipWith (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
zipWith (*) (replicate 5 2) [1..]
zipWith (zipWith (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

{-
zipWith' (+) [4,2,5,6] [2,6,2,3] == zipWith (+) [4,2,5,6] [2,6,2,3]
zipWith' max [6,3,2,1] [7,3,1,5] == zipWith max [6,3,2,1] [7,3,1,5]
zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"] == zipWith (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
zipWith' (*) (replicate 5 2) [1..] == zipWith (*) (replicate 5 2) [1..]
zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]] == zipWith (zipWith (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-}


{-
Re-implement "flip" function.
It takes a function and returns a function which is like
the original function, only the first two parameters are flipped.

zip [1,2,3,4,5] "hello"
flip zip [1,2,3,4,5] "hello" 
flip zip [1,2,3,4,5] "hello" == zip "hello" [1,2,3,4,5] 

zipWith div [2,2..] [10,8,6,4,2] 
zipWith (flip div) [2,2..] [10,8,6,4,2] 
zipWith (flip div) [2,2..] [10,8,6,4,2] == zipWith div [10,8,6,4,2] [2,2..] 
-}

flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c  
flip'' f y x = f x y 

{-
flip' zip [1,2,3,4,5] "hello" == flip zip [1,2,3,4,5] "hello"
flip'' zip [1,2,3,4,5] "hello" == flip zip [1,2,3,4,5] "hello"

zipWith (flip' div) [2,2..] [10,8,6,4,2]  == zipWith (flip div) [2,2..] [10,8,6,4,2] 
zipWith (flip'' div) [2,2..] [10,8,6,4,2]  == zipWith (flip div) [2,2..] [10,8,6,4,2]
-}