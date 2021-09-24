{- Lab 05: Haskell Getting Started [15 POINTS] -}

{-
Please solve the following problems and submit your solutions as
a SINGLE Hakell script (.hs file). Do NOT use stuff we haven't covered.
Note that your functions MUST use the names given in the problem descriptions.

Your submitted file MUST be compilable (i.e. NO compiling errors),
otherwise you will get ZERO.

After implementing these functions, you should figure out some test cases
and TEST your implementation thoroughly. The testing procedure is as follows:
-- Open a command prompt and locate to the folder of your Haskell script
-- Enter the GHC interactive environment by typing $ghci$
-- Load your Haskell script in GHCI by typing $:l YOURSCRIPTFILENAME$
    -- after modifying your script, type $:r$ in GHCI to re-load the latest version
-- Test your implementation by calling corresponding functions with your test cases
-}



{-
Q1 [2 POINTS]:
Write a function named $add1$ that takes a number and increases it by 1.

e.g. $add1 5$ returns $6$
-}
add1 x = x + 1


{-
Q2 [2 POINTS]:
Write a function named $always0$. The return value should always just be 0.
-}

always0 = 0
{-
Q3 [2 POINTS]:
Write a function named $mySubtract$ that takes two numbers and subtracts them.
-}
mySubtract x y = x - y

{-
Q4 [2 POINTS]:
Write a function named $addMult$ that takes three numbers. Let's call them $p$,
$q$, and $r$. The $addMult$ function should add $p$ and $q$ together and then
multiply the result by $r$.

e.g. $addMult 2 4 5$ returns $30$
-}
addMult p q r = ( p + q) * r

{-
Q5 [2 POINTS]:
Write a function named $cube$ that takes one number and calculates its cube.

e.g. $cube 3$ returns $27$

Hint: see Haskell power function here (https://wiki.haskell.org/Power_function)
-}
cube x = x^2


{-
Q6 [2 POINTS]:
Write a function named $myAbs$ that takes one number and computes its absolute value.
Do NOT use the built-in function $abs$.

Hint:
-- use if expression
-- always put parentheses around negative numbers
-}

myAbs x = if x > 0
            then x
            else x * (-1)


{-
Q7 [3 POINTS]:
Write a function named $pushOut$ that takes a number and returns the number that is
one step further from 0.

Examples:
$pushOut 3$ returns $4$
$pushOut (-10)$ returns $(-11)$
$pushOut 0$ returns $0$    -- because we don't know which direction to go

Hints:
-- use nested if expressons
-- always put parentheses around negative numbers
-}

pushOut 