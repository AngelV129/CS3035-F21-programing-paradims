{- Lecture 08-2: Functions -- Pattern Matching -}
{-
Function Topics:
-- Pattern Matching
-- Guards
-- Where Bindings
-- Let Bindings/Expressions
-- Case Expressions
-}


{-
Resource/Textbook:
-- Learn You a Haskell for Great Good
-- http://learnyouahaskell.com/
-- Chapter 4: Syntax in Functions

Good Haskell Programming Style:
-- https://www.seas.upenn.edu/~cis194/fall16/style.html

INDENTATION is important in Haskell programs, because Haskell relies on
indentation to reduce the verbosity of your code. Some syntax ERRORS may
be caused by indentation issues.
-- Indentation Rules: https://en.wikibooks.org/wiki/Haskell/Indentation
-}


{- Pattern Matching -}
{-
When defining functions, we can define separate function bodies for
different patterns. We can pattern match on any data type: numbers,
characters, lists, tuples, etc.

When a function is called, the patterns will be checked from top to bottom.
When the input conforms to a pattern, the corresponding function body will be used. 
-}

-- Write a function to check whether the input number is a seven.
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck."
{-
Two patters of $lucky$:
-- first pattern: the input is number 7
-- second pattern: the input is any other number EXCEPT 7

Note:
-- $lucky$ can also be implemented by using an $if$ statement
-- how about we have more patterns?
    -- Without pattern matching, we have to make a complex if-then-else tree.
-}

-- How about we change the pattern order of $lucky$?
lucky' :: (Integral a) => a -> String  
lucky' x = "Sorry, you're out of luck."
lucky' 7 = "LUCKY NUMBER SEVEN!"    --  WARNING: Pattern match is redundant 
{-
Two patters of $lucky'$:
-- first pattern: the input is ANY number
    -- the first pattern catchs ALL numbers including 7
-- second pattern: the input is number 7
    -- the second pattern is NEVEBER checked/used

The ORDER of PATTERNs is very important, because the patterns are checked
from TOP to BOTTOM.
-- good practice: specific pattern first, general pattern last
-}  

-- Write a function to say the numbers from 1 to 5, and say
--  "Not between 1 and 5" for any other number.
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

-- Write a function to calculate the factorial of the input non-negative integer.
-- previous implementation, more imperative way
factorial' :: (Integral a) => a -> a 
factorial' n = product [1..n] 

-- recursive implementation with pattern matching, more FUNCTIONAL way
factorial :: (Integral a) => a -> a 
factorial 0 = 1  
factorial n = n * factorial (n - 1) 
{-
recursive implementation: use the way that factorial is defined in mathematics
-- start by saying that the factorial of 0 is 1
-- state that the factorial of any positive integer is that integer
    multiplied by the factorial of its predecessor

RECURSION is important in functional programming (more details later).

Simulation of $factorial 3$:
-- try to compute $3 * factorial 2$
-- the factorial of 2 is $2 * factorial 1$, so far we have $3 * (2 * factorial 1)$
-- the factorial of 1 is $1 * factorial 0$, so far we have $3 * (2 * (1 * factorial 0))$
-- the factorial of 0 is $1$, hence we have $3 * (2 * (1 * 1))$ with value 6

How about we change the pattern order of $factorial$?
-- the first pattern $n$ will catch ALL numbers including 0
-- the calculation would NEVER TERMINATE

The ORDER is very important when specifying PATTERNs.
-- good practice: specify the most specific patterns first and
    then the more general patterns later
-}

-- Pattern matching can fail.
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  
{-
Try the following function calls:
-- charName 'a'  
-- charName 'b'  
-- charName 'h' -- ERROR: Non-exhaustive patterns in function $charName$ 

Good Practice:
ALWAYS include a catch-ALL pattern (as the LAST one) so that the program does
not crash if we get some unexpected input
-}
-- charName _ = "Other Characters"

{-
Pattern matching can also be used on tuples.

Problem: Write a function that takes two vectors in a 2D space (that are in
    the form of pairs) and adds them together.

Strategy: To add together two vectors, we add their x components separately
    and then their y components separately.
-}
-- without pattern matching
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors' a b = (fst a + fst b, snd a + snd b)  

-- with pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

-- Write functions to get the first, second, and third components of triples.
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z 
{-
The $_$ means that we don't care what that part is,
i.e. $_$ matches ANYTHING of the corresponding type.
-}

{-
Pattern matching can also be used in list comprehensions.
Should a pattern match fail, it will just move on to the next element.

Try the following:
-- let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
-- [a+b | (a,b) <- xs]
-}

{-
Lists themselves can also be used in pattern matching.
-- match with the empty list []
-- match with any pattern that involves : and the empty list
    -- e.g. pattern $(1:2:3:[])$ matches the list $[1,2,3]$
    -- e.g. can also use pattern $[1,2,3]$ to matche the list $[1,2,3]$
-- match with the pattern $(x:xs)$
    -- bind the $head$ of the list to $x$
    -- bind the $tail$ of the list to $xs$
    -- if the list only has one element, $xs$ will be an empty list

Note: The $(x:xs)$ pattern is used a lot, especially with recursive functions.
But patterns that have : in them only match lists of length 1 or more.

Another Example:
-- want to bind the first three elements to variables and the rest
    of the list to another variable
-- use pattern $(x:y:z:zs)$ 
-- only match lists that have three elements or more
-}

-- Implement our own version of the $head$ function
head' :: [a] -> a  
head' [] = error "Can't call head on an EMPTY list!"  
head' (x:_) = x
{-
To bind to several variables (even if one of them is just _ and doesn't
actually bind at all), we have to surround them in PARENTHESES.

The $error$ function (with type $error :: [Char] -> a$) takes a string and
generates a runtime error, using that string as information about what kind
of error occurred.
--  It causes the program to crash, so it's NOT good to use it too much.
    But calling $head'$ on an empty list doesn't make sense.

Tests:
-- head' [4,5,6]  
-- head [4,5,6] 
-- head' "Hello"
-- head "Hello"
-- head' [] 
-- head [] 
-}

-- Write a function to tells us some of the first elements of the list.
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  
{-
The $tell$ function is safe, because it takes care of all cases includeing
-- the empty list 
-- a singleton list
-- a list with two elements
-- a list with two or more than two elements

Note that $(x:[])$ and $(x:y:[])$ could be rewriten as $[x]$ and $[x,y]$.
We can't rewrite (x:y:_) with square brackets because it matches any list
of length 2 or more (we do not know what exactly is for _).

Tests:
-- tell []
-- tell [1]
-- tell "hi"
-- tell "hello"
-}

-- Implement our own version of the $length$ function
-- using list comprehension
length'' :: (Num b) => [a] -> b  
length'' xs = sum [1 | _ <- xs]

-- using pattern matching
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs
{-
Tests:
-- length' [4,5,6]  
-- length [4,5,6] 
-- length' "Hello"
-- length "Hello"
-- length' [] 
-- length [] 
-}

-- Implement our own version of the $sum$ function
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs
{-
Tests:
-- sum' [4,5,6]  
-- sum [4,5,6]
-- sum' [] 
-- sum [] 
-}

{-
There's also a thing called $as patterns$:
-- When specifying patterns, we can also keep a REFERENCE to the WHOLE thing by
    putting a reference name and an @ in front of a pattern.
-- e.g. pattern $xs@(x:y:ys)$ match exactly the same thing as $x:y:ys$,
    but we can easily get the whole list via $xs$ instead of $x:y:ys$
-- e.g. capital "hello"
-}
-- use $as patterns$
capital :: String -> String  
capital "" = "Empty string!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

-- NOT use $as patterns$
capital' :: String -> String  
capital' "" = "Empty string!"  
capital' (x:xs) = "The first letter of " ++ x:xs ++ " is " ++ [x]  

{-
We can NOT use ++ in pattern matches.
-}