{- Lecture 08-1: Types -}


{-
Resource/Textbook:
-- Learn You a Haskell for Great Good
-- http://learnyouahaskell.com/
-- Chapter 3: Types and Typeclasses

Good Haskell Programming Style:
-- https://www.seas.upenn.edu/~cis194/fall16/style.html

INDENTATION is important in Haskell programs, because Haskell relies on
indentation to reduce the verbosity of your code. Some syntax ERRORS may
be caused by indentation issues.
-- Indentation Rules: https://en.wikibooks.org/wiki/Haskell/Indentation
-}


{- Types -}
{-
Haskell has a static type system. The type of every expression is known
at COMPILE time, which leads to safer code. It's better to catch errors
at compile time instead of having your program crash.

EVERYTHING in Haskell has a type.

Haskell has type INFERENCE. The compiler will infer the type of expressions.
We do NOT have to explicitly write out the types of our functions and
expressions to get things done. However, understanding the type system is
a very IMPORTANT part of learning Haskell.

A type is a kind of label that every expression has. It tells us in which
category of things that expression fits.
-- e.g. $True$ is a boolean
-- e.g. "hello" is a string

How to examine type in GHCI?
-- use $:t$ command followed by any valid expression to print out the expression
    followed by $::$ and its type
    -- $::$ is read as "has type of"
    -- explicit types are always denoted with the first letter in CAPITAL case
-- e.g. :t 'a'          -- Char: character
-- e.g. :t True         -- Bool: boolean
-- e.g. :t "HELLO!"     -- [Char]: a list of characters
-- e.g. :t (True, 'a')  -- (Bool, Char): a pair of boolean and character
-- e.g. :t 4 == 5       -- Bool: boolean

Functions also have types. It is a GOOD PRACTICE to explicityly give type
declarations (also called type signature) for our own functions.

Function type declaration syntax:
-- the parameters and return type are separated with $->$
-- the return type is the last item in the declaration 

Functions are expressions too, so $:t$ works on functions.

Example 1: removeNonUppercase
-}
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 
{-
$removeNonUppercase$ has a type of $[Char] -> [Char]$, meaning that it maps
from a string to a string. It takes one string as a parameter and returns
another string as a result. 
The $[Char]$ type is synonymous with $String$, so we can also write as follows.
-} 
removeNonUppercase' :: String -> String  
removeNonUppercase' st = [ c | c <- st, c `elem` ['A'..'Z']]
{-
Example 2: addThree
-}
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  
{-
$addThree$ takes three Int as parameters and returns another Int as a result.
-}


{- Common Types -}
{-
(1) $Int$ stands for integer.
-- $Int$ is BOUNDed, which means that it has a minimum and a maximum value
-- e.g. $7$ can be an Int, but $7.2$ cannot

(2) $Integer$ also stands for integer.
-- $Integer$ is NOT bounded, so it can be used to represent really really big numbers
-- $Int$ is more efficient
-- e.g. factorial 50    -- returns a BIG number
-}
factorial :: Integer -> Integer  
factorial n = product [1..n] 
{- 

(3) $Float$ is a real floating point with single precision.
-- e.g. circumference 4.0   -- single precision
-}
circumference :: Float -> Float  
circumference r = 2 * pi * r  
{- 

(4) $Double$ is a real floating point with double precision.
-- e.g. circumference' 4.0  -- double precision
-}
circumference' :: Double -> Double  
circumference' r = 2 * pi * r 
{-

(5) $Bool$ is a boolean type. 
-- only two values: $True$ and $False$

(6) $Char$ represents a character.
-- denoted by single quotes
-- a list of characters is a string

(7) $[xxx]$ represents a list
-- e.g. [Int]: a list of Int
-- e.g. [Char]: a list of Char, i.e. a String
-- e.g. []: empty list type, which can ONLY have a single value []

(8) $(xxx, yyy, zzz, ...)$ represents a tuple
-- e.g. (Int, Int): a tuple of two Ints
-- e.g. (Int, Char, Char): a tuple of a Int and two Chars
-- e.g. (): empty tuple type, which can ONLY have a single value ()
-}


{- Type Variables -}
{-
What do you think is the type of the head function?
-- head :: [a] -> a
-- $head$ takes a list of any type and returns one element of that type
-- $a$ is a type variable
-- $a$ can be of ANY TYPE, e.g. Int, Char, Float, [Int], (Char, Int)
-- the names of type variables are usually $a$, $b$, $c$, $d$, etc.

Type variables allow us to easily write GENERIC functions if they don't
use any specific behavior of the types in them. 
Functions that have type variables are called polymorphic functions.

Another example:
-- fst :: (a, b) -> a 
-- $fst$ takes a tuple which contains two types and returns an element
    which is of the same type as the pair's first component
-- $a$ and $b$ do NOT have to be different types, e.g. fst (2,3)
-- it just states that the first component's type and the return value's
    type are the same
-}


{- Typeclasses -}
{-
A typeclass is a sort of interface that defines some behavior. If a type is
a part of a typeclass, that means that it supports and implements the behavior
the typeclass describes.

What's the type signature of the $==$ function? (note: every operator is a function)
-- (==) :: (Eq a) => a -> a -> Bool 
-- everything before the => symbol is called a class constraint
-- The equality function takes any two values that are of the same type and
    returns a Bool. The type of those two values must be a member of the Eq
    class (this is the class constraint).

The $Eq$ typeclass provides an interface for testing for equality. Any type
where it makes sense to test for equality between two values of that type
should be a member of the $Eq$ class. All standard Haskell types except for
IO and functions are a part of the $Eq$ typeclass.

Another example:
-- elem :: (Eq a) => a -> [a] -> Bool
-- $elem$ uses == over a list to check whether a value is in it.
-}


{- Basic Typeclasses -}
{-
(1) $Eq$ is used for types that support equality testing
-- $Eq$ covers functions $==$ and $/=$
-- all the types mentioned previously except for functions are part of $Eq$
-- e.g. 5 == 5  
-- e.g. 5 /= 5  
-- e.g. 'a' == 'a'  
-- e.g. "Ho Ho" == "Ho Ho"  
-- e.g. 3.432 == 3.432

(2) $Ord$ is for types that have an ordering
-- $Ord$ covers all the standard comparing functions such as $>$, $<$, $>=$,
    $<=$, and $compare$
-- (>) :: (Ord a) => a -> a -> Bool  
-- compare :: (Ord a) => a -> a -> Ordering
-- the $compare$ function takes two $Ord$ members of the same type and
    returns an ordering
-- $Ordering$ is a type that can be $GT$, $LT$ or $EQ$, meaning greater than,
    lesser than and equal, respectively
-- all the types covered so far except for functions are part of $Ord$
-- to be a member of $Ord$, a type must first have membership in $Eq$
-- e.g. "Abrakadabra" < "Zebra"  
-- e.g. "Abrakadabra" `compare` "Zebra"  
-- e.g. 5 >= 2  
-- e.g. 5 `compare` 3  

(3) Members of $Show$ can be presented as strings
-- all types covered so far except for functions are part of $Show$
-- show :: (Show a) => a -> String
-- the $show$ function takes a value whose type is a member of $Show$ and
    presents it as a string
-- e.g. show 3  
-- e.g. show 5.334  
-- e.g. show True  

(4) $Read$ is sort of the opposite typeclass of $Show$
-- all types covered so far except for functions are a part of $Read$
-- read :: (Read a) => String -> a
-- the $read$ function takes a string and returns a type which is a member of $Read$
-- e.g. read "True" || False  
-- e.g. read "8.2" + 3.8  
-- e.g. read "5" - 2  
-- e.g. read "[1,2,3,4]" ++ [3]  
-- e.g. read "4"    -- ERROR
    -- the compiler knows we want some type that is part of $Read$, but it does
        NOT know which exact type
        -- $4$ could be Int type, Integer type, Float type, or Double type
    -- Note that in the previous examples, we did something with the $read$
        result afterwards, so the compiler can infer what exact type we want.
    -- how to fix: use explicit type annotations
        -- type annotations explicitly specify what the type of an expression should be
        -- adding $::$ at the end of the expression and then specifying a type
        -- e.g. read "4" :: Int 
        -- e.g. read "4" :: Integer
        -- e.g. read "4" :: Float
        -- e.g. read "4" :: Double
-- e.g. read "5" :: Int    
-- e.g. read "5" :: Float  
-- e.g. (read "5" :: Float) * 4  
-- e.g. read "[1,2,3,4]" :: [Int]  
-- e.g. read "(3, 'a')" :: (Int, Char)  

(5) $Enum$ members are sequentially ordered types — they can be enumerated
-- advantages of $Enum$:
    -- can use its types in list ranges
    -- have defined successors and predecesors
        -- succ :: (Enum a) => a -> a
        -- pred :: (Enum a) => a -> a
-- types in $Enum$: (), Bool, Char, Ordering, Int, Integer, Float, Double
-- e.g. ['a'..'e']
-- e.g. [LT .. GT]
-- e.g. [3 .. 5]
-- e.g. succ 'B'
-- e.g. pred 'B'

(6) $Bounded$ members have an upper and a lower bound
-- types in $Bounded$: (), Bool, Char, Ordering, Int
-- all tuples are also part of $Bounded$ if the components are also in $Bounded$
-- minBound :: (Bounded a) => a
-- maxBound :: (Bounded a) => a
-- $minBound$ and $maxBound$ are polymorphic constants
-- e.g. minBound :: Int  
-- e.g. maxBound :: Int  
-- e.g. minBound :: Char 
-- e.g. maxBound :: Char  
-- e.g. minBound :: Bool
-- e.g. maxBound :: Bool  
-- e.g. minBound :: Ordering
-- e.g. maxBound :: Ordering 
-- e.g. minBound :: ()
-- e.g. maxBound :: () 
-- e.g. minBound :: (Bool, Int, Char)  
-- e.g. maxBound :: (Bool, Int, Char)  

(7) $Num$ is a numeric typeclass
-- its members have the property of being able to act like numbers
-- types in $Num$: Int, Integer, Float, Double
-- 20 :: (Num p) => p
    -- numbers are polymorphic constants
    -- e.g. 20 :: Int  
    -- e.g. 20 :: Integer  
    -- e.g. 20 :: Float  
    -- e.g. 20 :: Double 
-- (*) :: (Num a) => a -> a -> a
    -- it accepts all numbers
    -- it takes two numbers of the same type and returns a number of that type
    -- e.g. (5 :: Int) * (6 :: Integer) -- ERROR
    -- e.g. 5 * (6 :: Integer)  -- produce an Integer because 5 can act like an Integer or an Int

(8) $Integral$ is also a numeric typeclass
-- $Num$ includes all numbers, including real numbers and integral numbers, 
-- $Integral$ includes only integral numbers
-- types in $Integral$: Int, Integer

(9) $Floating$ is also a numeric typeclass
-- $Floating$ includes only floating point numbers
-- types in $Floating$: Float, Double
-}


{- $fromIntegral$ Function-}
{-
$fromIntegral$ is a very useful function for dealing with numbers
-- fromIntegral :: (Integral a, Num b) => a -> b
-- it takes an integral number and turns it into a more general number
-- it's useful when you want integral and floating point types to work together nicely.
    -- e.g. length :: [a] -> Int
        -- length [1,2] + 3.2   -- ERROR, try to add together an Int and a floating point number
        -- fromIntegral (length [1,2]) + 3.2
-- Note that $fromIntegral$ has more than one class constraints in its type signature
    -- multiple class constraints are separated by commas inside the parentheses
-}