{- Higher Order Functions 2: Application -}

{-
What is a higher order function?
A higher order function either take functions as parameters
or return functions as results.

Important Fact: Haskell is LAZY
-}




{- 1. Map and Filter -}

{-
Pre-defined functions in the standard library:

(1) "map" takes a function and a list and applies the function
    to every element in the list, producing a new list.

Type and Implementation:
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs

Examples:
map (+3) [1,5,3,1,6]   
map (++ "!") ["BIFF", "BANG", "POW"]  
map (replicate 3) [3..6] 

    map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  
==> map f [[1,2],[3,4,5,6],[7,8]]  
==> [f [1,2], f [3,4,5,6], f [7,8]]
==> [map (^2) [1,2], map (^2) [3,4,5,6], map (^2) [7,8]]
==> [[g 1, g 2], [g 3,g 4,g 5,g 6], [g 7,g 8]]
==> [[1, 4], [9,16,25,36], [49,64]]
where f = map (^2)
where g = (^2)

map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]  

Each "map" application could be achieved with a list comprehension.
e.g. map (+3) [1,5,3,1,6] == [x+3 | x <- [1,5,3,1,6]]
However, using "map" is much more readable.
-}


{-
Pre-defined functions in the standard library:

(2) "filter" takes a predicate and a list and then returns
    the list of elements that satisfy the predicate.

Note: a predicate is a function that tells whether something is 
true or not, (i.e., a function that returns a boolean value).

Type and Implementation:
filter :: (a -> Bool) -> [a] -> [a]  
filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs 

Examples:
filter (>3) [1,5,3,2,1,6,4,3,2,1]
filter (==3) [1,2,3,4,5]  
filter even [1..10]  
let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
filter (/= []) [[1,2,3],[],[3,4,5],[2,2],[],[],[]]  
filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  
filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"  

Similarly, each "filter" application could be achieved with a list comprehension.
e.g. filter (>3) [1,5,3,2,1,6,4,3,2,1] == [x | x <- [1,5,3,2,1,6,4,3,2,1], x > 3]

There's NO rule for when to use "map" and "filter" versus using
list comprehension, you just have to decide what's more READABLE
depending on the code and the context.
-}


{-
Application:

(1) find the largest number under 100,000 that's divisible by 3829

Note: Haskell is lazy
-}
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0


{-
Application:

(2) find the sum of all odd squares that are smaller than 10,000

use "takeWhile" function:
it takes a predicate and a list and then goes from the beginning of
the list and returns its elements while the predicate holds true.
Once an element is found for which the predicate does NOT hold,
it STOPs. (difference between "takeWhile" and "filter")

takeWhile :: (a -> Bool) -> [a] -> [a]

Example of "takeWhile":
get the first word of the string "elephants know how to party"
takeWhile (/=' ') "elephants know how to party"

Note: Haskell is lazy
-}
oddSquareSum :: (Integral a) => a 
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 

oddSquareSum' :: (Integral a) => a 
oddSquareSum' = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]) 
{-
Note: can NOT use "filter" instead of "takeWhile",
because "filter" NEVER stops for infinite lists
-}


{-
Application:

(3) Collatz Sequences:
Step 1: take a natural number
Step 2: calculate a resulting number
    -- if the number is EVEN, divide it by two
    -- if the number is ODD, multiply it by 3 and then add 1
Step 3: take the resulting number and repeat "Step 2" until we get 1
Step 4: return all resulting numbers as a chain/list/sequence

Example:
if the the starting number is 13,
the sequence is: 13, 40, 20, 10, 5, 16, 8, 4, 2, 1
the sequence length is 10

Problem: for all starting numbers between 1 and 100,
         how many chains have a length greater than 15?
-}
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1) 
{-
Testing Cases:
chain 10
chain 1  
chain 30
-}

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15


{-
So far, we only map functions that take one parameter over lists,
like "map (*2) [0..]" to get a list of type "(Num a) => [a]".

How about mapping functions that take MULTIPLE parameters over lists?

Example:
map (*) [0..]
(*) :: (Num a) => a -> a -> a
map (*) [0..] :: (Num a, Enum a) => [a -> a]
"map (*) [0..]" produces a list "[(0*),(1*),(2*),(3*),(4*),(5*)...]"

Note: "(*)" is a curried function and can be partially applied.
-}
listOfFuns = map (*) [0..]
a = (listOfFuns !! 4) 5




{- 2. Lambda -}

{-
Lambdas are basically ANONYMOUS functions that are used only once.
Normally, we make a lambda with the sole purpose of passing it to
a higher-order function.

Examples:
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15
-}
numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100])) 
{-
(\xs -> length xs > 15) :: (Foldable t) => t a -> Bool
-}

b = map (\x -> x + 3) [1,6,3,2] == map (+3) [1,6,3,2]
{-
(\x -> x + 3) :: (Num a) => a -> a

PREFER "map (+3) [1,6,3,2]" which using partial application
more readable
-}

c = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  
{-
(\a b -> (a * 30 + 3) / b) :: (Fractional a) => a -> a -> a
-}

d = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] 
{-
(\(a,b) -> a + b) :: (Num a) => (a, a) -> a
-}




{- 3. Fold -}

{-
A fold takes a binary function, a starting value
(also called the accumulator) and a list to fold up.
The binary function itself takes two parameters.

Functionality of fold:
-- The binary function is called with the accumulator and
    the first (or last) element and produces a new accumulator.
-- Then, the binary function is called again with the new accumulator
    and the new first (or last) element, and so on.
-- Once we've walked over the whole list, only the accumulator remains,
    which is what we've reduced the list to.

Note: Folds can be used to implement any function
where you TRAVERSE a list once, element by element,
and then return something based on that.
(That's why FOLDs are, along with MAPs and FILTERs,
one of the most useful types of functions in functional
programming.)
-}

{-
Pre-defined functions in the standard library:

(3) "foldl" (left fold) folds the list up from the LEFT side

The binary function is applied between the starting value (FIRST parameter)
and the head of the list (SECOND parameter).

foldl :: (Foldable t) => (b -> a -> b) -> b -> t a -> b

Examples:
-}

sumFoldl :: (Num a) => [a] -> a  
sumFoldl xs = foldl (\acc x -> acc + x) 0 xs
{-
Evaluations Steps:
    sumFoldl [3,5,2,1]
==> foldl f 0 [3,5,2,1]
==> f (f (f (f 0 3) 5) 2) 1
==> f (f (f 3 5) 2) 1
==> f (f 8 2) 1
==> f 10 1
==> 11

where f = \acc x -> acc + x
-}

sumFoldl' :: (Num a) => [a] -> a  
sumFoldl' xs = foldl (+) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
{-
Evaluations Steps:
    elem' 3 [1,3,4]
==> foldl g False [1,3,4]
==> g (g (g False 1) 3) 4
==> g (g False 3) 4
==> g True 4
==> True

where g = \acc x -> if x == y then True else acc
-}


{-
Pre-defined functions in the standard library:

(4) "foldr" (right fold) folds the list up from the RIGHT side

The binary function is applied between the last element of the list
(FIRST parameter) and the starting value (SECOND parameter).

foldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b

Examples:
-}

sumFoldr :: (Num a) => [a] -> a  
sumFoldr xs = foldr (\x acc -> acc + x) 0 xs

{-
Evaluations Steps:
    sumFoldr [3,5,2,1]
==> foldr f' 0 [3,5,2,1]
==> f' 3 (f' 5 (f' 2 (f' 1 0)))
==> f' 3 (f' 5 (f' 2 1))
==> f' 3 (f' 5 3)
==> f' 3 8
==> 11

where f' = \acc x -> acc + x
-}

sumFoldr' :: (Num a) => [a] -> a  
sumFoldr' xs = foldr (+) 0 xs


{-
Application:

(4) re-implement "map" with "foldl" or "foldr"
-}
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs

map'' :: (a -> b) -> [a] -> [b]  
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
{-
Testing:
map' (+3) [1,2,3] == map (+3) [1,2,3]
map'' (+3) [1,2,3] == map (+3) [1,2,3]
-}

{-
Note: the (++) function is much more expensive than (:) function

We usually use RIGHT folds when we're building up new lists from a list.
-}


{-
Pre-defined functions in the standard library:

(5) "foldl1" and "foldr1" functions work like "foldl" and "foldr",
only you do NOT need to provide them with an explicit starting value.

foldl1 :: (Foldable t) => (a -> a -> a) -> t a -> a
foldr1 :: (Foldable t) => (a -> a -> a) -> t a -> a

They assume the first (or last) element of the list to be the starting value
and then start the fold with the element next to it.

Constraint: the list must have at least one element

Examples:
-}
sumFoldl'' xs = foldl1 (+) xs
sumFoldr'' xs = foldr1 (+) xs



{-
Application:

(5) re-implement a bunch of standard library functions by using folds
-}
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)
{-
Testing:
maximum' [1,2,3] == maximum [1,2,3]
reverse' [1,2,3] == reverse [1,2,3]
product' [1,2,3] == product [1,2,3]
filter' (>1) [1,2,3] == filter' (>1) [1,2,3]
head' [1,2,3] == head [1,2,3]
last' [1,2,3] == last [1,2,3]
-}




{-
Pre-defined functions in the standard library:

(6) "scanl" and "scanr" are like "foldl" and "foldr",
only they report all the intermediate accumulator states
in the form of a list.

Note: Scans are used to monitor the progression of a
function that can be implemented as a fold

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanr :: (a -> b -> b) -> b -> [a] -> [b]

"scanl1" and "scanr1" are similar to "foldl1" and "foldr1", repectively.
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr1 :: (a -> a -> a) -> [a] -> [a]

Examples:
scanl (+) 0 [3,5,2,1] 
scanr (+) 0 [3,5,2,1]
scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
scanl (flip (:)) [] [3,2,1]  
-}


{-
Application:

(6) How many elements does it take for the sum
of the square roots of all natural numbers to exceed 1000?
-}
sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl (+) 0 (map sqrt [1..])))

sqrtSums' :: Int  
sqrtSums' = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1 

e = sum (map sqrt [1..131]) 
f = sum (map sqrt [1..130]) 




{- 4. Function Application with $ -}

{-
Function Application Operator in Haskell:
($) :: (a -> b) -> a -> b  
f $ x = f x
f $ x y == f (x y)

Difference:
-- Normal function application ("f x") has a high precedence and is left-associative ("f a b c == ((f a) b) c)")
-- the $ operator ("f $ x") has the lowest precedence and is right-associative ("f $ g $ z x == f (g (z x))")

Example:
(sum $ map sqrt [1..130]) == sum (map sqrt [1..130])
(sqrt $ 3 + 4 + 9) == sqrt (3 + 4 + 9)
(sum $ filter (> 10) $ map (*2) [2..10]) == sum (filter (> 10) (map (*2) [2..10]))
(map ($ 3) [(4+), (10*), (^2), sqrt]) == map (\f -> f 3) [(4+), (10*), (^2), sqrt]
-}




{- 5. Function Composition -}

{-
In mathematics, function composition is: (f . g)(x) = f(g(x))

Function Composition Operator in Haskell:
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x) 

Function composition is right-associative,
so we can compose MANY functions at a time.
e.g.
"f (g (z x))" is equivalent to "(f . g . z) x"

Examples:
(negate . (* 3)) 5 == negate ((* 3) 5)

map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-}

{- All followings are equivalent -}
eq1 = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
eq2 = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
eq3 = map (\x -> negate $ abs x) [5,-3,-6,7,-3,2,-19,24]
eq4 = map f [5,-3,-6,7,-3,2,-19,24]
    where f x = negate (abs x)


{-
Application:

(7) Re-implementation:
find the sum of all odd squares that are smaller than 10,000

oddSquareSum :: (Integral a) => a 
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 
-}
oddSquareSum'' :: Integer  
oddSquareSum'' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..] 

oddSquareSum''' :: Integer  
oddSquareSum''' =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit 
{-
oddSquareSum''' is more readable than oddSquareSum''
-}