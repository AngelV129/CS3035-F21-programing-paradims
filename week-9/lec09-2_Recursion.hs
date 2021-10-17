{- Lecture 09-2: Recursion -}


{-
Resource/Textbook:
-- Learn You a Haskell for Great Good
-- http://learnyouahaskell.com/
-- Chapter 5: Recursion

Good Haskell Programming Style:
-- https://www.seas.upenn.edu/~cis194/fall16/style.html

INDENTATION is important in Haskell programs, because Haskell relies on
indentation to reduce the verbosity of your code. Some syntax ERRORS may
be caused by indentation issues.
-- Indentation Rules: https://en.wikibooks.org/wiki/Haskell/Indentation
-}




{- Recursion -}
{-
Recursion is actually a way of defining functions in which the function
is applied inside its own definition.

Definitions in mathematics are often given recursively.
-- e.g. factorial: 0! = 1, n! = n * (n-1)!
-- e.g. fibonacci numbers: F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)

Edge Condition:
-- a few elements defined non-recursively in a recursion definition 
    -- e.g. edge condition of factorial: 0! = 1
    -- e.g. edge condition of fibonacci numbers: F(0) = 0, F(1) = 1
-- IMPORTANT if you want your recursive function to TERMINATE. 

Recursion is important to Haskell because unlike imperative languages,
you do computations in Haskell by declaring what something is instead of
declaring how you get it. That's why there are no $while$ loops or $for$
loops in Haskell and instead we have to use recursion to declare
what something is.
-}

-- Implement our own version of the $maximum$ function.
    -- Edge Condition: empty list and a list with only one element
    -- simulation of $maximum' [2,5,1] $: http://s3.amazonaws.com/lyah/maxs.png
maximum' :: (Ord a) => [a] -> a  
maximum' []  = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise   = maxTail  
    where maxTail = maximum' xs 

maximum'' :: (Ord a) => [a] -> a  
maximum'' []     = error "maximum of empty list"  
maximum'' [x]    = x  
maximum'' (x:xs) = max x (maximum' xs)

-- Implement our own version of the $replicate$ function.
    -- Edge Condition: $n <= 0$
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x : replicate' (n-1) x  

-- Implement our own version of the $take$ function.
    -- Edge Condition: $n <= 0$ and empty list
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

-- Implement our own version of the $reverse$ function.
    -- Edge Condition: empty list
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x] 

-- Implement our own version of the $repeat$ function.
    -- Edge Condition: NONE ($repeat$ returns an infinite list)
repeat' :: a -> [a]  
repeat' x = x : repeat' x  

-- Implement our own version of the $zip$ function.
    -- Edge Condition: at least one argument is empty list
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 

-- Implement our own version of the $elem$ function.
    -- Edge Condition: empty list
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs 


{- Quick Sort -}
{-
Reference: https://www.geeksforgeeks.org/quick-sort/

Edge Condition: empty list

Algorithm:
A sorted list is a list that has all the values smaller than (or equal to)
the head of the list in front (and those values are sorted), then comes the
head of the list in the middle and then come all the values that are bigger
than the head (they're also sorted).

Simulation of $quicksort [5,1,9,4,6,7,3]$: http://s3.amazonaws.com/lyah/quicksort.png

Test Cases:
-- quicksort [5,1,9,4,6,7,3]
-- quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]  
-- quicksort "the quick brown fox jumps over the lazy dog"  
-}
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted


{- Thinking Recursively -}
{-
Recursions can be used in lists, trees or other data structures.

Pattern to use recursion:
-- (1) define a few edge cases
    -- Usually the edge case is some scenario where a recursive application
        doesn't make sense. 
    -- e.g. for lists, the edge case is most often the empty list
    -- e.g. for trees, the edge case is usually a node that doesn't have any children
    -- often the edge case value turns out to be an identity
-- (2) define a function that does something between some element and the function
    applied to the rest

How to solve a problem with recursions?
-- (1) try to think of when a recursive solution doesn't apply and see if you can use
    that as an edge case, think about identities
-- (2) think about whether you'll break apart the parameters of the function and on
    which part you'll use the recursive call
    -- e.g. lists are usually broken into a head and a tail via pattern matching
-}    