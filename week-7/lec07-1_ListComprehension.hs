{- Lecture 07-1: List Comprehensions -}


{-
Resource/Textbook:
-- Learn You a Haskell for Great Good
-- http://learnyouahaskell.com/
-- Chapter 2: Starting Out

Good Haskell Programming Style:
-- https://www.seas.upenn.edu/~cis194/fall16/style.html

INDENTATION is important in Haskell programs, because Haskell relies on
indentation to reduce the verbosity of your code. Some syntax ERRORS may
be caused by indentation issues.
-- Indentation Rules: https://en.wikibooks.org/wiki/Haskell/Indentation
-}


{- Set Comprehensions in Math -}
{-
e.g. A set with the first 10 even natural numbers is {2*x | x \in N, x <= 10}.
-- output function: the part before the pipe
-- variable: x
-- input set: N
-- predicate: x <= 10
-- the set comprehension means that the set contains the doubles of
    all natural numbers that satisfy the predicate.
-}

{- List Comprehensions -}
{-
List comprehensions are very similar to set comprehensions and use list ranges.
-- perform list filtering: take a list of elements and filter them by predicates
-- allow multiple predicates
-- allow multiple input lists
-- allow nested list comprehensions for nested input lists

e.g. A list with the first 10 even numbers is [x*2 | x <- [1..10]].
-- x is drawn from [1..10]
-- for EVERY element in [1..10] (which we have bound to x),
    we get that element and double it

e.g. For the previous example, add another condition/predicate: greater than
or equal to 12 after double.
-- Predicates go after the binding parts and are separated by a comma.
-- [x*2 | x <- [1..10], x*2 >= 12]

e.g. We want all numbers from 50 to 100 whose remainder
when divided with the number 7 is 3.
-- [x | x <- [50..100], x `mod` 7 == 3]

List Filtering by Comprehensions: take a list of elements and
filter them by the predicate

e.g. Replaces each odd number greater than 10 with "BANG!" and each odd number
less than 10 with "BOOM!". If a number isn't odd, we throw it out of our list.
-- boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 
-- boomBangs [7..13]

e.g. Want all numbers from 10 to 20 that are not 13, 15 or 19.
-- [x | x <- [10..20], x /= 13, x /= 15, x /= 19]

If a list comprehension has multiple predicates, an element must satisfy ALL
predicates to be included in the resulting list.

A list comprehension can also draw from multiple lists, produce ALL combinations
of the given lists, and then join them by the output function.
-- e.g. If we do not filter elements out, drawing from two lists of
    length 4 will produce a list of length 16.

e.g. Get the products of all possible combinations between numbers
in list [2,5,10] and list [8,10,11].
-- [x*y | x <- [2,5,10], y <- [8,10,11]]
-- the length of the new list is 9

e.g. For the previous example, want all possible products more than 50.
-- [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

e.g. Combine a list of adjectives and a list of nouns.
-- let nouns = ["hobo","frog","pope"]  
-- let adjectives = ["lazy","grouchy","scheming"]  
-- [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

e.g. Use list comprehensions to re-implement the $length$ function.
-- length' xs = sum [1 | _ <- xs]
-- $_$ means that we don't care the element drawn from the list,
    i.e. $_$ matches ANY element
-- length'' xs = sum [1 | x <- xs]  -- x is never used
-- instead of unused $x$, we prefer $_$
-- Replace every element of a list with 1 and then sums that up.

e.g. Take a string and remove everything except uppercase letters from it.
-- removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
-- The predicate here does all the work. It says that the character will be
    included in the new list only if it's an element of the list ['A'..'Z']. 
-- test: removeNonUppercase "Hahaha! Ahahaha!"  
-- test: removeNonUppercase "IdontLIKEFROGS" 

Nested list comprehensions are also possible if you're operating on lists
that contain lists. 

e.g. Remove all odd numbers without flattening the list.
-- let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
-- [[x | x <- xs, even x ] | xs <- xxs] 

Note:
-- in Haskell script, we can write list comprehensions across several lines 
    to split longer list comprehensions (especially if they're nested).
-- in GHCI, we can ONLY write list comprehensions in ONE line.
-}