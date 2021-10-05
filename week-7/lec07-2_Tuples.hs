{- Lecture 07-2: Tuples -}


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


{- Tuples -}
{-
Tuple:
-- store several values into a single value
-- denoted with parentheses
-- components are separated by commas
-- e.g. (1, "hello", False)
-- can be used to represent a wide variety of data
-- can contain lists and tuples
    -- e.g. ((firstname, lastname), 30) -- tuple of tuple
    -- e.g. ([1..10], "hello")  -- tuple of list
    -- e.g. [(1,2), (5,6)]  -- list of tuple

Lists vs Tuples:
-- homogenous: lists must be homogenous, while tuples don't have to be homogenous
    -- all elements of a list have the same type
    -- a tuple can contain a combination of several types
-- length: the length of lists is dynamic, while length of tuples is static
    -- a list's length can be ranged from 0 to infinite
    -- tuples are used when you know EXACTLY how many values to combine
-- type: 
    -- list: list of elements (e.g. a list of numbers, a list of characters)
    -- tuple: depend on how many components it has and the types of the components
    -- e.g. (1,2)   -- type: tuple of two numbers
    -- e.g. (1, "") -- type: tuple of one number and one string
    -- e.g. (1,2,3) -- type: tuple of three numbers

How to represent a two-dimensional vector in Haskell?
-- Method 1: use a list
    -- type: list of numbers
    -- e.g. [1,2]
    -- e.g. [8,11]
    -- e.g. [4,5]
    -- How about putting a couple of vectors in a list to represent points
        of a shape on a two-dimensional plane?
        -- e.g. [[1,2], [8,11], [4,5]]
        -- Issue: we could also do stuff like [[1,2], [8,11,5], [4,5]] (NO ERROR),
            because it's still a list of lists with numbers, but it doesn't make sense.
-- Method 2: use a tuple of size TWO (a 2-tuple also called a pair)
    type: tuple of two numbers
    -- e.g. (1,2)
    -- e.g. (8,11)
    -- e.g. (4,5)
    -- How about putting a couple of vectors in a list to represent points
        of a shape on a two-dimensional plane?
        -- e.g. [(1,2), (8,11), (4,5)]
        -- try [(1,2), (8,11,5), (4,5)]
            -- ERROR
            -- (8,11,5) is a tuple of three numbers (a 3-tuple also called a triple)
            -- a list cannot have several pairs and several triples
-- Conclusion: use Method 2 (tuple)


e.g. [(1,2), ("One",2)]
-- ERROR
-- (1,2) is a pair of numbers 
-- ("One",2) is a pair consisting of a string and a number

e.g. Represent someone's name and age.
-- use a triple: ("Christopher", "Walken", 55)
-- tuples can contain lists

Use tuples when you know HOW MANY components the data should have.
Tuples with different size have different types.

There are singleton lists (i,e, list with only ONE element),
however there's NO singleton tuple. A singleton tuple would just
be the value it contains and as such would have no benefit to us.

Tuples can be compared with each other if their components can be compared.
We can NOT compare two tuples of different sizes, because their types are different.
-}

{- Tuple Functions -}
{-
Two useful functions ONLY for pairs (2-tuples):
-- $fst$ takes a PAIR and returns its first component
    -- e.g. fst (8, 11)
    -- e.g. fst ("Wow", False)
-- $snd$ takes a PAIR and returns its second component
    -- e.g. snd (8, 11)
    -- e.g. snd ("Wow", False)

$zip$: takes two lists and then zips them together into one list by joining the
matching elements into pairs.
-- especially useful when you want to combine two lists in a way or
    traverse two lists simultaneously
    -- e.g. zip [1,2,3,4,5] [5,5,5,5,5]  
-- can take two lists that contain different types
    -- e.g. zip [1 .. 5] ["one", "two", "three", "four", "five"]
-- if the lengths of the two lists don't match, the longer list gets cut off
    to match the length of the shorter one
    -- e.g. zip [1,2,3,4,5] [5,5,5,5,5,6,6,6,6]
    -- e.g. zip [1,2,3,4,5,6,7,8,9] [5,5,5,5,5]
    -- e.g. zip [1,2,3,4,5] [] 
    -- e.g. zip [] [5,5,5,5,5] 
    -- e.g. zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
-- can zip finite lists with infinite lists, because Haskell is lazy
    -- e.g. zip [1..] ["apple", "orange", "cherry", "mango"]
-}

{- Applications -}
{-
Problem: which right triangle that has integers for all sides and
    all sides equal to or smaller than 10 has a perimeter of 24?

Step 1: try to generate ALL triangles with sides equal to or smaller than 10
-- draw from three lists and output function is combining them into a triple
-- let triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

Step 2: add a condition that they all have to be RIGHT triangles
-- assumption: side c is the hypothenuse
-- condition: a^2 + b^2 == c^2
-- let rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] 

Step 3: filter right triangles with perimeter 24
-- let rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  



COMMON PATTERN to solve problems in functional programming:
-- (1) take a starting set of solutions
-- (2) apply transformations to those solutions
-- (3) filter them until getting the right ones
-}