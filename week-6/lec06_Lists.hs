{- Lecture 06: Lists -}
{-
List is the most used data structure and can be used in different ways 
to solve problems.
We will introduce the basics of lists, strings (which are lists of characters),
and list comprehensions (see the next lecture note).
-}


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


{- List Basics -}
{-
In Haskell, lists are a homogenous data structure, i.e., a list can ONLY
stores elements of the SAME type.
    -- e.g. a list of integers
    -- e.g. a list of characters
    -- can NOT have a list that has integers and characters

Lists are denoted by square brackets and the values in the lists
are separated by commas.
    -- the empty list is denoted by []

Strings are just lists of characters.
    -- character between single quotes
    -- string between double quotes
    -- e.g. "hello" is just syntactic sugar for ['h','e','l','l','o']
    -- CAN use list functions on strings

Note: We can use the $let$ keyword to define a name right in GHCI.
    -- e.g. Doing $let a = 1$ inside GHCI is the equivalent of writing $a = 1$
        in a script and loading it.
    -- ONLY use $let$ in GHCI, NOT in Haskell scripts

Try the following expressions in GHCI:
let lostNumbers = [4,8,15,16,23,42]  
lostNumbers
[1,2,'a',3,'b','c',4]   -- ERROR
"hello"
['h','e','l','l','o']

Nested Lists:
    -- Lists can also contain lists. 
    -- They can also contain lists that contain lists that contain lists ...
    -- e.g. let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
    -- The lists within a list CAN be of different lengths but they can NOT be
        of different types. e.g. You can NOT have a list that has some lists
        of characters and some lists of numbers.

The following three lists are DIFFERENT:
    -- [] is an empty list
    -- [[]] is a list that contains one empty list
    -- [[],[],[]] is a list that contains three empty lists
-}


{- List Operations -}
{-
(1) list concatenation: use the ++ operator which takes two lists
    -- e.g. [1,2,3,4] ++ [9,10,11,12]  
    -- e.g. "hello" ++ " " ++ "world"  
    -- e.g. ['w','o'] ++ ['o','t']
    -- e.g. b ++ [[1,1,1,1]]
    -- e.g. [[]] ++ [[],[],[]]
    -- Haskell has to walk through the ENTIRE list on the left side of ++ operator.
        Putting something at the end of a VERY LARGE list takes long time.

(2) cons operator: use the : operator (also called the cons operator) to put
ONE element at the beginning of a list
    -- e.g. 'A':" SMALL CAT"
    -- e.g. 5:[1,2,3,4,5]
    -- e.g. [6,6,6]:b  
    -- e.g. []:[[]]
    -- [1,2,3] is actually just syntactic sugar for 1:2:3:[]
    -- the cons operation is instantaneous (i.e. constant time)
    -- cons operator VS ++ operator
        -- the : operator takes a single element and a list
        -- the ++ operator takes two lists

(3) !! operator: get an element out of a list by index
    -- the index starts at 0
    -- e.g. "Steve Buscemi" !! 6  
    -- e.g. [9.4,33.2,96.2,11.2,23.25] !! 1  
    -- e.g. b !! 2
    -- e.g. [[],[],[]] !! 1
    -- e.g. "hello" !! 5    -- ERROR
    -- be careful about out-of-bound index error

(4) list comparson:
    -- Lists can be compared if the elements can be compared.
    -- Lists are compared in lexicographical order: the first elements are
        compared; if first elements are equal, the second elements are compared; ...
    -- comparson operators: <, <=, >, >=, ==, /=
    -- e.g. [3,2,1] > [2,1,0]  
    -- e.g. [3,2,1] > [2,10,100]  
    -- e.g. [3,4,2] > [3,4]  
    -- e.g. [3,4,2] > [2,4]  
    -- e.g. [3,4,2] == [3,4,2]
    -- e.g. [3,4,2] /= [3,4]
    -- e.g. [[]] == [[],[],[]]
    -- e.g. [] == [[],[],[]]
    -- e.g. [[]] == []
-}


{- List Functions -}
{-
(1) $head$ takes a list and returns its head
    -- head of a list is the FIRST element
    -- http://s3.amazonaws.com/lyah/listmonster.png
    -- e.g. head [5,4,3,2,1]
    -- e.g. head [5]
    -- e.g. head b
    -- e.g. head "hello"
    -- e.g. head [] -- ERROR
    -- e.g. head [[]]
    -- e.g. head [[],[],[]]

(2) $tail$ takes a list and returns its tail
    -- tail of a list is its sublist without the head, i.e. a list containing
        all elements after the head
    -- http://s3.amazonaws.com/lyah/listmonster.png
    -- e.g. tail [5,4,3,2,1]  
    -- e.g. tail [5] 
    -- e.g. tail b 
    -- e.g. tail "hello"
    -- e.g. tail [] -- ERROR
    -- e.g. tail [[]]
    -- e.g. tail [[],[],[]]

(3) $last$ takes a list and returns its last element
    -- http://s3.amazonaws.com/lyah/listmonster.png
    -- e.g. last [5,4,3,2,1]  
    -- e.g. last [5]  
    -- e.g. last b 
    -- e.g. last "hello"
    -- e.g. last [] -- ERROR
    -- e.g. last [[]]
    -- e.g. last [[],[],[]]

(4) $init$ takes a list and returns everything except its last element
    -- http://s3.amazonaws.com/lyah/listmonster.png
    -- e.g. init [5,4,3,2,1]  
    -- e.g. init [5]  
    -- e.g. init b 
    -- e.g. init "hello"
    -- e.g. init [] -- ERROR
    -- e.g. init [[]]
    -- e.g. init [[],[],[]]

CAUTION:
Do NOT apply $head$, $tail$, $last$, and $init$ functions to EMPTY lists.
This error cannot be caught at compile time, so it's always good practice
to take precautions against accidentally telling Haskell to give you some
elements from an empty list.

(5) $length$ takes a list and returns its length
    -- length of a list is the number of elements
    -- e.g. length [5,4,3,2,1]  
    -- e.g. length [5] 
    -- e.g. length b 
    -- e.g. length "hello" 
    -- e.g. length []
    -- e.g. length [[]]
    -- e.g. length [[],[],[]]

(6) $null$ checks if a list is empty
    -- If the list is empty, it returns $True$; otherwise it returns $False$.
    -- RECOMMENDATION: use $null xs$ instead of $xs == []$ to check whether list $xs$ is empty
    -- e.g. null [5,4,3,2,1]  
    -- e.g. null [5]  
    -- e.g. null b
    -- e.g. null "hello"
    -- e.g. null []
    -- e.g. null [[]]
    -- e.g. null [[],[],[]]

(6) $reverse$ reverses a list
    -- e.g. reverse [5,4,3,2,1]  
    -- e.g. reverse [5]
    -- e.g. reverse b 
    -- e.g. reverse "hello" 
    -- e.g. reverse []
    -- e.g. reverse [[]]
    -- e.g. reverse [[],[],[]]

(7) $take$ takes a number and a list, and extracts the number of elements from
the beginning of the list
    -- e.g. take 3 [5,4,3,2,1]  
    -- e.g. take 1 [3,9,3] 
    -- e.g. take 2 b 
    -- e.g. take 3 "hello" 
    -- e.g. take 5 [1,2]  
    -- e.g. take 0 [6,6,6]
    -- e.g. take (-2) [6,6,6]
    -- e.g. take 1 []
    -- e.g. take 0 []
    -- If we try to take MORE elements than there are in the list, it just returns the list. 
    -- If we try to take ZERO or NEGATIVE number elements, we get an empty list [].
    -- If we try to take ANY number elements from an empty list, it always returns [].

(8) $drop$ takes a number and a list, and drops the number of elements from
the beginning of the list
    -- e.g. drop 3 [5,4,3,2,1]  
    -- e.g. drop 1 [3,9,3] 
    -- e.g. drop 2 b 
    -- e.g. drop 3 "hello"
    -- e.g. drop 5 [1,2]  
    -- e.g. drop 0 [6,6,6]
    -- e.g. drop (-2) [6,6,6]
    -- e.g. drop 1 []
    -- e.g. drop 0 []
    -- If we try to drop MORE elements than there are in the list, we get an empty list [].
    -- If we try to drop ZERO or NEGATIVE number elements, it just returns the list.
    -- If we try to drop ANY number elements from an empty list, it always returns [].

(9) $maximum$ takes a list of elements that can be put in some kind of order and
returns the largest element
    -- e.g. maximum [5,4,3,2,1]
    -- e.g. maximum [5]
    -- e.g. maximum b
    -- e.g. maximum "hello"
    -- e.g. maximum []  -- ERROR
    -- e.g. maximum [[]]
    -- e.g. maximum [[],[],[]]

(10) $minimum$ takes a list of elements that can be put in some kind of order and
returns the smallest element
    -- e.g. minimum [5,4,3,2,1]
    -- e.g. minimum [5]
    -- e.g. minimum b
    -- e.g. minimum "hello"
    -- e.g. minimum []  -- ERROR
    -- e.g. minimum [[]]
    -- e.g. minimum [[],[],[]]

(11) $sum$ takes a list of NUMBERs and returns their sum
    -- e.g. sum [5,4,3,2,1]
    -- e.g. sum [5]
    -- e.g. sum b   -- ERROR
    -- e.g. sum "hello" -- ERROR
    -- e.g. sum []  -- returns 0
    -- e.g. sum [[]]    -- ERROR
    -- e.g. sum [[],[],[]]  -- ERROR

(12) $product$ takes a list of NUMBERs and returns their product
    -- e.g. product [5,4,3,2,1]
    -- e.g. product [5]
    -- e.g. product b   -- ERROR
    -- e.g. product "hello" -- ERROR
    -- e.g. product []  -- returns 1
    -- e.g. product [[]]    -- ERROR
    -- e.g. product [[],[],[]]  -- ERROR

(13) $elem$ takes a thing and a list of things, and tells whether that thing is an element
of the list. 
    -- e.g. elem 4 [5,4,3,2,1]
    -- e.g. elem 0 [5,4,3,2,1]
    -- e.g. elem 'h' [5,4,3,2,1]    -- ERROR
    -- e.g. elem [] b
    -- e.g. elem [1,2,3] b
    -- e.g. elem 5 b    -- ERROR
    -- e.g. elem 'h' "hello"
    -- e.g. elem 'a' "hello"
    -- e.g. elem 5 "hello"    -- ERROR
    -- e.g. elem 5 []   -- NO error
    -- e.g. elem 'h' [] -- NO error
    -- e.g. elem [] [[]]
    -- e.g. elem [] [[],[],[]]

Note: We only introduce a few basic functions that operate on lists. You can get all list
operation functions from the Data.List module (https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html).
-}


{- List Ranges -}
{-
What if we want a list of all numbers between 1 and 20?
    -- bad solution: just type all elements
    -- good solution: use ranges

Ranges are a way of making lists that are arithmetic sequences of elements
that can be enumerated.
    -- Numbers can be enumerated: one, two, three, four, etc.
    -- Characters can also be enumerated: the alphabet is an enumeration
        of characters from A to Z.
    -- Names can NOT be enumerated: what comes after "John"? (we don't know)
    -- syntax: inside the square brackets, separate the lower limit
        (i.e. the $head$ of the list) and the upper limit
        (i.e. the $last$ of the list) with two periods
    -- e.g. [1..20]  
    -- e.g. ['a'..'z']  
    -- e.g. ['K'..'Z'] 

What if we want all EVEN numbers between 1 and 20? Or every third number between 1 and 20?
    -- the range step is 1 by default
    -- can also specify a range step other than 1
    -- syntax: inside the square brackets, separate the first TWO elements
        with a comma, specify what the upper limit is, and separate the second
        element and the upper limit (which may NOT be the $last$ of the list)
        with two periods
    -- e.g. [2,4..20]   -- step 2
    -- e.g. [3,6..20]   -- step 3
    -- e.g. [20..1] -- returns [], because lower_limit > upper_limit using default step 1
    -- e.g. [20,19..1]  -- step (-1)

What if we want all positive EVEN numbers?
    -- note: we do not give an EXACT range
    -- can use ranges to make INFINITE lists
    -- syntax: do NOT specify the upper limit
    -- e.g. [2,4..]

What if we want the first 24 multiples of 13?
    -- method 1: [13,26..24*13]   -- WORK
    -- method 2: take 24 [13,26..] -- BETTER
        -- Because Haskell is lazy, it won't try to evaluate the infinite
            list immediately because it would never finish. It'll wait to
            see what you want to get out of that infinite lists. And here
            it sees you just want the first 24 elements.

Why the method 2 is better? Considering we want the first 34 multiples of 13.
    -- method 1
        -- need to change the list
        -- [13,26..34*13]
    -- method 2
        -- do NOT need to change the (infinite) list
        -- just change the element number we want
        -- take 34 [13,26..]


Limitations of Ranges:
-- (1) Ranges do NOT work if elements have different steps,
because firstly we can only specify ONE step, secondly some sequences
that are not arithmetic are ambiguous if given only by a few of their first terms.
    -- e.g. Can we get all the powers of 2 using ranges?
        -- [1,2,4,8,16..128]    -- ERROR
-- (2) Using floating-point numbers in ranges can yield some pretty funky results,
because floating-point numbers are NOT completely PRECISE (by definition).
    -- e.g. [0.1, 0.3 .. 1]
    -- RECOMMENDATION: do NOT use FLOATing-point numbers in list ranges
-}


{- Functions Producing Infinite Lists -}
{-
(1) $cycle$ takes a list and cycles it into an infinite list
    -- e.g. take 10 (cycle [1,2,3])  
    -- e.g. take 12 (cycle "LOL ") 

(2) $repeat$ takes an element and produces an infinite list of just that element
    -- like cycling a list with only one element
    -- e.g. take 10 (repeat 5)
    -- e.g. take 10 (cycle [5])
    -- It's simpler to just use the $replicate$ function if you want
        some number of the same element in a list.
    -- e.g. replicate 10 5
-}