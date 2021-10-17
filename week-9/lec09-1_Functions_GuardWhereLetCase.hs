{- Lecture 09-1: Functions -- Guard, Where, Let, Case -}
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




{- Guards -}
{-
Patterns are a way of making sure a value conforms to some form and
deconstructing it.

Guards are a way of testing whether some property of a value (or several of them)
are true or false. 
-- very similar to $if$ statements
-- advantages of guards (compared to $if$):
    -- more readable when having several conditions
    -- play really nicely with patterns

Syntax: Guards are indicated by pipes that follow a function's name and
its parameters.
-- THERE IS NO = RIGHT AFTER THE FUNCTION NAME AND ITS PARAMETERS
-- Programming Style: usually indented a bit to the right and lined up

Semantics: 
-- the guards will be checked from top to bottom
-- a guard is basically a boolean expression
    -- if it evaluates to $True$, then the corresponding function body is used
    -- if it evaluates to $False$, check the next guard

$otherwise$ guard:
-- for most cases, the last guard is $otherwise$
-- $otherwise$ catches everything

How patterns and guards work together?
-- one pattern can have multiple guards
-- the same guard may appear in multipe patterns
-- evaluation order:
    -- check pattern (from top to bottom)
        -- if satisfied, check guard (from top to bottom)
            -- if $True$, use function body
            -- if $False$, go to check the next guard
            -- if ALL guards are $False$ (NO $otherwise$), go to the next pattern
        -- if NOT satisfied, go to check the next pattern
    -- If NO suitable guards or patterns are found, an error is thrown.

Example: Write a function to classify BMI as follows
        -- underweight: BMI <= 18.5
        -- normal: 18.5 < BMI <= 25
        -- overweight: 25 < BMI <= 30
        -- obese: BMI > 30
-}
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "underweight"  
    | bmi <= 25.0 = "normal"  
    | bmi <= 30.0 = "overweight"  
    | otherwise   = "obese"

-- Re-write $bmiTell$ using $if$ statement.
bmiTell'' :: (RealFloat a) => a -> String  
bmiTell'' bmi = if bmi <= 18.5
                then "underweight"
                else
                    if bmi <= 25.0
                    then "normal"
                    else
                        if bmi <= 30.0
                        then "overweight"
                        else "obese"

{-
Simulation of function all $bmiTell 24.3$:
-- first check if bmi <= 18.5, False
-- then check if bmi <= 25.0, True, return "normal"
-}

-- Modify $bmiTell$ to take a height and weight, calculate BMI, and classify it.
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height  
    | weight / height ^ 2 <= 18.5 = "underweight"  
    | weight / height ^ 2 <= 25.0 = "normal"  
    | weight / height ^ 2 <= 30.0 = "overweight"  
    | otherwise                   = "obese"

-- Implement our own version of the $max$ function using guards
max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b  
{-
Guards can also be written inline
-- max' a b | a > b = a | otherwise = b

RECOMMENDATION: NEVER write INLINE guards
-- because it's less readable, even for very short functions
-}

-- Implement our own version of the $compare$ function using guards
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT
{-
Note: we can also define infix functions using backticks
-- like call functions as infix with backticks
-}




{- Where Bindings -}
{-
In $bmiTell'$ definition, we repeat $weight / height ^ 2$ three times.
-- good practice: calculate it once, bind it to a name,  and then use that name
    instead of the expression

$where$ bindings:
-- put the keyword $where$ after the guards and then define SEVERAL NAMEs and/or FUNCTIONs
    -- these names are ONLY visible across the GUARDs
        -- $where$ bindings are NOT shared across function bodies of different patterns
    -- advantage:
        -- not repeat the same thing
        -- improve readability by giving names to expressions
        -- make programs faster (only calculate it ONCE)
-- Programming Style: usually indent $where$ as much as the pipes are indented
-- ONLY ONE occurance of the $where$ keyword even defining MULTIPLE names and/or functions
-}
bmiTell''' :: (RealFloat a) => a -> a -> String
bmiTell''' weight height  
    | bmi <= 18.5 = "underweight"  
    | bmi <= 25.0 = "normal"  
    | bmi <= 30.0 = "overweight"  
    | otherwise   = "obese"
    where bmi = weight / height ^ 2

-- Write a function that takes a person's first and last names, and gives the initials.
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname
{-
Note: we use $where$ bindings to pattern matching
-}

-- Write a function that takes a list of weight-height pairs, and returns a list of BMIs.
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2
{-
Note: we define FUNCTIONs in $where$ blocks, like defining names

Note: $where$ bindings can also be nested
-- It's a common idiom to make a function and define some helper function in
    its $where$ clause and then to give those functions helper functions as well,
    each with its own $where$ clause.
-}




{- Let Bindings/Expressions -}
{-
$let$ bindings are very similar to $where$ bindings.

$let$ vs $where$:
--  $where$ bindings are a syntactic construct that let you bind to variables
    at the end of a function and the whole function can see them, including all
    the guards.
-- $let$ bindings let you bind to variables anywhere and are expressions
    themselves, but are very local, so they do NOT span across guards.
    

Syntax: $let <bindings> in <expression>$
-- The names defined in the $let$ part are accessible to the $<expression>$
    after the $in$ part.
    -- MULTIPLE names should be ALIGNED in a single column
    -- for inline format, MULTIPLE names should be SEPARATED by semicolons
        -- e.g. let a = 100; b = 200; c = 300 in a*b*c  -- separated by semicolons
-- $let$ bindings can be used for pattern matching
    -- e.g. let (a,b,c) = (100,200,300) in a*b*c
    -- e.g. (let (a,b,c) = (1,2,3) in a+b+c) * 100  

$let$ bindings are EXPRESSIONs themselves:
-- the value of a $let$ binding is the value of $<expression>$ (which is
    evaluated with the names defined in $let <bindings>$)
-- can use $let$ bindings almost anywhere
-- e.g. 4 * (let a = 9 in a + 1) + 2
-- e.g. [let square x = x * x in (square 5, square 3, square 2)] -- define local functons
-- e.g. (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
-}

-- Write a function that takes a cylinder's height and radius, and calculate its surface area.
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

-- Re-write $calcBmis$ using $let$ bindings
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
{-
Note: we use $let$ bindings inside list comprehensions
-- the names defined in a $let$ inside a list comprehension are visible to the
    output function (the part BEFORE the |) and all predicates and sections
    that come AFTER of the $let$ binding
-- OMIT $in$ part
    -- because the visibility of the names is already predefined
    -- acctually, the $in$ part is the list comprehension
    -- $calcBmis'$ is equivalent to $calcBmis''$
        -- calcBmis'' xs = let bmi w h = w / h ^ 2 in [bmi w h | (w, h) <- xs]
-}

-- Write a function that only returns BMIs of overweight and obese.
calcBmisOverWeight :: (RealFloat a) => [(a, a)] -> [a]  
calcBmisOverWeight xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
{-
Note: we can NOT use the $bmi$ name in the $(w, h) <- xs$ part,
    because it's defined prior to the $let$ binding.
-}

{-
The $in$ part can also be omitted when defining functions and constants directly
in GHCi. If we do that, then the names will be visible throughout the entire
interactive session.

Try the following in GHCI:
-- let zoot x y z = x * y + z  
-- zoot 3 9 2  
-- let boot x y z = x * y + z in boot 3 4 2  
-- boot 3 9 2   -- ERROR
-}




{- Case Expressions -}
{-
Syntax:
case <expression> of <pattern_1> -> <result_1>  
                     <pattern_2> -> <result_2>                       
                     ...  
                     <pattern_n> -> <result_n> 

Semantics: $<expression>$ is matched against the patterns
-- The pattern matching action is the same as expected: 
    -- the first pattern that matches the expression is used
    -- if it falls through the whole case expression and no suitable pattern
        is found, a runtime error occurs
-}

head' :: [a] -> a  
head' [] = error "Can't call head on an EMPTY list!"  
head' (x:_) = x

-- Re-write the $head'$ function using $case$ expressions.
head'' :: [a] -> a  
head'' xs = case xs of []    -> error "Can't call head on an EMPTY list!"  
                       (x:_) -> x
{-
Note: pattern matching on parameters in function definitions is just syntactic
    sugar for $case$ expressions
-}

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of []  -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs  -> "a longer list."  
{-
Note: $case$ expressions can be used pretty much anywhere
-}

describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."
{-
Note: in the $where$ clause, define the function $what$ using pattern matching
-}