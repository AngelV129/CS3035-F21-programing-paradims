{- Making Our Own Data Types 2 & I/O Examples -}




{- 1. Record Syntax -}

{-
Create a data type to describe a person,
including following information:
first name, last name, age, height, phone number,
and city.
-}
data Person = Person String String Int Float String String deriving (Show)
person1 = Person "John" "Smith" 43 6.0 "323-343-3000" "Los Angeles"


{-
Create a function to get seperate info from a person.
-}
firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
city :: Person -> String  
city (Person _ _ _ _ _ city) = city


{-
Disadvantages of Person type:
-- unreadable
-- require one function for every field
(think a data type with tens or hundreds fields)

Solution:
-- record syntax
-- benefit: create functions that lookup fields in the data type
-- e.g. Haskell automatically made 6 functions for the Person' type:
    firstName', lastName', age', height', phoneNumber', city'

firstName :: Person -> String
firstName' :: Person' -> String
-}
data Person' = Person' { firstName' :: String
                       , lastName' :: String  
                       , age' :: Int  
                       , height' :: Float  
                       , phoneNumber' :: String  
                       , city' :: String  
                       } deriving (Show)

{-
With record syntax, we do NOT have to necessarily put the fields
in the proper order, as long as we list all of them.
-}
person1'' = Person' { city' = "Los Angeles"
                    , firstName' = "John"
                    , phoneNumber' = "323-343-3000"
                    , lastName' = "Smith"
                    , age' = 43
                    , height' = 6.0                    
                    }
{-
Without record syntax, we have to specify all fields in order.
-}
person1' = Person' "John" "Smith" 43 6.0 "323-343-3000" "Los Angeles"




{- 2. Type Parameters -}

{-
A value constructor can take some value parameters and
then produce a new value. For instance, the Person'
constructor takes 6 values and produces a Person' value.

In a similar manner, type constructors can take types
as parameters to produce new types. 
(similar to templates in C++)

Example:
[a] -- list type, could be Int list, String list, Person list

Type parameters are useful because we can make different
types with them depending on what kind of types we want
contained in our data type.
-}

{-
Haskell pre-defined:
data Maybe a = Nothing | Just a

"a": type parameter
-- its value could be Int, Float, Bool, Char, String, Person, etc.

"Maybe": type constructor
"Nothing": value constructor
"Just": value constructor

"Maybe Int": type
-- its value could be "Just 3", "Nothing", etc.

"Maybe Bool": type
-- its value could be "Just False", "Nothing", etc.

"Maybe Person": type
-- its value could be "Just person1", "Nothing", etc.
-}

data Maybe' a = Nothing' | Just' a deriving (Show)
{-
Run the following in GHCI:

:t Just' 3

Just' 3 :: Maybe' Int
:t (Just' 3 :: Maybe' Int)

Just' 3 :: Maybe' Float
:t (Just' 3 :: Maybe' Float)

Just' "hello" :: Maybe' [Char]

:t Nothing'

Just' person1
:t Just' person1

Just' person1'
:t Just' person1'
-}




{- 3. Recursive Data Types -}

{-
A value constructor can have several (or none at all) fields
and each field must be of some concrete type.

How about some fields having the same type with the value constructor?
-- Recursive Data Types
-}


{-
List Data Structure:
[]
[5] == 5:[]
[3,4,5] == 3:[4,5] == 3:4:[5] == 3:4:5:[] == 3:(4:(5:[]))
: operator is right-associative
-}
data List a = Empty 
            | Cons a (List a)
            deriving (Show)

list1 = Empty  
list2 = 3 `Cons` (4 `Cons` (5 `Cons` Empty))  
list2' = Cons 3 (Cons 4 (Cons 5 Empty))


{-
Binary Search Tree Data Structure:
-- The LEFT subtree of a node contains only
    nodes with keys LESSER than the node's key.
-- The RIGHT subtree of a node contains only
    nodes with keys GREATER than the node's key.
-- The left and right subtree each must also
    be a binary search tree.
-}
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
            deriving (Show) 

tree1 = EmptyTree
tree2 = Node 5 (Node 3 (Node 1 EmptyTree EmptyTree)
                       (Node 4 EmptyTree EmptyTree) ) 
               (Node 7 (Node 6 EmptyTree EmptyTree)
                       (Node 8 EmptyTree EmptyTree) )

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  

treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

tree3 = singleton 5
tree4 = treeInsert 3 tree3
tree5 = treeInsert 1 tree4  
tree6 = treeInsert 4 tree5 
tree7 = treeInsert 7 tree6 
tree8 = treeInsert 6 tree7 
tree9 = treeInsert 8 tree8




{- 4. I/O Examples -}

a :: IO ()
a = putStrLn "Hello CS3035." 
b = putStr "Hello CS3035."

c = print 3
d = print "Hello CS3035." 

e = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    if null name then
        return ()
    else do  
        putStrLn ("Welcome to CS3035, " ++ name ++ ".")

f = mapM print [1,2,3]
g = mapM_ print [1,2,3]
     