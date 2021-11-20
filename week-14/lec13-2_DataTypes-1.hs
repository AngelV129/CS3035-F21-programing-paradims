{- Making Our Own Data Types 1 -}


{-
Syntax to define our own data types ("MyBool" as example):
-- "data" means that we're defining a new data type.
-- The part before the = denotes the type name, i.e. "MyBool".
-- The parts after the = are type value constructors, which
    specify the different values that this type can have.
    The | (read as "or") is to seperate different type values.
-- The FIRST character of both the type name and the value
    constructors have to be capitalized.
-}
data MyBool = Fal | Tru


{-
Why to define our own data types?

Suppose we would like to represent a "shape".
-- Solution One: use tuples
-- Solution Two: make our own type (better solution)

For Solution One (using tuples), a "circle" could be denoted as
(0.0, 0.0, 1.0) where the first and second fields are the
coordinates of the circle's center and the third field is the radius.
However, (0.0, 0.0, 1.0) could also represent a 3D vector or anything else.

In addition, different shapes requires tuples with different
number of fields. For instance, a "rectangle" could be denoted as
(0.0, 0.0, 2.0, 1.0) where the first and second fields are the
coordinates of the lower left corner and the third and fourth
fields are the coordinates of the upper right corner.

When we just see (0.0, 0.0, 1.0) and (0.0, 1.0, 2.0, 0.0),
we usually do NOT think they both represent a "shape".
Hence, the Solution One is not a good solution.


For Solution Two, the "Shape" type is defined as follows.
-- The "Circle" value constructor has three fields, which take floats. 
-- The "Rectangle" value constructor has four fields, which take floats.
-- When we write a value constructor, we can optionally add some types
    after it and those types define the values it will contain.
-- Actually, value constructors are FUNCTIONs that take the "fields"
    as input parameters and ultimately return a value of a data type.
    e.g. 
    Circle :: Float -> Float -> Float -> Shape
    Rectangle :: Float -> Float -> Float -> Float -> Shape
-}
data Shape = Circle Float Float Float 
           | Rectangle Float Float Float Float
           deriving (Show)


{-
How to define variables with our own data types?
-- types WITHOUT "input fields":
    the SAME with types pre-defined by Haskell (such as Bool, Int, Float)
    e.g. you can use "MyBool" type in the same way with "Bool" type.
-- types WITH "input fields":
    the SAME with function applications

To display these vairable (with our own data type) values
as a string, we need to  make our "Shape" type part of the
"Show" typeclass using "deriving" KEYWORD.
-}
testCircle = Circle 0.0 0.0 1.0
testRectangle = Rectangle 0.0 0.0 2.0 1.0
testCircleList = map (Circle 0 0) [1,2,3,4]


{-
How to use variables with our own data types?

Write a function that takes a shape and returns its area.
-- The function type is "Shape -> Float", not "Circle -> Float"
    nor "Rectangle -> Float". Because "Circle" or "Rectangle" is
    NOT a type, "Shape" is.
-- We can PATTERN MATCH against type value constructors.
-}
area :: Shape -> Float  
area (Circle _ _ r) = pi * r ^ 2  
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- area (Rectangle x1 y1 x2 y2) = (abs (x2 - x1)) * (abs (y2 - y1))

testCircleArea = area testCircle
testRectangleArea = area testRectangle
testCircleListArea = map area testCircleList


{-
Refine out "Shape" type with an intermediate "Point" data type,
we can make our shapes more understandable.

Note: both the type name and the value constructors must be UNIQUE.
-}
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float
             | Rectangle' Point Point
             deriving (Show)

point1 = Point 0.0 0.0
point2 = Point 2.0 1.0
testCircle' = Circle' point1 1.0
testRectangle' = Rectangle' point1 point2
testCircleList' = map (Circle' point1) [1,2,3,4]

area' :: Shape' -> Float  
area' (Circle' _ r) = pi * r ^ 2
area' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- area' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs (x2 - x1)) * (abs (y2 - y1))

testCircleArea' = area' testCircle'
testRectangleArea' = area' testRectangle'
testCircleListArea' = map area' testCircleList'


{-
We usually combine the use of our own data types with modules.
-- see "ShapesModule.zip" file
-}