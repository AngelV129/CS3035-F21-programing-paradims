{-
Export options for type value constructors:

-- "Shape(..)": export all constructors
    Whoever imports our module can make shapes
    (both circles and rectangles) by using the
    "Rectangle" and "Circle" value constructors
    or using the auxilliary functions "baseCircle"
    and "baseRect". It's the same as writing
    "Shape (Rectangle, Circle)".

-- "Shape(Circle)": only export constructor "Circle"
    Someone importing our module could make circles
    by using the "Circle" value constructors or the
    auxilliary function "baseCircle", and can make
    rectangles only by using the auxilliary function
    "baseRect".
    
-- "Shape": NOT export any contructor
    Someone importing our module could only make
    shapes (both circles and rectangles) by using
    the auxilliary functions "baseCircle" and "baseRect".

Not exporting the value constructors of a data types
makes them more ABSTRACT in such a way that we hide their
implementation. Also, whoever uses our module can NOT pattern
match against the non-exported value constructors.
-}

module Shapes   
( Point(..)
, Shape(..)
--, Shape(Circle)
--, Shape
, area  
, shift  
, baseCircle  
, baseRect  
) where  

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float
            | Rectangle Point Point
            deriving (Show)

area :: Shape -> Float  
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

shift :: Shape -> Float -> Float -> Shape
shift (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
shift (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  

baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)