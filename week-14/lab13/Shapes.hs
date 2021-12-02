module Shapes   
( Point(..)
, Shape(..)
, area 
) where  

data Point = Point Float Float

data Shape = Circle Point Float
            | Rectangle Point Point


area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)


perimeter :: Shape -> Float  
