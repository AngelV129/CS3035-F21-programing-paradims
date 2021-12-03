module Shapes   
( Point(..)
, Shape(..)
, area
, perimeter 
) where  

data Point = Point Float Float

data Shape = Circle Point Float
            | Rectangle Point Point
            | RightTriangle Point Point Point
            



area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
area (RightTriangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =   ((abs $ y3 - y1 ) * (abs $ x2 - x1)) / 2          -- 1/2 b*h


perimeter :: Shape -> Float  
perimeter (Circle _ r) = 2 * pi * r
perimeter (Rectangle (Point x1 y1) (Point x2 y2)) = 2 *  ((abs $ x2 - x1) + (abs $ y2 - y1))
perimeter (RightTriangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = ((abs $ y3 - y1 ) + (abs $ x2 - x1)) + sqrt (((abs $ y3 - y1 ) ^ 2) + ((abs $ x2 - x1) ^ 2 ) ) -- a + b + squrt (a ^ 2  + b^2)