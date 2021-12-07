module Shapes   
( Point(..)
, Shape(..)
, area
, perimeter 
) where  

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float
            | Rectangle Point Point
            | RightTriangle Point Point Point
            deriving (Show)
            



area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- area (RightTriangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =   ((abs $ y3 - y1 ) * (abs $ x2 - x1)) / 2          -- 1/2 b*h
-- area (RightTriangle (Point x1 y1) (Point x2 y2) (Point x3 y3))  
--     | (x1 == 0 && y1 == 0) = ((abs $ y3 - y1 ) * (abs $ x2 - x1)) / 2
--     | (x2 == 0 && y2 == 0) =  ((abs $ y3 - y2 ) * (abs $ x1 - x2)) / 2
--     | otherwise =  ((abs $ y2 - y3 ) * (abs $ x1 - x3)) / 2
area (RightTriangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =  (1/2) * abs (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))

perimeter :: Shape -> Float  
perimeter (Circle _ r) = 2 * pi * r
perimeter (Rectangle (Point x1 y1) (Point x2 y2)) = 2 *  ((abs $ x2 - x1) + (abs $ y2 - y1))
-- perimeter (RightTriangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = ((abs $ y3 - y1 ) + (abs $ x2 - x1)) + sqrt (((abs $ y3 - y1 ) ^ 2) + ((abs $ x2 - x1) ^ 2 ) ) -- a + b + squrt (a ^ 2  + b^2)
-- perimeter (RightTriangle (Point x1 y1) (Point x2 y2) (Point x3 y3))
--     | (x1 == 0 && y1 == 0) = ((abs $ y3 - y1 ) + (abs $ x2 - x1)) + sqrt (((abs $ y3 - y1 ) ^ 2) + ((abs $ x2 - x1) ^ 2 ) )
--     | (x2 == 0 && y2 == 0) = ((abs $ y3 - y2 ) + (abs $ x1 - x2)) + sqrt (((abs $ y3 - y2 ) ^ 2) + ((abs $ x1 - x2) ^ 2 ) )
--     | otherwise = ((abs $ y2 - y3 ) + (abs $ x1 - x3)) + sqrt (((abs $ y2 - y3 ) ^ 2) + ((abs $ x1 - x3) ^ 2 ) )
perimeter (RightTriangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = distance (Point x1 y1) (Point x2 y2) + distance (Point x2 y2) (Point x3 y3) + distance (Point x3 y3) (Point x1 y1)
    where distance (Point x1 y1) (Point x2 y2) = sqrt $ ((x2 - x1) ^2) +  (y2 - y1) ^ 2
