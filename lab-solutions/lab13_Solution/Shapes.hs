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
area (RightTriangle p1 p2 p3)
    | pythagorean p1 p2 p3 = (distance p1 p2) * (distance p1 p3) / 2 -- p1 is right angle
    | pythagorean p2 p1 p3 = (distance p2 p1) * (distance p2 p3) / 2 -- p2 is right angle
    | pythagorean p3 p1 p2 = (distance p3 p1) * (distance p3 p2) / 2 -- p3 is right angle
    where
        pythagorean p1 p2 p3 = (distance p1 p2) ^2 + (distance p1 p3) ^2 == (distance p2 p3) ^2 -- Pythagorean theorem

perimeter :: Shape -> Float
perimeter (Circle _ r) = 2 * pi * r
perimeter (Rectangle (Point x1 y1) (Point x2 y2)) = 2 * ((abs $ x2 - x1) + (abs $ y2 - y1))
perimeter (RightTriangle p1 p2 p3) = distance p1 p2 + distance p1 p3 + distance p2 p3

-- calculate the distance between two points, i.e. |p2 - p1|
distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2
