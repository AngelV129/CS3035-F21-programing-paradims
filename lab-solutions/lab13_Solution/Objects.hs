module Objects  
( Object(..)
, volume 
, area
) where  

data Object = Sphere Float
            | Cube Float
            | Cuboid Float Float Float

volume :: Object -> Float  
volume (Sphere radius) = (4.0 / 3.0) * pi * (radius ^ 3)
volume (Cube side) = volume (Cuboid side side side)
volume (Cuboid a b c) = rectangleArea a b * c 

area :: Object -> Float  
area (Sphere radius) = 4 * pi * (radius ^ 2)
area (Cube side) = area (Cuboid side side side)
area (Cuboid a b c) = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  
