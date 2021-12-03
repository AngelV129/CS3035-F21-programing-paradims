module Objects  
( Object(..)
, volume 
, area
) where  


data Object = Sphere Float -- radius
            | Cube Float  
            | Cuboid Float Float Float 


volume :: Object -> Float  
volume (Sphere r) = (4/3) * pi * (r ^ 3)
volume (Cube a) = a ^ 3
volume (Cuboid l w h) = l * w * h


area :: Object -> Float
area (Sphere r ) = 4 * pi * r ^ 2
area (Cube a) = 6 * (a^2)
area (Cuboid l w h) = (2 * l * w) + (2 * l * h) + (2 * h * w)  
