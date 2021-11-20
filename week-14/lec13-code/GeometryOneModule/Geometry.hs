{-At the beginning of a module, we specify the module name.
If we have a file called "Geometry.hs", then we should name
our module "Geometry" (same with the file name,
first character is CAPITALIZED).

Then, we specify the functions that it EXPORTs and after that,
we can start writing/implementing the functions.

When importing "Geometry" module, we can ONLY use functions
EXPORTed by "Geometry", i.e., "sphereVolume", "sphereArea",
"cubeVolume", "cubeArea", "cuboidArea", "cuboidVolume".

The NON-EXPORTed functions, such as "rectangleArea", can
ONLY be used by "Geometry" module internally.
-}

module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  

sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  
  
cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  
  
cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  