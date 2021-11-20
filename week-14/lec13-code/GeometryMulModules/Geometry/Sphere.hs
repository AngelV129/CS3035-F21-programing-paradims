{-
the module name is "Geometry.Sphere"

the "Sphere.hs" file inside "Geometry" folder
-}

module Geometry.Sphere  
( volume  
, area  
) where  

volume :: Float -> Float  
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
area :: Float -> Float  
area radius = 4 * pi * (radius ^ 2) 