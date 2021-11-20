import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube 

testCases = [
                (Sphere.volume 1, "Sphere.volume 1"),
                (Sphere.area 1, "Sphere.area 1"),
                (Cube.volume 2, "Cuboid.volume 2"),
                (Cube.area 2, "Cuboid.area 2"),
                (Cuboid.volume 1 2 3, "Cube.volume 1 2 3"),
                (Cuboid.area 1 2 3, "Cube.area 1 2 3")                
            ]

printTestResult x = do
    putStr ("The result of \"" ++ (snd x) ++ "\" is: ")
    print (fst x) 
    putStrLn ""

main = do
    putStrLn "\n**********************************************\n"
    mapM_ printTestResult testCases
    putStrLn "\n**********************************************\n"