import Geometry

testCases = [
                (sphereVolume 1, "sphereVolume 1"),
                (sphereArea 1, "sphereArea 1"),
                (cubeVolume 2, "cubeVolume 2"),
                (cubeArea 2, "cubeArea 2"),                
                (cuboidVolume 1 2 3, "cuboidVolume 1 2 3"),
                (cuboidArea 1 2 3, "cuboidArea 1 2 3")
            ]

printTestResult x = do
    putStr ("The result of \"" ++ (snd x) ++ "\" is: ")
    print (fst x) 
    putStrLn ""

main = do
    putStrLn "\n**********************************************\n"
    mapM_ printTestResult testCases
    putStrLn "\n**********************************************\n"