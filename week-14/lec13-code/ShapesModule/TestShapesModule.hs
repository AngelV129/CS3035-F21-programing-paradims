module Main where

import Shapes

testCases = [
                (area $ shift (baseCircle 1) 2 3, "area $ shift (baseCircle 1) 2 3"),
                (area $ shift (baseRect 1 2) 2 3, "area $ shift (baseRect 1 2) 2 3"),
                (area $ Circle (Point 2 3) 1, "area $ shift (baseCircle 1) 2 3"),
                (area $ Rectangle (Point 2 3) (Point 3 5), "area $ shift (baseRect 1 2) 2 3")
            ]

printTestResult x = do
    putStr ("The result of \"" ++ (snd x) ++ "\" is: ")
    print (fst x) 
    putStrLn ""

main = do
    putStrLn "\n**********************************************\n"
    mapM_ printTestResult testCases
    putStrLn "\n**********************************************\n"