module Main where

import qualified Objects as O

testCases = [
                (compareFloat (O.volume $ O.Sphere 1) (4/3*pi), "(O.volume $ O.Sphere 1) == (4/3)*pi"),
                (compareFloat (O.area $ O.Sphere 1) (4*pi), "(O.area $ O.Sphere 1) == 4*pi"),
                (compareFloat (O.volume $ O.Cube 2) 8, "(O.volume $ O.Cube 2) == 8"),
                (compareFloat (O.area $ O.Cube 2) 24, "(O.area $ O.Cube 2) == 24"),
                (compareFloat (O.volume $ O.Cuboid 1 2 3) 6, "(O.volume $ O.Cuboid 1 2 3) == 6"),
                (compareFloat (O.area $ O.Cuboid 1 2 3) 22, "(O.area $ O.Cuboid 1 2 3) == 22"),
                -- more testing cases
                (compareFloat (O.volume $ O.Sphere 3) (36*pi), "(O.volume $ O.Sphere 3) == 36*pi"),
                (compareFloat (O.area $ O.Sphere 3) (36*pi), "(O.area $ O.Sphere 3) == 36*pi"),
                (compareFloat (O.volume $ O.Cube 3) 27, "(O.volume $ O.Cube 3) == 27"),
                (compareFloat (O.area $ O.Cube 3) 54, "(O.area $ O.Cube 3) == 54"),
                (compareFloat (O.volume $ O.Cuboid 3 2 3) 18, "(O.volume $ O.Cuboid 3 2 3) == 18"),
                (compareFloat (O.area $ O.Cuboid 3 2 3) 42, "(O.area $ O.Cuboid 3 2 3) == 42")
            ]

compareFloat :: Float -> Float -> Bool
compareFloat a b = (abs $ a - b) <= 0.00001

filterFalseTestCases xs = filter (\x -> not (fst x)) xs

printTestResult xs
    | null xs   = do
                    putStrLn "\n*****************************"
                    putStrLn "All testing cases are TRUE."
                    putStrLn "*****************************\n"
    | otherwise = do
                    putStrLn "\n****************************************"
                    putStrLn "The following testing cases are FALSE:"
                    putStrLn "----------------------------------------\n"
                    mapM_ (\x -> putStrLn (snd x)) xs
                    putStrLn "\n****************************************\n"

main = printTestResult (filterFalseTestCases testCases)
