module Main where

import qualified Shapes as S
import qualified Objects as O

testCases = [
                (compareFloat (S.area $ S.Circle (S.Point 0 0) 1) pi, "(S.area $ S.Circle (S.Point 0 0) 1) == pi"),
                (compareFloat (S.perimeter $ S.Circle (S.Point 0 0) 1) (2*pi), "(S.perimeter $ S.Circle (S.Point 0 0) 1) == 2*pi"),
                (compareFloat (S.area $ S.Rectangle (S.Point 0 0) (S.Point 2 1)) 2, "(S.area $ S.Rectangle (S.Point 0 0) (S.Point 2 1)) == 2"),
                (compareFloat (S.perimeter $ S.Rectangle (S.Point 0 0) (S.Point 2 1)) 6, "(S.perimeter $ S.Rectangle (S.Point 0 0) (S.Point 2 1)) == 6"),
                (compareFloat (S.area $ S.RightTriangle (S.Point 0 0) (S.Point 3 0) (S.Point 0 4)) 6, "(S.area $ S.RightTriangle (S.Point 0 0) (S.Point 3 0) (S.Point 0 4)) == 6"),
                (compareFloat (S.perimeter $ S.RightTriangle (S.Point 0 0) (S.Point 3 0) (S.Point 0 4)) 12, "(S.perimeter $ S.RightTriangle (S.Point 0 0) (S.Point 3 0) (S.Point 0 4)) == 12"),
                (compareFloat (O.volume $ O.Sphere 1) (4/3*pi), "(O.volume $ O.Sphere 1) == (4/3)*pi"),
                (compareFloat (O.area $ O.Sphere 1) (4*pi), "(O.area $ O.Sphere 1) == 4*pi"),
                (compareFloat (O.volume $ O.Cube 2) 8, "(O.volume $ O.Cube 2) == 8"),
                (compareFloat (O.area $ O.Cube 2) 24, "(O.area $ O.Cube 2) == 24"),
                (compareFloat (O.volume $ O.Cuboid 1 2 3) 6, "(O.volume $ O.Cuboid 1 2 3) == 6"),
                (compareFloat (O.area $ O.Cuboid 1 2 3) 22, "(O.area $ O.Cuboid 1 2 3) == 22"),
                -- more testing cases
                (compareFloat (S.area $ S.Circle (S.Point (-3.4) (-1.5)) 2.7) (7.29*pi), "(S.area $ S.Circle (S.Point (-3.4) (-1.5)) 2.7) == 7.29*pi"),
                (compareFloat (S.perimeter $ S.Circle (S.Point (-3.4) (-1.5)) 2.7) (5.4*pi), "(S.perimeter $ S.Circle (S.Point (-3.4) (-1.5)) 2.7) == 5.4*pi"),
                (compareFloat (S.area $ S.Rectangle (S.Point (-3.4) (-1.5)) (S.Point 1.7 2.3)) 19.38, "(S.area $ S.Rectangle (S.Point (-3.4) (-1.5)) (S.Point 1.7 2.3)) == 19.38"),
                (compareFloat (S.perimeter $ S.Rectangle (S.Point (-3.4) (-1.5)) (S.Point 1.7 2.3)) 17.8, "(S.perimeter $ S.Rectangle (S.Point (-3.4) (-1.5)) (S.Point 1.7 2.3)) == 17.8"),
                (compareFloat (S.area $ S.RightTriangle (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5))) 30, "(S.area $ S.RightTriangle (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5))) == 30"),
                (compareFloat (S.perimeter $ S.RightTriangle (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5))) 30, "(S.perimeter $ S.RightTriangle (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5))) == 30"),
                (compareFloat (O.volume $ O.Sphere 3) (36*pi), "(O.volume $ O.Sphere 3) == 36*pi"),
                (compareFloat (O.area $ O.Sphere 3) (36*pi), "(O.area $ O.Sphere 3) == 36*pi"),
                (compareFloat (O.volume $ O.Cube 3) 27, "(O.volume $ O.Cube 3) == 27"),
                (compareFloat (O.area $ O.Cube 3) 54, "(O.area $ O.Cube 3) == 54"),
                (compareFloat (O.volume $ O.Cuboid 3 2 3) 18, "(O.volume $ O.Cuboid 3 2 3) == 18"),
                (compareFloat (O.area $ O.Cuboid 3 2 3) 42, "(O.area $ O.Cuboid 3 2 3) == 42")
            ]

extraCreditTestCases = [
                (compareFloat (S.area $ S.RightTriangle (S.Point 3 0) (S.Point 0 0) (S.Point 0 4)) 6, "(S.area $ S.RightTriangle (S.Point 3 0) (S.Point 0 0) (S.Point 0 4)) == 6"),
                (compareFloat (S.perimeter $ S.RightTriangle (S.Point 3 0) (S.Point 0 0) (S.Point 0 4)) 12, "(S.perimeter $ S.RightTriangle (S.Point 3 0) (S.Point 0 0) (S.Point 0 4)) == 12"),
                (compareFloat (S.area $ S.RightTriangle (S.Point 3 0) (S.Point 0 4) (S.Point 0 0)) 6, "(S.area $ S.RightTriangle (S.Point 3 0) (S.Point 0 4) (S.Point 0 0)) == 6"),
                (compareFloat (S.perimeter $ S.RightTriangle (S.Point 3 0) (S.Point 0 4) (S.Point 0 0)) 12, "(S.perimeter $ S.RightTriangle (S.Point 3 0) (S.Point 0 4) (S.Point 0 0)) == 12"),
                -- more testing cases
                (compareFloat (S.area $ S.RightTriangle (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5)) (S.Point (-3.4) (-1.5))) 30, "(S.area $ S.RightTriangle (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5)) (S.Point (-3.4) (-1.5))) == 30"),
                (compareFloat (S.perimeter $ S.RightTriangle (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5)) (S.Point (-3.4) (-1.5))) 30, "(S.perimeter $ S.RightTriangle (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5)) (S.Point (-3.4) (-1.5))) == 30"),
                (compareFloat (S.area $ S.RightTriangle (S.Point 1.6 (-1.5)) (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5)) 30, "(S.area $ S.RightTriangle (S.Point 1.6 (-1.5)) (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5)) == 30"),
                (compareFloat (S.perimeter $ S.RightTriangle (S.Point 1.6 (-1.5)) (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5)) 30, "(S.perimeter $ S.RightTriangle (S.Point 1.6 (-1.5)) (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5)) == 30")
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

printExtraCreditResult xs
    | null xs   = do
                    putStrLn "\n*****************************"
                    putStrLn "You DO get the extra credit."
                    putStrLn "*****************************\n"
    | otherwise = do
                    putStrLn "\n*********************************"
                    putStrLn "You do NOT get the extra credit."
                    putStrLn "*********************************\n"

main = do
        printTestResult (filterFalseTestCases testCases)
        printExtraCreditResult (filterFalseTestCases extraCreditTestCases)