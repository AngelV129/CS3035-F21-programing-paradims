module Main where

import qualified Shapes as S

testCases = [
                (compareFloat (S.area $ S.Circle (S.Point 0 0) 1) pi, "(S.area $ S.Circle (S.Point 0 0) 1) == pi"),
                (compareFloat (S.perimeter $ S.Circle (S.Point 0 0) 1) (2*pi), "(S.perimeter $ S.Circle (S.Point 0 0) 1) == 2*pi"),
                (compareFloat (S.area $ S.Rectangle (S.Point 0 0) (S.Point 2 1)) 2, "(S.area $ S.Rectangle (S.Point 0 0) (S.Point 2 1)) == 2"),
                (compareFloat (S.perimeter $ S.Rectangle (S.Point 0 0) (S.Point 2 1)) 6, "(S.perimeter $ S.Rectangle (S.Point 0 0) (S.Point 2 1)) == 6"),
                (compareFloat (S.area $ S.RightTriangle (S.Point 0 0) (S.Point 3 0) (S.Point 0 4)) 6, "(S.area $ S.RightTriangle (S.Point 0 0) (S.Point 3 0) (S.Point 0 4)) == 6"),
                (compareFloat (S.perimeter $ S.RightTriangle (S.Point 0 0) (S.Point 3 0) (S.Point 0 4)) 12, "(S.perimeter $ S.RightTriangle (S.Point 0 0) (S.Point 3 0) (S.Point 0 4)) == 12"),
                -- more testing cases
                (compareFloat (S.area $ S.Circle (S.Point (-3.4) (-1.5)) 2.7) (7.29*pi), "(S.area $ S.Circle (S.Point (-3.4) (-1.5)) 2.7) == 7.29*pi"),
                (compareFloat (S.perimeter $ S.Circle (S.Point (-3.4) (-1.5)) 2.7) (5.4*pi), "(S.perimeter $ S.Circle (S.Point (-3.4) (-1.5)) 2.7) == 5.4*pi"),
                (compareFloat (S.area $ S.Rectangle (S.Point (-3.4) (-1.5)) (S.Point 1.7 2.3)) 19.38, "(S.area $ S.Rectangle (S.Point (-3.4) (-1.5)) (S.Point 1.7 2.3)) == 19.38"),
                (compareFloat (S.perimeter $ S.Rectangle (S.Point (-3.4) (-1.5)) (S.Point 1.7 2.3)) 17.8, "(S.perimeter $ S.Rectangle (S.Point (-3.4) (-1.5)) (S.Point 1.7 2.3)) == 17.8"),
                (compareFloat (S.area $ S.RightTriangle (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5))) 30, "(S.area $ S.RightTriangle (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5))) == 30"),
                (compareFloat (S.perimeter $ S.RightTriangle (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5))) 30, "(S.perimeter $ S.RightTriangle (S.Point (-3.4) (-1.5)) (S.Point (-3.4) 10.5) (S.Point 1.6 (-1.5))) == 30")
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