module Main where

import qualified Shapes as S

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

main = printTestResult (filterFalseTestCases extraCreditTestCases)