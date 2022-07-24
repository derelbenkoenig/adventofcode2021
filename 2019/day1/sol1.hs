solve :: [Int] -> Int
solve = sum . map (flip (-) 2 . flip div 3)

main :: IO ()
main = interact ((++ "\n") . show . solve . map read . lines)
