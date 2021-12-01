exampleString = "123\n124\n125\n122\n126\n"
whichIncreasing xs = zipWith (>) (drop 1 xs) xs
countTrues = foldr (\x y -> if x then y + 1 else y) 0
process = countTrues . whichIncreasing . map (read :: String -> Int) . words
main = getContents >>= (return . process) >>= print
