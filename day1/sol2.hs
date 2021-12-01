windows xs = zip3 xs (drop 1 xs) (drop 2 xs)
windowSums = map (\(a,b,c) -> a + b + c) . windows
margins xs = zipWith (>) (drop 1 xs) xs
countIncreasing = sum . map fromEnum . margins

process = countIncreasing . windowSums . map read . words

main = getContents >>= (return . process) >>= print
