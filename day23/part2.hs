cups :: [Int]
cups = let base = [6,2,4,3,9,7,1,5,8] in base -- ++ [(maximum base) + 1 .. 1000000]

testCups :: [Int]
testCups = [3, 8, 9, 1, 2, 5, 4, 6, 7]

iterations :: Int
iterations = 100

main :: IO ()
main = mapM_ print . take (iterations+1) . iterate move $ testCups

solve :: [Int] -> String
solve = concat . map show . flip cycleToAfter 1 . last . take (iterations + 1) . iterate move

move :: [Int] -> [Int]
move (x:xs) = xs'
    where lifted = take 3 xs
          rest = (drop 3 xs) ++ [x]
          lowest = minimum (x:xs)
          highest = maximum (x:xs)
          pickDest n | n `elem` lifted = pickDest (n-1)
                     | n < lowest      = pickDest highest
                     | otherwise       = n
          dest = pickDest (x-1)
          (a, b) = span (/=dest) rest
          xs' = a ++ ((head b):lifted) ++ (tail b)

cycleToAfter :: [Int] -> Int -> [Int]
cycleToAfter xs x = take (n-1) . drop 1 . dropWhile (/=x) . cycle $ xs
    where n = length xs
