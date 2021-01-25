cups :: [Int]
cups = [6,2,4,3,9,7,1,5,8]

testCups :: [Int]
testCups = [3, 8, 9, 1, 2, 5, 4, 6, 7]

iterations :: Int
iterations = 100

main :: IO ()
main = putStrLn $ solve cups

solve :: [Int] -> String
solve = concat . map show . flip cycleToAfter 1 . last . take (iterations + 1) . iterate move

move :: [Int] -> [Int]
move (x:xs) = xs'
    where cw3 = take 3 xs
          ccw = (drop 3 xs) ++ [x]
          lowest = minimum (x:xs)
          highest = maximum (x:xs)
          pickDest n | n `elem` cw3 = pickDest (n-1)
                     | n < lowest   = pickDest highest
                     | otherwise    = n
          dest = pickDest (x-1)
          (a, b) = span (/=dest) ccw
          xs' = a ++ [head b] ++ cw3 ++ (tail b)

cycleToAfter :: [Int] -> Int -> [Int]
cycleToAfter xs x = take (n-1) . drop 1 . dropWhile (/=x) . cycle $ xs
    where n = length xs
