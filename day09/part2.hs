nPrevs :: Int
nPrevs = 25

isValid :: Int -> [Int] -> Bool
isValid n xs = n `elem` [x + y | x <- xs, y <- xs, x /= y]

findFirstInvalid :: [Int] -> [Int] -> Maybe Int
findFirstInvalid [] _ = error "missing previous"
findFirstInvalid _ [] = Nothing
findFirstInvalid prevs (x:rest) =
    if isValid x prevs
       then findFirstInvalid (x:take (nPrevs-1) prevs) rest
       else Just x

-- Find the first set of contiguous numbers which sum to the given number
sumHelper :: [Int] -> Int -> Int -> [Int] -> Maybe [Int]
sumHelper (x:xs) n acc addends
    | x + acc == n = Just (x:addends)
    | x + acc > n = Nothing
    | x + acc < n = sumHelper xs n (x + acc) (x:addends)

findContiguousSum :: [Int] -> Int -> Maybe [Int]
findContiguousSum [] _ = Nothing
findContiguousSum xs n =
    case sumHelper xs n 0 [] of
      Just ys -> Just ys
      Nothing -> findContiguousSum (tail xs) n

sumMinMax :: [Int] -> Int
sumMinMax xs = minimum xs + maximum xs

solve :: [Int] -> Maybe Int
solve xs = (uncurry findFirstInvalid $ preambled xs) >>= findContiguousSum xs >>= return . sumMinMax

-- Read ints from file
parseFile :: String -> IO [Int]
parseFile filename = readFile filename >>= return . map read . lines

-- Turn list of ints into tuple with prev and next ints
preambled :: [Int] -> ([Int], [Int])
preambled = (\(a,b) -> (reverse a,b)) . splitAt nPrevs

solveFile :: String -> IO (Maybe Int)
solveFile filename = parseFile filename >>= return . solve

main :: IO ()
main = solveFile "input.txt" >>= print
