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

-- Split file into prev ints (reversed) and next ints
parseFile :: String -> IO ([Int], [Int])
parseFile filename = readFile filename >>= return . (\(a,b) -> (reverse a, b)) . splitAt nPrevs . map read . lines

solveFile :: String -> IO (Maybe Int)
solveFile filename = parseFile filename >>= return . (uncurry findFirstInvalid)

main :: IO ()
main = solveFile "input.txt" >>= print
