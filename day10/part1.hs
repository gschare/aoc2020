import Data.List (sort)

makeEdges :: [Int] -> [(Int, Int)]
makeEdges xs = zip xs (tail xs)

-- count how many links in the given list of ints are separated by exactly the given integer
countDiffs :: Int -> [Int] -> Int
countDiffs n = length . filter (\(a,b) -> a + n == b) . makeEdges

parseFile :: String -> IO [Int]
parseFile filename = readFile filename >>= return . map read . lines

solve :: [Int] -> Int
solve xs = (countDiffs 1 seq) * (countDiffs 3 seq)
    where wallCharger = 0
          deviceAdapter = maximum xs + 3
          seq = sort (wallCharger:deviceAdapter:xs)

solveFile :: String -> IO Int
solveFile filename = parseFile filename >>= return . solve

main :: IO ()
main = solveFile "input.txt" >>= print
