import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

type Age = Int

times :: Int
times = 2020

play :: [(Int, Age)] -> Int -> Int -> Int
play nums prev 0 = prev
play nums prev n = uncurry play (step nums prev) (n-1)

step :: [(Int, Age)] -> Int -> ([(Int, Age)], Int)
step nums prev = (nums'', prev')
    where prev' = fromMaybe 0 $ lookup prev nums
          nums' = map (\(x,age) -> if x == prev then (x,1) else (x,age + 1)) nums
          nums'' = if prev' == 0 then (prev,1):nums' else nums'

solve :: [(Int, Age)] -> Int -> Int
solve [] _ = error "cannot solve with no starting numbers"
solve nums n = play nums (fst $ last nums) (n - length nums)

parseList :: [Int] -> [(Int, Age)]
parseList [] = error "cannot read empty starting list"
parseList xs = zip xs [length xs-1, length xs-2..0]

parseFile :: String -> IO [(Int, Age)]
parseFile filename = readFile filename >>= return . map read . splitOn "," >>= return . parseList

solveFile :: String -> IO Int
solveFile filename = parseFile filename >>= return . flip solve times

main :: IO ()
main = solveFile "input.txt" >>= print
