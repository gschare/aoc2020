import Data.Ord (comparing)
import Data.List (sortBy)
import Data.List.Split (splitOn)

type Time = Int
type Bus = Int

checkCoprime :: [Int] -> Bool
checkCoprime xs = [] == filter (/=1) [gcd x y | x <- xs, y <- xs, x /= y]

solve :: [(Int, Bus)] -> [Time]
solve buses =
    if not $ checkCoprime (map snd buses)
       then error "input not coprime"
       else filter check [0..n]
           where n = product (map snd buses)
                 check x = all (\(i,b) -> (x + i) `mod` b == 0) buses

parseBuses :: String -> [(Int, Bus)]
parseBuses = map (\(a,b) -> (a,read b)) . filter ((/=) "x" . snd) . zip [0..] . splitOn ","

parseFile :: String -> IO [(Int, Bus)]
parseFile filename = readFile filename >>= return . (\[_,b] -> parseBuses b) . lines

solveFile :: String -> IO [Time]
solveFile filename = parseFile filename >>= return . solve

main :: IO ()
main = undefined
