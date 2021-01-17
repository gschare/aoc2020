import Data.List.Split (splitOn)
import Data.Vector.Unboxed (freeze, fromList)
import qualified Data.Vector.Unboxed.Mutable as V

times :: Int
times = 30000000

next :: [Int] -> Int
next seq = abs (n - m)
    where x = last seq
          n = length seq - 1
          occurrences = filter ((==x) . snd) $ zip [0..] (init seq)
          m = if occurrences == [] then n else fst $ last $ occurrences

play :: [Int] -> Int -> [Int]
play seq n = take n gen
    where gen :: [Int]
          gen = seq ++ [next $ take i gen | i <- [length seq..]]

solve :: [Int] -> Int -> Int
solve nums n = last $ play nums n

parseFile :: String -> IO [Int]
parseFile filename = readFile filename >>= return . map read . splitOn ","

solveFile :: String -> IO Int
solveFile filename = parseFile filename >>= return . flip solve times

main :: IO ()
main = do
    xs <- parseFile "test.txt"
    let v = V.grow (fromList xs) times
    print v
    print "Hello World"
