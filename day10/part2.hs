import Data.List (sort)
import qualified Data.Map.Strict as M

type Vertex = (Int, [Int])
type Graph = M.Map Int [Int]

findConnections :: Int -> [Int] -> [Int]
findConnections n xs = filter (inRange n) xs

toVertex :: Int -> [Int] -> (Int, [Int])
toVertex x xs = (x, findConnections x xs)

toAdjList :: [Int] -> [(Int, [Int])]
toAdjList xs = map (flip toVertex xs) xs

toNats :: [(Int, [Int])] -> [Int] -> [(Int, [Int])]
toNats adjList xs = map (\(a,b) -> ((M.!) m a, map ((M.!) m) b)) adjList
    where m = M.fromList $ zip xs [0..]

inRange :: Int -> Int -> Bool
inRange a b = b - a >= 1 && b - a <= 3

prepare :: [Int] -> [Int]
prepare xs = sort (wallCharger:deviceAdapter:xs)
    where wallCharger = 0
          deviceAdapter = maximum xs + 3

solve :: [Int] -> Int
solve xs = countPaths u v graph
    where u = 0
          v = length xs - 1
          graph = M.fromList $ toNats (toAdjList xs) xs

countPaths :: Int -> Int -> Graph -> Int
countPaths src dest graph = memoize src
    where dfs f u = if u == dest
                        then 1
                        else sum [f c | c <- M.findWithDefault [] u graph]
          cache = map (dfs memoize) [0..]
          memoize u = cache !! u

parseFile :: String -> IO [Int]
parseFile filename = readFile filename >>= return . prepare . map read . lines

solveFile :: String -> IO Int
solveFile filename = parseFile filename >>= return . solve

main :: IO ()
main = solveFile "input.txt" >>= print
