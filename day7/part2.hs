import Data.List.Split
import qualified Data.Map as M

type Bag = String
type Vertex = (Bag, [(Int, Bag)])
type Graph = M.Map Bag [(Int, Bag)]

target :: Bag
target = "shiny gold"

isLeaf :: String -> Bool
isLeaf = (==) "no other bags."

-- Parser functions
lineToVertex :: String -> Vertex
lineToVertex l = (u, vs)
    where [left, right] = splitOn " contain " l
          readBag (x:xs)  = (read x :: Int, unwords xs)
          u = unwords $ take 2 $ words left
          vs = map (readBag . take 3 . words) $ filter (not . isLeaf) $ splitOn "," right

-- Some transformations from text/strings/lines to adjacency list graph
verticesToGraph :: [Vertex] -> Graph
verticesToGraph = M.fromList

linesToGraph :: [String] -> Graph
linesToGraph = verticesToGraph . map lineToVertex

textToGraph :: String -> Graph
textToGraph = linesToGraph . lines

-- Solution logic

-- Return all vertices that are reachable from one source vertex.
reachable :: (Int, Bag) -> Graph -> [(Int, Bag)]
reachable (n,src) graph = case M.lookup src graph of
                        Nothing -> []
                        Just vs -> map (\(m,v) -> (m*n, v)) vs ++ foldr (\(m,v) acc -> reachable (m*n,v) graph ++ acc) [] vs

-- Count the total number of bags nested inside a given source bag
countNestedBags :: Bag -> Graph -> Int
countNestedBags src graph = foldr (\(n,_) acc -> n + acc) 0 $ reachable (1,src) graph

-- IO operation for reading a Graph from file
open :: String -> IO Graph
open filename = readFile filename >>= return . textToGraph

-- IO operation to solve the problem
solve :: String -> IO Int
solve filename = open filename >>= return . countNestedBags target

main :: IO ()
main = solve "input.txt" >>= print
