import Data.List.Split
import qualified Data.Map as M

type Bag = String
type Vertex = (Bag, [Bag])
type Graph = M.Map Bag [Bag]

target :: Bag
target = "shiny gold"

isLeaf :: String -> Bool
isLeaf = (==) "no other bags."

-- Parser function
lineToVertex :: String -> Vertex
lineToVertex l = (u, vs)
    where [left, right] = splitOn " contain " l
          u = unwords $ take 2 $ words left
          vs = map (unwords . take 2 . drop 1 . words) $ filter (not . isLeaf) $ splitOn "," right

-- Some transformations from text/strings/lines to adjacency list graph
verticesToGraph :: [Vertex] -> Graph
verticesToGraph = M.fromList

linesToGraph :: [String] -> Graph
linesToGraph = verticesToGraph . map lineToVertex

textToGraph :: String -> Graph
textToGraph = linesToGraph . lines

-- Solution logic

-- Return all vertices that are reachable from one source vertex.
reachable :: Bag -> Graph -> [Bag]
reachable src graph = case M.lookup src graph of
                        Nothing -> []
                        Just vs -> vs ++ foldr (\v acc -> reachable v graph ++ acc) [] vs

-- Return whether or not a particular destination vertex is reachable from a particular source vertex.
isReachable :: Bag -> Bag -> Graph -> Bool
isReachable src dest graph = dest `elem` reachable src graph

-- Return the adjacency vectors for only those vertices which can reach the given destination vertex.
filterReachable :: Bag -> Graph -> Graph
filterReachable dest graph = M.filterWithKey (\k _ -> isReachable k dest graph) graph

-- IO operation for reading a Graph from file
open :: String -> IO Graph
open filename = readFile filename >>= return . textToGraph

-- IO operation to solve the problem
solve :: String -> IO Int
solve filename = open filename >>= return . M.size . filterReachable target

main :: IO ()
main = solve "input.txt" >>= print
