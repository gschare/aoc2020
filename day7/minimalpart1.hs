import Data.List.Split
import qualified Data.Map as M

type Bag = String
type Vertex = (Bag, [Bag])
type Graph = M.Map Bag [Bag]

-- Parser function
lineToVertex :: String -> Vertex
lineToVertex l = (u, vs)
    where [left, right] = splitOn " contain " l
          u = unwords $ take 2 $ words left
          vs = map (unwords . take 2 . drop 1 . words) $ filter (/= "no other bags.") $ splitOn "," right

-- Solution logic
-- Return all vertices that are reachable from one source vertex.
reachable :: Bag -> Graph -> [Bag]
reachable src graph = case M.lookup src graph of
                        Nothing -> []
                        Just vs -> vs ++ foldr (\v acc -> reachable v graph ++ acc) [] vs

-- Return the adjacency vectors for only those vertices which can reach the given destination vertex.
filterReachable :: Bag -> Graph -> Graph
filterReachable dest graph = M.filterWithKey (\k _ -> dest `elem` reachable k graph) graph

main :: IO ()
main = readFile "input.txt" >>= print . M.size . filterReachable "shiny gold" . M.fromList . map lineToVertex . lines
