import qualified Data.Map.Strict as M

data Cell = Floor | Empty | Full deriving (Show, Eq)
type Coord = (Int, Int)
type Grid = M.Map Coord Cell

offsetMatrix :: [Coord]
offsetMatrix = [ (-1,-1), (-1, 0), (-1, 1)
               , ( 0,-1),          ( 0, 1)
               , ( 1,-1), ( 1, 0), ( 1, 1)
               ]

splitEvery :: [a] -> Int -> [[a]]
splitEvery xs n = if length xs < n
                     then [xs]
                     else take n xs : splitEvery (drop n xs) n

gridToLines :: Grid -> [String]
gridToLines g = flip splitEvery (cols + 1) $ map (cellToChar . (M.!) g) $ [(i,j) | i <- [0..rows], j <- [0..cols]]
    where rows = maximum $ map fst $ M.keys g
          cols = maximum $ map snd $ M.keys g

printGrid :: Grid -> IO ()
printGrid = mapM_ putStrLn . gridToLines

charToCell :: Char -> Cell
charToCell '.' = Floor
charToCell 'L' = Empty
charToCell '#' = Full

cellToChar :: Cell -> Char
cellToChar Floor = '.'
cellToChar Empty = 'L'
cellToChar Full  = '#'

lineToCells :: String -> [Cell]
lineToCells = map charToCell

linesToGrid :: [String] -> Grid
linesToGrid = M.fromList . concat . fmap f . zip [0..]
    where f (i,l) = fmap (\(j,c) -> ((i,j), charToCell c)) $ zip [0..] l

getCell :: Coord -> Grid -> Maybe Cell
getCell c g = M.lookup c g

countFullNeighbors :: Coord -> Grid -> Int
countFullNeighbors (i,j) g = sum fullNeighbors
    where neighborCoords = map (\(a,b) -> (i+a,j+b)) offsetMatrix
          neighbors = map (flip getCell g) neighborCoords
          f Nothing = 0
          f (Just Full) = 1
          f _ = 0
          fullNeighbors = map f neighbors

progressCell :: (Coord, Cell) -> Grid -> Cell
progressCell (coord,state) g
    | state == Empty && fullNeighbors == 0 = Full
    | state == Full  && fullNeighbors >= 4 = Empty
    | otherwise = state
  where fullNeighbors = countFullNeighbors coord g

progressGrid :: Grid -> Grid
progressGrid g = M.mapWithKey (curry (flip progressCell g)) g

simulate :: Grid -> (Int, Grid)
simulate g = simulateN (0,g)

simulateN :: (Int, Grid) -> (Int, Grid)
simulateN (n,g) =
    if g == g'
       then (n,g')
       else simulateN (n+1,g')
    where g' = progressGrid g

countFull :: Grid -> Int
countFull = sum . map f . M.elems
    where f st = case st of
                   Full -> 1
                   otherwise -> 0

parseFile :: String -> IO Grid
parseFile filename = readFile filename >>= return . linesToGrid . lines

solveFile :: String -> IO (Int, Int)
solveFile filename = parseFile filename >>= return . (\(n,g) -> (n, countFull g)) . simulate

main :: IO ()
main = solveFile "test.txt" >>= print
