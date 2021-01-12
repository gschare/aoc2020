data Cell = Floor | Empty | Full deriving (Show, Eq)
type Coord = (Int, Int)
type Grid = [[(Coord, Cell)]]

offsetMatrix :: [Coord]
offsetMatrix = [ (-1,-1), (-1, 0), (-1, 1)
               , ( 0,-1),          ( 0, 1)
               , ( 1,-1), ( 1, 0), ( 1, 1)
               ]

gridMap :: ((Coord, Cell) -> (Coord, Cell)) -> Grid -> Grid
gridMap f g = map (map f) g

gridToLines :: Grid -> [String]
gridToLines g = map (map (cellToChar . snd)) g

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
linesToGrid = map f . zip [0..]
    where f (i,l) = map (\(j, c) -> ((i, j), charToCell c)) $ zip [0..] l

getCell :: Coord -> Grid -> Maybe Cell
getCell (i,j) g
    | i < 0 || j < 0 = Nothing
    | i >= length g = Nothing
    | j >= length (g !! i) = Nothing
    | otherwise = Just (snd $ g !! i !! j)

countFullNeighbors :: Coord -> Grid -> Int
countFullNeighbors (i,j) g = sum fullNeighbors
    where neighborCoords = map (\(a,b) -> (i+a,j+b)) offsetMatrix
          neighbors = map (flip getCell g) neighborCoords
          f Nothing = 0
          f (Just Full) = 1
          f _ = 0
          fullNeighbors = map f neighbors

progressCell :: (Coord, Cell) -> Grid -> (Coord, Cell)
progressCell (coord,state) g
    | state == Empty && fullNeighbors == 0 = (coord,Full)
    | state == Full  && fullNeighbors >= 4 = (coord,Empty)
    | otherwise = (coord,state)
  where fullNeighbors = countFullNeighbors coord g

progressGrid :: Grid -> Grid
progressGrid g = gridMap (flip progressCell g) g

simulate :: Grid -> Grid
simulate g =
    if g == g'
       then g'
       else simulate g'
    where g' = progressGrid g

countFull :: Grid -> Int
countFull = sum . concat . map (map f)
    where f (_,st) = case st of
                       Full -> 1
                       otherwise -> 0

parseFile :: String -> IO Grid
parseFile filename = readFile filename >>= return . linesToGrid . lines

solveFile :: String -> IO Int
solveFile filename = parseFile filename >>= return . countFull . simulate

main :: IO ()
main = solveFile "test.txt" >>= print
