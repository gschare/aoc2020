import System.FilePath
import Control.Monad

data Cell = Empty | Tree deriving (Eq)
instance Show Cell where
    show Empty = "."
    show Tree = "#"

type Grid = [[Cell]]
type Coord = (Int, Int)

getCell :: Coord -> Grid -> Maybe Cell
getCell (x,y) grid =
    if y < length grid
       then let row = grid !! y in
                Just $ row !! (x `mod` (length row))
       else Nothing

-- At first I had a separate boolean function "isTree" but I felt it was unwieldy when it came to using it in solve because then not only was I repeating work by getting the cell twice, but having the two different Maybe a return types (Maybe Coord and Maybe Cell) felt like poor design because it didn't make clear which failure state took precedence, as the failure of one implied the failure of the other.
-- So instead I opted to just include both at once, which I think makes it cleaner.
nextCell :: Coord -> Grid -> Maybe (Coord, Cell)
nextCell (x,y) grid = getCell (x,y) grid >>= \c -> Just ((x',y'), c)
    where x' = x + 3
          y' = y + 1

charToCell :: Char -> Cell
charToCell c = case c of
                 '#' -> Tree
                 '.' -> Empty

solve :: Grid -> Int
solve grid = iter grid (0,0) 0
    where iter grid (x,y) n = case nextCell (x,y) grid of
                                Nothing -> n
                                Just ((x',y'), Tree) -> iter grid (x',y') (n+1)
                                Just ((x',y'), Empty) -> iter grid (x',y') n

loadInput :: FilePath -> IO (Grid)
loadInput filename = do
    cont <- readFile filename
    return $ map (map charToCell) $ lines cont

main :: IO ()
main = loadInput "input.txt" >>= print . solve
