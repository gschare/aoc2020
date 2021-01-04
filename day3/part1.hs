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
