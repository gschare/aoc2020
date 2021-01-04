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

-- The nature of how I wrote this for part 1 made adapting to part 2 super straightforward.
-- And that's without having any prior knowledge that part 2 would involve changing the slopes!
nextCell :: Coord -> Grid -> (Int, Int) -> Maybe (Coord, Cell)
nextCell (x,y) grid (xoff, yoff) = getCell (x,y) grid >>= \c -> Just ((x',y'), c)
    where x' = x + xoff
          y' = y + yoff


charToCell :: Char -> Cell
charToCell c = case c of
                 '#' -> Tree
                 '.' -> Empty

solve :: Grid -> Int
solve grid = product [ iter grid (0,0) (1,1) 0
                     , iter grid (0,0) (3,1) 0
                     , iter grid (0,0) (5,1) 0
                     , iter grid (0,0) (7,1) 0
                     , iter grid (0,0) (1,2) 0
                     ]
     where iter grid (x,y) (xoff,yoff) n = case nextCell (x,y) grid (xoff,yoff) of
              Nothing -> n
              Just ((x',y'), Tree) -> iter grid (x',y') (xoff,yoff) (n+1)
              Just ((x',y'), Empty) -> iter grid (x',y') (xoff,yoff) n

loadInput :: FilePath -> IO (Grid)
loadInput filename = do
    cont <- readFile filename
    return $ map (map charToCell) $ lines cont

main :: IO ()
main = getLine >>= loadInput >>= print . solve
