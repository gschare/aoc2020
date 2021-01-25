import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Maybe (fromMaybe)

data Dir = E | W | SE | SW | NE | NW deriving (Show, Eq, Ord)
data Color = White | Black deriving (Show, Eq)
type Coord = (Int, Int, Int)

main :: IO ()
main = print =<< solve <$> parseFile "input.txt"

step :: M.Map Coord Color -> M.Map Coord Color
step m = let m' = m `M.union` (getAllNeighbors m) in M.mapWithKey (updateTile m') m'
    where offsets = map dirToOffset [E, W, SE, SW, NE, NW]
          getNeighborCoords (x,y,z) = map (\(a,b,c) -> (a+x, b+y, c+z)) offsets
          countBlackNeighbors m (x,y,z) = length . filter ((==Black) . fromMaybe White . (m M.!?)) . getNeighborCoords $ (x,y,z)
          updateTile m (x,y,z) White = if 2 == countBlackNeighbors m (x,y,z)
                                          then Black
                                          else White
          updateTile m (x,y,z) Black = let n = countBlackNeighbors m (x,y,z) in
                                           if n == 0 || n > 2
                                              then White
                                              else Black
          getAllNeighbors m = foldr (\x acc -> M.insert x White acc) M.empty . concatMap getNeighborCoords . M.keys . M.filter (==Black) $ m

parseFile :: String -> IO [Coord]
parseFile filename = map (parse tileParser) . lines <$> readFile filename

solve :: [Coord] -> Int
solve = length . filter (==Black) . M.elems . last . take 101 . iterate step . process

dirsToCoord :: [Dir] -> Coord
dirsToCoord = foldl (\(x,y,z) (dx,dy,dz) -> (x+dx, y+dy, z+dz)) (0,0,0) . map dirToOffset

dirToOffset :: Dir -> Coord
dirToOffset E  = ( 1,-1, 0)
dirToOffset W  = (-1, 1, 0)
dirToOffset SE = ( 0,-1, 1)
dirToOffset SW = (-1, 0, 1)
dirToOffset NE = ( 1, 0,-1)
dirToOffset NW = ( 0, 1,-1)

flipTile :: Color -> Color
flipTile Black = White
flipTile White = Black

process :: [Coord] -> M.Map Coord Color
process = foldl f M.empty
    where f acc tile = M.insertWith (\_ c -> flipTile c) tile Black acc

parse :: P.ReadP a -> String -> a
parse parser s = fst . head $ P.readP_to_S parser s

readDir :: String -> Dir
readDir "e" = E
readDir "w" = W
readDir "se" = SE
readDir "sw" = SW
readDir "ne" = NE
readDir "nw" = NW

dirParser :: P.ReadP Dir
dirParser = readDir <$> (P.string "e" <|>
    P.string "w" <|>
    P.string "se" <|>
    P.string "sw" <|>
    P.string "ne" <|>
    P.string "nw")

tileParser :: P.ReadP Coord
tileParser = dirsToCoord <$> P.many dirParser <* P.eof
