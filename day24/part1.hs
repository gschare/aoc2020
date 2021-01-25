import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.List (nub)

data Dir = E | W | SE | SW | NE | NW deriving (Show, Eq, Ord)
data Color = White | Black deriving (Show, Eq)
type Coord = (Int, Int, Int)

main :: IO ()
main = do
    print =<< solve <$> parseFile "input.txt"

parseFile :: String -> IO [Coord]
parseFile filename = map (parse tileParser) . lines <$> readFile filename

solve :: [Coord] -> Int
solve = length . filter (==Black) . M.elems . process

dirsToCoord :: [Dir] -> Coord
dirsToCoord = foldl (\(x,y,z) (dx,dy,dz) -> (x+dx, y+dy, z+dz)) (0,0,0) . map dirToOffset
    where dirToOffset E  = ( 1,-1, 0)
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
