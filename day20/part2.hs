import Data.List (intercalate, intersperse, delete)
import Data.List.Split (splitOn)
import Data.Function (on)
import Data.Char (isDigit)
import Data.Bool (bool)
import Data.Ix (Ix)
import Control.Monad (guard)
import qualified Data.Array.BitArray as B
import qualified Data.Map.Strict as M

data Tile = Tile { tId :: Int, tSize :: Int, tArr :: B.BitArray (Int, Int) }
type Grid = M.Map (Int, Int) Tile

instance Show Tile where
    show (Tile _ s tile) = intercalate "\n" [intersperse ' ' [(bool ' ' '*') $ tile B.! (i,j) | j <- [0..s-1]] | i <- [0..s-1]]

instance Eq Tile where
    (==) = (==) `on` tId

instance (Ix a) => Show (B.BitArray a) where
    show b = map (bool '0' '1') $ B.elems b

main :: IO ()
main = do
    tiles <- parseFile "input.txt"
    print $ countNotMonster $ combineGrid $ makeGrid tiles

solve :: [Tile] -> Int
solve tiles = product [tId $ grid M.! (i,j) | (i,j) <- [(0,0), (gridSize-1,0), (0,gridSize-1), (gridSize-1,gridSize-1)]]
    where grid = makeGrid tiles
          gridSize = floor . sqrt . fromIntegral . length $ tiles

-- operations for transforming and manipulating squares
top :: Tile -> [Bool]
top (Tile _ size tile) = [tile B.! (0, j) | j <- [0..size - 1]]

bottom :: Tile -> [Bool]
bottom (Tile _ size tile) = [tile B.! (size - 1, j) | j <- [0..size - 1]]

left :: Tile -> [Bool]
left (Tile _ size tile) = [tile B.! (i, 0) | i <- [0..size - 1]]

right :: Tile -> [Bool]
right (Tile _ size tile) = [tile B.! (i, size - 1) | i <- [0..size - 1]]

-- check if two tiles fit together
checkLeft :: Tile -> Tile -> Bool
checkLeft l r = right l == left r

checkTop :: Tile -> Tile -> Bool
checkTop t b = bottom t == top b

-- part 2 stuff
removeBorders :: Tile -> [((Int, Int), Bool)]
removeBorders (Tile i s tile) = [((x-1, y-1), b) | ((x, y), b) <- B.assocs tile, x /= 0, y /= 0, x /= s - 1, y /= s - 1]

combineGrid :: Grid -> Tile
combineGrid grid = Tile 0 imageSize image
    where imageSize = ((maximum $ map fst $ M.keys grid) + 1) * ((tSize $ grid M.! (0,0)) - 2)
          reindex ((x,y), Tile i s arr) = map (\((x',y'), b) -> ((x * (s-2) + x', y * (s-2) + y'), b)) $ removeBorders (Tile i s arr)
          image = B.array ((0,0), (imageSize-1,imageSize-1)) $ concatMap reindex $ M.assocs grid

seaMonster :: [[Char]]
seaMonster = [ "                  # "
             , "#    ##    ##    ###"
             , " #  #  #  #  #  #   " ]

monsterHeight :: Int
monsterHeight = length seaMonster

monsterLength :: Int
monsterLength = length $ head seaMonster

monsterSearchZones :: Tile -> [[Bool]]
monsterSearchZones (Tile _ size tile) = [[tile B.! (i + si, j + sj) | i <- [0..monsterHeight-1], j <- [0..monsterLength-1]] | si <- [0..size - monsterHeight - 2], sj <- [0..size - monsterLength - 2]]

seaMonsterSignature :: [Bool]
seaMonsterSignature = map charToBool . concat $ seaMonster

checkSeaMonster :: [Bool] -> Bool
checkSeaMonster = (==seaMonsterSignature) . zipWith (&&) seaMonsterSignature

countNotMonster :: Tile -> Int
countNotMonster tile = B.popCount (tArr tileWithMonster) - ((length $ filter id seaMonsterSignature) * monsterCount)
    where (tileWithMonster, monsterCount) = head . filter ((>1) . snd) . map (\t -> (t, length . filter checkSeaMonster . monsterSearchZones $ t)) $ orientations tile

-- transformations
transform :: (Int -> (Int, Int) -> (Int, Int)) -> Tile -> Tile
transform f (Tile id size tile) = Tile id size $ B.ixmap (B.bounds tile) (f size) tile

transformations =
    [ \_ (x,y) -> (x, y)
    , \s (x,y) -> (s - 1 - x, s - 1 - y)
    , \s (x,y) -> (s - 1 - y, s - 1 - x)
    , \_ (x,y) -> (y, x)
    , \s (x,y) -> (s - 1 - x, y)
    , \s (x,y) -> (x, s - 1 - y)
    , \s (x,y) -> (s - 1 - y, x)
    , \s (x,y) -> (y, s - 1 - x)
    ]

orientations :: Tile -> [Tile]
orientations tile = map (flip transform tile) transformations

makeGrid :: [Tile] -> M.Map (Int, Int) Tile
makeGrid tiles = head $ makeGrid' 0 M.empty tiles
    where gridSize = floor . sqrt . fromIntegral . length $ tiles

          makeGrid' :: Int -> M.Map (Int, Int) Tile -> [Tile] -> [M.Map (Int, Int) Tile]
          makeGrid' _ grid [] = return grid  -- Return because the entire thing is monadically wrapped in a list.
          makeGrid' count grid tiles = do
              let (row, col) = count `divMod` gridSize
                  leftTile = grid M.! (row, col - 1)
                  topTile  = grid M.! (row - 1, col)
              -- Perform monadic bind on tiles. If we can't find a place for this tile, we move to the next one.
              tile <- tiles
              -- Perform monadic bind on transformations of the current tile until we find one that fits or we exhaust our possibilities.
              tile' <- orientations tile
              guard $ case (row, col) of
                        (0, 0) -> True  -- top left corner is always ok
                        (0, _) -> checkLeft leftTile tile'  -- don't check top if we're in the first row
                        (_, 0) -> checkTop topTile tile'  -- don't check left if we're in the first column
                        _      -> checkLeft leftTile tile' && checkTop topTile tile'
              -- If we pass the guard, then this tile fits, so recur while inserting this tile into its proper place and removing it from the stack, and move onto the next grid coordinate.
              makeGrid' (count + 1) (M.insert (row, col) tile' grid) (delete tile tiles)


-- parsing
charToBool :: Char -> Bool
charToBool '.' = False
charToBool ' ' = False
charToBool '#' = True
charToBool _   = error "invalid symbol"

readTile :: String -> Tile
readTile s = Tile tileNo tileSize tileArr
    where (rawTileNo:rawData) = lines s
          tileNo = read $ takeWhile (/=':') $ dropWhile (not . isDigit) rawTileNo
          tileSize = length rawData
          tileArr = B.listArray ((0,0), (tileSize - 1, tileSize - 1)) . map charToBool . concat $ rawData

parseFile :: String -> IO [Tile]
parseFile filename = readFile filename >>= return . map readTile . splitOn "\n\n"
