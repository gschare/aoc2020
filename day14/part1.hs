import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Bits

type Memory = M.Map Int Int
data Instruction f =
    Mem (Memory -> (Int -> Int) -> Memory)
    | Mask (Int -> Int)

size :: Int
size = 36

-- For my convenience when testing. Converts decimal number to string of binary.
bin :: Int -> String
bin x = (take (size - length result) (repeat '0')) ++ result
    where binHelper 0 = []
          binHelper y = let (q,r) = quotRem y 2 in [r] ++ binHelper q
          result = concat $ map show $ reverse $ binHelper x

applyMask :: [(Int, Int)] -> Int -> Int
applyMask [] x = x
applyMask ((i,b):rest) x =
    case b of
      1 -> applyMask rest $ x `setBit` i
      0 -> applyMask rest $ x `clearBit` i

memSet :: Int -> Int -> Memory -> (Int -> Int) -> Memory
memSet k v mem f = M.insert k (f v) mem

doInstructions :: [Instruction f] -> Memory
doInstructions = doInstructionsHelper M.empty (applyMask [])
    where doInstructionsHelper :: Memory -> (Int -> Int) -> [Instruction f] -> Memory
          doInstructionsHelper mem _ [] = mem
          doInstructionsHelper mem f (instr:rest) =
              case instr of
                Mem g -> doInstructionsHelper (g mem f) f rest
                Mask g -> doInstructionsHelper mem g rest

solve :: [Instruction f] -> Int
solve = foldl (+) 0 . doInstructions

parseMask :: String -> [(Int, Int)]
parseMask xs = map (\(a,b) -> (a, read [b])) $ filter ((/='X') . snd) $ zip [length xs - 1, length xs - 2..0] xs

parseOp :: String -> (String -> Instruction f)
parseOp op = case op of
               "mask" -> \m -> Mask $ applyMask (parseMask m)
               ('m':'e':'m':'[':k) -> \x -> Mem (memSet (read $ takeWhile isDigit k) (read x))

parseLine :: String -> Instruction f
parseLine l = parseOp op val
    where [op,val] = splitOn " = " l

parseFile :: String -> IO [Instruction f]
parseFile filename = readFile filename >>= return . map parseLine . lines

solveFile :: String -> IO Int
solveFile filename = parseFile filename >>= return . solve

main :: IO ()
main = solveFile "input.txt" >>= print
