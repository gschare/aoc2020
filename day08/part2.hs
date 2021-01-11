import Data.List.Split (splitOn)

data Op a = Acc Int | Jmp Int | Nop Int deriving (Show, Eq)
type AccVal = Int
type LineNo = Int
type Step = (AccVal, LineNo)
type Instructions = [Op Int]

swapOp :: Op a -> Op a
swapOp (Jmp x) = Nop x
swapOp (Nop x) = Jmp x
swapOp op = op

start :: Step
start = (0,0)

parseInt :: String -> Int
parseInt ('+':ds) = read ds
parseInt all@('-':ds) = read all

parseLine :: String -> Op Int
parseLine xs = case splitOn " " xs of
                 ["acc", n] -> Acc $ parseInt n
                 ["jmp", n] -> Jmp $ parseInt n
                 ["nop", n] -> Nop $ parseInt n
                 _ -> error "bad read"

isLoop :: Step -> [Step] -> Bool
isLoop step prevSteps = snd step `elem` map snd prevSteps

execOp :: Op Int -> Step
execOp (Acc n) = (n, 1)
execOp (Jmp n) = (0, n)
execOp (Nop _) = (0, 1)

execStep :: Step -> Instructions -> Step
execStep (acc,line) set = (acc + dAcc, line + dLine)
    where (dAcc, dLine) = execOp (set !! line)

execInstructions :: [Step] -> Instructions -> Maybe Step
execInstructions _ [] = Just start
execInstructions [] set = execInstructions [start] set
execInstructions prevSteps set
    | isLoop nextStep prevSteps = Nothing
    | snd nextStep == length set = Just nextStep
    | otherwise = execInstructions (nextStep:prevSteps) set
    where nextStep = execStep (head prevSteps) set

fixInstructions :: Instructions -> [Maybe Step]
fixInstructions set = filter (/=Nothing) $ map (execInstructions []) alts
    where fix i op = take i set ++ [swapOp op] ++ drop (i+1) set 
          alts = map (uncurry fix) $ zip [0..] set

parseFile :: String -> IO [Op Int]
parseFile filename = readFile filename >>= return . map parseLine . lines

main :: IO ()
main = parseFile "input.txt" >>= print . fixInstructions
