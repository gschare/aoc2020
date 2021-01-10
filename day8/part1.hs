import Data.List.Split (splitOn)

data Op a = Acc Int | Jmp Int | Nop Int deriving (Show, Eq)
type AccVal = Int
type LineNo = Int
type Step = (AccVal, LineNo)
type Instructions = [Op Int]

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

execInstructions :: [Step] -> Instructions -> Step
execInstructions _ [] = start
execInstructions [] set = execInstructions [start] set
execInstructions prevSteps set =
    if isLoop nextStep prevSteps
       then head prevSteps
       else execInstructions (nextStep:prevSteps) set
    where nextStep = execStep (head prevSteps) set

parseFile :: String -> IO [Op Int]
parseFile filename = readFile filename >>= return . map parseLine . lines

main :: IO ()
main = parseFile "input.txt" >>= print . execInstructions []
