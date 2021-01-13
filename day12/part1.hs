data Ship = Ship
    { x :: Int
    , y :: Int
    , h :: Int
    } deriving (Show, Eq)

data Action = Action
    { magnitude :: Int
    , f :: Int -> Int -> (Int, Int, Int)
    }

radians :: Int -> Double
radians deg = (fromIntegral deg) * (pi / 180)

act :: Action -> Ship -> Ship
act (Action n f) (Ship x y h) = Ship x' y' h'
    where (dx,dy,dh) = f n h
          x' = x + dx
          y' = y + dy
          h' = h + dh

actMany :: [Action] -> Ship -> Ship
actMany [] ship = ship
actMany (action:rest) ship = actMany rest ship'
    where ship' = act action ship

actLog :: [Action] -> [Ship] -> [Ship]
actLog [] prevs = reverse prevs
actLog actions [] = actLog actions [Ship 0 0 0]
actLog (action:rest) (p:prevs) = actLog rest (p':p:prevs)
    where p' = act action p

solve :: [Action] -> Int
solve actions = abs x + abs y
    where Ship x y _ = actMany actions (Ship 0 0 0)

parseLine :: String -> Action
parseLine [] = error "cannot parse empty line"
parseLine ('N':x) = Action (read x) (\n _ -> (0,n,0))
parseLine ('S':x) = Action (read x) (\n _ -> (0,-n,0))
parseLine ('E':x) = Action (read x) (\n _ -> (n,0,0))
parseLine ('W':x) = Action (read x) (\n _ -> (-n,0,0))
parseLine ('L':x) = Action (read x) (\n _ -> (0,0,n))
parseLine ('R':x) = Action (read x) (\n _ -> (0,0,-n))
parseLine ('F':x) = Action (read x) (\n h -> (n * (round $ cos (radians h)), n * (round $ sin (radians h)), 0))
parseLine _ = error "invalid action"

parseFile :: String -> IO [Action]
parseFile filename = readFile filename >>= return . map parseLine . lines

solveFile :: String -> IO Int
solveFile filename = parseFile filename >>= return . solve

logFile :: String -> IO ()
logFile filename = parseFile filename >>= mapM_ print . flip actLog []

main :: IO ()
main = solveFile "input.txt" >>= print
