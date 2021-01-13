data Ship = Ship Int Int deriving (Show, Eq)

data Waypoint = Waypoint Int Int deriving (Show, Eq)

data Action = Action Int (Int -> Ship -> Waypoint -> (Ship, Waypoint))

radians :: Int -> Double
radians deg = (fromIntegral deg) * (pi / 180)

{-
fixAngle :: Int -> Int -> Double -> Double
fixAngle x y theta
    | x >= 0 && y >= 0 = theta          -- Quadrant I
    | x <  0 && y >= 0 = 180 - theta    -- Quadrant II
    | x <  0 && y <  0 = theta + 180    -- Quadrant III
    | x >= 0 && y <  0 = 360 - theta    -- Quadrant IV
-}

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x0,y0) (x1,y1) = sqrt $ fromIntegral ((y1 - y0)^2) + fromIntegral ((x1 - x0)^2)

moveWaypoint :: Waypoint -> Int -> Int -> Waypoint
moveWaypoint (Waypoint x y) dx dy = Waypoint (x + dx) (y + dy)

rotateWaypoint :: Waypoint -> Ship -> Int -> Waypoint
rotateWaypoint (Waypoint wx wy) (Ship sx sy) deg = Waypoint wx' wy'
    where theta = radians deg
          wx' = sx + ((wx-sx) * round (cos theta)) - ((wy-sy) * round (sin theta))
          wy' = sy + ((wx-sx) * round (sin theta)) + ((wy-sy) * round (cos theta))

toWaypoint :: Ship -> Waypoint -> Int -> (Ship, Waypoint)
toWaypoint (Ship sx sy) (Waypoint wx wy) n = (Ship sx' sy', Waypoint wx' wy')
    where dx = (wx - sx) * n
          dy = (wy - sy) * n
          sx' = sx + dx
          sy' = sy + dy
          wx' = wx + dx
          wy' = wy + dy

act :: Action -> Ship -> Waypoint -> (Ship, Waypoint)
act (Action n f) ship waypoint = f n ship waypoint

actMany :: [Action] -> Ship -> Waypoint -> Ship
actMany [] ship _ = ship
actMany (action:rest) ship waypoint = actMany rest ship' waypoint'
    where (ship', waypoint') = act action ship waypoint

actLog :: [Action] -> [(Ship, Waypoint)] -> [(Ship, Waypoint)]
actLog [] prevs = reverse prevs
actLog actions [] = actLog actions [(Ship 0 0, Waypoint 10 1)]
actLog (action:rest) ((s,wp):prevs) = actLog rest (p:(s,wp):prevs)
    where p = act action s wp

solve :: [Action] -> Int
solve actions = abs x + abs y
    where Ship x y = actMany actions (Ship 0 0) (Waypoint 10 1)

parseLine :: String -> Action
parseLine [] = error "cannot parse empty line"
parseLine ('N':x) = Action (read x) (\n s wp -> (s, moveWaypoint wp 0 n))
parseLine ('S':x) = Action (read x) (\n s wp -> (s, moveWaypoint wp 0 (-n)))
parseLine ('E':x) = Action (read x) (\n s wp -> (s, moveWaypoint wp n 0))
parseLine ('W':x) = Action (read x) (\n s wp -> (s, moveWaypoint wp (-n) 0))
parseLine ('L':x) = Action (read x) (\n s wp -> (s, rotateWaypoint wp s n))
parseLine ('R':x) = Action (read x) (\n s wp -> (s, rotateWaypoint wp s (-n)))
parseLine ('F':x) = Action (read x) (\n s wp -> toWaypoint s wp n)
parseLine _ = error "invalid action"

parseFile :: String -> IO [Action]
parseFile filename = readFile filename >>= return . map parseLine . lines

solveFile :: String -> IO Int
solveFile filename = parseFile filename >>= return . solve

logFile :: String -> IO ()
logFile filename = parseFile filename >>= mapM_ print . flip actLog []

main :: IO ()
main = solveFile "input.txt" >>= print
