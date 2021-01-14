import Data.Ord (comparing)
import Data.List (sortBy)
import Data.List.Split (splitOn)

type Time = Int
type Bus = Int

busSchedule :: Bus -> [Time]
busSchedule bus = [t*bus | t <- [0..]]

getEarliestDeparture :: Time -> [Bus] -> (Bus, Time)
getEarliestDeparture t buses = head $ sortBy (comparing snd) earliestDepartures
    where schedules = map (\b -> (b, busSchedule b)) buses
          earliestDepartures = map (\(b, s) -> (b, head $ dropWhile (< t) s)) schedules

solve :: Time -> [Bus] -> Time
solve t buses = (departureTime - t) * busID
    where (busID, departureTime) = getEarliestDeparture t buses

parseTime :: String -> Time
parseTime = read

parseBuses :: String -> [Bus]
parseBuses = map read . filter (/="x") . splitOn ","

parseFile :: String -> IO (Time, [Bus])
parseFile filename = readFile filename >>= return . (\[a,b] -> (parseTime a, parseBuses b)) . lines

solveFile :: String -> IO Time
solveFile filename = parseFile filename >>= return . uncurry solve

main :: IO ()
main = undefined
