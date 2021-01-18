import Data.List.Split (splitOn)

split :: (Eq a) => [a] -> [a] -> ([a], [a])
split xs sep = let ys = splitOn sep xs in (head ys, foldl1 (\acc x -> acc ++ sep ++ x) $ tail ys)

type Field = String
type Range = ((Int, Int), (Int, Int))
type Rule = (String, Range)
type Ticket = [Int]

inRange :: Int -> Range -> Bool
inRange x ((a,b),(c,d)) = (a <= x && x <= b) || (c <= x && x <= d)

checkTicket :: Ticket -> [Rule] -> [Int]
checkTicket ticket rules = [val | val <- ticket, all (not . inRange val) [range | (_,range) <- rules]]

solve :: [Rule] -> [Ticket] -> Int
solve rules tickets = sum $ concat $ map (flip checkTicket rules) tickets

parseRule :: String -> Rule
parseRule l = (field, (getRange r1, getRange r2))
    where (field, rs) = l `split` ": "
          (r1, r2) = rs `split` " or "
          getRange :: String -> (Int, Int)
          getRange s = (\(a,b) -> (read a, read b)) $ s `split` "-"

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parseContents :: String -> ([Rule], Ticket, [Ticket])
parseContents contents = (map parseRule rules, parseTicket myticket, map parseTicket tickets)
    where [a,b,c] = splitOn "\n\n" contents
          rules = lines a
          myticket = last $ lines b
          tickets = tail $ lines c

parseFile :: String -> IO ([Rule], Ticket, [Ticket])
parseFile filename = readFile filename >>= return . parseContents

solveFile :: String -> IO Int
solveFile filename = do
     (rules, _, tickets) <- parseFile filename
     return $ solve rules tickets

main :: IO ()
main = undefined
