import Data.List (nub, transpose, isPrefixOf, sortBy, sortOn)
import Data.Ord (comparing)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Control.Monad (guard)

split :: (Eq a) => [a] -> [a] -> ([a], [a])
split xs sep = let ys = splitOn sep xs in (head ys, foldl1 (\acc x -> acc ++ sep ++ x) $ tail ys)

count :: (Eq a) => a -> [a] -> Int
count x xs = foldr (\y acc -> if x == y then acc + 1 else acc) 0 xs

unique :: (Eq a) => [a] -> Bool
unique xs = nub xs == xs

remove :: (Eq a) => a -> [a] -> [a]
remove x xs = filter (/=x) xs

type Field = String
type Range = ((Int, Int), (Int, Int))
type Rule = (String, Range)
type Ticket = [Int]

inRange :: Int -> Range -> Bool
inRange x ((a,b),(c,d)) = (a <= x && x <= b) || (c <= x && x <= d)

possibleFields :: Int -> [Rule] -> [Field]
possibleFields val rules = [field | (field, range) <- rules, inRange val range]

possibleFieldsOfTicket :: Ticket -> [Rule] -> [[Field]]
possibleFieldsOfTicket ticket rules = [possibleFields val rules | val <- ticket]

isValid :: Ticket -> [Rule] -> Bool
isValid ticket rules = all (/=[]) $ possibleFieldsOfTicket ticket rules

-- Given the possible fields of each ticket at the same index, determine the domain of values for that index.
getDomain :: [[Field]] -> [Field]
getDomain xss = filter (\x -> count x xs == length xss) fields
    where xs = concat xss
          fields = nub xs

-- Given rules and tickets, return a list representing the domain of each index.
getAllDomains :: [Rule] -> [Ticket] -> [[Field]]
getAllDomains rules tickets = map getDomain $ transpose $ filter (all (/=[])) $ map (flip possibleFieldsOfTicket rules) tickets

-- Exploit the requirements of our specific constraint.
-- If i is an index with only one possible field x, then remove x from the domains at index [0..n] \ i.
-- Repeat until no further deductions can be made (i.e. when the previous is the same as the current).
iterReduce :: [[Field]] -> [Field]
iterReduce domains = map snd $ sortOn fst $ iter $ sortBy (comparing (length . snd)) $ zip [0..] domains
    where iter :: [(Int, [Field])] -> [(Int, Field)]
          iter [] = []
          iter ((i,fields):xs) = (i,head fields):(iter (map (\(a,b) -> (a, remove (head fields) b)) xs))

getFields :: [Rule] -> Ticket -> [Ticket] -> [Field]
getFields rules myticket tickets = iterReduce $ getAllDomains rules (myticket:tickets)

solve :: [Rule] -> Ticket -> [Ticket] -> Int
solve rules myticket tickets = product $ map snd $ filter (isPrefixOf "departure" . fst) $ zip (getFields rules myticket tickets) myticket

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
     (rules, myticket, tickets) <- parseFile filename
     return $ solve rules myticket tickets

main :: IO ()
main = parseFile "input.txt" >>= \(r,m,t) -> print $ (getFields r m t, m)
