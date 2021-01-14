import Data.Ord (comparing)
import Data.List (sortBy)
import Data.List.Split (splitOn)

type Time = Integer
type Bus = Integer

odds :: [a] -> [a]
odds = map snd . filter (odd . fst) . zip [0..]

evens :: [a] -> [a]
evens = map snd . filter (even . fst) . zip [0..]

-- More of a sanity check than anything else. Inputs should be coprime anyway.
checkCoprime :: [Integer] -> Bool
checkCoprime xs = [] == filter (/=1) [gcd x y | x <- xs, y <- xs, x /= y]

-- Compute Bezout coefficients.
bezout :: Integer -> Integer -> (Integer, Integer)
bezout a 0 = (1,0)
bezout a b = (t, s - q * t)
    where (q, r) = divMod a b
          (s, t) = bezout b r

-- Using the direct construction by summing.
partialDirect :: (Integer,Integer) -> [(Integer,Integer)] -> Integer
partialDirect (a,n) xs = a * bigN * bigM
    where bigN = (product $ map snd xs) `div` n
          (bigM,m) = bezout bigN n

crtDirect :: [(Integer,Integer)] -> Integer
crtDirect xs = (sum $ map (flip partialDirect xs) xs) `mod` n
    where n = product $ map snd xs

-- Using the inductive construction.
partial :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
partial (a1,n1) (a2,n2) = (a1*m2*n2 + a2*m1*n1, n1*n2)
    where (m1,m2) = bezout n1 n2

crt :: [(Integer, Integer)] -> Integer
crt = (\(a,n) -> a `mod` n) . foldl1 partial

-- Inductive construction by pairs.
-- Solution more concerned with integer overflow would intelligently assign
-- pairs based on the size of their product; however, I am not intelligent.
crtPairs :: [(Integer, Integer)] -> Integer
crtPairs [(a,n)] = a `mod` n
crtPairs xs =
    if odd $ length xs
       then crtPairs $ newTerms ++ [last xs]
       else crtPairs newTerms
    where pairs = zip (evens xs) (odds xs)
          newTerms = map (uncurry partial) pairs

crtBruteForce :: [(Integer, Bus)] -> [Time]
crtBruteForce buses = filter check [0,(snd $ head buses)..n]
    where n = product (map snd buses)
          check x = all (\(i,b) -> x `mod` b == i) buses

parseBuses :: String -> [(Integer, Bus)]
parseBuses = map (\(a,b) -> ((b-a) `mod` b,b)) . map (\(a,b) -> (a,read b)) . filter ((/=) "x" . snd) . zip [0..] . splitOn ","

parseFile :: String -> IO [(Integer, Bus)]
parseFile filename = readFile filename >>= return . (\[_,b] -> parseBuses b) . lines

solveFile :: String -> IO Time
solveFile filename = parseFile filename >>= return . crtDirect

main :: IO ()
main = solveFile "input.txt" >>= print
