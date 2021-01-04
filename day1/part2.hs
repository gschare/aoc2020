import Data.List (find)

n :: Int
n = 3

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

sumTo2020 :: [Int] -> Bool
sumTo2020 xs = sum xs == 2020

solve :: Int -> [Int] -> Maybe Int
solve n xs = (find sumTo2020 $ combinations n xs) >>= return . product

main :: IO ()
main = readFile "input.txt" >>= print . solve n . map read . lines
