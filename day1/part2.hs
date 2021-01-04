n :: Int
n = 3

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

sumTo2020 :: [Int] -> Bool
sumTo2020 xs = sum xs == 2020

solve :: Int -> [Int] -> Int
solve n = product . head . filter sumTo2020 . combinations n

main :: IO ()
main = interact $ (++"\n") . show . solve n . map read . lines
