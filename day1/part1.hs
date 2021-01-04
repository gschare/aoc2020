sumTo2020 :: Int -> Int -> Bool
sumTo2020 a b = (a + b) == 2020

solve :: [Int] -> Int
solve xs = head [x * y | x <- xs, y <- xs, x /= y, sumTo2020 x y]

main :: IO ()
main = interact $ (++"\n") . show . solve . map read . lines
