import Data.List (nub)
import Data.List.Split (splitOn)

arrangeInGroups :: String -> [[String]]
arrangeInGroups = map lines . splitOn "\n\n"

count :: (Eq a) => a -> [[a]] -> Int
count x = length . filter (elem x)

countAnswers :: [String] -> Int
countAnswers group = length $ filter (\x -> count x group == length group) ['a'..'z']

solve :: String -> IO ()
solve filename = readFile filename >>= print . sum . map countAnswers . arrangeInGroups

main :: IO ()
main = solve "input.txt"
