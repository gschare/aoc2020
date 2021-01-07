import Data.List (nub)
import Data.List.Split (splitOn)

arrangeInGroups :: String -> [String]
arrangeInGroups = map (filter (/='\n')) . splitOn "\n\n"

countAnswers :: String -> Int
countAnswers = length . nub

main :: IO ()
main = readFile "input.txt" >>= print . sum . map countAnswers . arrangeInGroups
