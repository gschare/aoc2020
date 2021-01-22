import Control.Arrow ((***))
import Data.List.Split (splitOn)
import Data.List (span, intersect)
import Data.Maybe (fromJust)

data Rule = Rule Char | SubRules [[Int]] deriving (Show)
type Id = Int
type Rules = [(Id, Rule)]

main :: IO ()
main = do
    (r,s) <- parseFile "input.txt"
    print $ solve r s

solve :: Rules -> [String] -> Int
solve rules msgs = length $ intersect (computeRule 0 rules) msgs

computeRule :: Id -> Rules -> [String]
computeRule ruleId rules =
    case lookup ruleId rules of
      Nothing -> error ("rule " ++ show ruleId ++ " not found")
      Just (Rule c) -> [[c]]
      Just (SubRules xss) -> concat $ map (map concat . sequence . map (flip computeRule rules)) xss

parseRule :: String -> (Id, Rule)
parseRule s = (read i, rule)
    where [i, r] = splitOn ": " s
          rule = case r of
                   ('"':c:'"':[]) -> Rule c
                   otherwise -> SubRules $ map (map read . words) $ splitOn " | " r

parseFile :: String -> IO (Rules, [String])
parseFile filename = readFile filename >>= return . ((map parseRule) *** tail) . span (/="") . lines

solveFile filename = parseFile filename >>= return . uncurry solve
