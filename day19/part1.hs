--import Data.List.Split (splitOn)
--import Data.List (span, intersect)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha)
import Control.Applicative
import Control.Arrow

data Rule = Rule `Seq` Rule | Rule `Alt` Rule | Val Char | Id Int deriving (Show)

main :: IO ()
main = do
    (rules, msgs) <- parseFile "input.txt"
    print $ length $ filter (match (Id 0) rules) msgs

int :: ReadP Int
int = read <$> many1 (satisfy isDigit)

ruleId :: ReadP Rule
ruleId = Id <$> int

ruleVal :: ReadP Rule
ruleVal = Val <$> (quote *> satisfy isAlpha <* quote)
    where quote = char '"'

ruleSeq :: ReadP Rule
ruleSeq = foldl1 Seq <$> ruleId `sepBy` (char ' ')

ruleAlt :: ReadP Rule
ruleAlt = Alt <$> ruleSeq <*> (string " | " *> ruleSeq)

rule :: ReadP (Int, Rule)
rule = (,) <$> (int <* string ": ") <*> ((ruleAlt <|> ruleSeq <|> ruleVal <|> ruleId) <* eof)

parse parser = fst . head . readP_to_S parser

message :: Rule -> [(Int, Rule)] -> ReadP String
message (Val c) _ = string [c]
message (Id n) rules = message (fromJust $ lookup n rules) rules
message (r1 `Seq` r2) rules = (++) <$> message r1 rules <*> message r2 rules
message (s1 `Alt` s2) rules = message s1 rules <|> message s2 rules

match :: Rule -> [(Int, Rule)] -> String -> Bool
match r rules msg = case readP_to_S (message r rules) msg of
                      [] -> False
                      [(_,"")] -> True
                      _ -> False

parseFile :: String -> IO ([(Int, Rule)], [String])
parseFile filename = readFile filename >>= return . ((map (parse rule)) *** tail) . span (/="") . lines

solve :: [(Int, Rule)] -> [String] -> Int
solve rules msgs = length $ filter (match (Id 0) rules) msgs

solveFile filename = parseFile filename >>= return . uncurry solve
