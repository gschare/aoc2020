import Control.Arrow ((***))

type Player = [Int]

main :: IO ()
main = print . solve =<< parseFile "input.txt"

playRound :: Player -> Player -> Maybe (Player, Player)
playRound p1 p2 = if null p1 || null p2
                     then Nothing
                     else let p1Card = head p1
                              p2Card = head p2
                              result = p1Card `compare` p2Card
                           in case result of
                                LT -> Just (tail p1, (tail p2) ++ [p2Card, p1Card])
                                GT -> Just ((tail p1) ++ [p1Card, p2Card], tail p2)
                                EQ -> error "unreachable"

play :: Player -> Player -> Player
play p1 [] = p1
play [] p2 = p2
play p1 p2 = let r = playRound p1 p2
              in case r of
                   Nothing -> error "unreachable"
                   Just (p1', p2') -> play p1' p2'

solve :: (Player, Player) -> Int
solve = sum . zipWith (*) [1..] . reverse . uncurry play

parseFile :: String -> IO (Player, Player)
parseFile filename = readFile filename >>= return . (f *** f . tail) . span (/="") . lines
    where f p = map read . tail $ p
