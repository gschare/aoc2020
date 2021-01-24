import Control.Arrow ((***))
import qualified Data.Set as Set

type Player = [Int]

main :: IO ()
main = print . solve =<< parseFile "input.txt"

playRound :: Set.Set (Player, Player) -> Player -> Player -> (Set.Set (Player, Player), Player, Player)
playRound prevRounds allP@(p:ps) allQ@(q:qs)
    | Set.member (allP, allQ) prevRounds = (prevRounds, allP, [])  -- instant win for p1
    | p > length ps || q > length qs     = decide
    | otherwise                          = subGame
    where prevRounds' = Set.insert (allP, allQ) prevRounds
          decide = case p `compare` q of
                     GT -> (prevRounds', ps ++ [p, q], qs)  -- p1 wins
                     LT -> (prevRounds', ps, qs ++ [q, p])  -- p2 wins
                     EQ -> error "unreachable"
          subGame = case play (take p ps) (take q qs) of
                          (_, []) -> (prevRounds', ps ++ [p, q], qs)  -- p1 wins
                          ([], _) -> (prevRounds', ps, qs ++ [q, p])  -- p2 wins

play :: Player -> Player -> (Player, Player)
play p q = (\(_,a,b) -> (a,b)) . head . snd . span (\(_,a,b) -> not (null a || null b)) . iterate (\(prev, p, q) -> playRound prev p q) $ (Set.empty, p, q)

solve :: (Player, Player) -> Int
solve = sum . zipWith (*) [1..] . reverse . uncurry (++) . uncurry play

parseFile :: String -> IO (Player, Player)
parseFile filename = readFile filename >>= return . (f *** f . tail) . span (/="") . lines
    where f p = map read . tail $ p
