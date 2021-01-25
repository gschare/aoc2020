import Data.Semigroup

newtype ModExp = ModExp Integer deriving (Show, Eq)

instance Semigroup ModExp where
    (ModExp a) <> (ModExp b) = ModExp $ (a * b) `mod` p

main :: IO ()
main = do
    [pubKeyCard, pubKeyDoor] <- parseFile "input.txt"
    print $ solve pubKeyCard pubKeyDoor


parseFile :: String -> IO [ModExp]
parseFile filename = map ModExp . map read . lines <$> readFile filename

solve :: ModExp -> ModExp -> ModExp
solve pub1 pub2 = (iterate (\x -> x <> pub2) . ModExp $ 1) !! priv1
    where priv1 = reversePublicKey pub1 g

-- a prime number
p :: Integer
p = 20201227

-- a primitive root modulo the modular base
g :: ModExp
g = ModExp 7

reversePublicKey :: ModExp -> ModExp -> Int
reversePublicKey pubKey b = length . takeWhile (/=pubKey) . iterate (\x -> x <> b) . ModExp $ 1
