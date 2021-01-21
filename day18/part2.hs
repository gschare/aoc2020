import Data.List.Split (splitOn)
import Data.Char (isDigit, digitToInt)

data Token = Val Int | Mul | Add | Open | Close deriving (Eq)
type Expr = [Token]

instance Show Token where
    show (Val x) = show x
    show Mul = "*"
    show Add = "+"
    show Open = "("
    show Close = ")"

instance Ord Token where
    Add `compare` Mul = GT
    Add `compare` Add = EQ
    Mul `compare` Mul = EQ
    Open `compare` Open = EQ

    -- Not actually used; just for completion
    Mul `compare` Open = LT
    Add `compare` Open = LT

    _ `compare` _ = EQ

eval :: Expr -> Int
eval = head . foldl f []
    where f :: [Int] -> Token -> [Int]
          f acc (Val x)   = x:acc
          f (x:y:acc) Mul = (x * y):acc
          f (x:y:acc) Add = (x + y):acc

shuntingYard :: [Token] -> Expr
shuntingYard ts = translate [] [] ts

opPop :: Token -> [Token] -> ([Token], [Token])
opPop op opStack = (op:(dropWhile hasPrecedence opStack), reverse $ takeWhile hasPrecedence opStack)
    where hasPrecedence x = op <= x && x /= Open

translate :: [Token] -> [Token] -> [Token] -> Expr
translate opStack outQueue [] = reverse ((reverse opStack) ++ outQueue)
translate opStack outQueue (t:ts) =
  case t of
    Val x -> translate opStack ((Val x):outQueue) ts
    Mul   -> let (opStack', outQueue') = opPop Mul opStack in translate opStack' (outQueue' ++ outQueue) ts
    Add   -> let (opStack', outQueue') = opPop Add opStack in translate opStack' (outQueue' ++ outQueue) ts
    Open  -> translate (Open:opStack) outQueue ts
    Close -> let ((_:_:opStack'), outQueue') = opPop Close opStack in translate opStack' (outQueue' ++ outQueue) ts  -- In case of right parenthesis, discard the ")(" that will end up at the top of the stack after popping until the left parenthesis.

parseToken :: Char -> Token
parseToken x | isDigit x = Val $ digitToInt x
parseToken '*' = Mul
parseToken '+' = Add
parseToken '(' = Open
parseToken ')' = Close
parseToken _   = error "unrecognized token"

parseExpr :: String -> Expr
parseExpr s = shuntingYard tokenized
    where tokenized :: [Token]
          tokenized = map parseToken $ filter (/=' ') s

parseFile :: String -> IO [[Token]]
parseFile filename = readFile filename >>= return . map parseExpr . lines

test :: IO ()
test = map eval <$> parseFile "test.txt" >>= \results -> readFile "test.txt" >>= mapM_ print . zip results . lines

main :: IO ()
main = sum . map eval <$> parseFile "input.txt" >>= print
