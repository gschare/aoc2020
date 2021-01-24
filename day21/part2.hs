import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Map.Strict as M
import Data.List (nub, sort, sortOn, intercalate)
import Data.Char (isAlpha)
import Control.Arrow ((***))
import Data.Function (on)

data Allergen = None | Allergen String deriving (Show, Eq)
type Ingredient = String
type Food = ([Ingredient], [Allergen])

instance Ord Allergen where
  None `compare` _ = LT
  (Allergen s1) `compare` (Allergen s2) = s1 `compare` s2

main :: IO ()
main = do
    foods <- parseFile "input.txt"
    putStrLn $ solve2 foods

makeIngredientMap :: [Food] -> M.Map Ingredient [Allergen]
makeIngredientMap = makeIngredientMap' M.empty
  where makeIngredientMap' assocs [] = assocs
        makeIngredientMap' assocs (food:rest) =
            let noAllergen = ((length $ fst food) /= (length $ snd food)) || ((length $ snd food) == 0)
                maybeNoAllergen allergens = if noAllergen then None:allergens else allergens
                assocs' = foldr (\(i, as) -> M.insertWith (\v1 v2 -> nub (v1 ++ v2)) i as) assocs $ [(i, maybeNoAllergen $ snd food) | i <- fst food]
             in makeIngredientMap' assocs' rest

makeAllergenMap :: [Food] -> M.Map Allergen [[Ingredient]]
makeAllergenMap = makeAllergenMap' M.empty
  where makeAllergenMap' assocs [] = assocs
        makeAllergenMap' assocs (food:rest) =
            let assocs' = foldr (\(a, is) -> M.insertWith (\v1 v2 -> (v1 ++ v2)) a [is]) assocs $ [(a, fst food) | a <- snd food]
             in makeAllergenMap' assocs' rest

-- Solution algorithms

-- These are mappings from ingredients to allergens where there is only one possible allergen.
findSurjections :: M.Map Ingredient [Allergen] -> [(Ingredient, Allergen)]
findSurjections = M.foldrWithKey f []
    where f i as acc = if length as == 1
                          then (i, head as):acc
                          else acc

-- These are mappings from allergens to ingredients where only one ingredient appears.
findInjections :: M.Map Allergen [[Ingredient]] -> [(Ingredient, Allergen)]
findInjections = M.foldrWithKey f []
    where f a iss acc = let alwaysIngredients = filter (\x -> all (x `elem`) iss) $ nub . concat $ iss
                         in if length alwaysIngredients == 1
                               then (head alwaysIngredients, a):acc
                               else acc

-- These are foods with only one ingredient and one allergen.
findBijections :: [Food] -> [(Ingredient, Allergen)]
findBijections = foldr f []
    where f (is,as) acc = if (length is == 1) && (length as == 1)
                           then (head is, head as):acc
                           else acc

-- Procedure for removing found associations from the list of foods.
removeFound :: [Food] -> [(Ingredient, Allergen)] -> [Food]
removeFound foods assocs = foldl (\acc (i,a) -> removeAll i a acc) foods assocs
    where removeAll i a acc = map ((filter (/=i)) *** (filter (/=a))) acc

solveConstraints :: [Food] -> [(Ingredient, Allergen)]
solveConstraints foods = solveConstraints' foods []
    where allIngredients = sort . nub . concatMap fst $ foods
          solveConstraints' :: [Food] -> [(Ingredient, Allergen)] -> [(Ingredient, Allergen)]
          solveConstraints' foods assocs = let iMap = makeIngredientMap foods
                                               aMap = makeAllergenMap foods
                                               newAssocs = findBijections foods ++ findSurjections iMap ++ findInjections aMap
                                               foods' = removeFound foods newAssocs
                                               assocs' = nub $ assocs ++ newAssocs
                                            in if (==) allIngredients (sort $ map fst assocs')
                                                  then assocs'
                                                  else solveConstraints' foods' assocs'

solve :: [Food] -> Int
solve foods = let nonAllergenic = map fst . filter ((==None) . snd) . solveConstraints $ foods
               in foldr (\food acc -> if food `elem` nonAllergenic then acc + 1 else acc) 0 $ concatMap fst foods

solve2 :: [Food] -> String
solve2 foods = intercalate "," . map fst . sortOn snd . filter ((/=None) . snd) . solveConstraints $ foods

-- Parsing
parse :: P.ReadP a -> String -> a
parse parser = fst . head . P.readP_to_S parser

ingredientsParser :: P.ReadP [Ingredient]
ingredientsParser = P.sepBy (P.many1 (P.satisfy isAlpha)) (P.char ' ')

allergensParser :: P.ReadP [Allergen]
allergensParser = map Allergen <$> (P.string "(contains " *> (P.sepBy (P.many1 (P.satisfy isAlpha)) (P.string ", ")) <* P.char ')')

foodParser :: P.ReadP Food
foodParser = (,) <$> ingredientsParser <*> (P.char ' ' *> allergensParser <* P.eof)

readLine :: String -> Food
readLine = parse foodParser

parseFile :: String -> IO [Food]
parseFile filename = readFile filename >>= return . map readLine . lines
