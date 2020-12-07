module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Prelude
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import qualified Data.Text as T
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser (Map Text [(Int, Text)])
inputParser =Map.fromList <$> (do
        name <- bagName
        string "bags contain "
        contents <- (string "no other bags" >> return []) <|>  (do
                bags <- parse1Bag `sepBy` string ", "
                takeTill (=='.')
                return bags)
        return (name, contents)
        `sepBy` string ".\n")

bagName :: Parser Text
bagName = do     
    v <- concat <$> count 2 (do 
        x <- many1 letter
        space
        return x)
    return $ T.pack v

parse1Bag :: Parser (Int, Text)
parse1Bag = do
    n <- decimal
    space
    name <- bagName
    string "bags" <|> string "bag"
    return (n, name)

orEmpty :: Ord a => a -> Map a [b] -> [b]
orEmpty = Map.findWithDefault []

containsGold :: Map.Map Text [(Int, Text)] -> [(Int, Text)] -> Bool
containsGold mp = any (\x -> let v = snd x in v == "shinygold" || containsGold mp (orEmpty v mp)) 

------------ PART A ------------
partA :: Map Text [(Int, Text)] -> Int
partA = Map.size . (Map.filter =<< containsGold)

------------ PART B ------------
partB :: Map Text [(Int, Text)] -> Int
partB = getContents =<< orEmpty "shinygold" where 
    getContents l v = sum (map (\(i, b) -> i + (i * getContents (orEmpty b v) v)) l)