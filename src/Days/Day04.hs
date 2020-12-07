module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Char
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
inputParser :: Parser Input
inputParser = ((do
        k <- Data.Attoparsec.Text.take 3
        char ':'
        v <- takeTill isSpace
        return (k, v)) `sepBy` space) `sepBy` (endOfLine >> endOfLine)

------------ TYPES ------------
type Input = [[(Text, Text)]]

type OutputA = Int

type OutputB = Int
------------ PART A ------------
partA :: Input -> OutputA
partA i = length $ filter (\x -> length x == 8 || (length x == 7 && not (any (\y -> fst y == "cid") x))) i
    

------------ PART B ------------

parseInt :: Text -> Int
parseInt j = fromRight 0 $ parseOnly decimal j
partB :: Input -> OutputB
partB i = length $ filter (\x -> all validPassport x && 
        (length x == 8 || (length x == 7 && not (any (\y -> fst y == "cid") x)))) i

validPassport :: (Text, Text) -> Bool
validPassport ("eyr", j) = i >= 2020 && i <= 2030 where i = parseInt j
validPassport ("byr", j) = i >= 1920 && i <= 2002 where i = parseInt j
validPassport ("iyr", j) = i >= 2010 && i <= 2020 where i = parseInt j
validPassport ("hgt", j) = (i >= 150 && i <= 193 && n == "cm") || (i >= 59 && i <= 76 && n == "in") 
    where 
        (i, n) = fromRight (0, "error") $ parseOnly (do
            v <- decimal
            n <- Data.Attoparsec.Text.take 2
            return (v, n)) j
validPassport ("hcl", j) = T.length (fromRight "" (str j)) == 6 where 
    str = parseOnly (do
        char '#'
        Data.Attoparsec.Text.takeWhile (inClass "0-9a-f"))
validPassport ("ecl", j) = isRight (parseOnly (string "amb" <|> string "blu" <|> string "brn" <|> string "gry" <|> string "grn" <|> string "hzl" <|> string "oth") j)
validPassport ("pid", j) = T.length (fromRight ""(parseOnly (
    Data.Attoparsec.Text.takeWhile isDigit) j)) == 9
validPassport ("cid", _) = True
validPassport _ = False