module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
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
inputParser :: Parser Input
inputParser = takeWhile1 (/= '\n') `sepBy` endOfLine

------------ TYPES ------------
type Input = [Text]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
takeEvery n [] = []
takeEvery n (x : xs)  = x : takeEvery n (drop (n-1) xs)

downSlope across dwn m = length $ filter (== '#') $ zipWith (\ x y -> T.index y (across * x `mod` wrap)) [0 .. ] (takeEvery dwn m) where
    wrap = T.length (Data.List.head m)
partA :: Input -> OutputA
partA = downSlope 3 1

------------ PART B ------------
partB :: Input -> OutputB
partB i = sl 1 1 * sl 3 1 * sl 5 1 * sl 7 1 * sl 1 2 where
    sl v r= downSlope v r i
