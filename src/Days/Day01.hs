module Days.Day01 (runDay) where

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
{- ORMOLU_ENABLE -}

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs)
       | l < n = []
       | otherwise = map (x : ) (subsets (n-1) xs) ++ subsets n xs
       where l = length (x : xs)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
day1 :: Int -> Input -> Int
day1 n input = maybe (-69) product . find ((2020 ==) . sum) $ subsets n input

partA :: Input -> OutputA
partA = day1 2

------------ PART B ------------
partB :: Input -> OutputB
partB = day1 3
