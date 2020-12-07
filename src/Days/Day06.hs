module Days.Day06 (runDay) where

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

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB
------------ TYPES ------------
type Input = [ [[Answer]] ]

type Answer = Char

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 letter `sepBy` endOfLine) `sepBy` (endOfLine >> endOfLine)
------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map (length . L.foldr1 L.union)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (length . L.foldr1 L.intersect)
