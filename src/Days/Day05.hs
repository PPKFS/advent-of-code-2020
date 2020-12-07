module Days.Day05 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = seat `sepBy` endOfLine where
    seat = many1 $ (char 'F' >> return F) <|> (char 'B' >> return B) <|> (char 'L' >> return L) <|> (char 'R' >> return R)


------------ TYPES ------------
data Seat = F | B | L | R deriving Show
type Input = [[Seat]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
expandTuple :: ((Int, Int), (Int, Int)) -> Int
expandTuple ((l, _), (r, _)) = l*8 + r

partA :: Input -> OutputA
partA s = maximum (map (expandTuple . foldl' findSeat ((0, 127), (0, 7))) s)

midpoint :: (Int, Int) -> Bool -> (Int, Int)
midpoint (l, h) True = (l, ((h+l+1) `div` 2) - 1)
midpoint (l, h) False = ((h+1+l) `div` 2, h)
findSeat ::((Int, Int), (Int, Int)) ->  Seat -> ((Int, Int), (Int, Int))
findSeat (r, v) F = (midpoint r True, v)
findSeat (r, v) B = (midpoint r False, v)
findSeat (v, r) L = (v, midpoint r True)
findSeat (v, r) R = (v, midpoint r False)
------------ PART B ------------

windows :: Int -> [a] -> [[a]]
windows n = map (Prelude.take n) . tails

partB :: Input -> OutputB
partB s = ((Data.List.head $ filter (\[x, y] -> y-x == 2) (windows 2 sortedList)) !! 0) +1 where
    sortedList = sort (map (expandTuple . foldl' findSeat ((0, 127), (0, 7))) s)
