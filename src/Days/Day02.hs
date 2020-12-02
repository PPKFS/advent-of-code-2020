module Days.Day02 (runDay) where

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
inputParser :: Parser Input
inputParser = parsePW `sepBy` endOfLine 
    where 
    parsePW = do
        sp1 <- decimal
        char '-'
        sp2 <- decimal
        space
        c <- anyChar
        char ':'
        space
        pw <- many1 letter
        return $ Password sp1 sp2 c (T.pack pw)
    

------------ TYPES ------------
data Password = Password Int Int Char Text deriving Show
type Input = [Password]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inputs = length $ filter pwCheck1 inputs

pwCheck1 :: Password -> Bool
pwCheck1 (Password lo hi l pw) = lo <= c && hi >= c
       where
       c = T.count (T.singleton l) pw
        

------------ PART B ------------
partB :: Input -> OutputB
partB inputs = length $ filter pwCheck2 inputs

pwCheck2 :: Password -> Bool
pwCheck2 (Password lo hi l pw) = xor (T.index pw (lo-1) == l) (T.index pw (hi-1) == l)
