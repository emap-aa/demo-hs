
module CommonWords where

import Data.Char
import Data.List
import Prelude hiding (Word)
import System.FilePath

type Word = [Char]
type Text = [Char]

data Run = Run
  { count :: Int
  , word :: Word
  } 

instance Eq Run where
  (==) x y = word x == word y && count x == count y

instance Ord Run where
  (<=) x y =
    if count x == count y
      then word x <= word y
      else count x <= count y

instance Show Run where
  show (Run c w) = w ++ ": " ++ show c ++ "\n"

commonWords :: Int -> Text -> String
commonWords n = 
  concatMap showRun . take n . sortRuns . countRuns . sortWords . words . map toLower

-- how to implement?
showRun :: Run -> Text
showRun (Run x y) = y ++ ": " ++ show x ++ "\n"

-- how to implement?
countRuns :: [Word] -> [Run]
countRuns [] = []
countRuns xs = [Run (length ys) (head ys) | ys <- group xs]

-- how to make it if runs are (Word,Int)?
sortRuns :: [Run] -> [Run]
sortRuns = reverse . sort

sortWords :: [Word] -> [Word]
sortWords = sort

