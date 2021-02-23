
module CommonWords where

import Data.Char
import Data.List

type Text' = [Char]
-- como lidar com um tipo ja declarado no prelude
type Word' = [Char]


-- OBS  concat . map == concatMap

commonWords :: Int -> Text' -> String
commonWords n = 
  concat . map showRun . take n . sortRuns . countRuns . sortWords . words . map toLower


-- how to implement?
showRun :: (Int,Word') -> Text'
showRun (x,y) = ""


-- how to implement?
countRuns :: [Word'] -> [(Int,Word')]
countRuns [] = []


-- how to make it if runs are (Word',Int)?
sortRuns :: [(Int,Word')] -> [(Int,Word')]
sortRuns = sort


sortWords :: [Word'] -> [Word']
sortWords = sort

