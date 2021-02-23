
module CommonWords where

import Data.Char
import Data.List

type Text' = [Char]
-- como lidar com um tipo ja declarado no prelude
type Word' = [Char]


-- :t map toLower

-- commonWords :: Int -> Text' -> String
commonWords n = 
  concat . map showRun . take n . sortRuns . countRuns . sortWords . words . map toLower


showRun :: (Int,Word') -> Text'
showRun (x,y) = ""

countRuns :: [Word'] -> [(Int,Word')]
countRuns [] = []

sortRuns :: [(Int,Word')] -> [(Int,Word')]
sortRuns = sort

sortWords :: [Word'] -> [Word']
sortWords = sort

