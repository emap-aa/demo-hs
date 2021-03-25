module Chapter5 where

import Data.List

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Ord, Show)


-- exercise E

nub1 :: (Eq a) => [a] -> [a]
nub1 [] = []

-- nub1 [x] = [x]
-- nubs (x:[]) = x:[]
-- nub1 (x:xs) =
--   if elem x xs
--     then nub1 xs
--     else (x : nub1 xs)

-- nub1 (x:xs) = x:nub1 [y | y <- xs, y /= x]

nub1 (x:xs) = x : nub1 (filter (/= x) xs)

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) =
  if p x
    then dropWhile' p xs
    else x : xs

nub2 :: (Ord a) => [a] -> [a]

-- nub2 = map head . group . sort
nub2 = remdups . sort
  where
    remdups []     = []
    remdups (x:xs) = x : remdups (dropWhile' (== x) xs) 


-- exercicio F

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) =
  if p x
    then x : takeWhile' p xs
    else []


span' p xs = (takeWhile' p xs, dropWhile' p xs)

whiteSpace c = (c == ' ') || (c == '\n') || (c == '\t')


words' :: String -> [String]
words' xs
  | null ys = []
  | otherwise = w : words' zs
  where
    ys = dropWhile' whiteSpace xs
    (w, zs) = break whiteSpace ys

