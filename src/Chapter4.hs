module Chapter4 where

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Ord, Show)

-- exercise B

allPairs :: [(Integer, Integer)]
allPairs = [(x,y) | x <- [0..], y <- [0..]]

allPairs2 :: [(Integer, Integer)]
allPairs2 = [(y,x-y) | x <- [0..], y <- [0..x]]

-- exercise C

disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint a [] = True
disjoint [] a = True

--disjoint (a:as) (b:bs) =
--  if b == a then False
--  else if b > a then disjoint as (b:bs)
--  else disjoint (a:as) bs

disjoint as'@(a:as) bs'@(b:bs)
  | b == a = False
  | b > a = disjoint as bs'
  | otherwise = disjoint as' bs

-- exercise H: Yuri

takeH :: Int -> [a] -> [a]
takeH 0 _  = []
takeH n (x : xs) = x : takeH (n-1) xs
takeH _ xs = []


-- Não tem como takeH 0 undefined e takeH undefined [] serem ambos []
-- pois ele vai precisar avaliar o primeiro argumento nesse código que
-- foi escrito. Se houvesse como, seria um absurdo.

-- λ> take undefined []
-- *** Exception: Prelude.undefined
-- λ> 0 == undefined
-- *** Exception: Prelude.undefined

-- custo 2 * O(n) = O(n)
-- :set +s

splitAt0 n xs = (take n xs, drop n xs)

-- ainda custo 2*O(n)

splitAt2 :: Int -> [a] -> ([a],[a])
splitAt2 n [] = ([],[])
splitAt2 n (x:xs) =
  if n == 0
    then ([], x : xs)
    else (x : ys, zs)
  where
    (ys, zs) = splitAt2 (n - 1) xs

