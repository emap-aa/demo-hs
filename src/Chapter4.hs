module Chapter4 where

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

-- exercise H
takeH :: Int -> [a] -> [a]
takeH 0 _ = []
takeH _ [] = []
takeH n (x : xs) = x : takeH (n-1) xs

-- Não tem como takeH 0 undefined e takeH undefined [] serem ambos [] pois ele vai precisar avaliar o primeiro argumento nesse código que foi escrito. Se houvesse como, seria um absurdo.

splitAtH :: Int -> [a] -> ([a], [a])
splitAtH n xs = splitAtHAux n ([], xs)

splitAtHAux :: Int -> ([a], [a]) -> ([a], [a])
splitAtHAux 0 x = x
splitAtHAux n (xs, []) = (xs, [])
splitAtHAux n (xs, y:ys) = splitAtHAux (n-1) (xs ++ [y], ys)
