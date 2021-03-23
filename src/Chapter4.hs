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


fork :: (a -> b, a -> c) -> a -> (b,c)
fork (f,g) x = (f x, g x)

cross (f,g) = fork (f . fst, g . snd)

{-

1. unzip       = fork (map fst, map snd)
2. cross (f,g) = fork (f . fst, g . snd)

3. map id      = id
4. map (f . g) = map f . map g

5. cross (f,g) . fork (h,k) = fork (f . h,g . k)
6. fork (f,g) . h           = fork (f . h,g . h)
7. fst . cross (f,g)        = f . fst
8. snd . cross (f,g)        = g . snd

prove:

cross (map f, map g) . unzip   = unzip . map (cross (f,g))
cross (map f, map g) . unzip                                  { 1 }
cross (map f, map g) . fork (map fst, map snd)                { 5 }
fork (map f . map fst, map g . map snd)                       { 4 }
fork (map (f . fst), map (g . snd))                           { 7,8 }
fork (map (fst . cross (f,g)), map (snd . cross (f,g)))       { 4 }
fork (map fst . map cross (f,g), map snd . map cross (f,g))   { 6 }
fork (map fst, map snd) . map (cross (f,g))                   { 1 }
unzip . map (cross (f,g))                                     

-}
