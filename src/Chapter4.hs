module Chapter4 where


data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Ord, Show)


-- exercise H : Yuri

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

