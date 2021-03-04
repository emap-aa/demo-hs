module Chapter4 where

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
