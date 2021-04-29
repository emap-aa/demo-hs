
module Chapter7 where

import Data.List

-- ## lazy evaluation

sqrt' x = x * x

subseqs1, subseqs2 :: [a] -> [[a]]

subseqs1 [] = [[]]
subseqs1 (x:xs) = subseqs1 xs ++ map (x:) (subseqs1 xs)

{- the second definition is faster but uses more memory. This balance
   must be controled by the programmer. -}

subseqs2 [] = [[]]
subseqs2 (x:xs) = xss ++ map (x:) xss
  where xss = subseqs2 xs

{- only possible under lazy evaluation -}

ones = 1 : ones

foo1 n = sum (take n primes')
  where
    primes' = [x | x <- [2..], divisors' x == [x]]
    divisors' x = [d | d <- [2..x], x `mod` d == 0]


foo2 n = sum (take n primes'')
primes'' = [x | x <- [2..], divisors'' x == [x]]
divisors'' x = [d | d <- [2..x], x `mod` d == 0]


foo3 = \n -> sum (take n primes')
  where
    primes' = [x | x <- [2..], divisors' x == [x]]
    divisors' x = [d | d <- [2..x], x `mod` d == 0]

foo4 = sum . flip take primes'
  where
    primes' = [x | x <- [2..], divisors' x == [x]]
    divisors' x = [d | d <- [2..x], x `mod` d == 0]


-- ## controlling space

{- under lazy evaluation, space is needed to hold the evaluations. See
   time with `:set +s` -}

sum1, sum2 :: [Int] -> Int
sum1 = foldl (+) 0
sum2 = foldl' (+) 0

-- >>> sum1 [1..100000]
-- 5000050000

-- >>> sum2 [1..100000]
-- 5000050000

-- exercicio C

f :: Int -> Int -> Int
f n x = if x == 0 then undefined else 0

-- See https://youtu.be/R1uBhRK2AKI

primes = sieve [2..]
sieve (p:xs) = p : sieve [ x | x <- xs, mod x p /= 0]

twin (x,y) = y == x + 2

twins = filter twin (zip primes (tail primes))

mean1 :: [Float] -> Float
mean1 [] = 0
mean1 xs = sum xs / fromIntegral (length xs)

{- sumlen2 == sumlen3 porque... law foldr/fodl -}

sumlen0, sumlen1, sumlen2, sumlen3, sumlen4, sumlen5 :: [Float] -> (Float, Int)

sumlen0 xs = (sum xs, length xs)

sumlen1 [] = (0,0)
sumlen1 (x:xs) = (s + x, n + 1)
  where
    (s, n) = sumlen1 xs

sumlen2 = foldr f (0, 0)
  where
    f x (s, n) = (s + x, n + 1)

sumlen3 = foldl g (0, 0)
  where
    g (s, n) x = (s + x, n + 1)

sumlen4 = foldl' g (0, 0)
  where
    g (s, n) x = (s + x, n + 1)

sumlen5 = foldl' f (0, 0)
  where
    f :: (Float, Int) -> Float -> (Float, Int)
    f (s, n) x = s `seq` n `seq` (s + x, n + 1)

mean2 f [] = 0
mean2 f xs = s / fromIntegral n
  where
    (s, n) = f xs

{- compare mean2 sumlen5 [1..10000000] with mean2 sumlen3 [1..10000000] -}


-- ## Controling time

cp1, cp1', cp2, cp3 :: [[a]] -> [[a]]

cp1 [] = [[]]
cp1 (xs:xss) = [x : ys | x <- xs, ys <- cp1 xss]

{- cp1' is repeatly evaluated -}

cp1' [] = [[]]
cp1' (xs:xss) = concat (map f xs)
  where
    f x = [x : ys | ys <- cp1' xss]

cp2 = foldr op [[]]
  where
    op xs yss = [x : ys | x <- xs, ys <- yss]

cp3 []       = [[]]
cp3 (xs:xss) = [x:ys | x <- xs, ys <- yss]
  where
    yss = cp3 xss

-- >>> sum $ map sum $ cp1 [[1..10]]
-- 55


-- ## Accumulating parameters

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

{- steps for apppend [1,2] ++ [3] ? -}

reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]


{- steps reverse1 [1,2]? ... ([] ++ [2]) ++ [1]

   reverse xs takes

   1 + 2 ... + (n+1)


   reverse' xs ys = (reverse xs) ++ ys

   base
     reverse' []  ys = ... = ys

   induction
     reverse' (x:xs) ys = reverse (x:xs) ++ ys = ... = reverse xs ++ ([x] ++ ys)
     reverse' xs x:ys
-}

