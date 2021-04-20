
module Chapter7 where

import Data.List

sqrt' x = x * x


subseqs1, subseqs2 :: [a] -> [[a]]

subseqs1 [] = [[]]
subseqs1 (x:xs) = subseqs1 xs ++ map (x:) (subseqs1 xs)

subseqs2 [] = [[]]
subseqs2 (x:xs) = xss ++ map (x:) xss
  where xss = subseqs2 xs


-- exemplo 2
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


-- sum

sum1 :: [Int] -> Int
sum1 = foldl (+) 0

-- exercicio C

f :: Int -> Int -> Int
f n x = if x == 0 then undefined else 0


-- https://youtu.be/R1uBhRK2AKI

primes = sieve [2..]
sieve (p:xs) = p : sieve [ x | x <- xs, mod x p /= 0]

twin (x,y) = y == x + 2

twins = filter twin (zip primes (tail primes))
