module Chapter6 where

import Prelude hiding (exp)
import Data.List

data Nat = Zero | Succ Nat

exp x Zero     = 1
exp x (Succ n) = x * exp x n


-- Exercise A

add :: Nat -> Nat -> Nat
add Zero a = a
add (Succ x) a = Succ (add x a)

mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult (Succ x) y = mult x y `add` y


-- See demo-lean/src/chapter-6.lean

-- Exercise B

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- See demo-lean/src/chapter-6.lean

-- Exercise C

-- See demo-lean/src/chapter-6.lean


mss, mss1, mss2 :: [Int] -> Int

mss = mss2

mss1 = maximum . map sum . segments
 where 
   segments = concat . map inits . tails

{-
mss = maximum . map sum . concat . map inits . tails

1) map f . concat = concat . map (map f)

mss = maximum . concat . map (map sum) . map inits . tails

2) map f . map g = map (f . g)

mss = maximum . concat . map (map sum . inits) . tails

3) maximum . concat = maximum . map maximum

mss = maximum . map maximum . map (map sum . inits) . tails

2) idem

mss = maximum . map (maximum . map sum . inits) . tails

4) map sum . inits = scanl (+) 0

mss = maximum . map (maximum . scanl (+) 0) . tails

5) ????

mss = maximum . map (foldr f 0) . tails
  where f x y = 0 `max` (x + y)

6) scanr ....

-}

mss2 = maximum . scanr f 0
  where f x y = 0 `max` (x + y)
  
