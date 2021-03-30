module Chapter6 where

import Prelude hiding (exp)


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

