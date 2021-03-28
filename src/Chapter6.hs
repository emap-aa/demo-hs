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

-- x+0=x

-- mult Zero 0 = Zero
-- mult (Succ n) 0 = mult n 0 + 0 = mult n 0 = Zero

-- Provar que:
-- mult (x+y) z = mult x z + mult y z

-- mult (x+y) z
-- Indução no y:

-- mult (x+Zero) z = mult x z = mult x z + 0 = mult x z + mult y z
-- mult (x+(Succ n)) z = mult (Succ (x+n)) z = mult (x+n) z + z = mult x z + (mult n z + z) = mult x z + mult (Succ n) z


-- Exercise B

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- data Lista = [] | x : Lista
-- xs ++ [] = xs

-- Provar que:
-- reverse (xs ++ ys) = reverse ys ++ reverse xs

-- reverse (xs ++ ys)
-- Indução no xs:

-- reverse ([] ++ ys) = reverse ys = (reverse ys) ++ [] = (reverse ys) ++ (reverse [])
-- reverse ((x : xs) ++ ys) = reverse (x : (xs ++ ys)) = reverse (xs ++ ys) ++ [x] = reverse ys ++ reverse xs ++ [x] = reverse ys ++ reverse (x:xs)


-- Exercise C

-- head . map f = f . head

-- Usando que Lista = [] | [x] : Lista
-- Considerando apenas o caso em que a função é definida (x : Lista)

-- head . map f (x : xs) = head (f x) : (map f xs) = f x
-- f . head (x : xs) = f x

-- Vale para qualquer lista não vazia

