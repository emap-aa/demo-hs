module Lib where

import Test.QuickCheck

someFunc :: IO ()
someFunc = putStrLn "someFunc"

f :: Ord a => [a] -> [a]
f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
  where
    ys = [a | a <- xs, a <= x]
    zs = [b | b <- xs, b > x]

-- >>> f $ reverse [1..20]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

-- >>> f "haskell"
-- "aehklls"


data Expr = Val Int | Add Expr Expr
  deriving (Eq, Show)

data Op = PUSH Int | ADD
  deriving (Eq, Show)


instance Arbitrary Expr where
  arbitrary = sized genExpr

genExpr :: Int -> Gen Expr
genExpr s
 | s > 0 = frequency [(1, Add <$> arbitrary <*> arbitrary), (1, Val <$> arbitrary)]
 | otherwise = Val <$> arbitrary


e1 = Val 1
e2 = Add (Val 2) (Val 3)
e3 = Add e2 (Val 4)

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

comp :: Expr -> [Op]
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

exec (PUSH n : c) s       = exec c (n : s)
exec (ADD : c)    (n:m:s) = exec c (n + m : s)
exec _            s       = s

-- >>> exec [PUSH 1] [4,5]
-- [1,4,5]

-- >>> exec [PUSH 2, PUSH 3, ADD] [3]
-- [5,3]

correct :: Expr -> Bool
correct e = exec (comp e) [] == [eval e]

-- >>> correct e3
-- True


-- ()
-- ()
-- ()
-- ()
{- QuickCheck

- https://cseweb.ucsd.edu/classes/wi14/cse230-a/lectures/lec-quickcheck.html
- https://www.dcc.fc.up.pt/~pbv/aulas/tapf/handouts/quickcheck.html
- https://stackoverflow.com/questions/2517152/verbosecheck-in-quickcheck-2

Example:

> generate arbitrary :: IO Expr
> sample (arbitrary :: Gen Expr)
> verboseCheck correct

-}
