
> module Chap09.Notes where


> repeat1 x = x : repeat1 x
> repeat2 x = xs
>   where
>     xs = x : xs

> fr (x:[]) = 1
> fr (x:xs) = 2

the function `fact` 

> fact = (\f n -> if n == 0 then 1 else n * f (n -1)) fact

-- >>> fact 10
-- 3628800

