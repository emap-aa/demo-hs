module Chapter6 where

import Prelude hiding (exp)


data Nat = Zero | Succ Nat

exp x Zero     = 1
exp x (Succ n) = x * exp x n


                 
