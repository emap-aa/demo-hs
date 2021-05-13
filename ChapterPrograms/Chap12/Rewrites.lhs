
> module Chap12.Rewrites (rewrites)
> where
> import Chap12.Expressions
> import Chap12.Laws (Equation)
> import Chap12.Matchings (match)
> import Chap12.Substitutions (apply)
> import Chap12.Utilities (anyOne, segments)

> rewrites :: Equation -> Expr -> [Expr]
> rewrites eqn (Compose as) = map Compose (
>   rewritesSeg eqn as ++ anyOne (rewritesA eqn) as)
> rewritesA eqn (Var v) = []
> rewritesA eqn (Con k es)
>   = map (Con k) (anyOne (rewrites eqn) es)

> rewritesSeg :: Equation -> [Atom] -> [[Atom]]
> rewritesSeg (e1,e2) as
>     = [as1 ++ deCompose (apply sub e2) ++ as3
>       | (as1,as2,as3) <- segments as,
>         sub <- match (e1,Compose as2)]


