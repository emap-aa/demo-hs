
Exemplo de modulo literate programming em Haskell

> module Literate where

O trecho que código abaixo é um exemplo de função usando guardas. Uma
forma de declarar funções onde cada saida tem uma condição, um guarda.

> test1 :: (Eq a, Num a, Num p) => a -> p
> test1 x
>   | x == 2    = 3
>   | otherwise = 1

Alternativamente, poderiamos usar diferentes equações, nete caso a
ordem das equações importa, para definir o comportamento da função.

> test2 :: (Eq a, Num a, Num p) => a -> p
> test2 2 = 3
> test2 _ = 1

Arquivos como este, como extensão `.lhs` podem ser lidos normalmente
pelo GHCI, o que torna mais simples do que alternativas como o OrgMode
do Emacs. Mas como estes arquivos são lidos pelo VSCode?

> -- >>> test2 10
> -- 1
