
module Chapter2 where

import Data.Char

-- Exercio A: “Is a half of two plus two equal to two or three?”

ansA = x == 2 || x == 3
  where 
    x = (2 + 2) `div` 2 

-- Tarefa, completar este modulo cada aluno contribuindo com a solucao
-- de um exercicio do capitulo 2 do livro Thinking Functionally with
-- Haskell (TFwH).

-- Exercício C : Eduardo (Acho que as 3 perguntas estão respondidas no código.)

modernise :: String -> String
modernise xss = unwords . map f $ words xss
  where 
    f xs =  toUpper (head xs) : tail xs 


testC = modernise "The morphology of prex – an essay in meta-algorithmics"
