module Pleasegofirst where
import Data.Map (Map)
import qualified Data.Map as Map
--import Control.Monad

{-
Na pior estratégia, os amigos descem quando o que estiver mais atrás na fila suba o elevador.

"Suppose someone lets another pass if doing this doesn’t change his 
own total waiting time, but saves time for the other person."

O único fator que importa para o tempo que uma pessoa espera na fila 
é a posição do amigo dela que está mais próximo do fim da fila.
Logo, deixar outra pessoa te passar na fila não altera o seu tempo de espera
se você não for o último amigo do seu grupo.
Portanto, cada pessoa iria deixar a pessoa de trás passar até chegar diretamente à frente do último de seu  grupo.
Logo, seguindo essa regra, a fila "ótima" teria todos os grupos de amigos juntos.
ex: AAABBCCCCCC
-}

{- goFirst
Lê a string de trás pra frente enquanto cria um dicionário:
 - Se acha um char novo, cria um par (key,value) em que 
   a chave é o Char novo e o value é uma tupla (posição do char, frequência)
 - Se é um char já visto, aumenta a frequência dele em 1 no dicionário.
-}

goFirst ::  String -> Map Char (Int, Integer) -> Int -> Int -> [(Char,(Int, Integer))]
goFirst [] mapa i n =  Map.assocs mapa
goFirst (x:xs) mapa i n
 | Map.member x mapa = goFirst xs (Map.adjust (\(a,b)->(a,b+1)) x mapa) (i+1) n
 | otherwise = goFirst xs (Map.insert x (n-i,1) mapa) (i+1) n

{-
O tempo médio de espera é o somatório de 5 * (tam. do grupo) * (posição do último do grupo)
para todo grupo da fila. waiting usa as informações dadas por goFirst para isso.
-}

waiting :: [(Char,(Int,Integer))] -> Float
waiting [] = 0
waiting ((char,(pos,qtd)):rest) = fromIntegral pos * fromIntegral qtd + waiting rest

-- Quicksort dos pares (key,(posição,quantidade)) em relação à posição.
qsort :: [(Char,(Int,Integer))] -> [(Char,(Int,Integer))]
qsort []= []
qsort ((c,(pos,qtd)):xs) = qsort [(a,(p,q)) | (a,(p,q)) <- xs, p < pos] ++ [(c,(pos,qtd))] ++ qsort [(a,(p,q)) | (a,(p,q)) <- xs, pos <= p]

{-
Constroi a fila após as pessoas trocarem de lugar a partir da:
 - Posição (invertida) que a última pessoa do grupo "friends" está &
 - Quantidade de pessoas que o grupo "friends" tem.
-}

buildQ :: [(Char, (Int,Integer))] -> String
buildQ [] = []
buildQ ((friends,(pos,qtd)):rest) = replicate (fromInteger qtd) friends ++ buildQ rest

{-
Dessa forma, podemos inputar essa String de novo em "goFirst" para então calcularmos a demora da lista
em que as pessoas são educadas.
-}

-- Calcula o tempo médio individual para cada um descer de ski com os amigos de maneira educada.
timeGoFirst n xs = waiting $ goFirst ( buildQ $ qsort $ goFirst (reverse xs) empty 0 m) empty 0 m where
    m = fromInteger n
    empty = Map.empty

-- Calcula o tempo médio de espera da fila das pessoas não educadas (multiplicaremos por 5 e dividiremos por n depois).
timeNotNice n xs =  waiting $ goFirst (reverse xs) Map.empty 0 (fromInteger n)
-- Hora de comparar os mal educados com os bem educados!
--compareAvg dá a diferença em segundos das médias

compareAvg :: Int -> String -> Float
compareAvg n [] = 0
compareAvg n xs = 5*(timeNotNice (toInteger n) xs - timeGoFirst (toInteger n) xs )/ fromIntegral n

{-
repeat:: Int -> IO ()
repeat 0 = return ()
repeat k = do
    n' <- getLine
    xs <- getLine
    let n = (read n' :: Int)
    when (k>=0) $ do
      print $ compareAvg n xs
      repeat k-1

answer :: IO ()
answer = do
    k' <- getLine
    let k = (read k :: Int)
    repeat k


main = do
  line <- getLine
  unless (line == "q") $ do
    -- process line
    main
-}
