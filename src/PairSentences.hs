module PairSentences where

type Sent = [Char]

so, sd :: [Sent]
so = ["A", "B", "C", "D", "F", "G", "H", "I"]
sd = ["X", "D", "F", "Y", "I"]

expected :: [([Sent], [Sent])]
expected = [(["A", "B", "C"], ["X"]),
            (["D"], ["D"]),
            (["F"], ["F"]),
            (["G", "H"], ["Y"]),
            (["I"], ["I"])]

splitWhen :: Eq a => [a] -> a -> ([a], [a]) --Left?
splitWhen [] s = ([], [])
splitWhen l@(la:ls) s
  | la == s = ([], l)
  | otherwise = (la:ll, ss)
  where (ll, ss) = splitWhen ls s

pairSentences :: Eq a => [a] -> [a] -> [ ([a], [a]) ]
pairSentences a [] = [(a, [])]
pairSentences [] a = [([], a)]
pairSentences lo@(loo:los) (ldd:lds)
  | loo == ldd = ([loo], [ldd]) : pairSentences los lds
  | otherwise = (fo, fd) : pairSentences go gd
  where ((fo, go), (fd, gd)) = splitPair ([], lo) ([ldd], lds)

-- fo go
-- fd gd

-- [] [A B C D F G H I]
-- [X] [D F Y I]

-- [A] [B C D F G H I]
-- [X D] [F Y I]

-- [A B] [C D F G H I]
-- [X D F] [Y I]

-- [A B C] [D F G H I]
-- [X D F Y] [I]

-- [A B C] [D F G H I]
-- [X] [D F Y I]

-- [A B C] [D F G H I]
-- [X D F Y I] []

splitPair :: Eq a => ([a], [a]) -> ([a], [a]) -> (([a], [a]), ([a], [a]))
splitPair (fo, go@(goo:gos)) (fd, gd)
  | null gd = ((fo, go), (fd, gd))
  | null rl = splitPair (fo ++ [goo], gos) (fd ++ [head gd], tail gd) --Não achou, próximo
  | otherwise = ((fo, go), (ll, rl ++ gd)) -- Achou, cria o par 
  where (ll, rl) = splitWhen fd goo

t0, tr, tm :: [Integer] -- 50 elementos, elimina 0.2, multiplica 0.2
t0 = [3,31,47,81,97,24,6,66,35,83,31,7,94,24,87,71,11,82,87,35,21,5,70,92,86,7,55,11,6,15,41,3,60,60,39,60,33,20,18,71,0,33,35,89,88,17,37,34,6,42]
tr = [3,31,47,81,97,6,66,35,83,31,7,94,24,87,71,11,82,87,35,92,86,7,55,11,6,15,41,3,60,60,39,60,33,20,18,71,0,35,89,88,17,37,34,6]
tm = [3,31,47,81,97,12,132,35,83,31,7,94,24,174,71,11,82,87,70,92,172,7,55,11,6,15,41,3,60,60,39,60,33,20,18,71,0,35,89,88,17,74,34,6]

u0, ur :: [Integer] -- 100 elementos, remove alguns blocos
u0 = [28,71,44,97,1,34,84,88,39,42,12,63,84,68,77,78,82,32,55,69,59,96,11,16,8,3,69,29,88,96,17,59,51,30,83,1,73,51,94,47,90,35,61,98,5,46,14,52,69,59,8,61,14,51,64,90,23,20,9,17,58,16,48,25,31,53,23,19,88,45,59,93,81,90,18,43,73,15,21,19,6,61,31,18,49,8,84,29,8,38,7,84,14,70,44,2,55,58,94,14]
ur = [28,71,44,84,68,77,78,82,32,55,69,59,96,11,16,883,1,73,51,94,47,90,35,61,98,6,14,52,69,59,8,61,16,48,25,31,53,23,19,88,45,59,93,81,90,18,43,73,15,21,19,6,61,31,18,49,8,84,29,8,38,7,84,14,70,44,2,55,58,94,14]
