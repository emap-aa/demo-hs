A Simple Sudoku Solver
27th September, 2007
In Chapter 05
_________________________________________________________
0. Basic data types

> module Sudoku where
> import Safe.Exact
> import qualified Data.List as L

> type Matrix a = [Row a]
> type Row a    = [a]

> type Grid     = Matrix Digit
> type Digit    = Char

> digits  :: [Digit]
> digits  =  ['1'..'9']

> blank   :: Digit -> Bool
> blank   =  (== '0')

1. Specification

> solve1 :: Grid -> [Grid]
> solve1 = filter valid . expand . choices

> type Choices = [Digit]

> choices :: Grid -> Matrix Choices
> choices = map (map choice)
>  where choice d | blank d   = digits
>                 | otherwise = [d]

> expand :: Matrix Choices -> [Grid]
> expand = cp . map cp

produto cartesiano:

> cp :: [[a]] -> [[a]]
> cp []       = [[]]
> cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

> valid  :: Grid -> Bool
> valid g = all nodups (rows g) &&
>           all nodups (cols g) &&
>           all nodups (boxs g)

> nodups1 :: Eq a => [a] -> Bool
> nodups1 []     = True
> nodups1 (x:xs) = x `notElem` xs && nodups1 xs

> nodups2 :: (Ord a) => [a] -> Bool
> nodups2 [] = True
> nodups2 xs = and $ zipWith (/=) ys (tail ys)
>   where ys = L.sort xs

O tipo de `nodups` vai ser instanciado pela condição imposta pelo seu
uso em valid . As funcoes nodups1 e nodumps2 permanencem com tipos mais
gerais, no entanto.

> nodups = nodups2

> rows :: Matrix a -> [Row a]
> rows = id

> cols          :: Matrix a -> [Row a]
> cols [xs]     = [[x] | x <- xs]
> cols (xs:xss) = zipWith (:) xs (cols xss)

> boxs :: Matrix a -> [Row a]
> boxs = map ungroup . ungroup . map cols .
>        group . map group

> ungroup          = concat
> group []         = []
> group (x:y:z:xs) = [x,y,z]:group xs

2. Pruning

> prune :: Matrix Choices -> Matrix Choices
> prune =
>  pruneBy boxs . pruneBy cols . pruneBy rows
>  where pruneBy f = f . map pruneRow . f

> pruneRow :: Row Choices -> Row Choices
> pruneRow row = map (remove ones) row
>  where ones = [d | [d] <- row]

> remove :: Choices -> Choices -> Choices
> remove xs [d] = [d]
> remove xs ds  = filter (`notElem` xs) ds

3. Single-cell expansion

> expand1   :: Matrix Choices -> [Matrix Choices]
> expand1 rows =
>  [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
>  where
>  (rows1,row:rows2) = break (any smallest) rows
>  (row1,cs:row2)    = break smallest row
>  smallest cs       = length cs == n
>  n                 = minimum (counts rows)

> counts = filter (/=1) . map length . concat

4. Final algorithm

> solve2 :: Grid -> [Grid]
> solve2 =  search . choices

> search :: Matrix Choices -> [Grid]
> search cm
>  |not (safe pm)  = []
>  |complete pm    = [map (map head) pm]
>  |otherwise      = (concat . map search . expand1) pm
>  where pm = prune cm

> complete :: Matrix Choices -> Bool
> complete = all (all single)

> single [_] = True
> single _   = False

> safe :: Matrix Choices -> Bool
> safe cm = all ok (rows cm) &&
>           all ok (cols cm) &&
>           all ok (boxs cm)

> ok row = nodups [d | [d] <- row]

> easy1 = ["690400817",
>          "000008230",
>          "138700000",
>          "900007420",
>          "053004096",
>          "200009001",
>          "360800002",
>          "780230100",
>          "520901060"]


Exercicio A


> ansA1 = map (map (+1)) [[1,2,3],[2,3,4]]
> ansA2a = sum $ concat [[1,2,3],[2,3,4]]
> ansA2b = sum $ map sum [[1,2,3],[2,3,4]]
> ansA3 = zipWith (zipWith (+)) [[1,2,3],[2,3,4]] [[1,2,3],[2,3,4]]

> escalar :: Num a => [a] -> [a] -> a
> escalar xs ys = sum (zipWithExact (*) xs ys)

> multmat :: Num a => [[a]] -> [[a]] -> [[a]]
> multmat [] _ = []
> multmat (xs:xss) yss = [escalar xs coly | coly <- cols yss] : multmat xss yss


Exercicio B

Dimensao de `Matrix [[],[]]`?  2x0  
Dimensao de `Matrix []`? 0xN for all N ~> transpose deveria ser Nx0

> transpose, transpose1, transpose2 :: [[a]] -> [[a]]

> transpose [xs] = [[x] | x <- xs]
> transpose (xs:xss) = zipWith (:) xs (transpose xss)

> transpose1 [] = repeat []
> transpose1 (xs:xss) = zipWith (:) xs (transpose1 xss) 

> transpose2 ([]:xss) = []
> transpose2 xss = map head xss : transpose2 (map tail xss)


Exercicio C (Lean dá para fazer!! Projeto)

(not . all (not . (> 5))) [1..10]
any (> 5) [1..10]

