{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--- Day 12: Rain Risk ---
module Aoc122020 where


{-
Action N means to move north by the given value.
Action S means to move south by the given value.
Action E means to move east by the given value.
Action W means to move west by the given value.
Action L means to turn left the given number of degrees.
Action R means to turn right the given number of degrees.
Action F means to move forward by the given value in the direction the ship is currently facing.

The ship starts by facing east. Only the L and R actions change the direction the ship is facing. 
(That is, if the ship is facing east and the next instruction is N10, the ship would move north 10 units, 
but would still move east if the following action were F.)
-}

--Transforma a linha de input "F10" em ('F',10) para melhor manipulação.
readline:: String -> (Char,Int)
readline x = (head x, read (tail x):: Int)

--Interpreta o caso 'F' para cada ângulo possível.
fCases :: Int -> Int -> (Int,Int) 
fCases x value
 | y == 0 = (value,0)
 | y == 90 = (0,value)
 | y == 180 = (-value,0)
 | y == 270 = (0,-value)
 where y = mod x 360

-- Faz o trabalho sujo de ler a string de input e interpretar ela apropriadamente.
navigate :: [String] -> Int -> (Int,Int) -> (Int,Int)
navigate [] rot (a,b) = (a,b)
navigate (x:xs) rot (a,b) 
 | dir == 'E' = navigate xs rot (a+value,b)
 | dir == 'N' = navigate xs rot (a,b+value)
 | dir == 'W' = navigate xs rot (a-value,b)
 | dir == 'S' = navigate xs rot (a,b-value)
 | dir == 'R' = navigate xs (rot+360-value) (a,b)
 | dir == 'L' = navigate xs (rot+value) (a,b)
 | dir == 'F' = navigate xs rot (a + fst (fCases rot value),b + snd (fCases rot value))
  where
      (dir,value) = readline x
      
manhattanvalue (a,b) = abs a + abs b

answer1 :: IO Int
answer1  = do
    input <- readFile "input1.txt"
    return (manhattanvalue $ navigate (words input) 0 (0,0))

{-
Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:

Action N means to move the waypoint north by the given value.
Action S means to move the waypoint south by the given value.
Action E means to move the waypoint east by the given value.
Action W means to move the waypoint west by the given value.
Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
Action F means to move forward to the waypoint a number of times equal to the given value.

The waypoint starts 10 units east and 1 unit north relative to the ship.
The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.
-}

-- Interpreta a rotação do waypoint para cada caso de ângulo.
fCases2 :: Int -> (Int,Int)-> (Int,Int) 
fCases2 x (w1,w2)
 | y == 0 = (w1,w2)
 | y == 90 = (-w2,w1)
 | y == 180 = (-w1,-w2)
 | y == 270 = (w2,-w1)
 where y = mod x 360

-- Quase a mesma coisa que o navigate1, com as alterações devidas para utilizar o waypoint
navigate2 :: [String] -> (Int,Int) -> (Int,Int) -> (Int,Int)
navigate2 [] _ (a,b) = (a,b)
navigate2 (x:xs) (w1,w2) (a,b) 
 | dir == 'E' = navigate2 xs (w1+value,w2) (a,b)
 | dir == 'N' = navigate2 xs (w1,w2+value) (a,b)
 | dir == 'W' = navigate2 xs (w1-value,w2) (a,b)
 | dir == 'S' = navigate2 xs (w1,w2-value) (a,b)
 | dir == 'R' = navigate2 xs (fCases2 (360-value) (w1,w2)) (a,b)
 | dir == 'L' = navigate2 xs (fCases2 value (w1,w2)) (a,b)
 | dir == 'F' = navigate2 xs (w1,w2) (a + value * w1,b + value * w2)
  where
      (dir,value) = readline x
      
answer2 ::IO Int
answer2 = do
    input <- readFile "input2.txt"
    return (manhattanvalue $ navigate2 (words input) (10,1) (0,0))