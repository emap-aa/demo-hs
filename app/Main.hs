
module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import Lib
import CommonWords

msg =
  " Usage: \n\
  \  count -c num file-name \n "

usage = putStrLn msg

parse ["-h"]    = usage >> exitSuccess
parse ("-c":ls) = countWords ls >> exitSuccess
parse _ = exitFailure

countWords as = do
  content <- readFile $ as!!1
  putStr $ commonWords (read $ head as) content

  
main :: IO ()
main = getArgs >>= parse
