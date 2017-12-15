module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Koks žaidimo pavadinimas?"
  gameName <- getLine
  startFirst gameName

