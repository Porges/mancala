{-# LANGUAGE LambdaCase #-}
module Main where

import Oware

import Control.Monad (replicateM)

printableResult :: Score -> String
printableResult (p1, p2) = case compare p1 p2 of
    GT -> "You won! " ++ score
    LT -> "You lost. " ++ score
    _ -> "The game was a draw. " ++ score
    where score = show p1 ++ " - " ++ show p2

main :: IO ()
main = do
    result <- playGame humanPlayer randomPlayer
    putStrLn (printableResult result)
 