module Main where

import Data.Complex
import Data.Complex as C
import Data.Matrix
import Data.Matrix as M

adjoint :: Num a => Matrix (Complex a) -> Matrix (Complex a)
adjoint = fmap conjugate . transpose

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"


main :: IO ()
main = do
  -- putStrLn "Hello! What's your name?"
  -- name <- getLine
  let c = 3 :+ 3
  let c2 = 1 :+ 1
  let m = M.fromLists [[c, c2]]
  -- let statement = helloPerson name
  putStrLn $ show $ m
  putStrLn $ show $ adjoint m