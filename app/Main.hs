module Main where

import Data.Complex
import Data.Complex as C
import Data.Matrix
import Data.Matrix as M


adjoint :: Num a => Matrix (Complex a) -> Matrix (Complex a)
adjoint = fmap conjugate . transpose

dot :: RealFloat a => Matrix (Complex a) -> Matrix (Complex a) -> Complex a
dot x y = M.trace $ M.multStd (adjoint x) y

norm :: RealFloat a => Matrix (Complex a) -> a
norm x = sqrt $ C.realPart $ dot x x

isHermitian :: (Eq a, Num a) => Matrix (Complex a) -> Bool
isHermitian m = adjoint m == m

isIdentity :: (Eq a, Num a) => Matrix a -> Bool
isIdentity m = M.identity (M.ncols m) == m

isUnitary :: RealFloat a => Matrix (Complex a) -> Bool
isUnitary m = isIdentity (m * adjoint m)

mult :: (Functor f, Num b) => f b -> b -> f b
mult m i = fmap (* i) m

tensor :: Num a => Matrix a -> Matrix a -> Matrix a
tensor m1 m2 = M.flatten $ fmap (mult m2) m1

main :: IO ()
main = do
  -- putStrLn "Hello! What's your name?"
  -- name <- getLine
  let c = 0 :+ 1
  let c2 = 1 :+ 1
  let m = M.fromLists [[0, 1 :+ (2)], [1 :+ 2, 3]]
  let m2 = M.fromLists [[1], [0]]
  let z = 3 * m
  putStrLn $ show m
  putStrLn $ show $ tensor m m2