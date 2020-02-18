module Main where

import Domains
import GHC.TypeLits


type Rect =
  Range 0 100 X0 :&&
  Range 0 100 X1 :&&
  X0 :% 2 :== C 2

main :: IO ()
main = print $ reifyConstraints @_ @Rect
