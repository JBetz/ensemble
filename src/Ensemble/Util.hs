module Ensemble.Util where

import Data.Char
import Prelude hiding (break)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ xs@[] = (xs, xs)
break p xs@(x : xs')
  | p x = ([], xs)
  | otherwise = let (ys, zs) = break p xs' in (x : ys, zs)

split :: Char -> String -> [String]
split c s = case rest of
  [] -> [chunk]
  _ : rest' -> chunk : split c rest'
  where
    (chunk, rest) = break (== c) s

uncapitalise :: String -> String
uncapitalise [] = []
uncapitalise (c : cs) = toLower c : cs
