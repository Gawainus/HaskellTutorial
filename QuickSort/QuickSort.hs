module QuickSort where

import Test.QuickCheck


-- In this implementation the pivot is always the head of the list
quickSort:: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort smaller) ++ [x] ++ (quickSort larger)
  where
    smaller = [e | e <- xs, e <= x]
    larger  = [e | e <- xs, e > x]
