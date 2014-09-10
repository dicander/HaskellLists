#!/usr/bin/env runhaskell
-- Haskell solution to the n-queens problem using list comprehension
-- Taken from http://www.testing-software.org/articles/functional/eight-queens-puzzle.html#haskell
boardSize = 8

queens :: Int -> [[Int]]
queens 0 = [[]]
queens n = [ x : y | y <- queens (n-1), x <- [1..boardSize], safe x y 1]
      where
         safe x [] n = True
         safe x (c:y) n = and [ x /= c , x /= c + n , x /= c - n , safe x y (n+1)]

--main  = print $ queens 8
main  = print ( queens 8 )
