{-# LANGUAGE FlexibleContexts #-}

module Lectures.SvnthLecture (mainRandomMap) where

import           Control.Monad (replicateM)
import           Data.Bool     (bool)
import           System.Random (randomIO)

-- | Fibonacci sequence (open recursion)
fibonacciOpenR :: Integer -> Integer
fibonacciOpenR 0 = 0
fibonacciOpenR 1 = 1
fibonacciOpenR n = fibonacciOpenR (n -2) + fibonacciOpenR (n -1)

-- | Fibonacci sequence (tail call optimization)
fibonacciTCO :: Integer -> Integer
fibonacciTCO n = go 0 1 n where
  go x _ 0 = x
  go _ y 1 = y
  go x y n = go y (x + y) (n - 1)

-- | Random Map generator
mainRandomMap :: IO ()
mainRandomMap = genMap 5 5 >>= printMap

type Map = [[Bool]]

genMap :: Int -> Int -> IO Map
genMap height width = replicateM height $ replicateM width randomIO

printMap :: Map -> IO ()
printMap = putStrLn . unlines . map (map $ bool '⬛' '⬜')

