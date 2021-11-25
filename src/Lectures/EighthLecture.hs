module Lectures.EighthLecture where

import           Control.Monad (replicateM)
import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.Vector   (Vector)
import qualified Data.Vector   as Vector
import           GHC.Natural   (Natural)
import           System.Random (randomIO)

-- Random Map generator (List edition)

testMapL :: Int -> Int -> IO ()
testMapL height width = do
  m <- genMapL height width
  printMapL m
  print $ countClustersL m

type MapL = [[Bool]]

genMapL :: Int -> Int -> IO MapL
genMapL height width = replicateM height $ replicateM width randomIO

printMapL :: MapL -> IO ()
printMapL m =
  putStrLn $
    unlines
      [ concat
          [ bool "⬛" "⬜" cell
          | cell <- row
          ]
      | row <- m
      ]

countClustersL :: MapL -> Natural
countClustersL m0 = go (0, 0) m0 where
  height = length m0
  width = length $ head m0

  go (i, j) m =
    case next (i, j) of
      Nothing
        | m !! i !! j -> 1
        | otherwise -> 0
      Just ij'
        | m !! i !! j ->
          1 + go ij' (removeCluster (i, j) m)
        | otherwise -> go ij' m

  next (i, j)
    | j < width - 1  = Just (i    , j + 1)
    | i < height - 1 = Just (i + 1, 0    )
    | otherwise      = Nothing

  removeCluster (i, j) m =
    m
    & removeCell (i, j)
    & tryRemove (i - 1, j    )
    & tryRemove (i    , j - 1)
    & tryRemove (i + 1, j    )
    & tryRemove (i    , j + 1)

  tryRemove(i, j) m
    | i >= 0, i < height, j >= 0, j < width,
      m !! i !! j =
        removeCluster (i, j) m
    | otherwise = m

  removeCell (i, j) m =
    modifyAt i (replaceAt j False) m

  modifyAt i f xs =
    case splitAt i xs of
      (before, x:after) ->
        before ++ [f x] ++ after
      _ -> error "Index out of bound"

  replaceAt i x xs = modifyAt i (const x) xs

-- $> testMapL 5 5

-- Процедура - это параметрическое действие

-- type MapV = Vector (Vector Bool)

-- testMapV :: Int -> Int -> IO ()
-- testMapV height width = do
--   m <- genMapV height width
--   printMapV m
--   print $ countClustersV m

-- genMapV :: Int -> Int -> IO MapV
-- genMapV height width = do
--   asList <- genMapL height width
--   pure $
--     Vector.fromList
--       [Vector.fromList row | row <- asList]

-- printMapV v = printMapL [toList row | row <- toList v]
