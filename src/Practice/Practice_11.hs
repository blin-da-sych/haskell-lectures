{-# LANGUAGE InstanceSigs #-}

module Practice.Practice_11 where

import           Lectures.Lecture_10 (Reader (..))
import           Lectures.Lecture_11 (State (..), Writer (..))

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (R f) k = R undefined

-- -- $> R (not)

a :: Writer Integer Integer
a = W (1, 2)

-- -- $> print a
