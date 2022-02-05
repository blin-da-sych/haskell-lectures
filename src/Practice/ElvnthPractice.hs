{-# LANGUAGE InstanceSigs #-}

module Practice.ElvnthPractice where

import           Lectures.ElvnthLecture (State (..), Writer (..))
import           Lectures.TnthLecture   (Reader (..))

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (R f) k = R _

-- -- $> R (not)

a :: Writer Integer Integer
a = W (1, 2)

-- -- $> print a
