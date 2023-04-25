{-# LANGUAGE InstanceSigs #-}

module Practice.Practice_10 where

import           Control.Applicative (Applicative (liftA2))

newtype Identity a = I a
  deriving (Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (I x) = I $ f x

instance Applicative Identity where
  pure :: a -> Identity a
  pure a = I a

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (<*>) (I f) (I x) = I $ f x

-- -- $> fmap (+1) $ I 10

-- -- $> (+1) <$> I 10

-- -- $> (+) <$> I 10 <*> I 10

-- -- $> I (+1) <*> I 10

-- $> (fmap . fmap . fmap) (+1) [Just (I 10), Just (I 20), Nothing]

