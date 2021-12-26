{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Lectures.ElvnthLecture where

newtype Writer w a = W (a, w)
  deriving Show

addWithLog :: Integer -> Integer -> Writer [String] Integer
addWithLog x y =
  W ( x + y
    , ["x = " ++ show x, "y = " ++ show y ]
    )

-- -- $> addWithLog 1 2

instance Functor (Writer w) where
  fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f (W (a, w)) = W (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure :: a -> Writer w a
  pure a = W (a, mempty)

  (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  (<*>) (W (f, w1)) (W (x, w2)) = W (f x, w1 <> w2)

-- -- $> (+1) <$> addWithLog 1 2

makeWithLog :: Show a => String -> a -> Writer [String] a
makeWithLog name value = W (value, [name <> " = " <> show value])

-- -- $> x = makeWithLog "x" 300

-- -- $> y = makeWithLog "y" 500

-- -- $> (+) <$> x <*> y

-- -- $> right <$> right (Zipper [1..10] 1 [10..15])

-- MONOID

-- Законы

--   Right identity
--     x <> mempty = x
--   Left identity
--     mempty <> x = x
--   Associativity
--     x <> (y <> z) = (x <> y) <> z
--     -- Semigroup law

-- Аппликатив позволят объединить несколько независимых компутэйшенов в один
-- Он не позволяет объединять зависимые компутэйшены
-- Это позволяет сделать монада

-- -- getConfig
-- -- |
-- -- | config
-- -- ↓
-- -- getConfigParam config "source"
-- -- |
-- -- | source
-- -- ↓
-- -- getSomeData source
-- -- |
-- -- | someData
-- -- ↓
-- -- logic

-- class Applicative m => Monad m where
--   -- | "bind"
--   (>>=) :: m a -> (a -> m b) -> m b

-- -- | Kleisli composition
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- (>=>) m n = \a -> m a >>= n

-- `m c` в выражении ... -> (a -> m c) - это параметризованный тип

-- getConfig >>=
--   \config -> getConfigParam config "source" >>=
--     \source -> getSomeData source >>=
--       \someData -> logic

newtype State s a = S (s -> (a, s))
--                     ↓
--           общее состояние (изменчивое окружение)
-- Само действие - action - принимает предыдущее состояние на входе и
-- и формирует новое состояние на выходе

-- State s :: Type -> Type        - это морфизм из а в State s a
--             a   -> State s a

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (S act) =
    S $ \s0 ->
      let
        (a, s1) = act s0
        b = f a
      in (b, s1)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = S (a,)         -- tuple section (instead of `S $ \s0 -> (a, s0)`)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (S sf) (S sx) =
    S $ \s0 ->
      let
        (f, s1) = sf s0
        (x, s2) = sx s1
      in (f x, s2)

instance Monad (State s) where
  -- | `k` in `(>>=) (S act1) k = ...`  - continuation
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (S act1) k =
    S $ \s0 ->
      let
        (a, s1) = act1 s0
        S act2 = k a
      in act2 s1

runState :: State s a -> s -> (a, s)
runState (S act) = act  -- η-reduced function

getUnique :: State Integer Integer
getUnique = S $ \n -> (n, n + 1)

-- -- $> runState getUnique 0

-- {- $>
--   runState
--     ( getUnique >>= \x ->
--       getUnique >>= \y ->
--       getUnique >>= \z ->
--       pure [x, y, z]
--     )
--     0
-- <$ -}
