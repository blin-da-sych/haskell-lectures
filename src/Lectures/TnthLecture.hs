{-# LANGUAGE InstanceSigs #-}

module Lectures.TnthLecture (Reader(..)) where

-- import           Prelude hiding (Applicative, pure, (<*>))

-- Композиция не коммутативна

-- Чистота:

-- - Определённость/вычислимость/завершимость

--    ∃ y = f x
--    counterexample: 2/0

-- - Детерминированность

--    f x = f x
--    counterexample: random()
--    Существует функция pseudorandom

-- - Отсутсвие побочных эффектов

-- Функтор - это морфизм, который сохраняет форму

-- - Недетерминированность - неявная зависимость

--         тип результата
--               ↓
newtype Reader r a = R (r -> a)
--             ↑
--    тип неявной зависимости

runReader :: Reader r a -> r -> a
runReader (R f) = f       -- η-reduced

instance Show (r -> a) where
  show r = "r -> a"

instance Show (Reader r a) where
  show (R r) = "Reader " ++ "(" ++ show r ++ ")"

isTemperatureGood :: Reader Integer Bool
isTemperatureGood = R $ \t -> t > 0 && t < 30

-- $> runReader isTemperatureGood 40

-- class Functor f => Applicative f where
--   pure :: a -> f a

--   -- | "ap(ply)"
--   (<*>) :: f (a -> b) -> f a -> f b
--   (<*>) = liftA2 id

--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--   liftA2 f x y = pure f <*> x <*> y

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (R g) = R (f . g)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = R . const

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (R getf) (R getx) = R (\r -> getf r $ getx r)
--                                   ______   ______
--                                      ↓        ↓
--                                      f        x
--                                  f is applied to x
--                                          ↑
--                        пояснение для особо одарённых (для тебя)

-- instance Applicative [] where
--   pure :: a -> [a]
--   pure x = [x]

--   fs <*> xs = [f x|f <- fs, x <- xs]

isTemperatureBad :: Reader Integer Bool
isTemperatureBad = not <$> isTemperatureGood

isTemperatureGoodOrBad :: Reader Integer Bool
isTemperatureGoodOrBad =
  (||)
  <$> isTemperatureGood
  <*> isTemperatureBad

-- Функции с названиями hoist promote lift отражают
-- идею расширения контекста

-- Законы

--   Identity
--     pure id <*> v == v
--   Composition
--     pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
--   Homomorphism
--     pure f <*> pure x == pure (f x)
--   Interchange
--     u <*> pure y == pure ($ y) <*> u
