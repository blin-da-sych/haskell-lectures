module Lectures.NnthLecture where

-- Eager (strict) languages - values are computed straightaway

a :: [Integer]
a = enumFrom 10

-- λ> :print x
-- a = (_t1::[Integer])
--       ^
--       thunk - задумка - ссылка на функцию

x :: Integer
xs :: [Integer]
x:xs = a

-- λ> x
-- 10
-- it :: Integer
-- λ> :print a
-- a = 10 : (_t2::[Integer])
--      ^
--      слабая головная форма

-- λ> take 4 a
-- [10,11,12,13]
-- it :: [Integer]
-- λ> :print a
-- a = 10 : 11 : 12 : 13 : (_t3::[Integer])

repeat :: a -> [a]
-- repeat x = x : repeat x    -- each time thunk is formed
repeat x = r
  where r = x : r             -- thunk is formed once and then structure is calculated

-- yeild в Python - это сопроцедура

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | n == 1 = Just 0
  | even n = (+ 1) <$> collatz (n `div` 2)
  | otherwise = (+ 1) <$> collatz (n * 3 + 1)

-- `for` imitation
for :: Applicative f => [a] -> (a -> f b) -> f [b]
for [] _          = pure []
for (x:xs) action = liftA2 (:) (action x) $ for xs action

-- преобразование обычной функции в контекст f называется поднятие
-- где f - это тип-функция
-- (:)        ::   b ->   [b] ->   [b]
-- liftA2 (:) :: f b -> f [b] -> f [b]
liftA2 :: Applicative f => (a1 -> a2 -> b) -> f a1 -> f a2 -> f b
liftA2 f x y = pure f <*> x <*> y
