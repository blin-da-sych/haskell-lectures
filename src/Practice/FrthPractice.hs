module Practice.FrthPractice (Solution, solveSquare) where

-- | data type to define square roots representation
data Solution a = Endless | NoRoots | RootList a
  deriving (Show)

-- | Solve equation of kind @ax² + bx + c@
solveSquare :: (Ord a, Floating a) => a -> a -> a -> Solution [a]
solveSquare a b c
  | a == 0 && b == 0  = Endless
  | a == 0            = RootList [- c / b]
  | d < 0             = NoRoots
  | otherwise         = RootList [ (- b ± sqrt d) / (2 * a)
                                 | (±) <- [(-), (+)]
                                 ]
  where
    d = b * b - 4 * a * c

-- $> solveSquare 1 2 1
