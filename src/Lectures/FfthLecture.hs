module Lectures.FfthLecture where

import           System.Environment       (getArgs)
import           System.Environment.Blank (getProgName)

import           Lectures.ScndLecture     (solveSquare)

-- | Simple terminal command parser
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case args of
    [as, bs, cs] ->
      let
        a = read as
        b = read bs
        c = read cs
      in print $ solveSquare a b c
    _ -> print $ "Number Number Number must be in " ++ prog ++ " as arguments"
