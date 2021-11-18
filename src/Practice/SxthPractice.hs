module Practice.SxthPractice (Expression(Variable, Application, Abstraction)) where

-- Lambda Calculus imitation

type VariableName = String

data Expression
  = Variable String
  | Application Expression Expression
  | Abstraction VariableName Expression

instance Show Expression where
  show (Variable a)      = a
  show (Application a b) = show a ++ " " ++ show b
  show (Abstraction a b) = "\\" ++ a ++ " -> " ++ show b
