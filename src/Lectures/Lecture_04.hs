{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Lectures.Lecture_04 where

data Quality = Ugly | Bad | Normal | Good
  deriving (Eq, Ord, Show)

type Age = Int

data TCharacter = DCharacter -- TCharacter - type; DCharacter - data constructor/deconstructor
  { quality :: Quality,
    age     :: Age
  }
  deriving (Show)

-- where True False LT GT EQ are pointers

-- Multiple declaration warning (because of True type):
-- data Bool = True | False
-- data Cool = True | OK

-- GHC.Types.C# GHC.Prim.Char#      - Primitives
-- Int                              - Fixed bit
-- Integer                          - Arbitrary precision

-- `Num => ...`                     - Числовой литерал принадлежит любому типу, являющемуся числом
-- class                            - Это свойство типов

-- Данные определяет только тип, а класс определяет поведение

deconstructCharacterE :: TCharacter -> (Quality, Age)
deconstructCharacterE DCharacter {quality = q, age = a} = (q, a)

-- :set -XNamedFieldPuns - для неявной именованной деконструкции
deconstructCharacterI :: TCharacter -> (Quality, Age)
deconstructCharacterI DCharacter {quality, age} = (quality, age)

-- :set -XRecordWildCards - для неявной неименованной деконструкции (~spread operator)
deconstructCharacterSmartI :: TCharacter -> (Quality, Age)
deconstructCharacterSmartI DCharacter {..} = (quality, age)

-- Σ-type - тип-сумма (or tagged union / variant record / coproduct)
data Either a b = Left a | Right b
-- Maybe a ~ Either () a
