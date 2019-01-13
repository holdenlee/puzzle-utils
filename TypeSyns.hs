{-# OPTIONS

 -XPatternSynonyms
#-}

module TypeSyns where

import Data.Array
import Data.Either
import Data.Graph.Inductive
import Data.Map.Strict
import Data.MultiMap
import Data.Set

type A = Array
type B = Bool
type C = Char
type D = Double
type E = Either
type F = Float
type G = Gr
type I = Int
type In = Integer
type M = Map
type MM = MultiMap
type My = Maybe
type S = String
type Se = Set

pattern J x = Just x
pattern N = Nothing

pattern L x = Left x
pattern R x = Right x
