module Abstraction.Ast where

data Abs = Join
         | Ignore [String]
         | Project [Lit]
         deriving (Show, Eq, Ord)

data Lit = PosLit {feature :: String} | NegLit {feature :: String}
         deriving (Show, Eq, Ord)
