module Abstraction.Ast where

data Abs = Join
         | Ignore [String]
         | Project [Lit]
         deriving (Show)

data Lit = PosLit String | NegLit String
         deriving (Show)
