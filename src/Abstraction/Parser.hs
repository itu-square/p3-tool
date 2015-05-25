module Abstraction.Parser (pAbstraction, Abs, Lit) where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec

import Control.Applicative ((<$>), (<*), (*>), (<*>), pure)

import Abstraction.Ast

type Parser st a = Parsec String st a

lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser emptyDef

identifier :: Parser st String
identifier = Tok.identifier lexer

reserved :: String -> Parser st ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser st ()
reservedOp = Tok.reservedOp lexer

semi :: Parser st String
semi = Tok.semi lexer

comma :: Parser st String
comma = Tok.comma lexer

whiteSpace :: Parser st ()
whiteSpace = Tok.whiteSpace lexer

pAbstraction :: Parser st [Abs]
pAbstraction = whiteSpace *> sepBy1 pAbs semi <* eof

pAbs :: Parser st Abs
pAbs =   reserved "join" *> pure Join
    <|>  reserved "ignore" *> (Ignore <$> sepBy1 identifier comma)
    <|>  reserved "project" *> (Project <$> sepBy1 pLit comma)

pLit :: Parser st Lit
pLit =   reservedOp "!" *> (NegLit <$> identifier)
    <|>  PosLit <$> identifier
