module FPromela.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

type Parser st a = Parsec String st a

promelaDef :: Tok.LanguageDef st
promelaDef = emptyDef {
  Tok.commentStart   = "/*",
  Tok.commentEnd     = "*/",
  Tok.commentLine    = "//",
  Tok.nestedComments = False,
  Tok.reservedNames  = ["proctype", "init", "never",
                        "trace", "mtype",
                        "bit", "bool", "byte", "short", "int", "chan",
                        "active", "priority", "provided", "hidden", "show",
                        "unless", "xr", "xs",
                        "eval", "if", "fi", "gd", "dg", "do", "od",
                        "for", "atomic", "d_step",
                        "select", "else", "break",
                        "goto", "print", "assert",
                        "c_code", "c_expr", "c_decl", "c_track", "c_state",
                        "in", "len", "timeout",
                        "np_", "enabled", "pc_value", "run",
                        "full", "empty", "nfull", "nempty",
                        "true", "false", "skip"]
}

lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser promelaDef

identifier :: Parser st String
identifier = Tok.identifier lexer

reserved :: String -> Parser st ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser st ()
reservedOp = Tok.reservedOp lexer

charLiteral :: Parser st Char
charLiteral = Tok.charLiteral lexer

stringLiteral :: Parser st String
stringLiteral = Tok.stringLiteral lexer

natural :: Parser st Integer
natural = Tok.natural lexer

symbol :: String -> Parser st String
symbol = Tok.symbol lexer

lexeme :: Parser st a -> Parser st a
lexeme = Tok.lexeme lexer

whiteSpace :: Parser st ()
whiteSpace = Tok.whiteSpace lexer

parens :: Parser st a -> Parser st a
parens = Tok.parens lexer

braces :: Parser st a -> Parser st a
braces = Tok.braces lexer

angles :: Parser st a -> Parser st a
angles = Tok.angles lexer

brackets :: Parser st a -> Parser st a
brackets = Tok.brackets lexer

semi :: Parser st String
semi = Tok.semi lexer

comma :: Parser st String
comma = Tok.comma lexer

colon :: Parser st String
colon = Tok.colon lexer

dot :: Parser st String
dot = Tok.dot lexer

semiSep :: Parser st a -> Parser st [a]
semiSep = Tok.semiSep lexer

semiSep1 :: Parser st a -> Parser st [a]
semiSep1 = Tok.semiSep1 lexer

commaSep :: Parser st a -> Parser st [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser st a -> Parser st [a]
commaSep1 = Tok.commaSep1 lexer
