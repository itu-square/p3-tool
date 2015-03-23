module TVL.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

type Parser st a = Parsec String st a

tvlDef :: Tok.LanguageDef st
tvlDef = emptyDef {
  Tok.reservedNames  = [ "int", "real", "enum", "bool", "struct"
                       , "const", "true", "false"
                       , "this", "parent", "root"
                       , "group", "oneOf", "someOf", "allOf", "opt", "shared"
                       , "ifin:", "ifout:", "is"
                       , "and", "xor", "or"
                       , "abs", "sum", "mul", "min", "max", "count", "avg"
                       , "data", "selectedchildren", "children"
                       ]
  , Tok.caseSensitive = False
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> oneOf "_"
}

lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser tvlDef

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

integer :: Parser st Integer
integer = Tok.integer lexer

float :: Parser st Double
float = Tok.float lexer

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
