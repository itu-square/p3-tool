module TVL.Parser where

import Text.Parsec.Prim (modifyState)
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec hiding (Parser)
import TVL.Lexer
import TVL.Ast
import Control.Applicative ((<$>), (<*), (*>), (<*>))
import Control.Monad
import Control.Monad.Identity
import Data.Maybe


pModel :: Parser st Model
pModel = whiteSpace *> many pDecl <* eof

pDecl :: Parser st Decl
pDecl =   (DType <$> pTypeDecl)
      <|> (DConstant <$> pConstantDecl)
      <|> (DFeature <$> pFeatureDecl)


pTypeDecl :: Parser st TypeDecl
pTypeDecl = (TSimple <$> pSimpleTypeDecl)
        <|> (TRecord <$> (reserved "struct" *> identifier) <*> braces (many1 pRecordField))


pSimpleTypeDecl :: Parser st SimpleTypeDecl
pSimpleTypeDecl = do reserved "int"
                     name <- identifier
                     sexpr <- optionMaybe (reserved "in" *> pSetExpr <* semi)
                     return $ maybe (TIntScalar name) (TIntRange name) sexpr
                <|> do reserved "real"
                       name <- identifier
                       sexpr <- optionMaybe (reserved "in" *> pSetExpr <* semi)
                       return $ maybe (TRealScalar name) (TRealRange name) sexpr
                <|> do reserved "bool"
                       name <- identifier
                       semi
                       return $ TBoolScalar name
                <|> do reserved "enum"
                       name <- identifier
                       reserved "in"
                       sexpr <- pSetExpr
                       semi
                       return $ TEnum name sexpr

pRecordField :: Parser st RecordField
pRecordField = FSimple <$> pSimpleTypeDecl
            <|> FReference <$> identifier <*> identifier <* semi

pConstantDecl :: Parser st ConstantDecl
pConstantDecl = do reserved "const"
                   (do reserved "int"
                       name <- identifier
                       val <- natural
                       semi
                       return $ CstInt name val)
                   <|> (do reserved "real"
                           name <- identifier
                           val <- float
                           semi
                           return $ CstReal name val)
                   <|> (do reserved "bool"
                           name <- identifier
                           val <- (reserved "true" *> return True) <|> (reserved "false" *> return False)
                           return $ CstBool name val)

pShortId :: Parser st Name
pShortId =  (reserved "root" *> return "root")
        <|> (reserved "this" *> return "this")
        <|> (reserved "parent" *> return "parent")
        <|> identifier

pLongId :: Parser st Name
pLongId = sepBy1 (char '.') pShortId

pFeatureDecl = undefined
pSetExpr = undefined
