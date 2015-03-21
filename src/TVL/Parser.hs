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

pFeatureDecl :: Parser st FeatureDecl
pFeatureDecl = do
  (root, name) <- ((,) <$> (reserved "root" *> return True) <*> identifier)
                  <|> ((,) False <$> pLongId)
  (FtFeature root name <$> braces (many1 pFtBodyItem))
    <|> (FtGroup root name <$> (reserved "group" *> pCardinality) <*> braces (commaSep1 pFtHiDecl))

pFtBodyItem :: Parser st FtBodyItem
pFtBodyItem = pData
            <|> FtiConstraint <$> pConstraintDecl
            <|> FtiAttribute <$> pAttributeDecl
            <|> pFtGroup


pFtGroup :: Parser st FtBodyItem
pFtGroup = FtiGroup <$> (reserved "group" *> pCardinality) <*> braces (commaSep1 pFtHiDecl)

pFtHiDecl :: Parser st FtHiDecl
pFtHiDecl = do presence <- pPresence
               (FthItem presence <$> try (pLongId <* notFollowedBy (symbol "{"))) -- In order to avoid feature declaration
                  <|> (ensureNotShared presence *> FthFeature presence <$> pFeatureDecl)
  where ensureNotShared PShared = fail "Features can not be shared"
        ensureNotShared _ = return ()
  
pPresence :: Parser st Presence
pPresence = option PNone ((reserved "opt" *> return POptional) <|> (reserved "shared" *> return PShared))

pCardinality :: Parser st Cardinality
pCardinality =  (reserved "oneof" *> return CdOneOf)
            <|> (reserved "someof" *> return CdSomeOf)
            <|> (reserved "allof" *> return CdAllOf)
            <|> brackets (CdRange <$> natural <*> ((Just <$> natural) <|> (symbol "*" *> return Nothing)))

pAttributeDecl :: Parser st AttributeDecl
pAttributeDecl =  (AttrInt <$> (reserved "int" *> identifier) <*> (optionMaybe pAttributeBody <* semi))
              <|> (AttrReal <$> (reserved "real" *> identifier) <*> (optionMaybe pAttributeBody <* semi))                 
              <|> (AttrBool <$> (reserved "bool" *> identifier) <*> (optionMaybe pAttributeBody <* semi))                 
              <|> (AttrEnum <$> (reserved "enum" *> identifier) <*> (optionMaybe pAttributeBody <* semi))
              <|> do tname <- try (identifier <* lookAhead identifier) -- In order to avoid constraint expressions
                     name <- identifier
                     (AttrComplex tname name <$> braces (many1 ((,) <$> identifier <*> (pAttributeBody <* semi))))

pAttributeBody :: Parser st AttributeBody
pAttributeBody =  (reserved "is" *> (AbIs <$> pExpr))
              <|> (reserved "in" *> (AbIn <$> pSetExpr <*> optionMaybe (comma *> pAttributeConditional)))
              <|> (comma *> (AbCond <$> pAttributeConditional))

pAttributeConditional :: Parser st AttributeConditional
pAttributeConditional = do valin <- optionMaybe (reserved "ifin:" *> pAttributeValue)
                           maybe (return ()) (const $ symbol "," *> return ()) valin
                           valout <- optionMaybe (reserved "ifout:" *> pAttributeValue)
                           case (valin, valout) of
                            (Nothing, Nothing) -> fail "expected one of ifin: or ifout:"
                            _ -> return $ AcIfInOut valin valout

pAttributeValue :: Parser st AttributeValue
pAttributeValue =  (reserved "is" *> (AvIs <$> pExpr))
               <|> (reserved "in" *> (AvIn <$> pSetExpr))

pExpr = undefined
  
pSetExpr = undefined

pConstraintDecl :: Parser st ConstraintDecl
pConstraintDecl =  (CtIfIn <$> (reserved "ifin:" *> pExpr <* semi))
               <|> (CtIfOut <$> (reserved "ifout:" *> pExpr <* semi))
               <|> (CtExpr <$> (pExpr <* semi))

pData :: Parser st FtBodyItem
pData = FtiData <$> (reserved "data" *>
                       braces (many1 ((,) <$> stringLiteral <*> (stringLiteral <* semi))))

