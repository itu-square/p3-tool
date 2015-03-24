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
import Data.List

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
pLongId = intercalate "." <$> pShortId `sepBy1` (char '.')

pFeatureDecl :: Parser st FeatureDecl
pFeatureDecl = do
  (root, name) <- ((,) <$> (reserved "root" *> return True) <*> identifier)
                  <|> ((,) False <$> pLongId)
  (FtGroup root name <$> (reserved "group" *> pCardinality) <*> braces (commaSep1 pFtHiDecl))
    <|> (FtFeature root name <$> braces (many1 pFtBodyItem))
  <?> "feature declaration"

pFtBodyItem :: Parser st FtBodyItem
pFtBodyItem = pData
            <|> FtiConstraint <$> pConstraintDecl
            <|> FtiAttribute <$> pAttributeDecl
            <|> pFtGroup


pFtGroup :: Parser st FtBodyItem
pFtGroup = FtiGroup <$> (reserved "group" *> pCardinality) <*> braces (commaSep1 pFtHiDecl)

pFtHiDecl :: Parser st FtHiDecl
pFtHiDecl = do presence <- pPresence
               (FthItem presence <$> try (pLongId <* notFollowedBy ((symbol "{" *> return ())  <|> reserved "group"))) 
               -- In order to avoid feature declaration
                  <|> (ensureNotShared presence *> FthFeature presence <$> pFeatureDecl)
  where ensureNotShared PShared = fail "Features can not be shared"
        ensureNotShared _ = return ()

pPresence :: Parser st Presence
pPresence = option PNone ((reserved "opt" *> return POptional) <|> (reserved "shared" *> return PShared))

pCardinality :: Parser st Cardinality
pCardinality =  (reserved "oneOf" *> return CdOneOf)
            <|> (reserved "someOf" *> return CdSomeOf)
            <|> (reserved "allOf" *> return CdAllOf)
            <|> brackets (CdRange <$> (natural <* symbol "..") <*> ((Just <$> natural) <|> (symbol "*" *> return Nothing)))

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

pExpr :: Parser st Expr
pExpr = do expr <- pEExpr
           checkExpr expr
           (reserved "in" *> (InSet expr <$> pSetExpr))
             <|> return expr
   where checkExpr (BinOp ":" _ _) = unexpected "(:) is not a valid top-level operator" -- Simple non-exhaustful check
         checkExpr _ = return ()

pEExpr :: Parser st Expr
pEExpr = buildExpressionParser eTable pEExpr'

pEExpr' :: Parser st Expr
pEExpr' =   parens pExpr
        <|> (reserved "true" *> return (EBool True))
        <|> (reserved "false" *> return (EBool False))
        <|> (do name <- pLongId
                (reserved "excludes" *> (Exclusion name <$> pLongId))
                  <|> (reserved "requires" *> (Dependency name <$> pLongId))
                  <|> (return . Ref $ name))
        <|> (reserved "abs" *> (EAbs <$> parens pExpr))
        <|> choice (map pBuiltInFun ["and", "or", "xor", "sum", "mul", "min", "max", "count", "average"])
        <|> (EInt <$> integer)
        <|> (EReal <$> float)

pBuiltInFun :: String -> Parser st Expr
pBuiltInFun name = do reserved name
                      args <- pBuiltInFunArg
                      return $ BuiltinFun name args
  where pBuiltInFunArg =  (Right <$> pChildrenId)
                      <|> (Left <$> commaSep1 pExpr)

pChildrenId :: Parser st ChildrenId
pChildrenId = ChdId <$> pChildren <*> (dot *> pLongId)
  where pChildren =  (reserved "selectedchildren" *> return ChdSelected)
                 <|> (reserved "children" *> return ChdAll)
                         
 
                      


eTable :: OperatorTable String st Identity Expr
eTable =   [ [unary "-", unary "!"]
           , [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft]
           , [binary "+" AssocLeft, binary "-" AssocLeft]
           , [binary "<" AssocLeft, binary "<=" AssocLeft, binary ">" AssocLeft, binary ">=" AssocLeft]
           , [binary "==" AssocLeft, binary "!=" AssocLeft]
           , [binary "&&" AssocLeft]
           , [binary "||" AssocLeft]
           , [binary "->" AssocRight, binary "<-" AssocRight, binary "<->" AssocRight]
           , [ternary "?"]
           , [binary ":" AssocNone]
           ]
   where binary op assoc                    = Infix (reservedOp op *> return (BinOp op)) assoc
         unary  op                          = Prefix (reservedOp op *> return (UnOp op))
         ternary op                         = Infix (reservedOp op *> return (mkTernary op)) AssocRight
         mkTernary op e1 (BinOp ":" e2 e3)  = Ternary e1 e2 e3
 
pSetExpr :: Parser st SetExpr
pSetExpr =  braces (SExprs <$> commaSep1 pExpr)
        <|> brackets (SRange <$> pSetExprBound <*> (symbol ".." *> pSetExprBound))

pSetExprBound :: Parser st SetExprBound
pSetExprBound =  (SBReal <$> float)
             <|> (SBInt <$> integer)
             <|> (symbol "*" *> return SBUnbounded)

pConstraintDecl :: Parser st ConstraintDecl
pConstraintDecl =  (CtIfIn <$> (reserved "ifin:" *> pExpr <* semi))
               <|> (CtIfOut <$> (reserved "ifout:" *> pExpr <* semi))
               <|> (CtExpr <$> (pExpr <* semi))

pData :: Parser st FtBodyItem
pData = FtiData <$> (reserved "data" *>
                       braces (many1 ((,) <$> stringLiteral <*> (stringLiteral <* semi))))

