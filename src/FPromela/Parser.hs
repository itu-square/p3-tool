module FPromela.Parser where

import Text.Parsec.Prim (modifyState)
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec hiding (Parser)
import FPromela.Lexer
import FPromela.Ast
import Control.Applicative ((<$>), (<*), (*>), (<*>))
import Control.Monad
import Control.Monad.Identity
import Language.C.Syntax.AST (CExtDecl, CStat, CExpr)

data ParserState = ParserState { declared_types :: [String] }

emptyParserState :: ParserState
emptyParserState = ParserState []

pName :: Parser ParserState String
pName = try $ do
  st <- getState
  name <- identifier
  when (name `elem` declared_types st) $ unexpected ("Type name as pName: " ++ name)
  return name

pUname :: Parser ParserState Type
pUname = do
  st <- getState
  name <- identifier
  when (not (name `elem` declared_types st)) $ unexpected ("Type: " ++ name)
  return (TUName name)

pSpec :: Parser ParserState Spec
pSpec = whiteSpace *> many1 pModule
      <?> "specification"

pModule :: Parser ParserState Module
pModule =   pProctype
        <|> pInit
        <|> pNever
        <|> pTrace
        <|> pUtype
        <|> pMtype
        <|> (MDecls <$> pDecls)
        <?> "module"

pProctype :: Parser ParserState Module
pProctype = do
  active <- optionMaybe pActive
  reserved "proctype"
  name <- pName
  decls <- parens (option [] pDecls)
  priority <- optionMaybe pPriority
  enabler <- optionMaybe pEnabler
  seq <- braces pSequence
  return $ MProcType active name decls priority enabler seq

pInit :: Parser ParserState Module
pInit = do
  reserved "init"
  priority <- optionMaybe pPriority
  seq      <- braces pSequence
  return $ MInit priority seq

pNever :: Parser ParserState Module
pNever = do
  reserved "never"
  seq <- braces pSequence
  return $ MNever seq

pTrace :: Parser ParserState Module
pTrace = do
  reserved "trace"
  seq <- braces pSequence
  return $ MTrace seq

pUtype :: Parser ParserState Module
pUtype = do
  reserved "typedef"
  name <- pName
  decls <- braces pDecls
  modifyState (\st -> st { declared_types = name : declared_types st })
  return $ MUType name decls

pMtype :: Parser ParserState Module
pMtype = do
  -- In order to avoid failing on mtype variables
  try (reserved "mtype" *> notFollowedBy pName)
  optional (symbol "=")
  names <- braces (commaSep1 pName)
  return $ MMType names

pDecls :: Parser ParserState [Decl]
pDecls = semiSep pDecl

pDecl :: Parser ParserState Decl
pDecl = do
  visible <- optionMaybe pVisible
  typ <- pType
  ivars <- commaSep1 pIvar
  return $ Decl visible typ ivars

pType :: Parser ParserState Type
pType =   (reserved "bit" *> return TBit)
      <|> (reserved "bool" *> return TBool)
      <|> (reserved "byte" *> return TByte)
      <|> (reserved "short" *> return TShort)
      <|> (reserved "int" *> return TInt)
      -- In order to avoid global mtype
      <|> (try (reserved "mtype" *> notFollowedBy (symbol "{" <|> symbol "=")) *> return TMType)
      <|> (reserved "chan" *> return TChan)
      <|> pUname
      <?> "type"

pActive :: Parser ParserState Active
pActive = do
  reserved "active"
  const <- optionMaybe (brackets pConst)
  return $ Active const

pPriority :: Parser ParserState Priority
pPriority = do
  reserved "priority"
  const <- pConst
  return $ Priority const

pEnabler :: Parser ParserState Enabler
pEnabler = do
  reserved "provided"
  e <- parens pExpr
  return (Enabler e)

pVisible :: Parser ParserState Bool
pVisible = (reserved "show" *> return True)
          <|> (reserved "hidden" *> return False)

pSequence :: Parser ParserState Sequence
pSequence = semiSep1 pStep

pStep :: Parser ParserState Step
pStep =  (reserved "xr" *> commaSep1 pVarRef >>= return . SXs)
     <|> (reserved "xs" *> commaSep1 pVarRef >>= return . SXr)
     <|> (SDecl <$> pDecls)
     <|> do stmt <- pStmt
            ustmt <- optionMaybe (reserved "unless" *> pStmt)
            return $ SStmt stmt ustmt


pIvar :: Parser ParserState IVar
pIvar = do
  name <- pName
  const <- optionMaybe (brackets pConst)
  val <- optionMaybe (symbol "=" *> ((pChInit >>= return . Right) <|> (pAnyExpr >>= return . Left)))
  return $ IVar name const val

pChInit :: Parser ParserState ChInit
pChInit = do
  const <- brackets pConst
  reserved "of"
  types <- braces (commaSep1 pType)
  return $ ChInit const types

pVarRef :: Parser ParserState VarRef
pVarRef = do
  name <- pName
  iex <- optionMaybe (brackets pAnyExpr)
  acc <- optionMaybe (dot *> pVarRef)
  return $ VarRef name iex acc

pSend :: Parser ParserState Send
pSend = do var <- try (pVarRef <* symbol "!")
           sorted <- option False (symbol "!" *> return True)
           args <- pSendArgs
           return $ Send var sorted args

pReceive :: Parser ParserState Receive
pReceive = do (var, random) <- try $ do
                var <- pVarRef
                symbol "?"
                random <- option False (symbol "?" *> return True)
                notFollowedBy (symbol "[") -- In order to avoid poll
                return (var, random)
              (angles pReceiveArgs >>= return . PollReceive var random)
                <|> (pReceiveArgs >>= return . Receive var random)


pPoll :: Parser ParserState Poll
pPoll = do (var, random) <- try $ do
                var <- pVarRef
                symbol "?"
                random <- option False (symbol "?" *> return True)
                symbol "["
                return (var, random)
           args <- brackets pReceiveArgs
           return $ Poll var random args

pSendArgs :: Parser ParserState SendArgs
pSendArgs = do
  expr1 <- pAnyExpr
  (parens (commaSep1 pAnyExpr) >>= return . SnCall expr1)
    <|> (option [] (comma >> commaSep1 pAnyExpr) >>= return . SnArgs . (expr1 :))

pReceiveArgs :: Parser ParserState ReceiveArgs
pReceiveArgs = do
  arg1 <- pReceiveArg
  (parens pReceiveArgs >>= return . RcCall arg1)
    <|> (option [] (comma >> commaSep1 pReceiveArg) >>= return . RcArgs . (arg1 :))

pReceiveArg :: Parser ParserState ReceiveArg
pReceiveArg =   (pVarRef >>= return . RcVarRef)
           <|>  (reserved "eval" >> parens pVarRef >>= return . RcEval)
           <|>  do neg <- option False (symbol "-" *> return True)
                   const <- pConst
                   return $ RcConst neg const

pAssign :: Parser ParserState Assign
pAssign =   (try (pVarRef <* symbol "+" <* symbol "+") >>= return . AssignIncr)
        <|> (try (pVarRef <* symbol "-" <* symbol "-") >>= return . AssignDecr)
        <|> do var <- (pVarRef <* symbol "=" <* notFollowedBy (symbol "=")) -- to avoid equality test
               val <- pAnyExpr
               return $ AssignExpr var val

pStmt :: Parser ParserState Stmt
pStmt =   (reserved "if" *> pOptions <* reserved "fi" >>= return . StIf)
      <|> (reserved "do" *> pOptions <* reserved "od" >>= return . StDo)
      <|> do reserved "for"
             range <- parens pRange
             seq <- braces pSequence
             return $ StFor range seq
      <|> (reserved "atomic" *> braces pSequence >>= return . StAtomic)
      <|> (reserved "d_step" *> braces pSequence >>= return . StDStep)
      <|> (reserved "select" *> parens pRange >>= return . StSelect)
      <|> (braces pSequence >>= return . StBlock)
      <|> (pSend >>= return . StSend)
      <|> (pReceive >>= return . StReceive)
      <|> (pAssign >>= return . StAssign)
      <|> (reserved "else" *> return StElse)
      <|> (reserved "break" *> return StBreak)
      <|> (reserved "goto" *> pName >>= return . StGoto)
      <|> do label <- (try pName <* symbol ":")
             stmt <- pStmt
             return $ StLabelled label stmt
      <|> do reserved "print"
             (fmt, args) <- parens ((,) <$> stringLiteral <*> option [] (comma *> commaSep1 pAnyExpr))
             return $ StPrint fmt args
      <|> (reserved "assert" *> pExpr >>= return . StAssert)
      <|> (reserved "c_code" *> many1 pCStmt >>= return . StCCode)
      <|> (reserved "c_expr" *> many1 pCExpr >>= return . StCExpr)
      <|> (reserved "c_decl" *> many1 pCDecl >>= return . StCDecl)
      <|> do reserved "c_track"
             s1 <- stringLiteral
             s2 <- stringLiteral
             s3 <- optionMaybe stringLiteral
             return $ StCTrack s1 s2 s3
      <|> do reserved "c_state"
             s1 <- stringLiteral
             s2 <- stringLiteral
             s3 <- optionMaybe stringLiteral
             return $ StCState s1 s2 s3
      <|> (pExpr >>= return . StExpr)
      <?> "statement"

-- To be defined later
pCStmt :: Parser ParserState CStat
pCStmt = fail "parsing c code is currently unsupported"

pCExpr :: Parser ParserState CExpr
pCExpr = fail "parsing c expressions is currently unsupported"

pCDecl :: Parser ParserState CExtDecl
pCDecl = fail "parsing c declarations is currently unsupported"

pRange :: Parser ParserState Range
pRange = do var <- pVarRef
            (reserved "in" *> pVarRef >>= return . RnMember var)
              <|> do symbol ":"
                     e1 <- pExpr
                     symbol ".."
                     e2 <- pExpr
                     return $ RnInterval var e1 e2

pAnyExpr :: Parser ParserState AnyExpr
pAnyExpr = buildExpressionParser anyExprTable pAnyExpr'

pAnyExpr' :: Parser ParserState AnyExpr
pAnyExpr' = parens (do expr1 <- pAnyExpr
                       option expr1
                        (do symbol "->"
                            expr2 <- pAnyExpr
                            symbol ":"
                            expr3 <- pAnyExpr
                            return $ AeTrans expr1 expr2 expr3))
         <|> (reserved "len" *> parens pVarRef >>= return . AeLen)
         <|> (pPoll >>= return . AePoll)
         <|> (reserved "timeout" *> return AeTimeout)
         <|> (reserved "np_" *> return AeNp)
         <|> (reserved "enabled" *> parens pAnyExpr >>= return . AeEnabled)
         <|> (reserved "pc_value" *> parens pAnyExpr >>= return . AePCValue)
         <|> (do name1 <- (pName <* lookAhead (symbol "["))
                 e <- brackets pAnyExpr
                 symbol "@"
                 name2 <- pName
                 return $ AeRemote name1 e name2)
         <|> (do reserved "run"
                 name <- pName
                 args <- parens (commaSep pAnyExpr)
                 priority <- optionMaybe pPriority
                 return $ AeRun name args priority)
         <|> (do reserved "get_priority"
                 arg <- parens pExpr
                 return $ AeGetPriority arg)
         <|> (do reserved "set_priority"
                 (arg1, arg2) <- parens ((,) <$> pExpr <*> pExpr)
                 return $ AeSetPriority arg1 arg2)
         <|> (pVarRef >>= return . AeVarRef)
         <|> (pConst >>= return . AeConst)

anyExprTable :: OperatorTable String ParserState Identity AnyExpr
anyExprTable = [ [unary "~", unary "-", unary "!"]
               , [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft]
               , [binary "+" AssocLeft, binary "-" AssocLeft]
               , [binary "<<" AssocLeft, binary ">>" AssocLeft]
               , [binary "<" AssocLeft, binary "<=" AssocLeft, binary ">" AssocLeft, binary ">=" AssocLeft]
               , [binary "==" AssocLeft, binary "!=" AssocLeft]
               , [binary "&" AssocLeft]
               , [binary "^" AssocLeft]
               , [binary "|" AssocLeft]
               , [binary "&&" AssocLeft]
               , [binary "||" AssocLeft]
               ]
  where binary op assoc = Infix (reservedOp op *> return (flip AeBinOp op)) assoc
        unary  op       = Prefix (reservedOp op *> return (AeUnOp op))

pOptions :: Parser ParserState Options
pOptions = many1 (symbol ":" *> symbol ":" *> pSequence)

pExpr :: Parser ParserState Expr
pExpr = buildExpressionParser exprTable pExpr'

pExpr' :: Parser ParserState Expr
pExpr' =    parens pExpr
       <|> (do cp <- pChanPoll
               var <- parens pVarRef
               return $ EChanPoll cp var)
       <|> (pAnyExpr >>= return . EAnyExpr)

exprTable :: OperatorTable String ParserState Identity Expr
exprTable = [ [binary "&&" AssocLeft]
            , [binary "||" AssocLeft]
            ]
  where binary op assoc = Infix (reservedOp op *> return (flip ELogic op)) assoc


pChanPoll :: Parser ParserState ChanPoll
pChanPoll =   (reserved "full" *> return CpFull)
          <|> (reserved "empty" *> return CpEmpty)
          <|> (reserved "nfull" *> return CpNFull)
          <|> (reserved "nempty" *> return CpNEmpty)

pConst :: Parser ParserState Const
pConst =   (reserved "true" *> return CstTrue)
       <|> (reserved "false" *> return CstFalse)
       <|> (reserved "skip" *> return CstSkip)
       <|> (natural >>= return . CstNum)

