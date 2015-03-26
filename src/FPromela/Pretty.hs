module FPromela.Pretty where

import FPromela.Ast

import Text.PrettyPrint

niceBraces :: Doc -> Doc
niceBraces inner = text "{" $+$ inner $+$ text "}"


optionally :: (a -> Doc) -> Maybe a -> Doc
optionally = maybe empty

prettySpec :: Spec -> Doc
prettySpec spec = vcat $ punctuate semi (map prettyModule spec)

prettyModule :: Module -> Doc
prettyModule (MProcType active name decls prior enab seq) =
  (optionally prettyActive active) <+> text "proctype" <+> text name <+> parens (sep $ punctuate semi (map prettyDecl decls))
    <+> (optionally prettyPriority prior) <+> (optionally prettyEnabler enab) $+$ niceBraces (nest 4 (prettySequence seq))
prettyModule (MInit prior seq) =
  text "init" <+> (optionally prettyPriority prior) $+$ niceBraces (nest 4 (prettySequence seq))
prettyModule (MNever seq) =
  text "never" $+$ niceBraces (nest 4 (prettySequence seq))
prettyModule (MTrace seq) =
  text "trace" $+$ niceBraces (nest 4 (prettySequence seq))
prettyModule (MUType name decls) =
  text "typedef" <+> text name $+$ niceBraces (nest 4 (vcat $ punctuate semi (map prettyDecl decls)))
prettyModule (MDecls decls) =
 vcat $ punctuate semi (map prettyDecl decls)

prettyDecl :: Decl -> Doc
prettyDecl (Decl vis typ vars) = (optionally prettyVisible vis) <+> prettyType typ <+> hsep (punctuate comma (map prettyIVar vars))
prettyDecl (MType names)       = text "mtype" <+> equals <+> braces (hsep (punctuate comma (map text names)))

prettyType :: Type -> Doc
prettyType TBit = text "bit"
prettyType TBool = text "bool"
prettyType TByte = text "byte"
prettyType TShort = text "short"
prettyType TInt = text "int"
prettyType TMType = text "mtype"
prettyType TChan = text "chan"
prettyType (TUName name) = text name

prettyActive :: Active -> Doc
prettyActive (Active const) = text "active" <+> optionally (brackets . prettyConst) const

prettyPriority :: Priority -> Doc
prettyPriority (Priority const) = text "priority" <+> prettyConst const

prettyEnabler :: Enabler -> Doc
prettyEnabler (Enabler expr) = text "provided" <+> parens (prettyExpr expr)

prettyVisible :: Visible -> Doc
prettyVisible True = text "show"
prettyVisible False = text "hidden"

prettySequence :: Sequence -> Doc
prettySequence steps = sep (punctuate semi (map prettyStep steps))

prettyStep :: Step -> Doc
prettyStep (SStmt s1 s2) = prettyStmt s1 <+> optionally ((text "unless" <+>) . prettyStmt) s2
prettyStep (SDecl decl) = prettyDecl decl
prettyStep (SXr varrefs) = text "xr" <+> hsep (punctuate comma (map prettyVarRef varrefs))
prettyStep (SXs varrefs) = text "xs" <+> hsep (punctuate comma (map prettyVarRef varrefs))

prettyIVar :: IVar -> Doc
prettyIVar (IVar name const inval) =
    text name <+> optionally (brackets . prettyConst) const <+> optionally ((equals <+>) . prettyInVal) inval
  where prettyInVal (Left ae) = prettyAnyExpr ae
        prettyInVal (Right chinit) = prettyChInit chinit

prettyChInit :: ChInit -> Doc
prettyChInit (ChInit const typs) = brackets (prettyConst const) <+> text "of" <+> braces (sep $ punctuate comma (map prettyType typs))

prettyVarRef :: VarRef -> Doc
prettyVarRef (VarRef name ae rest) = text name <> optionally (brackets . prettyAnyExpr) ae <> optionally ((text "." <>) . prettyVarRef) rest

prettySend :: Send -> Doc
prettySend (Send var sort args) = prettyVarRef var <+> prettySorted sort <+> prettySendArgs args
  where prettySorted False = text "!"
        prettySorted True = text "!!"

prettyRandom :: Random -> Doc
prettyRandom False = text "?"
prettyRandom True  = text "??"

prettyReceive :: Receive -> Doc
prettyReceive (Receive var rand args) = prettyVarRef var <+> prettyRandom rand <+> prettyReceiveArgs args
prettyReceive (PollReceive var rand args) = prettyVarRef var <+> prettyRandom rand <+> text "<" <> prettyReceiveArgs args <> text ">"

prettyPoll :: Poll -> Doc
prettyPoll (Poll var rand args) = prettyVarRef var <+> prettyRandom rand <+> brackets (prettyReceiveArgs args)

prettySendArgs :: SendArgs -> Doc
prettySendArgs (SnArgs aes)      = sep $ punctuate comma (map prettyAnyExpr aes)
prettySendArgs (SnCall ae aes)   = prettyAnyExpr ae <+> parens (sep $ punctuate comma (map prettyAnyExpr aes))

prettyReceiveArgs :: ReceiveArgs -> Doc
prettyReceiveArgs (RcArgs args) = sep $ punctuate comma (map prettyReceiveArg args)
prettyReceiveArgs (RcCall arg args) = prettyReceiveArg arg <+> parens (prettyReceiveArgs args)

prettyReceiveArg :: ReceiveArg -> Doc
prettyReceiveArg (RcVarRef var) = prettyVarRef var
prettyReceiveArg (RcEval var)   = text "eval" <+> parens (prettyVarRef var)
prettyReceiveArg (RcConst neg const) = prettyNeg neg <> prettyConst const
  where prettyNeg False = empty
        prettyNeg True  = text "-"


prettyAssign :: Assign -> Doc
prettyAssign (AssignExpr var ae) = prettyVarRef var <+> equals <+> prettyAnyExpr ae
prettyAssign (AssignIncr var)    = prettyVarRef var <> text "++"
prettyAssign (AssignDecr var)    = prettyVarRef var <> text "--"

prettyStmt :: Stmt -> Doc
prettyStmt (StIf opts) = text "if" $+$ nest 4 (prettyOptions opts) $+$ text "fi"
prettyStmt (StDo opts) = text "do" $+$ nest 4 (prettyOptions opts) $+$ text "od"
prettyStmt (StFor range seq) = text "for" <+> parens (prettyRange range) <+> brackets (nest 4 (prettySequence seq))
prettyStmt (StAtomic seq) = text "atomic" <+> brackets (nest 4 (prettySequence seq))
prettyStmt (StDStep seq) = text "d_step" <+> brackets (nest 4 (prettySequence seq)) 
prettyStmt (StSelect range) =  text "select" <+> parens (prettyRange range)
prettyStmt (StBlock seq) = brackets (nest 4 (prettySequence seq))
prettyStmt (StSend send) = prettySend send
prettyStmt (StReceive recv) = prettyReceive recv
prettyStmt (StAssign assgn) = prettyAssign assgn
prettyStmt StElse = text "else"
prettyStmt StBreak = text "break"
prettyStmt (StGoto nm) = text "goto" <+> text nm
prettyStmt (StLabelled nm stmt) = text nm <> colon <+> prettyStmt stmt
prettyStmt (StPrint str args) = text "print" <+> parens (text str) <> comma <+> (hsep $ punctuate comma (map prettyAnyExpr args))
prettyStmt (StAssert expr) = text "assert" <+> prettyExpr expr
prettyStmt (StExpr expr) = prettyExpr expr
prettyStmt (StCCode _) = text "c_code" <+> brackets (text "...")
prettyStmt (StCExpr _) = text "c_expr" <+> brackets (text "...")
prettyStmt (StCDecl _) = text "c_decl" <+> brackets (text "...")
prettyStmt (StCTrack _ _ _) = text "c_track" <+> brackets (text "...")
prettyStmt (StCState _ _ _) = text "c_state" <+> brackets (text "...")

prettyRange :: Range -> Doc
prettyRange (RnInterval var e1 e2) = prettyVarRef var <+> colon <+> prettyExpr e1 <+> text ".." <+> prettyExpr e2
prettyRange (RnMember var1 var2) = prettyVarRef var1 <+> text "in" <+> prettyVarRef var2

prettyOptions :: Options -> Doc
prettyOptions opts = vcat $ map printOption opts
  where printOption (cnd:step:rest) = text "::" <+> prettyStep cnd <+> text "->" <+> prettySequence (step:rest)
        printOption seq             = text "::" <+> prettySequence seq

prettyAnyExpr :: AnyExpr -> Doc
prettyAnyExpr (AeBinOp ae1 op ae2) = parens (prettyAnyExpr ae1) <+> text op <+> parens (prettyAnyExpr ae2)
prettyAnyExpr (AeUnOp  op ae     ) = text op <+> parens (prettyAnyExpr ae)
prettyAnyExpr (AeTrans ae1 ae2 ae3) = parens (prettyAnyExpr ae1 <+> text "->" <+> prettyAnyExpr ae2 <+> colon <+> prettyAnyExpr ae3)
prettyAnyExpr (AeLen var) = text "len" <+> parens (prettyVarRef var)
prettyAnyExpr (AePoll poll) = prettyPoll poll
prettyAnyExpr (AeVarRef var) = prettyVarRef var
prettyAnyExpr (AeConst const) = prettyConst const
prettyAnyExpr AeTimeout = text "timeout"
prettyAnyExpr AeNp = text "np_"
prettyAnyExpr (AeEnabled ae) = text "enabled" <+> parens (prettyAnyExpr ae)
prettyAnyExpr (AePCValue ae) = text "pc_value" <+> parens (prettyAnyExpr ae)
prettyAnyExpr (AeRemote nm1 ae nm2) = text nm1 <+> brackets (prettyAnyExpr ae) <+> text "@" <+> text nm2
prettyAnyExpr (AeRun var aes prior) =
   text "run" <+> prettyVarRef var <+> parens (hsep $ punctuate comma (map prettyAnyExpr aes)) <+> optionally (brackets . prettyPriority) prior
prettyAnyExpr (AeGetPriority e) = text "get_priority" <+> parens (prettyExpr e)
prettyAnyExpr (AeSetPriority e1 e2) = text "set_priority" <+> parens (prettyExpr e1 <> comma <+> prettyExpr e2)

prettyExpr :: Expr -> Doc
prettyExpr (EAnyExpr ae) = prettyAnyExpr ae
prettyExpr (ELogic e1 op e2) = parens (prettyExpr e1) <+> text op <+> parens (prettyExpr e2)
prettyExpr (EChanPoll cp var) = prettyChanPoll cp <+> parens (prettyVarRef var)

prettyChanPoll :: ChanPoll -> Doc
prettyChanPoll CpFull = text "full"
prettyChanPoll CpEmpty = text "empty"
prettyChanPoll CpNFull = text "nfull"
prettyChanPoll CpNEmpty = text "nempty"

prettyConst :: Const -> Doc
prettyConst CstTrue = text "true"
prettyConst CstFalse = text "false"
prettyConst CstSkip = text "skip"
prettyConst (CstNum num) = integer num
