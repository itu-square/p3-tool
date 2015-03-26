module FPromela.Pretty where

import FPromela.Ast

import Text.PrettyPrint

optionally :: (a -> Doc) -> Maybe a -> Doc
optionally = maybe empty

prettySpec :: Spec -> Doc
prettySpec spec = vcat $ punctuate semi (map prettyModule spec)

prettyModule :: Module -> Doc
prettyModule (MProcType active name decls prior enab seq) =
  (optionally prettyActive active) <+> text "proctype" <+> text name <+> parens (sep $ punctuate semi (map prettyDecl decls))
    <+> (optionally prettyPriority prior) <+> (optionally prettyEnabler enab) <+> braces (nest 4 (prettySequence seq))
prettyModule (MInit prior seq) =
  text "init" <+> (optionally prettyPriority prior) <+> braces (nest 4 (prettySequence seq))
prettyModule (MNever seq) =
  text "never" <+> braces (nest 4 (prettySequence seq))
prettyModule (MTrace seq) =
  text "trace" <+> braces (nest 4 (prettySequence seq))
prettyModule (MUType name decls) =
  text "typedef" <+> text name <+> braces (nest 4 (vcat $ punctuate semi (map prettyDecl decls)))
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

prettyAnyExpr = undefined
prettyExpr = undefined
prettyConst = undefined
prettyStmt = undefined
