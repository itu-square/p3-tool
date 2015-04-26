module TVL.Pretty where

import TVL.Ast

import Text.PrettyPrint

prettyModel :: Model -> Doc
prettyModel = vcat . map prettyDecl

prettyDecl :: Decl -> Doc
prettyDecl (DType td) = prettyTypeDecl td
prettyDecl (DConstant cd) = prettyConstantDecl cd
prettyDecl (DFeature fd) = prettyFeatureDecl fd

prettyTypeDecl :: TypeDecl -> Doc
prettyTypeDecl (TSimple td) = prettySimpleTypeDecl td
prettyTypeDecl (TRecord nm fields) = text "struct" <+> text nm <+> text "{" $+$ (vcat $ map prettyRecordField fields) $+$ text "}"

prettySimpleTypeDecl :: SimpleTypeDecl -> Doc
prettySimpleTypeDecl (TIntScalar nm) = text "int" <+> text nm <> semi
prettySimpleTypeDecl (TRealScalar nm) = text "real" <+> text nm <> semi
prettySimpleTypeDecl (TBoolScalar nm) = text "bool" <+> text nm <> semi
prettySimpleTypeDecl (TIntRange nm se) = text "int" <+> text nm <+> text "in" <+> prettySetExpr se <> semi
prettySimpleTypeDecl (TRealRange nm se) = text "real" <+> text nm <+> text "in" <+> prettySetExpr se <> semi
prettySimpleTypeDecl (TEnum nm se) = text "enum" <+> text nm <+> text "in" <+> prettySetExpr se <> semi

prettyRecordField :: RecordField -> Doc
prettyRecordField (FSimple td) = prettySimpleTypeDecl td
prettyRecordField (FReference tn vn) = text tn <+> text vn <> semi

prettyConstantDecl :: ConstantDecl -> Doc
prettyConstantDecl (CstInt nm i) = text "const" <+> text "int" <+> text nm <+> (text . show $ i) <> semi
prettyConstantDecl (CstReal nm r) = text "const" <+> text "real" <+> text nm <+> double r <> semi
prettyConstantDecl (CstBool nm b) = text "const" <+> text "bool" <+> text nm <+> (text . show $ b) <> semi

prettyFeatureDecl :: FeatureDecl -> Doc
prettyFeatureDecl (FtFeature r nm body) =
  (if r then text "root" else empty) <+> text nm <+> text "{" $+$ (vcat $ map prettyFtBodyItem body) $+$ text "}"
prettyFeatureDecl (FtGroup r nm crd body) =
  (if r then text "root" else empty) <+> text nm <+> text "group" <+> prettyCardinality crd <+> text "{" $+$ (vcat $ punctuate comma (map prettyFtHiDecl body)) $+$ text "}"

prettyFtBodyItem :: FtBodyItem -> Doc
prettyFtBodyItem (FtiData dat) = text "data" <+> text "{" $+$ (vcat $ map (\(x,y) -> (text . show $ x) <+> (text . show $ y) <> semi) dat) $+$ text "}"
prettyFtBodyItem (FtiConstraint cd) = prettyConstraintDecl cd
prettyFtBodyItem (FtiAttribute ad) = prettyAttributeDecl ad
prettyFtBodyItem (FtiGroup crd body) = text "group" <+> prettyCardinality crd <+> text "{" $+$ (vcat $ map prettyFtHiDecl body) $+$ text "}"

prettyPresence :: Presence -> Doc
prettyPresence PShared = text "shared"
prettyPresence POptional = text "opt"
prettyPresence PNone = empty

prettyFtHiDecl :: FtHiDecl -> Doc
prettyFtHiDecl (FthFeature pr fd) = prettyPresence pr <+> prettyFeatureDecl fd
prettyFtHiDecl (FthItem pr nm) = prettyPresence pr <+> text nm

prettyCardinality :: Cardinality -> Doc
prettyCardinality CdOneOf   = text "oneof"
prettyCardinality CdSomeOf  = text "someof"
prettyCardinality CdAllOf   = text "allof"
prettyCardinality (CdRange il ih) = brackets ((text . show $ il) <+> comma <+> maybe (text "*") (text . show) ih)

prettyAttributeDecl :: AttributeDecl -> Doc
prettyAttributeDecl (AttrInt nm body) = text "int" <+> text nm <+> maybe empty prettyAttributeBody body <> semi
prettyAttributeDecl (AttrReal nm body) = text "real" <+> text nm <+> maybe empty prettyAttributeBody body <> semi
prettyAttributeDecl (AttrBool nm body) = text "bool" <+> text nm <+> maybe empty prettyAttributeBody body <> semi
prettyAttributeDecl (AttrEnum nm body) = text "enum" <+> text nm <+> maybe empty prettyAttributeBody body <> semi
prettyAttributeDecl (AttrReference tn vn body) = text tn <+> text vn <+> maybe empty prettyAttributeBody body <> semi
prettyAttributeDecl (AttrComplex tn vn sattrs) = text tn <+> text vn <+> (vcat $ map (\(tn', body) -> text tn' <+> prettyAttributeBody body) sattrs)

prettyAttributeBody :: AttributeBody -> Doc
prettyAttributeBody (AbIs e) = text "is" <+> prettyExpr e
prettyAttributeBody (AbIn se ac) = text "in" <+> prettyExpr se <+> maybe empty ((comma <+>) . prettyAttributeConditional) ac
prettyAttributeBody (AbCond ac) = prettyAttributeConditional ac

prettyAttributeValue :: AttributeValue -> Doc
prettyAttributeValue (AvIn se) = text "in" <+> prettySetExpr se
prettyAttributeValue (AvIs e) = text "is" <+> prettyExpr e

prettyAttributeConditional :: AttributeConditional -> Doc
prettyAttributeConditional (AcIfInOut (Just ac1) ac2) = text "ifin:" <+> prettyAttributeValue ac1 <+> maybe empty ((comma <+> text "ifout:" <+>) . prettyAttributeValue) ac2
prettyAttributeConditional (AcIfInOut Nothing ac2) = text "ifout:" <+> maybe empty prettyAttributeValue ac2

prettyConstraintDecl :: ConstraintDecl -> Doc
prettyConstraintDecl (CtIfIn e) = text "ifin:" <+> prettyExpr e <> semi
prettyConstraintDecl (CtIfOut e) = text "ifout:" <+> prettyExpr e <> semi
prettyConstraintDecl (CtExpr e) = prettyExpr e <> semi

prettyExpr :: Expr -> Doc
prettyExpr (BinOp op e1 e2) = parens (prettyExpr e1) <+> text op <+> parens (prettyExpr e2)
prettyExpr (UnOp op e) = text op <+> parens (prettyExpr e)
prettyExpr (EBool b) = if b then text "true" else text "false"
prettyExpr (EInt i) = text . show $ i
prettyExpr (EReal r) = double r
prettyExpr (Count chd) = text "count" <+> parens (prettyChildren chd)
prettyExpr (Ref nm) = text nm
prettyExpr (InSet e se) = parens (prettyExpr e) <+> text "in" <+> prettySetExpr se
prettyExpr (EAbs e) = text "abs" <+> parens (prettyExpr e)
prettyExpr (Exclusion nm1 nm2) = text nm1 <+> text "excludes" <+> text nm2
prettyExpr (Dependency nm1 nm2) = text nm1 <+> text "requires" <+> text nm2
prettyExpr (BuiltinFun nm (Left es)) = text nm <+> parens (cat $ punctuate comma (m)) prettyExpr es))
prettyExpr (BuiltinFun nm (Right cid)) = text nm <+> parens (prettyChildrenId cid))
prettyExpr (Ternary e1 e2 e3) = parens (prettyExpr e1) <+> text "?" <+> parens (prettyExpr e2) <+> colon <+> parens (prettyExpr e3)

prettySetExpr :: SetExpr -> Doc
prettySetExpr (SExprs e) = braces (cat $ punctuate comma (map prettyExpr e))
prettySetExpr (SRange b1 b2) = brackets (prettySetExprBound b1 <+> text ".." <+> prettySetExprBound b2)

prettySetExprBound :: SetExprBound -> Doc
prettySetExprBound (SBInt i) = text . show $ i
prettySetExprBound (SBReal r) = double r
prettySetExprBound SBUnbounded = text "*"

prettyChildren :: Children -> Doc
prettyChildren ChdAll = text "children"
prettyChildren ChdSelected = text "selectedchildren"

prettyChildrenId :: ChildrenId -> Doc
prettyChildrenId (ChdId chd nm) = prettyChildren chd <> dot <> text nm
