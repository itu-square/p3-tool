{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module TVL.Ast where

import Data.Data
import Data.Typeable

-- Based on: https://projects.info.unamur.be/tvl/files/TVL-Grammar-20100112.txt

{-
MODEL          = ( TYPE | CONSTANT | FEATURE )*
 -}
type Model = [Decl]

data Decl = DType TypeDecl
          | DConstant ConstantDecl
          | DFeature FeatureDecl
          deriving (Eq, Show, Data, Typeable)


{-
TYPE          =  SIMPLE_TYPE
              | RECORD

RECORD          = "struct" ID "{" RECORD_FIELD+ "}"
 -}
data TypeDecl = TSimple SimpleTypeDecl
              | TRecord Name [RecordField]
              deriving (Eq, Show, Data, Typeable)

{-
SIMPLE_TYPE   =  "int" ID "in" SET_EXPRESSION ";"
             | "real" ID "in" SET_EXPRESSION ";"
             | "enum" ID "in" SET_EXPRESSION ";"
             | "int" ID ";"
             | "real" ID ";"
             | "bool" ID ";"
 -}
data SimpleTypeDecl = TIntScalar Name
                    | TRealScala Name
                    | TBoolScalar Name
                    | TIntRange Name SetExpr
                    | TRealRange Name SetExpr
                    | TEnum Name SetExpr
                    deriving (Eq, Show, Data, Typeable)

{-
 RECORD_FIELD =  SIMPLE_TYPE
              | ID ID ";"
-}
data RecordField = FSimple SimpleTypeDecl
                 | FReference String String
                 deriving (Eq, Show, Data, Typeable)

{-
CONSTANT  =  "const" "int" ID INTEGER ";"
         | "const" "real" ID REAL ";"
         | "const" "bool" ID ( "true" | "false" ) ";"
 -}
data ConstantDecl = CstInt Name Integer
                  | CstReal Name Double
                  | CstTrue Name
                 deriving (Eq, Show, Data, Typeable)



{-
FEATURE =  "root" ID "{" FEATURE_BODY_ITEM+ "}" 
        | LONG_ID "{" FEATURE_BODY_ITEM+ "}"
        | "root" ID FEATURE_GROUP
        | LONG_ID FEATURE_GROUP

FEATURE_GROUP = "group" CARDINALITY "{" HIERARCHICAL_FEATURE ("," HIERARCHICAL_FEATURE)* "}"  
 -}
type Root = Bool

data FeatureDecl = FtFeature Root Name [FtBodyItem]
                 | FtGroup Root Name Cardinality [FtHiDecl]
                 deriving (Eq, Show, Data, Typeable)


{-
FEATURE_BODY_ITEM =  DATA
                  | CONSTRAINT
                  | ATTRIBUTE
                  | FEATURE_GROUP
 -}
data FtBodyItem = FtiData [(String, String)]
                | FtiConstraint ConstraintDecl
                | FtiAttribute AttributeDecl
                | FtiGroup Cardinality [FtHiDecl]
                 deriving (Eq, Show, Data, Typeable)

data Presence = PShared
              | POptional
              | PNone
              deriving (Eq, Show, Data, Typeable)

{-
HIERARCHICAL_FEATURE  =  ("opt")? FEATURE
                      | ( "shared" | "opt" )? LONG_ID
 -}
data FtHiDecl = FthFeature Presence FeatureDecl
              | FthItem Presence Name
              deriving (Eq, Show, Data, Typeable)

{-
CARDINALITY  = "oneof"
             | "someof"
             | "allof"
             | "[" NATURAL "," (NATURAL | "*" ) "]"
 -}
data Cardinality = CdOneOf
                 | CdSomeOf
                 | CdAllOf
                 | CdRange Integer (Maybe Integer)
                 deriving (Eq, Show, Data, Typeable)


{-
ATTRIBUTE =  BASE_ATTRIBUTE
          | ID ID "{" SUB_ATTRIBUTE+ "}"

BASE_ATTRIBUTE =  "int" ID  ATTRIBUTE_BODY? ";"
               | "real" ID  ATTRIBUTE_BODY? ";"
               | "bool" ID  ATTRIBUTE_BODY? ";"
               | "enum" ID  ATTRIBUTE_BODY? ";"
               | ID ID ATTRIBUTE_BODY? ";"

SUB_ATTRIBUTE = ID ATTRIBUTE_BODY ";"
 -}
data AttributeDecl = AttrInt Name (Maybe AttributeBody)
                   | AttrReal Name (Maybe AttributeBody)
                   | AttrBool Name (Maybe AttributeBody)
                   | AttrEnum Name (Maybe AttributeBody)
                   | AttrReference Name Name (Maybe AttributeBody)
                   | AttrComplex Name Name [(Name, AttributeBody)]
                   deriving (Eq, Show, Data, Typeable)

{-
ATTRIBUTE_BODY =  "is" EXPRESSION
               | "in" SET_EXPRESSION "," ATTRIBUTE_CONDITIONNAL
               | "in" SET_EXPRESSION
               | "," ATTRIBUTE_CONDITIONNAL
 -}
data AttributeBody = AbVal AttributeValue
                   | AbCond AttributeConditional
                   deriving (Eq, Show, Data, Typeable)

data AttributeValue = AvIn SetExpr (Maybe AttributeConditional)
                    | AvIs Expr
                    deriving (Eq, Show, Data, Typeable)

{-
ATTRIBUTE_CONDITIONNAL  =  "ifin:" "is" EXPRESSION "," "ifout:" "is" EXPRESSION
                         | "ifin:" "is" EXPRESSION
                         | "ifout:" "is" EXPRESSION
                         | "ifin:" "in" SET_EXPRESSION "," "ifout:" "is" EXPRESSION
                         | "ifin:" "is" EXPRESSION "," "ifout:" "in" SET_EXPRESSION
                         | "ifin:" "in" SET_EXPRESSION "," "ifout:" "in" SET_EXPRESSION
                         | "ifin:" "in" SET_EXPRESSION
                         | "ifout:" "in" SET_EXPRESSION
 -}
data AttributeConditional = AcIfInOut (Maybe AttributeValue) (Maybe AttributeValue)
                    deriving (Eq, Show, Data, Typeable)


{-
EXPRESSION   =  EXPRESSION "&&" EXPRESSION
             | EXPRESSION "||" EXPRESSION
             | EXPRESSION "->" EXPRESSION
             | EXPRESSION "<-" EXPRESSION
             | EXPRESSION "<->" EXPRESSION
             | "!" EXPRESSION
             | "(" EXPRESSION ")"
             | "true"
             | "false"
             | LONG_ID
             | EXPRESSION "==" EXPRESSION
             | EXPRESSION "!=" EXPRESSION
             | EXPRESSION "<=" EXPRESSION
             | EXPRESSION "<" EXPRESSION
             | EXPRESSION ">=" EXPRESSION
             | EXPRESSION ">" EXPRESSION
             | EXPRESSION "in" SET_EXPRESSION
             | "and" "(" ( EXPRESSION_LIST | CHILDREN_ID ) ")"
             | "or" "(" ( EXPRESSION_LIST | CHILDREN_ID ) ")"
             | "xor" "(" ( EXPRESSION_LIST | CHILDREN_ID ) ")"
             | LONG_ID "excludes" LONG_ID
             | LONG_ID "requires" LONG_ID
             | EXPRESSION "+" EXPRESSION
             | EXPRESSION "-" EXPRESSION
             | EXPRESSION "/" EXPRESSION
             | EXPRESSION "*" EXPRESSION
             | "-" EXPRESSION
             | "abs" "(" EXPRESSION ")"
             | EXPRESSION "?" EXPRESSION ":" EXPRESSION
             | "sum" "(" (EXPRESSION_LIST | CHILDREN_ID) ")"
             | "mul" "(" ( EXPRESSION_LIST | CHILDREN_ID ) ")"
             | "min" "(" ( EXPRESSION_LIST | CHILDREN_ID ) ")"
             | "max" "(" ( EXPRESSION_LIST | CHILDREN_ID ) ")"
             | "count" "(" ( "children" | "selectedchildren" ) ")"
             | "avg" "(" ( EXPRESSION_LIST | CHILDREN_ID ) ")"
             | INTEGER
             | REAL
EXPRESSION_LIST      = EXPRESSION ("," EXPRESSION_LIST)*
 -}
data Expr = BinOp String Expr Expr
          | UnOp String Expr
          | Ref Name
          | InSet Expr SetExpr
          | BuiltinFun (Either [Expr] ChildrenId)
          | Exclusion Name Name
          | Dependency Name Name
          | Ternary Expr Expr Expr
          | Count Children
          | EInt Integer
          | EReal Double
          | EBool Bool
          deriving (Eq, Show, Data, Typeable)

data SetExpr = SExprs [Expr]
             | SRange SetExprBound SetExprBound
             deriving (Eq, Show, Data, Typeable)

{-
SET_EXPRESSION_BOUND  =  INTEGER
                     | REAL
                     | "*"
 -}
data SetExprBound = SBInt Integer
                  | SBReal Double
                  | SBUnbounded
                  deriving (Eq, Show, Data, Typeable)

data Children = ChdAll
              | ChdSelected
              deriving (Eq, Show, Data, Typeable)

{-
CHILDREN_ID   =  "selectedchildren" "." LONG_ID
              | "children" "." LONG_ID
 -}
data ChildrenId = ChdId Children Name
               deriving (Eq, Show, Data, Typeable)


{-
CONSTRAINT  = "ifin:" EXPRESSION ";"
            | "ifout:" EXPRESSION ";"
            | EXPRESSION ";
 -}
data ConstraintDecl = CtIfIn Expr
                    | CtIfOut Expr
                    | CtExpr Expr
                    deriving (Eq, Show, Data, Typeable)

type Name = String
