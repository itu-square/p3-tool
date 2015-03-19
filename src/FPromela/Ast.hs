{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module FPromela.Ast where

import Language.C.Syntax.AST (CExtDecl, CStat, CExpr)
import Data.Data
import Data.Typeable

--Based on: http://spinroot.com/spin/Man/grammar.html

{-
 spec : module [ module ] *
-}
type Spec = [Module]

{-
 module  : proctype  /* proctype declaration */
  | init    /* init process       */
  | never    /* never claim        */
  | trace    /* event trace        */
  | utype    /* user defined types */
  | mtype    /* mtype declaration  */
  | decl_lst  /* global vars, chans */

  proctype: [ active ] PROCTYPE name '(' [ decl_lst ]')'
    [ priority ] [ enabler ] '{' sequence '}'

  init  : INIT [ priority ] '{' sequence '}'

  never  : NEVER  '{' sequence '}'

  trace  : TRACE '{' sequence '}'

  utype  : TYPEDEF name '{' decl_lst '}'

  mtype  : MTYPE [ '=' ] '{' name [ ',' name ] * '}'

  decl_lst: one_decl [ ';' one_decl ] *
 -}
data Module = MProcType (Maybe Active) Name [Decl] (Maybe Priority) (Maybe Enabler) Sequence
            | MInit (Maybe Priority) Sequence
            | MNever Sequence
            | MTrace Sequence
            | MUType Name [Decl]
            | MDecls [Decl]
            deriving (Eq, Show, Data, Typeable)

{-
  one_decl: [ visible ] typename  ivar [',' ivar ] *
 -}
data Decl = Decl (Maybe Visible) Type [IVar]
          | MType [Name]
            deriving (Eq, Show, Data, Typeable)

{-
  typename: BIT | BOOL | BYTE | SHORT | INT | MTYPE | CHAN
    | uname  /* user defined type names (see utype) */
 -}
data Type = TBit
          | TBool
          | TByte
          | TShort
          | TInt
          | TMType
          | TChan
          | TUName Name
          deriving (Eq, Show, Data, Typeable)

{-
   active  : ACTIVE [ '[' const ']' ]
 -}
newtype Active = Active (Maybe Const)
            deriving (Eq, Show, Data, Typeable)
{-
  priority: PRIORITY const
-}
newtype Priority = Priority Const
            deriving (Eq, Show, Data, Typeable)

{-
  enabler : PROVIDED '(' expr ')'
-}
newtype Enabler  = Enabler Expr
          deriving (Eq, Show, Data, Typeable)

{-
  visible  : HIDDEN | SHOW
 -}
type Visible  = Bool

{-
  sequence: step [ ';' step ] *
-}
type Sequence = [Step]

{-
  step    : stmnt  [ UNLESS stmnt ]
  | decl_lst
  | XR varref [',' varref ] *
  | XS varref [',' varref ] *
 -}
data Step = SStmt Stmt (Maybe Stmt)
          | SDecl Decl
          | SXr [VarRef]
          | SXs [VarRef]
          deriving (Eq, Show, Data, Typeable)
{-
  ivar    : name [ '[' const ']' ] [ '=' any_expr | '=' ch_init ]
-}
data IVar = IVar Name (Maybe Const) (Maybe (Either AnyExpr ChInit))
          deriving (Eq, Show, Data, Typeable)

{-
  ch_init : '[' const ']' OF '{' typename [ ',' typename ] * '}'
-}
data ChInit = ChInit Const [Type]
          deriving (Eq, Show, Data, Typeable)

{-
  varref  : name [ '[' any_expr ']' ] [ '.' varref ]
 -}
data VarRef = VarRef Name (Maybe AnyExpr) (Maybe VarRef)
          deriving (Eq, Show, Data, Typeable)

type Sorted = Bool

{-
  send    : varref '!' send_args    /* normal fifo send */
  | varref '!' '!' send_args  /* sorted send */
-}
data Send = Send VarRef Sorted SendArgs
          deriving (Eq, Show, Data, Typeable)

type Random = Bool

{-
receive : varref '?' recv_args    /* normal receive */
  | varref '?' '?' recv_args  /* random receive */
  | varref '?' '<' recv_args '>'  /* poll with side-effect */
  | varref '?' '?' '<' recv_args '>'  /* ditto */
-}
data Receive = Receive     VarRef Random ReceiveArgs
             | PollReceive VarRef Random ReceiveArgs
          deriving (Eq, Show, Data, Typeable)

{-
  poll    : varref '?' '[' recv_args ']'  /* poll without side-effect */
  | varref '?' '?' '[' recv_args ']'  /* ditto */
 -}
data Poll = Poll VarRef Random ReceiveArgs
          deriving (Eq, Show, Data, Typeable)

{-
 send_args: arg_lst | any_expr '(' arg_lst ')'

 arg_lst  : any_expr [',' any_expr ] *
 -}
data SendArgs = SnArgs [AnyExpr]
              | SnCall AnyExpr [AnyExpr]
          deriving (Eq, Show, Data, Typeable)

{-
  recv_args: recv_arg [ ',' recv_arg ] *  |  recv_arg '(' recv_args ')'
 -}
data ReceiveArgs = RcArgs [ReceiveArg]
                 | RcCall ReceiveArg ReceiveArgs
          deriving (Eq, Show, Data, Typeable)

type RcNeg = Bool

{-
  recv_arg : varref | EVAL '(' varref ')' | [ '-' ] const
 -}
data ReceiveArg = RcVarRef VarRef
                | RcEval VarRef
                | RcConst RcNeg Const
          deriving (Eq, Show, Data, Typeable)

{-
 assign  : varref '=' any_expr  /* standard assignment */
  | varref '+' '+'  /* increment */
  | varref '-' '-'  /* decrement */
 -}
data Assign = AssignExpr VarRef AnyExpr
            | AssignIncr VarRef
            | AssignDecr VarRef
          deriving (Eq, Show, Data, Typeable)

{-
stmnt  : IF options FI    /* selection */
  | DO options OD    /* iteration */
  | FOR '(' range ')' '{' sequence '}'    /* iteration */
  | ATOMIC '{' sequence '}'  /* atomic sequence */
  | D_STEP '{' sequence '}'  /* deterministic atomic */
  | SELECT '(' range ')'  /* non-deterministic value selection */
  | '{' sequence '}'  /* normal sequence */
  | send
  | receive
  | assign
  | ELSE      /* used inside options */
  | BREAK      /* used inside iterations */
  | GOTO name
  | name ':' stmnt  /* labeled statement */
  | PRINT '(' string [ ',' arg_lst ] ')'
  | ASSERT expr    
  | expr      /* condition */
  | c_code '{' ... '}'  /* embedded C code */
  | c_expr '{' ... '}'
  | c_decl '{' ... '}'
  | c_track '{' ... '}'
  | c_state '{' ... '}'
 -}
data Stmt = StIf Options
          | StDo Options
          | StFor Range Sequence
          | StAtomic Sequence
          | StDStep Sequence
          | StSelect Range
          | StBlock Sequence
          | StSend Send
          | StReceive Receive
          | StAssign Assign
          | StElse
          | StBreak
          | StGoto Name
          | StLabelled Name Stmt
          | StPrint String [AnyExpr]
          | StAssert Expr
          | StExpr Expr
          | StCCode [CStat]
          | StCExpr [CExpr]
          | StCDecl [CExtDecl]
          | StCTrack String String (Maybe String)
          | StCState String String (Maybe String)
          deriving (Show, Data, Typeable)

instance Eq Stmt where
  (StIf opt1) == (StIf opt2) = opt1 == opt2
  (StDo opt1) == (StDo opt2) = opt1 == opt2
  (StFor r1 s1) == (StFor r2 s2) = r1 == r2 && s1 == s2
  (StAtomic s1) == (StAtomic s2) = s1 == s2
  (StDStep s1) == (StDStep s2) = s1 == s2
  (StSelect r1) == (StSelect r2) = r1 == r2
  (StBlock s1) == (StBlock s2) = s1 == s2
  (StSend s1) == (StSend s2) = s1 == s2
  (StReceive r1) == (StReceive r2) = r1 == r2
  (StAssign a1) == (StAssign a2) = a1 == a2
  StElse == StElse = True
  StBreak == StBreak = True
  (StGoto n1) == (StGoto n2) = n1 == n2
  (StLabelled n1 s1) == (StLabelled n2 s2) = n1 == n2 && s1 == s2
  (StPrint s1 l1) == (StPrint s2 l2) = s1 == s2 && l1 == l2
  (StAssert e1) == (StAssert e2) = e1 == e2
  (StExpr e1) == (StExpr e2) = e1 == e2
  _ == _ = False




{-
range  : varref ':' expr '..' expr
  | varref IN varref
 -}
data Range = RnInterval VarRef Expr Expr
           | RnMember VarRef VarRef
          deriving (Eq, Show, Data, Typeable)

{-
options : ':' ':' sequence [ ':' ':' sequence ] *
-}
type Options = [Sequence]

{-
any_expr: '(' any_expr ')'
  | any_expr binarop any_expr
  | unarop any_expr
  | '(' any_expr '-' '>' any_expr ':' any_expr ')'
  | LEN '(' varref ')'  /* nr of messages in chan */
  | poll
  | varref
  | const 
  | TIMEOUT
  | NP_      /* non-progress system state */
  | ENABLED '(' any_expr ')'    /* refers to a pid */
  | PC_VALUE '(' any_expr ')'    /* refers to a pid */
  | name '[' any_expr ']' '@' name  /* refers to a pid */
  | RUN name '(' [ arg_lst ] ')' [ priority ]
  | get_priority( expr )      /* expr refers to a pid */
  | set_priority( expr , expr )    /* first expr refers to a pid */
 -}
data AnyExpr = AeBinOp  AnyExpr String AnyExpr
             | AeUnOp   String AnyExpr
             | AeTrans  AnyExpr AnyExpr AnyExpr
             | AeLen VarRef
             | AePoll Poll
             | AeVarRef VarRef
             | AeConst Const
             | AeTimeout
             | AeNp
             | AeEnabled AnyExpr
             | AePCValue AnyExpr
             | AeRemote Name AnyExpr Name
             | AeRun Name [AnyExpr] (Maybe Priority)
             | AeGetPriority Expr
             | AeSetPriority Expr Expr
          deriving (Eq, Show, Data, Typeable)

{-
expr  : any_expr
  | '(' expr ')'
  | expr andor expr
  | chanpoll '(' varref ')'  /* may not be negated */
 -}
data Expr = EAnyExpr AnyExpr
          | ELogic Expr String Expr
          | EChanPoll ChanPoll VarRef
          deriving (Eq, Show, Data, Typeable)

{-
  chanpoll: FULL | EMPTY | NFULL | NEMPTY
-}
data ChanPoll = CpFull | CpEmpty | CpNFull | CpNEmpty
          deriving (Eq, Show, Data, Typeable)

{- name  : alpha [ alpha | number ] * -}
type Name     = String

{- const  : TRUE | FALSE | SKIP | number [ number ] * -}
data Const = CstTrue
           | CstFalse
           | CstSkip
           | CstNum Integer
          deriving (Eq, Show, Data, Typeable)
