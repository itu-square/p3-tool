{-# LANGUAGE DeriveDataTypeable #-}

module FPromela.Ast where

import Language.C.Syntax.AST (CExtDecl, CStat, CExpr)

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

{-
  one_decl: [ visible ] typename  ivar [',' ivar ] *
 -}
data Decl = Decl (Maybe Visible) Type [IVar]
          | MType [Name]


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

{-
   active  : ACTIVE [ '[' const ']' ]
 -}
newtype Active = Active (Maybe Const)

{-
  priority: PRIORITY const
-}
newtype Priority = Priority Const

{-
  enabler : PROVIDED '(' expr ')'
-}
newtype Enabler  = Enabler Expr

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

{-
  ivar    : name [ '[' const ']' ] [ '=' any_expr | '=' ch_init ]
-}
data IVar = IVar Name (Maybe Const) (Maybe (Either AnyExpr ChInit))

{-
  ch_init : '[' const ']' OF '{' typename [ ',' typename ] * '}'
-}
data ChInit = ChInit Const [Type]

{-
  varref  : name [ '[' any_expr ']' ] [ '.' varref ]
 -}
data VarRef = VarRef Name (Maybe AnyExpr) (Maybe VarRef)

type Sorted = Bool

{-
  send    : varref '!' send_args    /* normal fifo send */
  | varref '!' '!' send_args  /* sorted send */
-}
data Send = Send VarRef Sorted SendArgs

type Random = Bool

{-
receive : varref '?' recv_args    /* normal receive */
  | varref '?' '?' recv_args  /* random receive */
  | varref '?' '<' recv_args '>'  /* poll with side-effect */
  | varref '?' '?' '<' recv_args '>'  /* ditto */
-}
data Receive = Receive     VarRef Random ReceiveArgs
             | PollReceive VarRef Random ReceiveArgs

{-
  poll    : varref '?' '[' recv_args ']'  /* poll without side-effect */
  | varref '?' '?' '[' recv_args ']'  /* ditto */
 -}
data Poll = Poll VarRef Random ReceiveArgs

{-
 send_args: arg_lst | any_expr '(' arg_lst ')'

 arg_lst  : any_expr [',' any_expr ] *
 -}
data SendArgs = SnArgs [AnyExpr]
              | SnCall AnyExpr [AnyExpr]

{-
  recv_args: recv_arg [ ',' recv_arg ] *  |  recv_arg '(' recv_args ')'
 -}
data ReceiveArgs = RcArgs [ReceiveArg]
                 | RcCall ReceiveArg ReceiveArgs

type RcNeg = Bool

{-
  recv_arg : varref | EVAL '(' varref ')' | [ '-' ] const
 -}
data ReceiveArg = RcVarRef VarRef
                | RcEval VarRef
                | RcConst RcNeg Const

{-
 assign  : varref '=' any_expr  /* standard assignment */
  | varref '+' '+'  /* increment */
  | varref '-' '-'  /* decrement */
 -}
data Assign = AssignExpr VarRef AnyExpr
            | AssignIncr VarRef
            | AssignDecr VarRef

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

{-
range  : varref ':' expr '..' expr
  | varref IN varref
 -}
data Range = RnInterval VarRef Expr Expr
           | RnMember VarRef VarRef

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

{-
expr  : any_expr
  | '(' expr ')'
  | expr andor expr
  | chanpoll '(' varref ')'  /* may not be negated */
 -}
data Expr = EAnyExpr AnyExpr
          | ELogic Expr String Expr
          | EChanPoll ChanPoll VarRef

{-
  chanpoll: FULL | EMPTY | NFULL | NEMPTY
-}
data ChanPoll = CpFull | CpEmpty | CpNFull | CpNEmpty

{- name  : alpha [ alpha | number ] * -}
type Name     = String

{- const  : TRUE | FALSE | SKIP | number [ number ] * -}
data Const = CstTrue
           | CstFalse
           | CstSkip
           | CstNum Integer
