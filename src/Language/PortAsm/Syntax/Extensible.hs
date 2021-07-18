{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Language.PortAsm.Syntax.Extensible
  ( -- * Identifiers
    Typ
  , Var
  , ProcName
  , ContName
  , BlockName
  , InstrName
  -- * General Data Structures
  , Map
  -- * Definitions
  , Prog(..)
  , XProg
  , Decl(..)
  , XDeclConst
  , Proc(..)
  , XProc
  , Scope(..)
  , XScope
  , ScopeLet(..)
  , XScopeLet
  , StackAlloc(..)
  , XStackAlloc
  , Block(..)
  , XBlock
  -- * Implementation
  , Stmt(..)
  , XBlockLet
  , XInstr
  , XGoto
  , XRet
  , ConstExpr(..)
  , XConstLit
  , XConstSizeof
  , XConstAlignof
  , XConstVar
  , XConstMul
  , RVal(..)
  , LVal(..)
  ) where

import Data.Primitive (SmallArray)

------------------------------------ Identifiers ------------------------------------

-- | Identifier for a native type
type family Typ xi

-- | variable name
type family Var xi

-- | name of a procedure (multiple-entry, multiple-exit, with calling convention)
type family ProcName xi

-- | name of a continuation (C has one continuation accessible with `return`, but ediacara allows procedures with mutliple exit points)
type family ContName xi

-- | name of a block (single-entry, multiple-exit unit of control flow with explicit live variables)
type family BlockName xi

-- | mnemonic name of an instruction
type family InstrName xi

------------------------------------ General Data Structures ------------------------------------

type family Map xi :: * -> * -> *

------------------------------------ Declarations ------------------------------------


data Prog xi = Prog
  { progInfo :: XProg xi
  , decls :: SmallArray (Decl xi)
  }
type family XProg xi

data Decl xi
  = DeclProc (Proc xi)
  | DeclConst
    { declConstInfo :: XDeclConst xi
    , var :: Var xi
    , expr :: ConstExpr xi
    }
  -- TODO

type family XDeclConst xi

------------------------------------ Code Organization ------------------------------------

data Proc xi = Proc
  { procInfo :: XProc xi
  -- TODO calling convention
  , entries :: Map xi (ProcName xi) (BlockName xi)
  , exits :: Map xi (ContName xi) (SmallArray (Typ xi))
  , scopes :: SmallArray (Scope xi)
  }

type family XProc xi

data Scope xi = Scope
  { scopeInfo :: XScope xi
  , scopeLets :: SmallArray (ScopeLet xi)
  , stackAlloc :: Map xi (Var xi) (StackAlloc xi)
  , children :: SmallArray (Scope xi)
  , blocks :: Map xi (BlockName xi) (Block xi)
  }
data ScopeLet xi = ScopeLet
  { scopeLetInfo :: XScopeLet xi
  , var :: Var xi
  , expr :: ConstExpr xi
  }
data StackAlloc xi = StackAlloc -- stackdata arr(n) = u8 * n
  { stackAllocInfo :: XStackAlloc xi
  , params :: [Var xi]
  , expr :: RVal xi
  }

type family XScope xi
type family XScopeLet xi
type family XStackAlloc xi

data Block xi = Block
  { blockInfo :: XBlock xi
  , binds :: [(Var xi, Typ xi)]
  , stmts :: SmallArray (Stmt xi)
  }

type family XBlock xi

------------------------------------ Statements ------------------------------------

data Stmt xi
  = BlockLet
    { letInfo :: XBlockLet xi
    , var :: Var xi
    , expr :: ConstExpr xi
    }
  -- TODO Move
  | Instr
    { instrInfo :: XInstr xi
    , dst :: [Var xi]
    , mnemonic :: InstrName xi
    , src :: [RVal xi]
    }
  | Goto
    { gotoInfo :: XGoto xi
    , target :: BlockName xi
    , stackArgs :: Map xi (Var xi) [RVal xi]
    , args :: [RVal xi]
    }
  -- TODO
  -- | Ret
  --   { retInfo :: XRet xi
  --   , target :: ContName xi
  --   , vals :: SmallArray (RVal xi)
  --   }

type family XBlockLet xi
type family XInstr xi
type family XGoto xi
type family XRet xi

------------------------------------ Expressions ------------------------------------

data ConstExpr xi
  = ConstLit (XConstLit xi) Integer
  | ConstSizeof (XConstSizeof xi) (Typ xi)
  | ConstAlignof (XConstAlignof xi) (Typ xi)
  | ConstVar (XConstVar xi) (Var xi)
  | ConstMul (XConstMul xi) (ConstExpr xi) (ConstExpr xi)
  -- TODO
type family XConstLit xi
type family XConstSizeof xi
type family XConstAlignof xi
type family XConstVar xi
type family XConstMul xi

data RVal xi
  = LitRV (ConstExpr xi)
  | VarRV (Var xi)
  | ScaleRV Integer (RVal xi)
  -- TODO

data LVal xi
  = VarLV (Var xi)
  -- TODO
