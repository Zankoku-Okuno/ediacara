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
  , Proc(..)
  , XProc
  , Scope(..)
  , XScope
  , Block(..)
  , XBlock
  -- * Implementation
  , Stmt(..)
  , XLet
  , XInstr
  , XRet
  , ConstExpr(..)
  , XConstLit
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

-- | name of a continuation (C has one continuation accessible with `return`, but port-asm allows procedures with mutliple exit points)
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
  -- TODO

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
  -- , stackAlloc :: -- TODO I'll need an argument list for each reserved stack slot; calling into a deeper scope will need to provide these extra arguments
  , children :: SmallArray (Scope xi)
  , blocks :: Map xi (BlockName xi) (Block xi)
  }

type family XScope xi

data Block xi = Block
  { blockInfo :: XBlock xi
  , binds :: Map xi (Var xi) (Typ xi)
  , stmts :: SmallArray (Stmt xi)
  }

type family XBlock xi

------------------------------------ Statements ------------------------------------

data Stmt xi
  = Let
    { letInfo :: XLet xi
    , var :: Var xi
    , expr :: ConstExpr xi
    }
  -- TODO Move
  | Instr
    { instrInfo :: XInstr xi
    , dst :: [(LVal xi, Typ xi)]
    , mnemonic :: InstrName xi
    , src :: [RVal xi]
    }
  -- TODO
  | Ret
    { retInfo :: XRet xi
    , target :: ContName xi
    , vals :: SmallArray (RVal xi)
    }

type family XLet xi
type family XInstr xi
type family XRet xi

------------------------------------ Expressions ------------------------------------

data ConstExpr xi
  = ConstLit (XConstLit xi) Integer
  -- TODO
type family XConstLit xi

data RVal xi
  = LitRV (ConstExpr xi)
  | VarRV (Var xi)
  -- TODO

data LVal xi
  = VarLV (Var xi)
  -- TODO
