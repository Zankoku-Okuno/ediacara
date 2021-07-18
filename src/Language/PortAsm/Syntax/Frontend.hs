{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.PortAsm.Syntax.Frontend
  ( Frontend
  ) where

import Language.PortAsm.Syntax.Extensible

import Data.Text (Text)

import qualified Data.Map.Strict as Map


-- | Extension descriptor marking the syntax as having just been parsed
data Frontend

type Location = () -- FIXME replace with a real location from Eexpr

------------------------------------ Identifiers ------------------------------------

type instance Typ Frontend = Text

type instance Var Frontend = Text

type instance ProcName Frontend = Text
type instance ContName Frontend = Text
type instance BlockName Frontend = Text
type instance InstrName Frontend = Text

------------------------------------ General Data Structures ------------------------------------

type instance Map Frontend = Map.Map -- FIXME should probly be an AList here

------------------------------------ Code Organization ------------------------------------

type instance XProg Frontend = Location

type instance XDeclConst Frontend = Location

type instance XProc Frontend = Location

type instance XScope Frontend = Location
type instance XScopeLet Frontend = Location
type instance XStackAlloc Frontend = Location

type instance XBlock Frontend = Location

------------------------------------ Statements ------------------------------------

type instance XBlockLet Frontend = Location
type instance XInstr Frontend = Location
type instance XGoto Frontend = Location
type instance XRet Frontend = Location

------------------------------------ Expressions ------------------------------------

type instance XConstLit Frontend = Location
type instance XConstSizeof Frontend = Location
type instance XConstAlignof Frontend = Location
type instance XConstVar Frontend = Location
type instance XConstMul Frontend = Location

------------------------------------ Expressions ------------------------------------

deriving instance Show (Scope Frontend)
deriving instance Show (ScopeLet Frontend)
deriving instance Show (StackAlloc Frontend)
deriving instance Show (Block Frontend)
deriving instance Show (Stmt Frontend)
deriving instance Show (ConstExpr Frontend)
deriving instance Show (RVal Frontend)
