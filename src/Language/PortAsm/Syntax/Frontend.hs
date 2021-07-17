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

type instance XProc Frontend = Location

type instance XScope Frontend = Location

type instance XBlock Frontend = Location

------------------------------------ Statements ------------------------------------

type instance XLet Frontend = Location
type instance XInstr Frontend = Location
type instance XRet Frontend = Location

------------------------------------ Expressions ------------------------------------

type instance XConstLit Frontend = Location
