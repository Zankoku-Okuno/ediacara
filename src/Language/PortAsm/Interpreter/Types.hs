{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, GADTs #-}


module Language.PortAsm.Interpreter.Types
  ( Conf(..)
  , Value
  , StackPointer
  , startSp
  , bumpSp
  , FramePointer
  , Stop(..)
  , ValueBindings(..)
  , fromConstBinds
  , onlyConstBinds
  , lookup
  , ConstEnv
  , StackEnv
  , RegEnv
  , Frame(..)
  , ScopeEnv(..)
  , theSp
  , theConsts
  , theScopeAddrs
  ) where

import Prelude hiding (lookup)

import Language.PortAsm.Syntax.Extensible

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError(..))
import Data.Primitive.Contiguous (SmallArray)
import Data.Word (Word8,Word64)
import Language.PortAsm.Syntax.Frontend (Frontend) -- FIXME have an interpreter language that can speed up identifier comparisons/lookups

import qualified Data.Map.Strict as Map
import qualified Data.Primitive.Contiguous as Arr


data Conf = Conf
  { program :: Prog Frontend
  , programEntry :: ProcName Frontend
  , maxMemory :: Word64
  }
  -- TODO stuff like max stack size, max memory size, &c


type Value = Word64

data Stop
  = Exit Word8 -- the only expected termination
  | TypeUnknown (Typ Frontend)
  | ProgEntryUnknown
  | ProcEntryUnknown
  | VarUnknown (Var Frontend)
  | InstrUnknown
  | InstrBadArgs
  | InstrBadValues
  | EndOfBlock
  | StackUnderflow
  deriving(Show)

type FramePointer = StackPointer
newtype StackPointer = SP Word64 -- pointers to the stack, including the pointer to the top of the stack
  deriving (Show)
startSp :: Conf -> StackPointer
startSp Conf{maxMemory} = SP maxMemory
bumpSp :: (MonadError Stop m) => StackPointer -> Word64 -> m StackPointer
bumpSp (SP a) off = pure $ SP (a - off) -- TODO check stack limit
spAsValue :: StackPointer -> Value
spAsValue (SP a) = a


data ValueBindings = ValBinds
  { regs :: !RegEnv
  , stackAddrs :: !StackEnv
  , consts :: !ConstEnv
  }
  deriving (Show)

fromConstBinds :: ConstEnv -> ValueBindings
fromConstBinds consts = ValBinds{regs = Map.empty, stackAddrs = Map.empty, consts}

onlyConstBinds :: ValueBindings -> ValueBindings
onlyConstBinds binds = binds{stackAddrs = Map.empty, consts = Map.empty}

lookup :: (MonadError Stop m) => Var Frontend -> ValueBindings -> m Value
lookup var binds = do
  let registers = Map.lookup var binds.regs
      stackAddrs = spAsValue <$> Map.lookup var binds.stackAddrs
      consts = Map.lookup var binds.consts
  case registers <|> stackAddrs <|> consts of
    Nothing -> throwError $ VarUnknown var
    Just val -> pure val

type ConstEnv = Map.Map (Var Frontend) Value
type StackEnv = Map.Map (Var Frontend) StackPointer
type RegEnv = Map.Map (Var Frontend) Value


data Frame = Frame
  { scopes :: {-# UNPACK #-} !(SmallArray ScopeEnv)
  , fp :: !FramePointer
  , regs :: !RegEnv
  , consts :: !ConstEnv
  , ip :: ![Stmt Frontend]
  }

data Conts -- TODO when a function is called, save the continuations that might be taken on the stack

data ScopeEnv = ScopeEnv
  { consts :: !ConstEnv
  , stackAddrs :: !StackEnv
  , scopeId :: {-# UNPACK #-} !Int -- index of this scope within its paren't children array
  , sp :: {-# UNPACK #-} !StackPointer -- point on the stack where this scope ends
  }
  deriving (Show)

theSp :: SmallArray ScopeEnv -> FramePointer -> StackPointer
theSp arr fp
  | Arr.null arr = fp
  | otherwise = (Arr.index arr (Arr.size arr - 1)).sp

theConsts :: ConstEnv -> SmallArray ScopeEnv -> ConstEnv
theConsts globals xs
  | Arr.size xs == 0 = globals
  | otherwise = globals `Map.union` (Arr.index xs (Arr.size xs - 1)).consts

theScopeAddrs :: SmallArray ScopeEnv -> StackEnv
theScopeAddrs xs
  | Arr.size xs == 0 = Map.empty
  | otherwise = (Arr.index xs (Arr.size xs - 1)).stackAddrs
