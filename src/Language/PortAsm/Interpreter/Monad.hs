{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, GADTs #-}

module Language.PortAsm.Interpreter.Monad
  ( -- * Execute Monad
    M
  , runM
  , St(..)
  , popInstr
  , getBindings
  , setConst
  , setReg
  -- * Loader Monad
  , L
  , runL
  , LSt(..)
  , getLoaderState
  ) where

import Language.PortAsm.Interpreter.Types
import Language.PortAsm.Syntax.Extensible

import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Reader (MonadReader(..))
import Data.Primitive.ByteArray (MutableByteArray,newByteArray)
import Language.PortAsm.Syntax.Frontend (Frontend) -- FIXME have an interpreter language that can speed up identifier comparisons/lookups

import qualified Data.Map.Strict as Map


------------------------------------ loader monad ------------------------------------

newtype L a = L { unL :: Conf -> LSt -> IO (LSt, Either Stop a) }

data LSt = LSt
  { mem :: {-# UNPACK #-} !(MutableByteArray RealWorld)
  }

runL :: L a -> Conf -> IO (LSt, Either Stop a)
runL action conf = do
  mem <- newByteArray 0
  unL action conf LSt{mem}

getLoaderState :: L LSt
getLoaderState = L $ \_ st -> pure (st, Right st)

instance Functor L where
  fmap f action = L $ \conf st -> unL action conf st >>= \case
    (st', Right x) -> pure (st', Right (f x))
    (st', Left err) -> pure (st', Left err)

instance Applicative L where
  pure x = L $ \_ st -> pure (st, Right x)
  actionF <*> actionX = L $ \conf st -> unL actionF conf st >>= \case
    (st', Right f) -> unL actionX conf st' >>= \case
      (st'', Right x) -> pure (st'', Right (f x))
      (st'', Left err) -> pure (st'', Left err)
    (st', Left err) -> pure (st', Left err)

instance Monad L where
  return = pure
  actionX >>= k = L $ \conf st -> unL actionX conf st >>= \case
    (st', Right x) -> unL (k x) conf st'
    (st', Left err) -> pure (st', Left err)

instance MonadReader Conf L where
  ask = L $ \conf st -> pure (st, Right conf)
  local _ action = action

instance MonadError Stop L where
  throwError err = L $ \_ st -> pure (st, Left err)
  catchError action k = L $ \conf st -> unL action conf st >>= \case
    (st', Right x) -> pure (st', Right x)
    (st', Left err) -> unL (k err) conf st'

instance MonadIO L where
  liftIO action = L $ \_ st -> do
    x <- action
    pure (st, Right x)


------------------------------------ execute monad ------------------------------------

newtype M a = M { unM :: Conf -> St -> IO (St, Either Stop a) }

runM :: M a -> Conf -> St -> IO (St, Either Stop a)
runM action conf st = unM action conf st

popInstr :: M (Stmt Frontend)
popInstr = M $ \_ st -> case st.frame.ip of
  [] -> pure (st, Left EndOfBlock)
  (instr:rest) -> pure (st{frame = st.frame{ip = rest}}, Right instr)

getBindings :: M ValueBindings
getBindings = M $ \_ st -> do
  let binds = ValBinds
        { regs = st.frame.regs
        , stackAddrs = theScopeAddrs st.frame.scopes
        , consts = st.frame.consts
        }
  pure (st, Right binds)

setConst :: Var Frontend -> Value -> M ()
setConst name val = M $ \_ st -> do
  pure (st{frame = st.frame{consts = Map.insert name val st.frame.consts}}, Right ())

setReg :: Var Frontend -> Value -> M ()
setReg name val = M $ \_ st ->
  pure (st{frame = st.frame{regs = Map.insert name val st.frame.regs}}, Right ())


data St = St
  { mem :: {-# UNPACK #-} !(MutableByteArray RealWorld)
  , frame :: {-# LANGUAGE UNPACK #-} !Frame -- current activation
  , stack :: ![Frame] -- saved activations
  }

instance Functor M where
  fmap f action = M $ \conf st -> unM action conf st >>= \case
    (st', Right x) -> pure (st', Right (f x))
    (st', Left err) -> pure (st', Left err)

instance Applicative M where
  pure x = M $ \_ st -> pure (st, Right x)
  actionF <*> actionX = M $ \conf st -> unM actionF conf st >>= \case
    (st', Right f) -> unM actionX conf st' >>= \case
      (st'', Right x) -> pure (st'', Right (f x))
      (st'', Left err) -> pure (st'', Left err)
    (st', Left err) -> pure (st', Left err)

instance Monad M where
  return = pure
  actionX >>= k = M $ \conf st -> unM actionX conf st >>= \case
    (st', Right x) -> unM (k x) conf st'
    (st', Left err) -> pure (st', Left err)

instance MonadReader Conf M where
  ask = M $ \conf st -> pure (st, Right conf)
  local _ action = action

instance MonadError Stop M where
  throwError err = M $ \_ st -> pure (st, Left err)
  catchError action k = M $ \conf st -> unM action conf st >>= \case
    (st', Right x) -> pure (st', Right x)
    (st', Left err) -> unM (k err) conf st'

instance MonadIO M where
  liftIO action = M $ \_ st -> do
    x <- action
    pure (st, Right x)
