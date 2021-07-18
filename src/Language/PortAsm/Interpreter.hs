{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, GADTs #-}

module Language.PortAsm.Interpreter
  ( Conf(..)
  , exec
  , Stop(..)
  ) where

import Prelude hiding (lookup)

import Language.PortAsm.Syntax.Extensible

-- TODO organize
import Language.PortAsm.Interpreter.Monad
import Language.PortAsm.Interpreter.Types

import Control.Applicative ((<|>))
import Control.Monad (when, forM, forM_)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ask)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Primitive.Contiguous (SmallArray)
import Data.Word (Word8,Word64)
import Language.PortAsm.Syntax.Frontend (Frontend) -- FIXME have an interpreter language that can speed up identifier comparisons/lookups

import qualified Data.Map.Strict as Map
import qualified Data.Primitive.Contiguous as Arr

------------------------------------ Top Level ------------------------------------

exec :: Conf -> IO Stop
exec conf = do
  runL load conf >>= \case
    (_, Left err) -> pure err
    (_, Right st) -> runM execLoop conf st >>= \case
      (_, Left stop) -> pure stop
      (_, Right _) -> pure StackUnderflow

execLoop :: M () -- TODO actually, this should return the return continuation name and values
execLoop = popInstr >>= execStmt >> execLoop

load :: L St
load = do
  conf <- ask
  globals <- loadConsts conf.program.decls
  -- TODO initialize low memory
  case Arr.find (isEntrypoint conf.programEntry) conf.program.decls of
    Just (DeclProc proc) -> do
      frame <- mkFrame globals (Arr.empty, startSp conf) proc conf.programEntry
      LSt{mem} <- getLoaderState
      pure St{mem,frame,stack=[]}
    Just _ -> errorWithoutStackTrace "internalError Language.PortAsm.Interpreter.load"
    Nothing -> throwError ProgEntryUnknown
  where
  isEntrypoint :: ProcName Frontend -> Decl Frontend -> Bool
  isEntrypoint entryName (DeclProc proc) = entryName `Map.member` proc.entries
  isEntrypoint _ _ = False

loadConsts :: SmallArray (Decl Frontend) -> L ConstEnv
loadConsts = go Map.empty . toList
  where
  go acc [] = pure acc
  go acc (DeclConst{var,expr}:rest) = do
    val <- evalConstExpr (fromConstBinds acc) expr
    go (Map.insert var val acc) rest
  -- TODO also create function pointers and addresses for global data
  go acc (_:rest) = go acc rest

------------------------------------ Evaluator ------------------------------------

evalConstExpr :: (MonadError Stop m) => ValueBindings -> ConstExpr Frontend -> m Value
evalConstExpr (onlyConstBinds -> binds) = \case
  ConstLit _ i -> pure $ fromIntegral @Integer @Value i
  ConstSizeof _ typename -> case typename of
    "u8" -> pure 1
    _ -> throwError $ TypeUnknown typename
  ConstAlignof _ typename -> case typename of
    "u8" -> pure 1
    _ -> throwError $ TypeUnknown typename
  ConstVar _ var -> lookup var binds

evalRVal :: (MonadError Stop m) => ValueBindings -> RVal Frontend -> m Value
evalRVal binds = \case
  LitRV e -> evalConstExpr binds e
  VarRV x -> lookup x binds

------------------------------------ Instructions ------------------------------------

execStmt :: Stmt Frontend -> M ()
execStmt = \case
  BlockLet{var,expr} -> do
    binds <- getBindings
    val <- evalConstExpr binds expr
    setConst var val
  Instr{dst,mnemonic,src} -> do
    binds <- getBindings
    inVals <- evalRVal binds `mapM` src
    outVals <- execInstr mnemonic inVals
    when (length dst /= length outVals) $
      throwError InstrBadValues
    forM_ (zip dst outVals) $ uncurry setReg

execInstr :: InstrName Frontend -> [Value] -> M [Value]
execInstr "add8" = \case
  [a, b] -> pure . (:[]) $ fromIntegral @Word8 @Value (fromIntegral a + fromIntegral b)
  _ -> throwError InstrBadArgs
execInstr "exit" = \case
  [code] -> throwError $ Exit (fromIntegral @Word64 @Word8 code)
  _ -> throwError InstrBadArgs
execInstr "debug" = \case
  vals -> liftIO (print `mapM_` vals) >> pure []
execInstr _ = const $ throwError InstrUnknown

------------------------------------ Helpers ------------------------------------

mkFrame :: forall m. (MonadError Stop m, MonadIO m)
        => ConstEnv -- globals
        -> (SmallArray ScopeEnv, FramePointer) -- currently active scopes (and frame pointer, in case there are no active scopes)
        -> Proc Frontend -- procedure to search in
        -> ProcName Frontend -- entrypoint to search for
        -> m Frame
mkFrame globals (scopes0, fp0) proc name = case findBlock proc name of
  Nothing -> throwError ProcEntryUnknown
  Just (reqScopes, block) -> do
    scopes <- reallocFrame globals reqScopes (scopes0, fp0)
    let fp = theSp scopes fp0
    let consts = theConsts globals scopes
    let regs = Map.empty -- TODO needs initialization with arguments
    let ip = Arr.toList block.stmts
    pure Frame{scopes,fp,consts,regs,ip}

reallocFrame :: forall m. (MonadError Stop m, MonadIO m)
        => ConstEnv -- globals
        -> [(Int, Scope Frontend)] -- requested scopes
        -> (SmallArray ScopeEnv, FramePointer) -- existing scopes (and frame pointer, in case there are no active scopes)
        -> m (SmallArray ScopeEnv) -- new scope state
reallocFrame globals req0 (now0, fp0) = do
  frameDealloc req0 0
  where
  -- first, slice off the trailing part of the existing scope that does nto agree with the request
  frameDealloc :: [(Int, Scope Frontend)] -> Int -> m (SmallArray ScopeEnv)
  frameDealloc [] i = pure $ Arr.clone now0 0 i
  frameDealloc req@((scopeIx, _):rest) i
    | i >= Arr.size now0 = frameAlloc req now0
    | (Arr.index now0 i).scopeId /= scopeIx = frameAlloc req (Arr.clone now0 0 i)
    | otherwise = frameDealloc rest (i + 1)
  -- then, create new scopes and place them on top of the original
  frameAlloc :: [(Int, Scope Frontend)] -> SmallArray ScopeEnv -> m (SmallArray ScopeEnv)
  frameAlloc req now = do
    scopes' <- scopesLoop (theConsts globals now) (theScopeAddrs now) (theSp now fp0) [] req
    pure $ now <> scopes'
    where
    scopesLoop :: ConstEnv -- accumulated constants
               -> StackEnv -- accumulated stack addresses
               -> StackPointer -- updated stack pointer
               -> [ScopeEnv] -- freshly allocated scopes
               -> [(Int, Scope Frontend)] -- remaining scope requests
               -> m (SmallArray ScopeEnv)
    scopesLoop _ _ _ revScopes' [] = pure . Arr.fromList $ reverse revScopes'
    scopesLoop prevConsts prevAddrs sp revScopes' ((i, scope):rest) = do
      let binds = ValBinds{consts=prevConsts,stackAddrs=Map.empty,regs=Map.empty}
      freshConsts <- (Map.fromList <$>) . forM (toList scope.scopeLets) $ \ScopeLet{var,expr} ->
        (var,) <$> evalConstExpr binds expr
      let consts' = prevConsts `Map.union` freshConsts
      scope' <- allocScope sp consts' prevAddrs i scope.stackAlloc
      scopesLoop scope'.consts scope'.stackAddrs scope'.sp (scope' : revScopes') rest
    allocScope :: StackPointer -- current top of stack
            -> ConstEnv -- previously-defined constants
            -> StackEnv -- adresses of previously-allocated stack data
            -> Int -- scope id for the output scope
            -> Map Frontend (Var Frontend) (StackAlloc Frontend) -- allocation requests
            -> m ScopeEnv
    allocScope sp0 consts addrs0 scopeId allocs = do
      allocLoop ScopeEnv{consts,stackAddrs=addrs0,scopeId,sp=sp0} consts (Map.toList allocs)
      where
      allocLoop :: ScopeEnv -- initial scope
                -> ConstEnv -- TODO include stack allocation parameters
                -> [(Var Frontend, StackAlloc Frontend)] -- allocation requests
                -> m ScopeEnv -- updated scope with new allocations
      allocLoop scope _ [] = pure scope
      allocLoop scope vars ((name, alloc):rest) = do
        let binds = fromConstBinds vars
        size <- evalRVal binds alloc.expr
        -- TODO align stack pointer
        sp' <- scope.sp `bumpSp` size
        let scope' = scope{stackAddrs = Map.insert name scope.sp scope.stackAddrs, sp = sp'}
        allocLoop scope' vars rest


findBlock :: Proc Frontend -> ProcName Frontend -> Maybe ([(Int, Scope Frontend)], Block Frontend)
findBlock proc name = go 0 proc.scopes
  where
  go :: Int -> SmallArray (Scope Frontend) -> Maybe ([(Int, Scope Frontend)], Block Frontend)
  go !i !scopes
    | i >= Arr.size scopes = Nothing
    | otherwise =
      let scope = Arr.index scopes i
          here = ([(i, scope)],) <$> Map.lookup name scope.blocks
          fromChild = first ((i, scope):) <$> go 0 scope.children
          fromNextScope = go (i + 1) scopes
       in here <|> fromChild <|> fromNextScope
