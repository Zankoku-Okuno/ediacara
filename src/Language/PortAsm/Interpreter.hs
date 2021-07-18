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
import Data.Maybe (fromMaybe)
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
      frame <- mkFrame globals (Arr.empty, startSp conf) proc conf.programEntry (Map.empty, [])
      LSt{mem} <- getLoaderState
      pure St{globals,mem,frame,stack=[]}
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
evalConstExpr (onlyConstBinds -> binds) = go
  where
  go = \case
    ConstLit _ i -> pure $ fromIntegral @Integer @Value i
    ConstSizeof _ typename -> case typename of
      "u8" -> pure 1
      _ -> throwError $ TypeUnknown typename
    ConstAlignof _ typename -> case typename of
      "u8" -> pure 1
      _ -> throwError $ TypeUnknown typename
    ConstVar _ var -> lookup var binds
    ConstMul _ a b -> (*) <$> go a <*> go b -- FIXME apparently I need a reader monad to pass binds around with

evalRVal :: (MonadError Stop m) => ValueBindings -> RVal Frontend -> m Value
evalRVal binds = go
  where
  go = \case
    LitRV e -> evalConstExpr binds e
    VarRV x -> lookup x binds
    ScaleRV scale e -> (fromInteger scale *) <$> go e

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
  Goto{target,stackArgs,args} -> do
    binds <- getBindings
    stackVals <- traverse (evalRVal binds `mapM`) stackArgs
    regVals <- evalRVal binds `mapM` args
    globals <- getGlobals
    currentFrame <- peekFrame
    frame' <- mkFrame globals (currentFrame.scopes, currentFrame.fp) currentFrame.proc target (stackVals, regVals)
    setFrame frame'

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
        -> (SmallArray ScopeRecord, FramePointer) -- currently active scopes (and frame pointer, in case there are no active scopes)
        -> Proc Frontend -- procedure to search in
        -> ProcName Frontend -- entrypoint to search for
        -> (Map Frontend (Var Frontend) [Value], [Value]) -- stack allocation arguments and initial register values
        -> m Frame
mkFrame globals (scopes0, fp0) proc name (stackVals, regVals) = case findBlock proc name of
  Nothing -> throwError $ BlockUnknown name
  Just (reqScopes, block) -> do
    scopes <- reallocFrame globals reqScopes (scopes0, fp0) stackVals
    let fp = theSp scopes fp0
    let consts = theConsts globals scopes
    when (length regVals /= length block.binds) $
      throwError BlockBadArgs
    let regs = Map.fromList $ zip (fst <$> block.binds) regVals
    let ip = Arr.toList block.stmts
    pure Frame{scopes,fp,consts,regs,proc,ip}

reallocFrame :: forall m. (MonadError Stop m, MonadIO m)
        => ConstEnv -- globals
        -> [(Int, Scope Frontend)] -- requested scopes
        -> (SmallArray ScopeRecord, FramePointer) -- existing scopes (and frame pointer, in case there are no active scopes)
        -> Map Frontend (Var Frontend) [Value] -- stack allocation arguments
        -> m (SmallArray ScopeRecord) -- new scope state
reallocFrame globals req0 (now0, fp0) stackVals = do
  frameDealloc req0 0
  where
  -- first, slice off the trailing part of the existing scope that does nto agree with the request
  frameDealloc :: [(Int, Scope Frontend)] -> Int -> m (SmallArray ScopeRecord)
  frameDealloc [] i = pure $ Arr.clone now0 0 i
  frameDealloc req@((scopeIx, _):rest) i
    | i >= Arr.size now0 = frameAlloc req now0
    | (Arr.index now0 i).scopeId /= scopeIx = frameAlloc req (Arr.clone now0 0 i)
    | otherwise = frameDealloc rest (i + 1)
  -- then, create new scopes and place them on top of the original
  frameAlloc :: [(Int, Scope Frontend)] -> SmallArray ScopeRecord -> m (SmallArray ScopeRecord)
  frameAlloc req now = do
    scopes' <- scopesLoop (theConsts globals now) (theScopeAddrs now) (theSp now fp0) [] req
    pure $ now <> scopes'
    where
    scopesLoop :: ConstEnv -- accumulated constants
               -> StackEnv -- accumulated stack addresses
               -> StackPointer -- updated stack pointer
               -> [ScopeRecord] -- freshly allocated scopes
               -> [(Int, Scope Frontend)] -- remaining scope requests
               -> m (SmallArray ScopeRecord)
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
            -> m ScopeRecord
    allocScope sp0 consts addrs0 scopeId allocs = do
      allocLoop ScopeRecord{consts,stackAddrs=addrs0,scopeId,sp=sp0} consts (Map.toList allocs)
      where
      allocLoop :: ScopeRecord -- initial scope
                -> ConstEnv -- TODO include stack allocation parameters
                -> [(Var Frontend, StackAlloc Frontend)] -- allocation requests
                -> m ScopeRecord -- updated scope with new allocations
      allocLoop scope _ [] = pure scope
      allocLoop scope vars ((name, alloc):rest) = do
        let allocVals = fromMaybe [] $ Map.lookup name stackVals
        when (length allocVals /= length alloc.params) $
          throwError StackdataBadArgs
        let binds = (fromConstBinds vars){regs = Map.fromList (zip alloc.params allocVals)}
        liftIO $ print "hello" -- DEBUG
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
