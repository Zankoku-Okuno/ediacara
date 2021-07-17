{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Language.PortAsm.Interpreter
  ( Conf(..)
  , exec
  , Stop(..)
  ) where

import Language.PortAsm.Syntax.Extensible

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Primitive (RealWorld)
import Data.Int (Int,Int16,Int32,Int64)
import Data.Primitive.ByteArray (MutableByteArray,newByteArray)
import Data.Text (Text)
import Data.Word (Word8,Word16,Word32,Word64)
import Language.PortAsm.Syntax.Frontend (Frontend) -- FIXME have an interpreter language that can speed up identifier comparisons/lookups

import qualified Data.Map.Strict as Map
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Text as T

------------------------------------ Top Level ------------------------------------

exec :: Conf -> IO Stop
exec conf = do
  case Arr.find isEntrypoint conf.program.decls of
    Just (DeclProc proc) -> do
      mem <- newByteArray 0 -- TODO create memory
      mkFrameIO Map.empty proc conf.programEntry >>= \case -- TODO call with gloabl vars
        Nothing -> pure "cannot find entry point"
        Just frame -> do
          let st = St{mem,frame,stack=[]}
          unM loop conf st >>= \case
            (_, Left stop) -> pure stop
            (_, Right ()) -> pure "terminated normally"
    Nothing -> pure "cannot find entry point"
  where
  isEntrypoint :: Decl Frontend -> Bool
  isEntrypoint (DeclProc proc) = conf.programEntry `Map.member` proc.entries

loop :: M ()
loop = popInstr >>= execStmt >> loop

type Stop = Text -- TODO


------------------------------------ Evaluator ------------------------------------

data Value
  = IntVal Integer
  | StackAddr (MutableByteArray RealWorld) Word64
  | MemAddr Word64

evalConstExpr :: ConstExpr Frontend -> M Value
evalConstExpr (ConstLit _ i) = pure (IntVal i)

evalRVal :: RVal Frontend -> M Value
evalRVal (LitRV e) = evalConstExpr e
evalRVal (VarRV x) = getVar x

------------------------------------ Instructions ------------------------------------

execStmt :: Stmt Frontend -> M ()
execStmt = \case
  Let{var,expr} -> do
    val <- evalConstExpr expr
    setVar var val
  Instr{dst,mnemonic,src} -> do
    inVals <- evalRVal `mapM` src
    outVals <- execInstr mnemonic inVals
    -- TODO assign to lvals
    pure ()

execInstr :: InstrName Frontend -> [Value] -> M [Value]
execInstr "exit" = \case
  [IntVal code] -> case fromIntegral @Integer @Word8 code of
    0 -> stuck "terminated normally"
    i -> stuck $ "terminated normally: exit code " <> T.pack (show i)
  _ -> stuck "bad instruction args"
execInstr "debug" = \case
  vals -> liftIO (printVal `mapM_` vals) >> pure []
    where
    printVal (IntVal i) = print i
    printVal (StackAddr _ _) = putStrLn "<stack address>"
    printVal (MemAddr addr) = putStrLn $ "<" ++ show addr ++ ">"
execInstr _ = const $ stuck "unknown instruction"

------------------------------------ Helpers ------------------------------------

mkFrameIO :: VarEnv -> Proc Frontend -> ProcName Frontend -> IO (Maybe Frame)
mkFrameIO vars proc name = case findBlock proc name of
  Nothing -> pure Nothing
  Just (scopes, block) -> do
    stackData <- newByteArray 0 -- TODO figure out how much memory to allocate on the stack
    let ip = Arr.toList block.stmts
    pure $ Just Frame{stackData,vars,ip}

mkFrame :: Proc Frontend -> ProcName Frontend -> M Frame
mkFrame proc name = do
  let vars = Map.empty -- TODO needs initialization with initial vars
  liftIO (mkFrameIO vars proc name) >>= \case
    Nothing -> stuck "unknown entrypoint"
    Just frame -> pure frame

findBlock :: Proc Frontend -> ProcName Frontend -> Maybe ([Scope Frontend], Block Frontend)
findBlock proc name = go (Arr.toList proc.scopes)
  where
  go [] = Nothing
  go (scope:rest) = case Map.lookup name scope.blocks of
    Just block -> pure ([], block)
    Nothing -> do
      (parents, block) <- go (Arr.toList scope.children ++ rest)
      pure (scope:parents, block)

getVar :: Var Frontend -> M Value
getVar var = M $ \_ st -> case Map.lookup var st.frame.vars of
  Nothing -> pure (st, Left "unknown variable")
  Just val -> pure (st, Right val)

setVar :: Var Frontend -> Value -> M ()
setVar var val = M $ \_ st ->
  pure (st{frame.vars = Map.insert var val st.frame.vars}, Right ())

popInstr :: M (Stmt Frontend)
popInstr = M $ \_ st -> case st.frame.ip of
  [] -> pure (st, Left "end of block")
  (instr:rest) -> pure (st{frame.ip = rest}, Right instr)

------------------------------------ Implementation Monad ------------------------------------

newtype M a = M { unM :: Conf -> St -> IO (St, Either Stop a) }

data Conf = Conf
  { program :: Prog Frontend
  , programEntry :: ProcName Frontend
  }
  -- TODO stuff like max stack size, max memory size, &c

data St = St
  { mem :: {-# UNPACK #-} !(MutableByteArray RealWorld)
  , frame :: {-# LANGUAGE UNPACK #-} Frame -- current activation
  , stack :: [Frame] -- saved activations
  }

data Frame = Frame
  { stackData :: {-# LANGUAGE UNPACK #-} !(MutableByteArray RealWorld)
  , vars :: VarEnv
  , ip :: [Stmt Frontend]
  }

type VarEnv = Map.Map (Var Frontend) Value

data Conts -- TODO when a function is called, save the continuations that might be taken on the stack

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

instance MonadIO M where
  liftIO action = M $ \_ st -> do
    x <- action
    pure (st, Right x)

stuck :: Stop -> M a
stuck stop = M $ \_ st -> pure (st, Left stop)
