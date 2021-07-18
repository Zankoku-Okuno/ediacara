{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.PortAsm.Syntax.Extensible

import Language.PortAsm.Interpreter (Conf(..), exec, Stop(..))
import Language.PortAsm.Syntax.Frontend (Frontend)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(..))

import qualified Data.Map.Strict as Map
import qualified Data.Primitive.Contiguous as Arr

-- TODO LIST
--   load/store, global data
--   call/ret, alt ret continuations, tail call
--   branch
--   inner functions (local call/ret): goto is like a local tail call

prog :: Prog Frontend
prog = Prog () $ Arr.fromList
  [ DeclConst () "globalVar" $ ConstLit () 255
  , DeclProc Proc
    { procInfo = ()
    , entries = Map.fromList [("_start", "_start")]
    , exits = Map.empty
    , scopes = Arr.fromList
      [ Scope
        { scopeInfo = ()
        , scopeLets = Arr.fromList
          [ ScopeLet () "yoyoyo" $ ConstLit () 3
          ]
        , stackAlloc = Map.fromList
          [ ("x", StackAlloc
            { stackAllocInfo = ()
            , params = []
            , expr = LitRV (ConstSizeof () "u8")
            })
          ]
        , children = Arr.fromList
          [ Scope
            { scopeInfo = ()
            , scopeLets = Arr.empty
            , stackAlloc = Map.fromList
              [ ("y", StackAlloc
                { stackAllocInfo = ()
                , params = []
                , expr = LitRV (ConstSizeof () "u8")
                })
              ]
            , blocks = Map.fromList
              [ ("_start", Block
                { blockInfo = ()
                , binds = []
                , stmts = Arr.fromList
                  [ BlockLet
                    { letInfo = ()
                    , var = "random"
                    , expr = ConstLit () 42
                    }
                  , Instr () [] "debug" [VarRV "x", VarRV "y", VarRV "random", VarRV "globalVar"]
                  , Instr () ["z"] "add8" [VarRV "random", LitRV $ ConstLit () 95]
                  , Instr () [] "debug" [VarRV "z", VarRV "yoyoyo"]
                  , Goto () "cometo" (Map.fromList $ [("alloca", [LitRV $ ConstLit () 4])]) [VarRV "z"]
                  ]
                })
              ]
            , children = Arr.empty
            }
          , Scope
            { scopeInfo = ()
            , scopeLets = Arr.empty
            , stackAlloc = Map.fromList
              [ ( "alloca", StackAlloc
                { stackAllocInfo = ()
                , params = ["n"]
                , expr = ScaleRV 8 (LitRV $ ConstVar () "n")
                })
              , ( "y", StackAlloc () [] $ LitRV $ ConstLit () 8)
              ]
            , children = Arr.empty
            , blocks = Map.fromList
              [ ("cometo", Block
                { blockInfo = ()
                , binds = [("someRegister", "u8")]
                , stmts = Arr.fromList
                  [ Instr () [] "debug" [VarRV "someRegister", VarRV "alloca", VarRV "y"]
                  , Instr () [] "exit" [LitRV (ConstLit () 0)]
                  ]
                })
              ]
            }
          ]
        , blocks = Map.empty
        }
      ]
    }
  ]

conf :: Conf
conf = Conf
  { program = prog
  , programEntry = "_start"
  , maxMemory = 1024 * 1024 * 1024
  }

main :: IO ()
main = do
  stop <- exec conf
  case stop of
    Exit 0 -> exitSuccess
    Exit i -> exitWith (ExitFailure (fromIntegral i))
    _ -> do
      hPutStrLn stderr (show stop)
      exitFailure
