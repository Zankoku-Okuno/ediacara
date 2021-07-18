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

prog :: Prog Frontend
prog = Prog () $ Arr.fromList
  [ DeclProc Proc
    { procInfo = ()
    , entries = Map.fromList [("_start", "_start")]
    , exits = Map.empty
    , scopes = Arr.fromList
      [ Scope
        { scopeInfo = ()
        , stackAlloc = Map.fromList
          [ ("x", StackAlloc
            { stackAllocInfo = ()
            , params = Arr.empty
            , expr = LitRV (ConstSizeof () "u8")
            })
          ]
        , children = Arr.fromList
          [ Scope
            { scopeInfo = ()
            , stackAlloc = Map.fromList
              [ ("y", StackAlloc
                { stackAllocInfo = ()
                , params = Arr.empty
                , expr = LitRV (ConstSizeof () "u8")
                })
              ]
            , blocks = Map.fromList
              [ ("_start", Block
                { blockInfo = ()
                , binds = Map.empty
                , stmts = Arr.fromList
                  [ Let
                    { letInfo = ()
                    , var = "random"
                    , expr = ConstLit () 42
                    }
                  , Instr () [] "debug" [VarRV "x", VarRV "y", VarRV "random"]
                  , Instr () ["z"] "add8" [VarRV "random", LitRV $ ConstLit () 95]
                  , Instr () [] "debug" [VarRV "z"]
                  , Instr () [] "exit" [LitRV (ConstLit () 0)]
                  ]
                })
              ]
            , children = Arr.empty
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
