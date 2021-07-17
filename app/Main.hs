{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.PortAsm.Syntax.Extensible

import Language.PortAsm.Interpreter (Conf(..), exec)
import Language.PortAsm.Syntax.Frontend (Frontend)
import System.Exit (exitSuccess, exitFailure)

import qualified Data.Map.Strict as Map
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Text as T

prog :: Prog Frontend
prog = Prog () $ Arr.fromList
  [ DeclProc Proc
    { procInfo = ()
    , entries = Map.fromList [("_start", "_start")]
    , exits = Map.empty
    , scopes = Arr.fromList
      [ Scope
        { scopeInfo = ()
        , children = Arr.empty
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
              , Instr () [] "debug" [VarRV "random"]
              , Instr () [] "exit" [LitRV (ConstLit () 0)]
              ]
            })
          ]
        }
      ]
    }
  ]

conf :: Conf
conf = Conf
  { program = prog
  , programEntry = "_start"
  }

main :: IO ()
main = do
  stop <- exec conf
  case stop of
    "terminated normally" -> exitSuccess
    _ -> do
      putStrLn $ T.unpack stop
      exitFailure
