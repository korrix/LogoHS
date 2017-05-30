{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Types ( module Types
             , module Ast
             , module Data.Scientific
             ) where

import Data.Scientific
import Control.Monad.RWS.Lazy
import Control.Monad.Except

import qualified Data.HashMap.Strict as HM
import Graphics.X11.Turtle
import Lens.Micro.TH

import Ast (Identifier)

type Computation = RWST Turtle (IO ()) Scope (Except RuntimeError) Value

data Value = VNumber Scientific
           | VString String
           | VBool Bool
           | VList [Value]
           | VNull
           deriving Show


data RuntimeError = RuntimeError
                  | TypeMismatch
                  | VariableNotInScope Identifier
                  | FunctionNotInScope Identifier
                  | NativeFunctionDoesNotExists Identifier
                  | InvalidAssigmentLvalue
                  deriving Show

data Function = Function { argNames :: [Identifier]
                         , runFunction :: Computation
                         }

instance Show Function where
  show _ = "[function]"

instance Show Turtle where
  show _ = "[turtle]"

data Scope = Scope { _parentScope :: Maybe Scope
                   , _variables :: HM.HashMap Identifier Value
                   , _functions :: HM.HashMap Identifier Function
                   } deriving Show

scopeLookup id getter error scope = case HM.lookup id (getter scope) of
  Just v -> return $ v
  Nothing -> case _parentScope scope of
    Just scope' -> scopeLookup id getter error scope'
    Nothing -> throwError $ error id

lookupVariable id = get >>= scopeLookup id _variables VariableNotInScope
lookupFunction id = get >>= scopeLookup id _functions FunctionNotInScope

makeLenses ''Scope
