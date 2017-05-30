{-# LANGUAGE DeriveGeneric #-}

module Ast where

import Data.Text
import Data.String (IsString(..))
import Data.Scientific
import GHC.Generics (Generic)
import Data.Hashable

newtype Identifier = Identifier Text
                     deriving (Show, Eq, Generic)
instance Hashable Identifier

getIdentifier (Identifier a) = a

instance IsString Identifier where
  fromString s = Identifier (pack s)

data Expr = EVar Identifier
          | ENumber Scientific
          | EHex Integer
          | EString String
          | EBool Bool
          | EFunApp Identifier [Expr]
          | EOpApp Identifier Expr Expr
          | EList [Expr]
          | ENeg Expr
          | EAssign Expr Expr
          deriving Show

data Stmt = Seq [Stmt]
          | FunDef Identifier [Identifier] (Maybe Stmt)
          | OpDef Assoc Identifier Integer Identifier Identifier (Maybe Stmt)
          | Expression Expr
          | If Expr Stmt (Maybe Stmt)
          | For Expr Expr Expr Stmt
          deriving Show

data Assoc = AssocLeft | AssocRight | AssocNeutral
           deriving (Eq, Ord, Show)
