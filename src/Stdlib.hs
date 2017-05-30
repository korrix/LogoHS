{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Stdlib where

import Types

import Control.Monad.RWS.Lazy
import Control.Monad.Except

import qualified Data.HashMap.Strict as HM

import Graphics.X11.Turtle

logoStdlib :: HM.HashMap Identifier Computation
logoStdlib = HM.fromList [
    ("np", do
      lookupVariable "krok" >>= \case
        VNumber krok -> do
          turtle <- ask
          tell $ forward turtle (toRealFloat krok)
          return VNull
        _ -> throwError TypeMismatch
    ),
    ("pw", do
      lookupVariable "kąt" >>= \case
        VNumber angle -> do
          turtle <- ask
          tell $ right turtle (toRealFloat angle)
          return VNull
        _ -> throwError TypeMismatch
    ),
    ("*", do
      liftM2 (,) (lookupVariable "l") (lookupVariable "r") >>= \case
        (VNumber a, VNumber b) -> return $ VNumber (a * b)
        _ -> throwError TypeMismatch
    ),
    ("+", do
      liftM2 (,) (lookupVariable "l") (lookupVariable "r") >>= \case
        (VNumber a, VNumber b) -> return $ VNumber (a + b)
        _ -> throwError TypeMismatch
    ),
    ("<", do
      liftM2 (,) (lookupVariable "l") (lookupVariable "r") >>= \case
        (VNumber a, VNumber b) -> return $ VBool (a < b)
        _ -> throwError TypeMismatch
    )
  ]
