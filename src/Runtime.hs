{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Runtime where

import Ast

import Control.Monad.RWS.Lazy
import Control.Monad.Except

import qualified Data.HashMap.Strict as HM

import Lens.Micro.Mtl

import Graphics.X11.Turtle

import Types
import Stdlib

runExpression :: Expr -> Computation
runExpression = \case
  EVar id -> lookupVariable id
  ENumber n -> return $ VNumber n
  EHex i -> return $ VNumber (fromIntegral i)
  EString s -> return $ VString s
  EBool b -> return $ VBool b
  EFunApp fun argexps -> do
    fs <- use functions
    Function argnames funBody <- lookupFunction fun
    withFreshScope $ do
      forM_ (zip argnames argexps) $ \(anam, aval) -> do
        val <- runExpression aval
        variables %= HM.insert anam val
      funBody
  EOpApp op larg rarg -> runExpression $ EFunApp op [larg, rarg]
  EList exprs -> VList <$> mapM runExpression exprs
  ENeg expr -> runExpression expr >>= \case
    VNumber n -> return $ VNumber (negate n)
    VBool b -> return $ VBool (not b)
    _ -> throwError TypeMismatch
  EAssign lexpr rexpr -> case lexpr of
    EVar id -> do
      r <- runExpression rexpr
      variables %= HM.insert id r
      return r
    _ -> throwError InvalidAssigmentLvalue


runProgram :: Stmt -> Computation
runProgram = \case
  Seq [st] -> runProgram st
  Seq (st:sts) -> runProgram st >> runProgram (Seq sts)
  Seq [] -> return VNull

  FunDef id args body -> do
    let function = Function args $ case body of
          Just b -> runProgram b
          Nothing -> case HM.lookup id logoStdlib of
            Just b -> b
            Nothing -> throwError $ NativeFunctionDoesNotExists id
    functions %= HM.insert id function
    return VNull

  OpDef _ id _ lval rval body -> runProgram $ FunDef id [lval, rval] body

  Expression e -> runExpression e
  If cond ifthen ifelse -> withFreshScope $ do
    r <- runExpression cond
    case r of
      VBool True  -> runProgram ifthen
      VBool False -> case ifelse of
        Just els -> runProgram els
        Nothing  -> return VNull
      _  -> throwError TypeMismatch

  For preexpr cond postexpr body -> withFreshScope $ do
    runExpression preexpr
    let action final = do
          r <- runExpression cond
          case r of
            VBool True -> do
              res <- runProgram body
              runExpression postexpr
              action res
            VBool False -> return final
            _           -> throwError TypeMismatch
    action VNull

withFreshScope :: MonadState Scope m => m b -> m b
withFreshScope action = do
  currentScope <- get
  put $ Scope (Just currentScope) HM.empty HM.empty
  res <- action
  put currentScope
  return res

runTurtle :: Stmt -> IO ()
runTurtle ast = do
  f <- openField
  turtle <- newTurtle f
  let rootScope = Scope Nothing HM.empty HM.empty
  case runExcept (evalRWST (runProgram ast) turtle rootScope) of
    Right (val, io) -> io >> print val
    Left err -> print err
