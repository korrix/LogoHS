module Lib
    ( logoMain
    ) where

import qualified Data.Text.IO as T

import Parser
import Text.Megaparsec (runParserT, parseErrorPretty)
import Control.Monad.State (runStateT)
import Data.Functor.Identity (runIdentity)

import Runtime

logoMain :: IO ()
logoMain = do
  let filename = "advanced.logo"
  file <- T.readFile "advanced.logo"
  let state  = runParserT logoParser filename file
  let result = runIdentity $ runStateT state initialScope

  case fst result of
    Left  e -> putStr (parseErrorPretty e)
    Right ast -> runTurtle ast

