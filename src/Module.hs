{-# LANGUAGE FlexibleContexts #-}

-- FIXME(nickhs): rename to types or split up

module Module (
  Result(..),
  Action(..),
  DidUpdate(..),
  unimplemented,
  describe,
  Actionable(..),
) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Lens (makeClassy)

import Env (HasRemoteExec, HasLocalExec, ExecFunc, SSHHost, HasRemoteHost, Env)

data Result a = Success a | Warning [Text] a | Failed [Text] a
data DidUpdate = ChangeMade | NoChange

data ExecutionResult = ExecutionResult {
  stderr :: [Text],
  stdout :: [Text],
  returnCode :: Int
} deriving (Show, Eq, Ord)

data Action = Action {
  -- FIXME(hanley): should consider making a check and postCheck method,
  execute :: forall m. (MonadReader Env m, MonadIO m) => m (Result DidUpdate),
  describe :: Text
}
-- makeClassy ''Action

-- this class name is awful
class Actionable a where
    toAction :: a -> Action

unimplemented = Failed ["not implemented yet"] NoChange
