{-# LANGUAGE FlexibleContexts #-}

module Module (
  Error,
  Result(..),
  Module(..),
  DidUpdate(..),
  unimplemented,
  Env(..),
  LogMsgType(..),
  SSHHost(..)
) where

import Data.Text as Text
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)

type Error = Text

data Result a = Success a | Warning [Text] a | Failed [Text] a
data DidUpdate = ChangeMade | NoChange

data LogMsgType =
  Debug |
  Info |
  Warn |
  Error |
  Cmdin |
  Stdin |
  Stdout |
  Stderr deriving (Show, Eq, Ord)

data Env = Env {
  envLog :: LogMsgType -> Text -> IO (),
  envRemoteExec :: forall m. (MonadReader Env m, MonadIO m) => FilePath -> [Text] -> m Text,
  envLocalExec :: forall m. (MonadReader Env m, MonadIO m) => FilePath -> [Text] -> m Text,
  envRemoteHost :: SSHHost
}

data SSHHost = SSHHost {
  hostname :: Text,
  port :: Int
}

class Module a where
  -- FIXME(hanley): should consider making a check and postCheck method,
  execute :: (MonadReader Env m, MonadIO m) => a -> m (Result DidUpdate)

unimplemented = Failed ["not implemented yet"] NoChange
