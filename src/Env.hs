{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Env (
    Env(..),
    hostname,
    port,
    pLog,
    remoteExec,
    localExec,
    remoteHost,
    ExecFunc,
    SSHHost(..),
    LogFunc,
    HasRemoteExec,
    HasLocalExec,
    HasPLog,
    HasRemoteHost,
) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Lens (makeLenses, makeFieldsNoPrefix, makeClassy, view)

import Log (LogMsgType)
import PrivEsc (PrivEsc)

type LogFunc = LogMsgType -> Text -> IO ()

data SSHHost = SSHHost {
  _hostname :: Text,
  _port :: Int
} deriving (Show, Eq, Ord)
makeLenses ''SSHHost

data Env = Env {
  _pLog :: LogFunc,
  _remoteHost :: SSHHost,
  _remoteExec :: Env -> FilePath -> [Text] -> IO Text,
  _localExec :: Env -> FilePath -> [Text] -> IO Text
}
makeFieldsNoPrefix ''Env

type ExecFunc = forall m. Env -> FilePath -> [Text] -> m Text

{-
class HasLocalExec a where
    localExec :: a -> ExecFunc
class HasRemoteExec a where
    remoteExec :: a -> RemoteExecFunc

instance HasRemoteExec Env where
    remoteExec a = _remoteExec (view executors a)
instance HasLocalExec Env where
    localExec a = _localExec (view executors a)
-}
