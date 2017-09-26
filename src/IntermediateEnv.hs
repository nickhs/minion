module IntermediateEnv (
    IntermediateEnv(..),
    pLog,
    localExec,
    remoteExec,
    remoteHost,
) where

import Data.Text (Text)
import Control.Lens (makeLenses)

import qualified Env
import PrivEsc (PrivEsc)

data IntermediateEnv = IntermediateEnv {
    _pLog :: Env.LogFunc,
    _localExec :: Maybe PrivEsc -> Env.Env -> FilePath -> [Text] -> IO Text,
    _remoteExec :: Maybe PrivEsc -> Env.Env -> FilePath -> [Text] -> IO Text,
    _remoteHost :: Env.SSHHost
}
makeLenses ''IntermediateEnv
