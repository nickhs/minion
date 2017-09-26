module Module.Shell (
  shellExec,
  ShellModule(..)
) where

import Control.Monad.Reader (MonadReader(ask), liftIO, runReaderT)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as DT
import Data.Text (Text)
import Control.Lens (view)

import Module(Result(..), DidUpdate(..), Action(..), Actionable(..))
import Env(Env(..), HasRemoteExec, remoteExec, ExecFunc)

data ShellModule = ShellModule {
  exec :: (FilePath, [Text]),
  shell :: (FilePath, [Text])
}deriving(Show, Eq, Ord)

instance Actionable ShellModule where
  toAction self = Action {
    execute = shellExec' self,
    describe = "Calling " `DT.append` DT.pack (show (exec self))
  }

shellExec :: FilePath -> [Text] -> ShellModule
shellExec bin args = ShellModule {
  exec = (bin, args),
  shell = ("/bin/bash", ["-c"])
}

shellExec' :: (MonadReader Env m, MonadIO m) => ShellModule -> m (Result DidUpdate)
shellExec' (ShellModule (innerBin, innerArgs) (bin, args)) = do
    env <- ask
    remoteExec <- view remoteExec <$> ask
    let commandString = "" `DT.append` DT.intercalate " " (DT.pack innerBin : innerArgs) `DT.append` ""
    result <- liftIO $ remoteExec env bin (args ++ [commandString])
    return $ Success ChangeMade
