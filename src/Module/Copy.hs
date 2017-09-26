module Module.Copy (
    CopyModule(..),
    PropertiesMode(..)
) where

import Module.File (FileProperties(FileProperties))
import Module (Action(..), Actionable(..), Result(..), DidUpdate(..))
import Env(HasLocalExec, HasRemoteHost, ExecFunc, Env(..), SSHHost(..), localExec, remoteHost, hostname, port, remoteExec)

import Control.Monad.Reader (MonadReader(ask), liftIO)
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DT (toStrict)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO)
import Data.Text.Format (format)
import Control.Lens (view, (^.))
import System.Random (randomRIO)

type LocalFilePath = DT.Text
type RemoteFilePath = DT.Text

data PropertiesMode = InheritFromSrc | UseProperties FileProperties deriving (Show, Generic, Eq, Ord)

data CopyModule = CopyModule {
    src :: LocalFilePath,
    dest :: RemoteFilePath,
    properties :: PropertiesMode
} deriving (Generic, Show, Eq, Ord)
instance Actionable CopyModule where
    toAction self = Action {
        execute = cp self,
        describe = DT.toStrict $ format "Copying {} to {}" (src self, dest self)
    }


cp :: (MonadReader Env m, MonadIO m) => CopyModule -> m (Result DidUpdate)
cp self = do
    env <- ask
    remoteHost <- view remoteHost <$> ask
    localExec <- view localExec <$> ask
    remoteExec <- view remoteExec <$> ask
    randNum <- liftIO $ (DT.pack . show) <$> (randomRIO (0, 9999999) :: IO Integer)
    let tempPath = "/tmp/minion" `DT.append` randNum
    let tempScpPath = (remoteHost ^. hostname) `DT.append` ":" `DT.append` tempPath
    result <- liftIO $ localExec env "scp" ["-r", src self, tempScpPath]
    _ <- liftIO $ remoteExec env "cp" [tempPath, dest self]
    -- FIXME(hanley): check properties
    -- FIXME(hanley): delete file in tmp
    return $ Success ChangeMade
