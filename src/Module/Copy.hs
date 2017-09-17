module Module.Copy (
    CopyModule(..)
) where

import Module.File (FileProperties(FileProperties))
import Module (Module, Result(..), DidUpdate(..), execute, Env(..), SSHHost(..))
import Control.Monad.Reader (MonadReader(ask), liftIO)
import qualified Data.Text as DT

type LocalFilePath = DT.Text
type RemoteFilePath = DT.Text

data PropertiesMode = InheritFromSrc | UseProperties FileProperties

data CopyModule = CopyModule {
    src :: LocalFilePath,
    dest :: RemoteFilePath,
    properties :: PropertiesMode
}

instance Module CopyModule where
    execute self = do
        remoteHost <- envRemoteHost <$> ask
        localExec <- envLocalExec <$> ask
        let path = hostname remoteHost `DT.append` ":" `DT.append` dest self
        result <- localExec "scp" [src self, path]
        -- FIXME(hanley): check properties
        return $ Success ChangeMade
