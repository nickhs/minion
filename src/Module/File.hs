{-# LANGUAGE MultiWayIf #-}

module Module.File (
    FileStates(..),
    FileModule(..),
    FileProperties(FileProperties)
) where

import Module (Module, Result(..), DidUpdate(..), LogMsgType(..), execute, unimplemented, Env(..))

import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Shelly (shelly)
import Data.Text (Text)
import qualified Data.Text as DT

data FileStates = Absent | File FileProperties

data FileProperties = FileProperties {
    owner :: Text,
    group :: Text,
    mode :: Text
}

data FileModule = FileModule {
    path :: FilePath,
    state :: FileStates
}

instance Module FileModule where
    execute self = do
        exec <- envRemoteExec <$> ask
        result <- exec "ls" ["-lp --time-style=\"+%s\"", DT.pack $ path self]

        -- parse
        let lsItems = map parseLs (DT.lines result)
        -- find matching items
        let matches = filter (\x -> name x == (DT.pack . path) self) lsItems

        case state self of
            Absent -> processAbsent matches
            File properties -> processFile properties matches

processAbsent :: (MonadReader Env m, MonadIO m) => [LsLine] -> m (Result DidUpdate)
processAbsent lines
    | num == 0 = return $ Success NoChange
    | num == 1 = do
        exec <- envRemoteExec <$> ask
        result <- exec "rm" [name (head lines)]
        -- FIXME(nickhs): make sure we actually deleted it
        return $ Success ChangeMade
    | num > 1 = return $ Failed ["too many items", (DT.pack . show) lines] NoChange
    where num = length lines

processFile :: (MonadReader Env m, MonadIO m) => FileProperties -> [LsLine] -> m (Result DidUpdate)
processFile properties lines
    | num == 0 = return $ Failed ["no file found?"] NoChange
    | num == 1 = do
        let line = head lines
        exec <- envRemoteExec <$> ask
        -- fuck it, just chmod and chown it and we're done.
        let chownStr = owner properties `DT.append` ":" `DT.append` group properties
        _ <- exec "chown" [chownStr, name line]
        _ <- exec "chmod" [mode properties, name line]
        -- FIXME(hanley): make sure it actually happened
        return $ Success ChangeMade
    | num > 1 = return $ Failed ["too many items", (DT.pack . show) lines] NoChange
    where num = length lines

data LsLine = LsLine {
    perms :: Text,
    numLinks :: Text,
    lsOwner :: Text,
    lsGroup :: Text,
    size :: Text,
    timestamp :: Text,
    name :: Text
} deriving (Show, Eq, Ord)

parseLs :: Text -> LsLine
parseLs line = do
    let result = DT.words line
    LsLine {
        perms = head result,
        numLinks = result !! 1,
        lsOwner = result !! 2,
        lsGroup = result !! 3,
        size = result !! 4,
        timestamp = result !! 5,
        name = result !! 6
    }
