{-# LANGUAGE MultiWayIf #-}

module Module.File (
    FileStates(..),
    FileModule(..),
    FileProperties(FileProperties)
) where

import Control.Monad.Reader (MonadReader(ask), MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens ((^.), view)
import Shelly (shelly)
import Data.Text (Text)
import qualified Data.Text as DT

import Env(HasRemoteExec, ExecFunc, remoteExec, Env)
import Module (Action(..), Actionable(..), Result(..), DidUpdate(..), unimplemented)

data FileStates = Absent | File FileProperties deriving (Show, Eq, Ord)

data FileProperties = FileProperties {
    owner :: Text,
    group :: Text,
    mode :: Text
} deriving (Show, Eq, Ord)

data FileModule = FileModule {
    path :: DT.Text,
    state :: FileStates
} deriving (Show, Eq, Ord)
instance Actionable FileModule where
    toAction self = Action {
        execute = file self,
        describe = describe' self
    }

describe' :: FileModule -> Text
describe' (FileModule path' Absent) = "Deleting file " `DT.append` path'
describe' (FileModule path' (File _)) = "Changing file perms" `DT.append` path'

file :: (MonadReader Env m, MonadIO m) => FileModule -> m (Result DidUpdate)
file self = do
    env <- ask
    let exec = view remoteExec env
    result <- liftIO $ exec env "ls" ["-lp --time-style=\"+%s\"", path self]

    -- parse
    let lsItems = map parseLs (DT.lines result)
    -- find matching items
    let matches = filter (\x -> name x == path self) lsItems

    case state self of
        Absent -> processAbsent matches
        File properties -> processFile properties matches

processAbsent :: (MonadReader Env m, MonadIO m) => [LsLine] -> m (Result DidUpdate)
processAbsent lines
    | num == 0 = return $ Success NoChange
    | num == 1 = do
        env <- ask
        let exec = view remoteExec env
        result <- liftIO $ exec env "rm" [name (head lines)]
        -- FIXME(nickhs): make sure we actually deleted it
        return $ Success ChangeMade
    | num > 1 = return $ Failed ["too many items", (DT.pack . show) lines] NoChange
    where num = length lines

processFile :: (MonadReader Env m, MonadIO m) => FileProperties -> [LsLine] -> m (Result DidUpdate)
processFile properties lines
    | num == 0 = return $ Failed ["no file found?"] NoChange
    | num == 1 = do
        let line = head lines
        env <- ask
        exec <- view remoteExec <$> ask
        -- fuck it, just chmod and chown it and we're done.
        let chownStr = owner properties `DT.append` ":" `DT.append` group properties
        _ <- liftIO $ exec env "chown" [chownStr, name line]
        _ <- liftIO $ exec env "chmod" [mode properties, name line]
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
