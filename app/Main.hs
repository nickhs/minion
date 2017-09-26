{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text.IO as DT (readFile)
import Data.Text.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text as DT
import qualified Shelly
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, liftIO, runReaderT, ask)
import GHC.Generics (Generic)
import Control.Monad (unless, when)
import qualified Options.Applicative as Optparse
import Options.Applicative ((<**>))
import Data.Semigroup ((<>))
import System.Directory (setCurrentDirectory)
import Control.Lens (view, (^.), set, makeFieldsNoPrefix)

import Module (Actionable(..), Action(..), Result(..), DidUpdate(..))
import Module.File (FileModule(..), FileStates(..))
import Module.Copy (CopyModule)
import qualified Module.Copy as CopyModule (CopyModule(CopyModule), src, dest)
import qualified Env
import qualified Log (LogMsgType(..))
import qualified Task
import PrivEsc (PrivEsc)
import qualified PrivEsc
import qualified IntermediateEnv

import Config.Main (root)

import Debug.Trace

logger :: (MonadIO m) => Log.LogMsgType -> Text -> m ()
logger msgType msg = liftIO $ putStrLn $ msgName ++ ": " ++ padding ++ DT.unpack msg
    where msgName = show msgType
          padding = concat $ replicate (6 - length msgName) " "

shellyWrapper :: (Shelly.FilePath -> [Text] -> Shelly.Sh Text)
              -> Maybe PrivEsc -> Env.Env -> FilePath -> [Text] -> IO Text
shellyWrapper executer maybePrivChange e filepath cmd = do
    let log = e ^. Env.pLog
    let cmds = DT.pack filepath : cmd
    newCmds <- case maybePrivChange of
        Just privChange -> return $ [DT.pack $ privChange ^. PrivEsc.executable, "-u", privChange ^. PrivEsc.username] ++ privChange ^. PrivEsc.args ++ cmds
        Nothing -> return cmds
    liftIO $ log Log.Cmdin $ DT.pack $ show filepath ++ " " ++ show newCmds
    let configuredExecutor = Shelly.errExit True $ Shelly.escaping False $ Shelly.silently $ executer (Shelly.fromText $ head newCmds) (tail newCmds)
    (exitCode, stderr, stdout) <- liftIO $ Shelly.shelly $ do
        stdout <- configuredExecutor
        exitCode <- Shelly.lastExitCode
        stderr <- Shelly.lastStderr
        return (exitCode, stderr, stdout)
    when (exitCode /= 0) (liftIO $ log Log.Error $ "result: " `DT.append` (DT.pack . show) exitCode)
    unless (DT.null stdout) (liftIO $ log Log.Stdout stdout)
    unless (DT.null stderr) (liftIO $ log Log.Stderr stderr)
    return stdout

shellySSHPairsWrapper :: ([(Shelly.FilePath, [Text])] -> Shelly.Sh Text) -> Shelly.FilePath -> [Text] -> Shelly.Sh Text
shellySSHPairsWrapper remoteExec fp args = remoteExec [(fp, args)]

convertStupidPaths :: FilePath -> Shelly.FilePath
convertStupidPaths = Shelly.fromText . DT.pack

exec :: (MonadIO m) => IntermediateEnv.IntermediateEnv -> Task.Task -> m (Result DidUpdate)
exec env task = do
    let log = env ^. IntermediateEnv.pLog
    let mod = view Task.action task
    let privEsc = task ^. Task.become
    let taskEnv = Env.Env {
        Env._pLog = log,
        Env._remoteHost = env ^. IntermediateEnv.remoteHost,
        Env._remoteExec = (env ^. IntermediateEnv.remoteExec) privEsc,
        Env._localExec = (env ^. IntermediateEnv.localExec) Nothing
    }

    liftIO $ log Log.Info "-----------------------------"
    liftIO $ log Log.Info $ Module.describe mod
    runReaderT (Module.execute mod) taskEnv

data Opts = Opts {
    optHostname :: String,
    optPort :: Int,
    optWorkDir :: FilePath
} deriving (Show, Eq)

getOpts :: Optparse.Parser Opts
getOpts = Opts
    <$> Optparse.strOption
        (  Optparse.long "hostname" )
    <*> Optparse.option Optparse.auto
        (  Optparse.long "port"
        <> Optparse.showDefault
        <> Optparse.value 22 )
    <*> Optparse.strOption
        (  Optparse.long "work-dir"
        <> Optparse.showDefault
        <> Optparse.value "./" )

createEnv :: Env.SSHHost -> IntermediateEnv.IntermediateEnv
createEnv remote = IntermediateEnv.IntermediateEnv {
    IntermediateEnv._pLog = logger,
    IntermediateEnv._localExec = shellyWrapper Shelly.bash,
    IntermediateEnv._remoteExec = shellyWrapper (shellySSHPairsWrapper (Shelly.sshPairs (view Env.hostname remote))),
    IntermediateEnv._remoteHost = remote
}

main :: IO ()
main = do
    let o = Optparse.info (getOpts <**> Optparse.helper)
            (  Optparse.fullDesc
            <> Optparse.progDesc "Nick's bad ansible clone"
            <> Optparse.header "something else" )

    opts <- Optparse.execParser o

    logger Log.Debug (DT.pack (show opts))

    setCurrentDirectory (optWorkDir opts)

    let remote = Env.SSHHost {
        Env._hostname = DT.pack $ optHostname opts,
        Env._port = optPort opts
    }

    mapM_ (exec (createEnv remote)) root
    return ()
