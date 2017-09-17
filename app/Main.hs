{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text.IO as DT (readFile)
import qualified Dhall (input, auto)
import Data.Text.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text as DT (unpack, pack)
import qualified Shelly
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Reader (liftIO, MonadReader(ask), runReaderT)

import Module (Module(execute), Env(..), LogMsgType(..), SSHHost(..))
import Module.File (FileModule(..), FileStates(..))
import Module.Copy (CopyModule(..))

logger :: LogMsgType -> Text -> IO ()
logger msgType msg = putStrLn $
    show msgType ++ ": " ++ DT.unpack msg

shellyWrapper :: (MonadReader Env m, MonadIO m) => (Shelly.FilePath -> [Text] -> Shelly.Sh Text) -> FilePath -> [Text] -> m Text
shellyWrapper executer filepath cmd = do
    log <- envLog <$> ask
    liftIO $ log Cmdin $ DT.pack $ show filepath ++ " " ++ show cmd
    result <- Shelly.shelly $ Shelly.silently $ executer (convertStupidPaths filepath) cmd
    liftIO $ log Stdout result
    return result

shellySSHPairsWrapper :: ([(Shelly.FilePath, [Text])] -> Shelly.Sh Text) -> Shelly.FilePath -> [Text] -> Shelly.Sh Text
shellySSHPairsWrapper remoteExec fp args = remoteExec [(fp, args)]

convertStupidPaths :: FilePath -> Shelly.FilePath
convertStupidPaths = Shelly.fromText . DT.pack

main :: IO ()
main = do
    {--
    -- read all dhall files
    contents <- DT.readFile "./test.min.dhall"
    parsed <- Dhall.input Dhall.auto (fromStrict contents) :: IO [Integer]
    --}

    let x = FileModule {
        path = "/bin/ls",
        state = Absent
    }

    let remote = SSHHost {
        hostname = "162.243.29.116",
        port = 22
    }

    let env = Env {
        envLog = logger,
        envRemoteExec = shellyWrapper (shellySSHPairsWrapper (Shelly.sshPairs (hostname remote))),
        envLocalExec = shellyWrapper Shelly.bash,
        envRemoteHost = remote
    }

    let x2 = CopyModule {
        src = "~/crap.txt",
        dest = "~/crap.txt"
    }

    runReaderT (execute x2) env
    return ()
