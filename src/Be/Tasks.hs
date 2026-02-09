{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Tasks
  ( -- * Utility functions
    readFile
  , exec

    -- * Sandbox helpers
  , sandboxConfig
  , fourmoluProfile
  , nixfmtProfile
  , hlintProfile

    -- * Tasks
  , GitRoot (..)
  , Which (..)
  , ChangedFiles (..)
  , ChangedHaskellFiles (..)
  , ChangedNixFiles (..)
  )
where

import Beget.Build
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LByteString
import Data.String.Interpolate (__i)
import Prelude hiding (readFile, stderr, stdout)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension)
import System.Info (os)
import System.Process.Typed qualified as Process
import UnliftIO.Exception (throwIO)
import UnliftIO.IO qualified as IO

readFile :: MonadIO m => FilePath -> m LByteString
readFile path = liftIO do
  IO.withBinaryFile path IO.ReadMode \handle ->
    LByteString.hGetContents handle

exec :: MonadIO m => Process.ProcessConfig () () () -> m LByteString
exec config = do
  (exitCode, stdout, stderr) <- Process.readProcess config
  when (exitCode /= ExitSuccess) do
    throwIO $
      Process.ExitCodeException
        { Process.eceExitCode = exitCode
        , Process.eceProcessConfig = config
        , Process.eceStdout = stdout
        , Process.eceStderr = stderr
        }
  pure stdout

sandboxConfig :: String -> FilePath -> [String] -> Process.ProcessConfig () () ()
sandboxConfig profile binary args =
  if os == "darwin" then
    Process.proc "/usr/bin/sandbox-exec" (["-p", profile, "--", binary] <> args)
      & Process.setEnv []
      & Process.setWorkingDir "/var/empty"
  else
    Process.proc binary args
      & Process.setEnv []
      & Process.setWorkingDir "/var/empty"

fourmoluProfile :: String
fourmoluProfile = [__i|
  (version 1)
  (deny default)
  (allow process-exec*
    (regex \#"^/nix/store/[a-z0-9]+-fourmolu-[^/]+/bin/fourmolu$"))
  (allow file-read*)
  (deny file-read*
    (subpath "/Users"))
|]

nixfmtProfile :: String
nixfmtProfile = [__i|
  (version 1)
  (deny default)
  (allow process-exec*
    (regex \#"^/nix/store/[a-z0-9]+-nixfmt-[^/]+/bin/nixfmt$"))
  (allow file-read*)
  (deny file-read*
    (subpath "/Users"))
|]

-- TODO: Lock this down further
hlintProfile :: String
hlintProfile = [__i|
  (version 1)
  (allow default)
  (deny file-read*
    (subpath "/Users"))
|]

gitRoot :: Build FilePath
gitRoot = do
  pathBytes <- exec (Process.proc "git" ["rev-parse", "--show-toplevel"])
  let path = decodeUtf8 (Char8.strip (toStrict pathBytes))
  pure path

registerTaskVolatile 'gitRoot

which :: String -> Build FilePath
which name = do
  initialPathBytes <- exec (Process.proc "which" [name])
  let initialPath = decodeUtf8 (Char8.strip (toStrict initialPathBytes))
  realPathBytes <- exec (Process.proc "realpath" [initialPath])
  let realPath = decodeUtf8 (Char8.strip (toStrict realPathBytes))
  pure realPath

registerTaskVolatile 'which

changedFiles :: [FilePath] -> Build [FilePath]
changedFiles dirs = do
  root <- realize GitRoot
  git <- realize Which "git"
  trackedBytes <- exec (Process.proc git (["-C", root, "diff", "--diff-filter=dt", "--name-only", "--merge-base", "origin/master", "--"] <> dirs))
  untrackedBytes <- exec (Process.proc git (["-C", root, "ls-files", "--others", "--exclude-standard", "--"] <> dirs))
  let parse bs =
        let text = decodeUtf8 (Char8.strip (toStrict bs)) :: Text
            ls = lines text :: [Text]
        in map toString (filter (\l -> l /= "") ls)
  pure (parse trackedBytes <> parse untrackedBytes)

registerTaskVolatile 'changedFiles

changedHaskellFiles :: Build [FilePath]
changedHaskellFiles = do
  files <- realize ChangedFiles ["src/", "test/", "local-packages/", "nix/packages/mercury/"]
  pure (filter (\f -> takeExtension f == ".hs") files)

registerTaskVolatile 'changedHaskellFiles

changedNixFiles :: Build [FilePath]
changedNixFiles = do
  files <- realize ChangedFiles ["."]
  pure (filter (\f -> takeExtension f == ".nix") files)

registerTaskVolatile 'changedNixFiles
