{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Test.Logs.Tests
  ( tests
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVarIO)
import           Control.Monad (filterM)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Directory
import           System.FilePath

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log (isItLog, isItSymLink)
import           Cardano.Tracer.Run (runCardanoTracerWithConfigBrakes)

import           Cardano.Tracer.Test.Forwarder (launchForwardersSimple)

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Logs.File"
  [ testProperty ".log"  $ propFile ForHuman   "text" "cardano-tracer-log.sock"
  , testProperty ".json" $ propFile ForMachine "json" "cardano-tracer-json.sock"
  ]

propFile
  :: LogFormat
  -> FilePath
  -> String
  -> Property
propFile format suffix localSockName = ioProperty $ do
  tmpDir <- getTemporaryDirectory
  let rootDir = tmpDir </> ("test-logs-" <> suffix)
      localSock = tmpDir </> localSockName
  -- Remove old paths if needed.
  removePathForcibly rootDir
  removePathForcibly localSock

  stopEKG <- newTVarIO False
  stopTF  <- newTVarIO False
  withAsync (runCardanoTracerWithConfigBrakes (config rootDir localSock) [(stopEKG, stopTF)]) $ \_ ->
    withAsync (launchForwardersSimple localSock) $ \_ -> do
      threadDelay 15000000 -- Wait till some rotation is done.
      atomically $ do
        modifyTVar' stopEKG . const $ True
        modifyTVar' stopTF  . const $ True
      threadDelay 1000000

  doesDirectoryExist rootDir >>= \case
    True ->
      -- ... and contains one node's subdir...
      listDirectory rootDir >>= \case
        []  -> false "root dir is empty"
        [subDir] ->
          withCurrentDirectory rootDir $
            -- ... with *.log-files inside...
            listDirectory subDir >>= \case
              [] -> false "subdir is empty"
              logsAndSymLink ->
                withCurrentDirectory subDir $
                  case filter (isItLog format) logsAndSymLink of
                    [] -> false "subdir doesn't contain expected logs"
                    logsWeNeed ->
                      if length logsWeNeed > 1
                        then
                          -- ... and one symlink...
                          filterM (isItSymLink format) logsAndSymLink >>= \case
                            [] -> false "subdir doesn't contain a symlink"
                            [symLink] -> do
                              -- ... to the latest *.log-file.
                              maybeLatestLog <- getSymbolicLinkTarget symLink
                              -- The logs' names contain timestamps, so the
                              -- latest log is the maximum one.
                              let latestLog = maximum logsWeNeed
                              return $ latestLog === maybeLatestLog
                            _ -> false "there is more than one symlink"
                        else false "there is still 1 single log, no rotation"
        _ -> false "root dir contains more than one subdir"
    False -> false "root dir doesn't exist"
 where
  config root p = TracerConfig
    { connectMode    = Initiator
    , acceptAt       = [LocalSocket p]
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = [LoggingParams root FileMode format]
    , rotation       = Just $ RotationParams
                         { rpLogLimitBytes = 100
                         , rpMaxAgeHours   = 1
                         , rpKeepFilesNum  = 10
                         }
    }

  false :: String -> IO Property
  false msg = return . counterexample msg $ property False
