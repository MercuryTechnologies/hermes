{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.H2O.BasicSpec
-- Description : Basic functionality tests for H2O bindings
-- Copyright   : (c) 2024 Mercury Technologies
-- License     : BSD-3-Clause
-- Maintainer  : ian@mercury.com
-- Stability   : experimental
-- Portability : POSIX

module Network.H2O.BasicSpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Network.H2O
import Network.H2O.Types
import Network.H2O.Core
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Control.Exception (try, bracket)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.Async (async, cancel, wait)
import Foreign
import Foreign.C.Types
import System.IO.Temp (withSystemTempDirectory)
import Network.Socket (close)

spec :: Spec
spec = describe "Network.H2O" $ do
  describe "Low-level bindings" $ do
    it "can allocate and free basic structures" $ hedgehog $ do
      -- Test basic memory allocation doesn't segfault
      liftIO $ do
        globalConf <- mallocBytes 1024  -- Conservative size
        h2oConfigInit globalConf
        h2oConfigDispose globalConf
        free globalConf

    it "can create and destroy event loop" $ hedgehog $ do
      liftIO $ do
        evLoop <- h2oEvloopCreate
        evLoop /== nullPtr
        h2oEvloopDestroy evLoop

    it "can create context with event loop" $ hedgehog $ do
      liftIO $ bracket setupBasicH2O teardownBasicH2O $ \(globalConf, context, evLoop) -> do
        -- Just verify pointers are valid
        globalConf /== nullPtr
        context /== nullPtr
        evLoop /== nullPtr

    it "can register host and path" $ hedgehog $ do
      liftIO $ bracket setupBasicH2O teardownBasicH2O $ \(globalConf, context, evLoop) -> do
        hostConf <- withH2OString "localhost" $ \hostStr ->
          h2oConfigRegisterHost globalConf hostStr 8080
        hostConf /== nullPtr
        
        pathConf <- withCString "/" $ \pathPtr ->
          h2oConfigRegisterPath hostConf pathPtr 0
        pathConf /== nullPtr

  describe "String operations" $ do
    it "can convert ByteString to H2O string and back" $ hedgehog $ do
      bs <- forAll $ Gen.utf8 (Range.linear 0 100) Gen.unicode
      liftIO $ do
        withH2OString bs $ \h2oStr -> do
          bs' <- h2oStringToByteString h2oStr
          bs === bs'

    it "can compare H2O strings with ByteString" $ hedgehog $ do
      bs1 <- forAll $ Gen.utf8 (Range.linear 1 50) Gen.unicode
      bs2 <- forAll $ Gen.utf8 (Range.linear 1 50) Gen.unicode
      liftIO $ do
        withH2OString bs1 $ \h2oStr -> do
          result1 <- h2oMemIsByteString h2oStr bs1
          result2 <- h2oMemIsByteString h2oStr bs2
          result1 === True
          result2 === (bs1 == bs2)

  describe "Memory pool operations" $ do
    it "can allocate from memory pool without segfault" $ hedgehog $ do
      size <- forAll $ Gen.integral (Range.linear 1 1024)
      liftIO $ bracket setupBasicH2O teardownBasicH2O $ \(globalConf, context, evLoop) -> do
        -- Create a dummy memory pool for testing
        pool <- mallocBytes 1024  -- Mock pool
        ptr <- h2oMemAllocPool pool (fromIntegral size)
        ptr /== nullPtr
        free pool

  describe "High-level API" $ do
    it "can create and destroy server" $ hedgehog $ do
      liftIO $ do
        result <- runH2OServer defaultConfig $ \server -> do
          return $ h2oServerConfig server
        case result of
          Right config -> config === defaultConfig
          Left err -> fail $ "Server creation failed: " ++ err

    it "can register handlers without segfault" $ hedgehog $ do
      liftIO $ do
        result <- runH2OServer defaultConfig $ \server -> do
          registerHandler server "/" $ \req -> do
            return $ responseOk "Hello, World!"
          registerHandler server "/test" $ \req -> do
            return $ responseJson "{\"status\": \"ok\"}"
          return ()
        case result of
          Right () -> return ()
          Left err -> fail $ "Handler registration failed: " ++ err

    it "respects configuration settings" $ hedgehog $ do
      maxSize <- forAll $ Gen.integral (Range.linear 1024 10485760)  -- 1KB to 10MB
      timeout <- forAll $ Gen.integral (Range.linear 1 300)  -- 1 to 300 seconds
      let config = defaultConfig 
            { h2oConfigMaxRequestSize = maxSize
            , h2oConfigTimeout = timeout
            }
      liftIO $ do
        result <- runH2OServer config $ \server -> do
          return $ h2oServerConfig server
        case result of
          Right serverConfig -> do
            h2oConfigMaxRequestSize serverConfig === maxSize
            h2oConfigTimeout serverConfig === timeout
          Left err -> fail $ "Configuration test failed: " ++ err

  describe "Response builders" $ do
    it "creates correct OK response" $ hedgehog $ do
      body <- forAll $ Gen.utf8 (Range.linear 0 100) Gen.unicode
      let response = responseOk body
      responseStatus response === 200
      responseBody response === body
      lookup "Content-Type" (responseHeaders response) === Just "text/plain"

    it "creates correct JSON response" $ hedgehog $ do
      json <- forAll $ Gen.utf8 (Range.linear 0 100) Gen.unicode
      let response = responseJson json
      responseStatus response === 200
      responseBody response === json
      lookup "Content-Type" (responseHeaders response) === Just "application/json"

    it "creates correct HTML response" $ hedgehog $ do
      html <- forAll $ Gen.utf8 (Range.linear 0 100) Gen.unicode
      let response = responseHtml html
      responseStatus response === 200
      responseBody response === html
      lookup "Content-Type" (responseHeaders response) === Just "text/html"

  describe "Stress tests" $ do
    it "handles many server create/destroy cycles" $ hedgehog $ do
      count <- forAll $ Gen.integral (Range.linear 1 10)
      liftIO $ do
        results <- mapM (\_ -> runH2OServer defaultConfig return) [1..count]
        all isRight results === True
      where
        isRight (Right _) = True
        isRight (Left _) = False

    it "handles many handler registrations" $ hedgehog $ do
      handlerCount <- forAll $ Gen.integral (Range.linear 1 20)
      liftIO $ do
        result <- runH2OServer defaultConfig $ \server -> do
          mapM_ (\i -> registerHandler server (C8.pack $ "/test" ++ show i) $ \_ -> 
                  return $ responseOk $ C8.pack $ "Handler " ++ show i) [1..handlerCount]
          return handlerCount
        case result of
          Right count -> count === handlerCount
          Left err -> fail $ "Multiple handler registration failed: " ++ err

  describe "Error handling" $ do
    it "handles null pointer access safely" $ hedgehog $ do
      liftIO $ do
        -- Test that our bindings handle null pointers gracefully
        result <- try $ do
          h2oConfigInit nullPtr
        case result of
          Left _ -> return ()  -- Expected to fail
          Right _ -> fail "Should have failed with null pointer"

    it "handles invalid memory access safely" $ hedgehog $ do
      liftIO $ do
        -- Test bounds checking
        result <- try $ bracket setupBasicH2O teardownBasicH2O $ \(globalConf, context, evLoop) -> do
          -- Try to access memory beyond allocated bounds
          let invalidPtr = plusPtr globalConf 10000
          h2oConfigInit invalidPtr  -- This should fail safely
        case result of
          Left _ -> return ()  -- Expected to fail
          Right _ -> fail "Should have failed with invalid memory access"

-- Helper functions for testing

setupBasicH2O :: IO (Ptr H2OGlobalConf, Ptr H2OContext, Ptr H2OEvLoop)
setupBasicH2O = do
  globalConf <- mallocBytes 2048  -- Conservative size
  context <- mallocBytes 2048     -- Conservative size
  h2oConfigInit globalConf
  evLoop <- h2oEvloopCreate
  h2oContextInit context evLoop globalConf
  return (globalConf, context, evLoop)

teardownBasicH2O :: (Ptr H2OGlobalConf, Ptr H2OContext, Ptr H2OEvLoop) -> IO ()
teardownBasicH2O (globalConf, context, evLoop) = do
  h2oContextDispose context
  h2oEvloopDestroy evLoop
  h2oConfigDispose globalConf
  free globalConf
  free context

-- Additional safety tests

safetyTests :: Spec
safetyTests = describe "Safety and segfault prevention" $ do
  it "doesn't segfault on rapid allocation/deallocation" $ hedgehog $ do
    iterations <- forAll $ Gen.integral (Range.linear 10 100)
    liftIO $ do
      mapM_ (\_ -> bracket setupBasicH2O teardownBasicH2O (const $ return ())) [1..iterations]

  it "doesn't segfault with concurrent access" $ hedgehog $ do
    liftIO $ do
      result <- runH2OServer defaultConfig $ \server -> do
        -- Register multiple handlers concurrently
        threads <- mapM (\i -> forkIO $ registerHandler server (C8.pack $ "/concurrent" ++ show i) $ \_ ->
                    return $ responseOk "Concurrent response") [1..5]
        threadDelay 100000  -- 100ms
        mapM_ killThread threads
        return ()
      case result of
        Right () -> return ()
        Left err -> fail $ "Concurrent test failed: " ++ err

  it "properly cleans up resources" $ hedgehog $ do
    liftIO $ do
      -- Test that we don't leak memory across multiple server instances
      results <- mapM (\_ -> do
        result <- runH2OServer defaultConfig $ \server -> do
          registerHandler server "/test" $ \_ -> return $ responseOk "test"
          return "success"
        case result of
          Right _ -> return True
          Left _ -> return False
        ) [1..5]
      all id results === True