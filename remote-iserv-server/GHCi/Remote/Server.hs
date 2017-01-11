{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-|
Module : GHCi.Remote.Server
Description : Server side libraries for the remote external interpreter
Copyright : (c) 2016, Obsidian Systems LLC
License : BSD3
Maintainer : shea@shealevy.com
Stability : experimental
-}
module GHCi.Remote.Server (serve) where

import Control.Monad
import Control.Exception
import System.IO

import Control.DeepSeq

import Data.Binary

import Data.ByteString

import System.FilePath

import System.Directory

import System.IO.Temp

import GHCi.Message
import GHCi.Run
import GHCi.TH

import GHCi.Remote.LibMessage

-- Largely copied frim iserv/Main.hs from ghc, should merge these somehow...
-- | Serve the remote iserv protocol
serve :: Pipe -- ^ Connection to the client
      -> FilePath -- ^ Directory where we can store uploaded libraries
      -> IO ()
serve pipe libdir = withSystemTempDirectory "iserv-libs" loop
  where
    loop tmpdir = do
      Msg msg <- readPipe pipe getMessage
      case msg of
        Shutdown -> return ()
        RunTH st q ty loc -> wrapRunTH tmpdir $ runTH pipe st q ty loc
        RunModFinalizers st qrefs ->
          wrapRunTH tmpdir $ runModFinalizerRefs pipe st qrefs
        LoadArchive p -> do
          let p' = libdir ++ p
          getLibMessage p'
          run (LoadArchive p') >>= reply tmpdir
        LoadObj p -> do
          let p' = tmpdir ++ p
          getLibMessage p'
          run (LoadObj p') >>= reply tmpdir
        _ -> run msg >>= reply tmpdir

    reply :: (Binary a) => FilePath -> a -> IO ()
    reply tmpdir r = do
      writePipe pipe $ put r
      loop tmpdir

    wrapRunTH :: forall a. (Binary a, Show a)
              => FilePath
              -> IO a
              -> IO ()
    wrapRunTH tmpdir io = do
      r <- try io
      writePipe pipe $ putTHMessage RunTHDone
      case r of
        Left e
          | Just (GHCiQException _ err) <- fromException e  -> do
              reply tmpdir (QFail err :: QResult a)
          | otherwise -> do
              str <- showException e
              reply tmpdir (QException str :: QResult a)
        Right a -> do
          reply tmpdir (QDone a)

    showException :: SomeException -> IO String
    showException e0 = do
      r <- try $ evaluate (force (show (e0::SomeException)))
      case r of
        Left e -> showException e
        Right str -> return str

    getLibMessage :: FilePath -> IO ()
    getLibMessage p = do
        createDirectoryIfMissing True $ takeDirectory p
        needsLib <- not <$> doesFileExist p
        writePipe pipe $ put needsLib
        when needsLib $ bracket (openFile p WriteMode) hClose loop'
      where
        loop' file = do
          msg <- readPipe pipe get
          case msg of
            Chunk bs -> hPut file bs >> loop' file
            Done bs -> hPut file bs
