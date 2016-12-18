{-# LANGUAGE GADTs #-}
{-|
Module : GHCi.Remote.Client
Description : Client side libraries for the remote external interpreter
Copyright : (c) 2016, Obsidian Systems LLC
License : BSD3
Maintainer : shea@shealevy.com
Stability : experimental
-}
module GHCi.Remote.Client (clientMain, Loop, ConnectThen) where

import Data.IORef
import System.IO
import System.Environment
import System.Exit
import Control.Exception

import Data.Binary

import System.Posix.IO
import System.Posix.Types

import qualified Data.ByteString as BS

import GHCi.Message

import GHCi.Remote.LibMessage

-- | Value type yielded by the main serve loop
newtype Loop = Loop { unLoop :: () }

-- | Action to connect to the server then run the serve loop
type ConnectThen = (Pipe -> IO Loop) -> IO Loop

-- | The main loop of the client side of remote iserv
clientMain :: ConnectThen
           -> IO ()
clientMain connectThen = do
  args <- getArgs
  (localToFd, localFromFd) <-
    case args of
      arg0:arg1:[] -> return (read arg0, read arg1)
      _ -> do
        prog <- getProgName
        die $ "usage: " ++ prog ++ " <write-fd> <read-fd>"
  localFrom <- fdToHandle $ Fd localFromFd
  localTo <- fdToHandle $ Fd localToFd
  -- TODO: Figure out proper signal handling
  lo_ref <- newIORef Nothing
  let local = Pipe { pipeRead = localFrom
                   , pipeWrite = localTo
                   , pipeLeftovers = lo_ref
                   }
  unLoop <$> (connectThen $ serv local)

serv :: Pipe -> Pipe -> IO Loop
serv local remote = loop realWarn
  where
    warning = "Warning: DLL forwarding not implemented for remote iserv!\n" ++
      "Fails unless the remote machine has the DLL in the linker search path."

    realWarn = hPutStrLn stderr warning

    loop warn = do
      Msg msg <- readPipe local getMessage
      (res, warn') <- forward msg warn
      writePipe local $ put res
      next msg warn'

    forward :: (Binary a) => Message a -> IO () -> IO (a, IO ())
    forward m@(LoadDLL _) warn = do
      warn
      res <- forwardMessage remote m
      return (res, return ())
    forward m@(LoadArchive p) warn = do
      res <- forwardLibMessage local remote p m
      return (res, warn)
    forward m@(LoadObj p) warn = do
      res <- forwardLibMessage local remote p m
      return (res, warn)
    forward m@(RunTH _ _ _ _) warn = do
      res <- forwardTHMessage local remote m
      return (res, warn)
    forward m@(RunModFinalizers _ _) warn = do
      res <- forwardTHMessage local remote m
      return (res, warn)
    forward m warn = do
      res <- forwardMessage remote m
      return (res, warn)

    next :: Message a -> IO () -> IO Loop
    next Shutdown _ = return $ Loop ()
    next _ warn = loop warn

forwardMessage :: (Binary a) => Pipe -> Message a -> IO a
forwardMessage remote msg = do
  writePipe remote $ putMessage msg
  readPipe remote get

forwardLibMessage :: (Binary a)
                  => Pipe
                  -> Pipe
                  -> String
                  -> Message a
                  -> IO a
forwardLibMessage local remote path msg = do
  writePipe remote $ putMessage msg
  last <- bracket (openFile path ReadMode) hClose $ \file -> do
    let sz = 8192 -- Profile?
    let loop bs = do
          bs' <- BS.hGet file sz
          if bs' == BS.empty
            then return bs
            else do
              writePipe remote . put $ Chunk bs
              loop bs'
    BS.hGet file sz >>= loop
  writePipe remote . put $ Done last
  readPipe remote get

forwardTHMessage :: (Binary a)
                 => Pipe
                 -> Pipe
                 -> Message a
                 -> IO a
forwardTHMessage local remote msg = do
    writePipe remote $ putMessage msg
    loop
    readPipe remote get
  where
    loop :: IO ()
    loop = do
      THMsg msg <- readPipe remote getTHMessage
      res <- fwd local msg
      writePipe remote $ put res
      case msg of
        RunTHDone -> return ()
        _ -> loop

    fwd :: (Binary a) => Pipe -> THMessage a -> IO a
    fwd local msg = do
      writePipe local $ putTHMessage msg
      readPipe local get
