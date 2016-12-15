{-# LANGUAGE BangPatterns #-}

module Main where

import System.IO
import Data.IORef
import Control.Monad
import System.INotify
import Control.Concurrent
import Data.Serialize.Get
import System.Environment(getArgs)
import qualified Data.ByteString.Lazy as BSL

import Pid
import Flac
import MPlayer

getSize :: FilePath -> IO Integer
getSize file = do
  h <- openFile file ReadMode
  s <- hFileSize h
  hClose h
  return s

main :: IO ()
main = do
  (namedPipe:outputLog:_) <- getArgs
  chan    <- newChan
  size    <- getSize outputLog
  inotify <- initINotify
  sizeRef <- newIORef size

  wd <- addWatch inotify [Modify] outputLog $ \_ -> do
    h        <- openFile outputLog ReadMode
    lastSize <- readIORef sizeRef

    hSeek h AbsoluteSeek (pred lastSize)

    newSize     <- hFileSize h
    !newContent <- hGetContents h

    writeIORef sizeRef newSize
    writeChan chan newContent

  forever $ do
    content <- readChan chan
    case parseOutput content of
      Right (pair : _) -> do
        path <- getAbsolutePath mplayerBin $ snd pair
        case path of
          Just o ->
            case linkToPath o of
              Right abs -> do
                input <- BSL.readFile abs
                case runGetLazy vorbisComments input of
                  Left e -> print e
                  Right r -> print $ runGetLazy getVorbisComments (blockData $ head r)

          Nothing -> return ()
      Left e -> return ()

  removeWatch wd
