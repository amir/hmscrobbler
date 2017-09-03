{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Data.IORef
import Control.Monad
import System.INotify
import Control.Concurrent
import System.Environment (getArgs, getEnv)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text, pack)

import Pid
import Flac
import LastFM
import MPlayer

getSize :: FilePath -> IO Integer
getSize file = do
  h <- openFile file ReadMode
  s <- hFileSize h
  hClose h
  return s

fromEnv :: String -> IO Text
fromEnv key = do
  v <- getEnv key
  pure $ pack v

offset :: Integer -> Integer
offset i
  | i > 0 = pred i
  | otherwise = i

main :: IO ()
main = do
  (namedPipe:outputLog:_) <- getArgs
  chan         <- newChan
  size         <- getSize outputLog
  inotify      <- initINotify
  sizeRef      <- newIORef size
  lastfmKey    <- fromEnv "LASTFM_KEY"
  lastfmSecret <- fromEnv "LASTFM_SECRET"
  session      <- getLastfmSession lastfmKey lastfmSecret

  wd <- addWatch inotify [Modify] outputLog $ \_ -> do
    h        <- openFile outputLog ReadMode
    lastSize <- readIORef sizeRef

    hSeek h AbsoluteSeek (offset lastSize)

    newSize     <- hFileSize h
    !newContent <- hGetContents h

    writeIORef sizeRef newSize
    writeChan chan newContent

  forever $ do
    content <- readChan chan
    case parseOutput content of
      Right outputs ->
        forM_ outputs $ \output ->
          case output of
            Answer Path p -> do
              path <- getAbsolutePath mplayerBin p
              case path of
                Just abs -> do
                  comments <- getFileVorbisComments abs
                  case comments of
                    Right cs ->
                      case session of
                        Just s -> do
                          ts <- round `fmap` getPOSIXTime
                          g  <- scrobbleItem cs lastfmKey lastfmSecret s ts
                          pure ()
                        Nothing ->
                          putStrLn "unable to scrobble. no last.fm session found."
                    Left _ ->
                      putStrLn $ "unable to extract vorbis comments from " ++ abs

                Nothing ->
                  putStrLn $ "unable to locate " ++ p

            StartingPlayback -> do
              pipeHandle <- openFile namedPipe ReadWriteMode
              hPutStrLn pipeHandle $ getProperty "path"
              hClose pipeHandle

            Noise -> pure ()

      Left e -> pure ()

  removeWatch wd
