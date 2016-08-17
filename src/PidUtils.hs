module PidUtils
     (
         getAbsolutePath
     ) where

import System.Exit(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)
import Data.List(find, isSuffixOf)

pidFdsPath :: String -> [Char]
pidFdsPath x = "/proc/" ++ x ++ "/fd"

absolutePath :: String -> [String] -> Maybe String
absolutePath basename candidates = find (\x -> isSuffixOf basename x) candidates

getAbsolutePath :: String -> IO (Maybe String)
getAbsolutePath filename = do
  pid <- pidOf "mplayer"
  case pid of
    Just p -> do
      fds <- pidFds p
      case fds of
        Just f -> do
          return $ absolutePath filename f
        _ -> return Nothing
    _ -> return Nothing

pidFds :: String -> IO (Maybe [String])
pidFds x = do
  (e, r, c) <- readProcessWithExitCode "ls" ["-l", p] []
  case e of
    ExitSuccess -> return $ Just (lines r)
    _           -> return Nothing
  where
    p = pidFdsPath x

pidOf :: [Char] -> IO (Maybe String)
pidOf x = do
  (e, r, _) <- readProcessWithExitCode "pidof" [x] []
  case e of
    ExitSuccess -> return $ Just (head $ words r)
    _           -> return Nothing
