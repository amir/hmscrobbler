module PidUtils
     (
         getAbsolutePath
     ) where

import System.Exit(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)
import Data.List(find, isSuffixOf)

pidFdsPath :: String -> String
pidFdsPath x = "/proc/" ++ x ++ "/fd"

absolutePath :: String -> [String] -> IO (Maybe String)
absolutePath basename candidates = return $ find (isSuffixOf basename) candidates

getAbsolutePath :: String -> String -> IO (Maybe String)
getAbsolutePath pname fname = do
  pid <- pidOf pname
  fds <- maybe (return Nothing) pidFds pid
  maybe (return Nothing) (absolutePath fname) fds

pidFds :: String -> IO (Maybe [String])
pidFds x = do
  (e, r, c) <- readProcessWithExitCode "ls" ["-l", p] []
  case e of
    ExitSuccess -> return $ Just (lines r)
    _           -> return Nothing
  where
    p = pidFdsPath x

pidOf :: String -> IO (Maybe String)
pidOf x = do
  (e, r, _) <- readProcessWithExitCode "pidof" [x] []
  case e of
    ExitSuccess -> return $ Just (head $ words r)
    _           -> return Nothing
