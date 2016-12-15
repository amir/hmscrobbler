module Pid
     (
         getAbsolutePath
       , linkToPath
     ) where

import Data.List(find, isSuffixOf)
import Text.ParserCombinators.Parsec
import System.Exit(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)

pidFdsPath :: String -> String
pidFdsPath x = "/proc/" ++ x ++ "/fd"

linkToPath' = manyTill anyChar (try (string "-> ")) >> many (noneOf "\n")
linkToPath = parse linkToPath' ""

absolutePath :: String -> [String] -> IO (Maybe String)
absolutePath basename candidates =
  case find (isSuffixOf basename) candidates of
    Just e ->
      case linkToPath e of
        Right r -> return $ Just r
        Left  _ -> return Nothing
    Nothing -> return Nothing

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
