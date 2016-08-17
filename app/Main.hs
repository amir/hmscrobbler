module Main where

import qualified Data.ByteString.Lazy as BSL
import Data.Serialize.Get
import System.Environment(getArgs)
import GHC.IO.Handle.FD(openFile)
import System.IO(IOMode(ReadWriteMode))
import GHC.IO.Handle(hPutStr)

import Flac
import PidUtils

main :: IO ()
main = do
  (namedPipe:outputLog:_) <- getArgs
  handle <- openFile namedPipe ReadWriteMode
  hPutStr handle "get_property filename\n"
  print outputLog
