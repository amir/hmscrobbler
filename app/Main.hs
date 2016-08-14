module Main where

import qualified Data.ByteString.Lazy as BSL
import Data.Serialize.Get

import Lib

main :: IO ()
main = do
  input <- BSL.getContents
  case runGetLazy vorbisComments input of
    Left  e -> print e
    Right r -> print $ runGetLazy getVorbisComments (blockData $ head r)
