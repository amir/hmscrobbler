{-# LANGUAGE OverloadedStrings #-}

module LastFM
  (
      scrobbleItem
    , getLastfmSession
  ) where

import System.IO
import Data.Aeson.Types
import Data.List (find)
import Data.Text (Text, pack)
import Data.Aeson hiding (json)
import Data.Int (Int64)
import System.Directory
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.IO as TIO

import Lastfm
import Lastfm.Track (item, scrobble)
import Lastfm.Response (Secret)
import Lastfm.Authentication (link, getSession, getToken)

import Flac

getToken' :: Value -> Parser String
getToken' o = parseJSON o >>= (.: "token")

getSession' :: Value -> Parser Text
getSession' o = parseJSON o >>= (.: "session") >>= (.: "key")

getLastfmToken :: Text -> IO (Maybe String)
getLastfmToken key = withConnection $ \conn -> do
  r <- lastfm conn $ getToken <*> apiKey key <* json
  case r of
    Right t ->
      return $ parseMaybe getToken' t
    Left e ->
      return Nothing

getSessionFile :: IO FilePath
getSessionFile = do
  c <- getXdgDirectory XdgConfig "hmscrobbler"
  createDirectoryIfMissing False c
  return $ c ++ "/lastfm_session"

writeSession :: Text -> IO ()
writeSession s = do
  f <- getSessionFile
  o <- openFile f WriteMode
  TIO.hPutStr o s
  hClose o

readSession :: IO (Maybe Text)
readSession = do
  f <- getSessionFile
  e <- doesFileExist f
  if e then do
    co <- TIO.readFile f
    return $ Just co
  else return Nothing

getValue :: VorbisCommentItem -> Text
getValue (VorbisCommentItem _ v) = v

getItem :: [VorbisCommentItem] -> Text -> Maybe Text
getItem cs i = do
  a <- find (\(VorbisCommentItem k v) -> k == i) cs
  Just $ getValue a

vorbisCommentsToScrobble :: VorbisComments -> Maybe (Request f (Timestamp -> Scrobble))
vorbisCommentsToScrobble (VorbisComments _ comments) = do
  ar <- getItem comments "ARTIST"
  tl <- getItem comments "TITLE"
  al <- getItem comments "ALBUM"

  Just $ item <*> artist ar <*> track tl <* album al

scrobbleItem :: VorbisComments -> Text -> Text -> Text -> Int64 -> IO (Either LastfmError Value)
scrobbleItem cs key secret session ts =
  case vorbisCommentsToScrobble cs of
    Just item -> withConnection $ \conn ->
      lastfm conn . sign (Secret secret) $ scrobble (NEL.fromList [item <*> timestamp ts]) <*>
        apiKey key <*> sessionKey session <* json
    Nothing -> return $ Left $ LastfmBadResponse "unable to extract metadata"

getLastfmSession :: Text -> Text -> IO (Maybe Text)
getLastfmSession key secret = do
  s <- readSession
  case s of
    Just se -> return $ Just se
    Nothing -> getLastfmSession' key secret

getLastfmSession' :: Text -> Text -> IO (Maybe Text)
getLastfmSession' key secret = withConnection $ \conn -> do
  to <- getLastfmToken key
  case to of
    Just t -> do
      let b = pack t
      putStrLn $ "approve: " ++ link (apiKey key <* token b)
      _ <- getChar
      r <- lastfm conn . sign (Secret secret) $ getSession <*> token b <*> apiKey key <* json
      case r of
        Right p ->
          case parseMaybe getSession' p of
            Just s -> do
              writeSession s
              return $ Just s
            Nothing -> return Nothing
        Left e ->
          return Nothing
