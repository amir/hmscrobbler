{-# LANGUAGE OverloadedStrings #-}

module LastFM
  (
      scrobbleItem
    , getLastfmSession
  ) where

import Data.Aeson.Types
import Data.List (find)
import Data.Text (Text, pack)
import Data.Aeson hiding (json)
import qualified Data.List.NonEmpty as NEL
import Data.Int (Int64)

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
      lastfm conn . sign (Secret secret) $ scrobble (NEL.repeat (item <*> timestamp ts)) <*>
        apiKey key <*> sessionKey session <* json
    Nothing -> return $ Left $ LastfmBadResponse "die"

getLastfmSession :: Text -> Text -> IO (Maybe Text)
getLastfmSession key secret = withConnection $ \conn -> do
  to <- getLastfmToken key
  case to of
    Just t -> do
      let b = pack t
      putStrLn $ "approve: " ++ link (apiKey key <* token b)
      _ <- getChar
      r <- lastfm conn . sign (Secret secret) $ getSession <*> token b <*> apiKey key <* json
      case r of
        Right p ->
          return $ parseMaybe getSession' p
        Left e ->
          return Nothing
