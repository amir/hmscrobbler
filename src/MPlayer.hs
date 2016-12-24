module MPlayer
  (
      mplayerBin
    , getProperty
    , parseOutput
    , Output(..)
    , AnswerType(..)
  ) where

import Text.ParserCombinators.Parsec

data AnswerType = Path
                deriving (Eq, Show)

data Output = Answer AnswerType String
            | StartingPlayback
            | Noise
            deriving (Eq, Show)

mplayerBin :: String
mplayerBin = "mplayer"

getProperty :: String -> String
getProperty p = "get_property " ++ p

answerTypeParser :: GenParser Char st AnswerType
answerTypeParser = string "path" >> return Path

inputLines :: GenParser Char st [Output]
inputLines =
  do result <- many line
     eof
     return result

spi  =  try startingPlayback
    <|> try pairs
    <|> ignored

line :: GenParser Char st Output
line =
  do result <- spi
     eol
     return result

pairs :: GenParser Char st Output
pairs = do
  beginning
  k <- key
  char '='
  v <- value
  case k of
    Path -> return $ Answer Path v

ignored :: GenParser Char st Output
ignored = many (noneOf "\n") >> return Noise

beginning :: GenParser Char st String
beginning = optional (string "\n") *> string "ANS_"

startingPlayback :: GenParser Char st Output
startingPlayback =
  string "Starting playback..." >> return StartingPlayback

key :: GenParser Char st AnswerType
key = answerTypeParser

value :: GenParser Char st String
value = many (noneOf "\n")

eol :: GenParser Char st Char
eol = char '\n'

parseOutput :: String -> Either ParseError [Output]
parseOutput = parse inputLines ""
