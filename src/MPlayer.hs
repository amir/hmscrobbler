module MPlayer
  (
      mplayerBin
    , parseOutput
  ) where

import Text.ParserCombinators.Parsec

mplayerBin :: String
mplayerBin = "mplayer"

inputLines :: GenParser Char st [(String, String)]
inputLines =
  do result <- many line
     eof
     return result

line :: GenParser Char st (String, String)
line =
  do result <- pairs
     eol
     return result

pairs :: GenParser Char st (String, String)
pairs =
  do beginning
     k <- key
     char '='
     v <- value
     return (k, v)

beginning = optional (string "\n") *> string "ANS_"

key :: GenParser Char st String
key = many (noneOf "=")

value :: GenParser Char st String
value = many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseOutput :: String -> Either ParseError [(String, String)]
parseOutput = parse inputLines ""
