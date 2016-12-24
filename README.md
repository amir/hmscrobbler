# MPlayer Scrobbler
## Why
I bought a Sennheiser HD 380 PRO, then a Schiit Asgard 2 and the next thing I know I was purchasing music via Bandcamp and downloading FLAC encoded version of them—I know. I found MPlayer the best way to listen to them but I was missing the ability to scrobble what I was listening to. This project helps in this regard.

## How
MPlayer has a slave mode in which you can communicate with using a named pipe. Using the named pipe you can ask MPlayer what file it's playing. Unfortunately MPlayer's answer is a relative path so we need to go through all file descriptors owned by the MPlayer process and substring match to find the absolute path. The absolute path will then be sent to a Vorbis parser to extract comments—e.g., artist, title and album.

## Build
`stack build` and you should be good to go.

## Run
I use a bash script like this—assuming you've copied `hmscrobbler-exe` to your PATH:
```bash
#!/bin/bash

set -e

TEMPDIR=$(mktemp -d)
INPUT="$TEMPDIR/input"
OUTPUT="$TEMPDIR/output"

function clean_up {
  rm -rf $TEMPDIR
  exit 0
}

trap clean_up SIGINT SIGTERM

export LASTFM_KEY="YOUR_LASTFM_KEY"
export LASTFM_SECRET="YOUR_LASTFM_SECRET"

mkfifo $INPUT

mplayer -slave -quiet -input file=$INPUT "$@" > $OUTPUT &

hmscrobbler-exe $INPUT $OUTPUT
```
If all goes well, a few seconds after running the script you'll see a line like this:

    approve: http://www.last.fm/api/auth/?api_key=API_KEY&token=TOKEN

Open the link in your browser and allow access to the application. Then go back to the terminal and press Enter. That's it.
