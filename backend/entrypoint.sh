#!/bin/sh

# initial build
echo 'Starting initial haskell build... this may take a veeeery long time! Grab a coffee ☕'
stack clean build

server() {
  while true; do echo 'Starting server'; stack exec spa-template-exe; echo 'Server stopped'; sleep 1; done
}
echo 'Starting server loop...'
server &

echo 'Starting file watcher...'
stack build --fast --file-watch-poll --exec 'killall spa-template-exe'
