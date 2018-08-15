#!/bin/sh

server() {
  while true; do echo 'Starting server'; stack exec servant-example-exe; echo 'Server stopped'; sleep 1; done
}
echo 'Starting server loop...'
server & 

echo 'Starting file watcher...'
stack build --fast --file-watch-poll --exec 'killall servant-example-exe'
