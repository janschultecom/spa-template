#!/bin/sh

#./server.sh & ./hotreload.sh & wait
#sh server.sh &
#reload() {
#  stack build --fast --file-watch-poll --exec 'echo "YEAH"; killall servant-example-exe'
#}
#
server() {
  while true; do echo 'Starting server'; stack exec servant-example-exe; echo 'Server stopped'; sleep 1; done
}
echo 'Starting server loop...'
server & # Start server loop
echo 'Starting file watcher...'
stack build --fast --file-watch-poll --exec 'killall servant-example-exe'

#server & reload & wait
