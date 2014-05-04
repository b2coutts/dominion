#!/bin/bash
# connect to a running server instance

if [ "$#" -lt 2 ]; then echo "Usage: $0 PID username"; exit 1; fi

tosrv="/tmp/hsdom_$1/${2}_tosrv"
tousr="/tmp/hsdom_$1/${2}_tousr"

if [ ! -r "$tousr" ]; then
    echo "ERROR: $tousr does not exist, or is not readable"
    exit 2
elif [ ! -w "$tosrv" ]; then
    echo "ERROR: $tosrv does not exist, or is not writable"
    exit 3
fi

cat $tousr &
while read -r line; do printf "%s\n" "$line"; done > $tosrv
