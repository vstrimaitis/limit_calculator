#!/bin/bash
set -e
cd LimitCalc
stack build
pid=$(pgrep LimitServer || echo "")
if [ -n "$pid" ]; then kill -9 $pid; fi
stack exec LimitServer 80 &
cd ..
