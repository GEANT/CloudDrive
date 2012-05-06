#!/bin/sh

export port=$(expr $(cat base_port) + 1)

echo "Starting the 'preview' Jetty process at port $port"

nohup java -Xmx$(cat ram_size) -Drun.mode=pilot -Djetty.port=$port -jar start.jar etc/jetty.xml >> logs/std.out 2>> logs/err.out &
echo $! > preview.pid
echo "The PID is $!"
