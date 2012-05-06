#!/bin/sh

export port=$(expr $(cat base_port) + 2)

echo "Starting the 'developement' Jetty process at port $port"

nohup java -Xmx$(cat ram_size) -Drun.mode=Development -Djetty.port=$port -jar start.jar etc/jetty.xml >> logs/std.out 2>> logs/err.out &
echo $! > development.pid
echo "The PID is $!"
