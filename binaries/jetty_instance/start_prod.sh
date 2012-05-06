#!/bin/sh

export port=$(expr $(cat base_port) + 0)

echo "Starting the 'production' Jetty process at port $port"

nohup java -Xmx$(cat ram_size) -Drun.mode=production -Djetty.port=$port -jar start.jar etc/jetty.xml >> logs/std.out 2>> logs/err.out &
echo $! > prod.pid
echo "The PID is $!"
