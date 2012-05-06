#!/bin/bash
ulimit -n 65536
cd build
JAVA_OPTS="-agentpath:/home/ubuntu/yjp/bin/linux-x86-64/libyjpagent.so -Xss512k -Xms5G -Xmx5G" nohup scala MainServerStart &
