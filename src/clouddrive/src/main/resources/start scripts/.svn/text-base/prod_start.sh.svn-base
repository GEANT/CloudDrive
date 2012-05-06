#!/bin/bash
ulimit -n 65536
cd build
JAVA_OPTS="-Xss512k -Xmx6G -Xms6G -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:-UseGCOverheadLimit" nohup scala MainServerStart &
