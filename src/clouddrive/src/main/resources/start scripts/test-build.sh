#!/bin/bash
source ~/.bash_profile
clear
rm -rf build/*
scalac -explaintypes -d build config/*.scala
scala -explaintypes -d build exceptional/*.scala
scalac -explaintypes -d build utils/*.scala
scalac -explaintypes -d build clouddrive/httpsupport/*.scala
scalac -explaintypes -d build control/*.scala
scalac -explaintypes -d build providers/*.scala
scalac -explaintypes -d build providers/AWS/AWS.scala
scalac -explaintypes -d build providers/AWS/S3.scala
scalac -explaintypes -d build providers/AWS/S3FileSystem.scala
scalac -explaintypes -d build providers/Filesystem/*.scala
scalac -explaintypes -d build pipes/*.scala
scalac -explaintypes -d build pipes/webdavcmds/*.scala
scalac -explaintypes -d build clouddrive/*.scala
scalac -explaintypes -d build setup/*.scala
scalac -explaintypes -d build *.scala
scalac -explaintypes -d build experiments/*.scala
cp test-config.txt build/config.txt
